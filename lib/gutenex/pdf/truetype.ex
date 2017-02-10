defmodule Gutenex.PDF.OpenTypeFont do
  use GenServer
  alias Gutenex.PDF.TrueType

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end
  def init(:ok) do
    {:ok, TrueType.new()}
  end

  # read in and parse a TTF or OTF file
  def parse(pid, filename) do
    GenServer.cast(pid, {:parse, filename})
    pid
  end

  # layout a run of text
  # send back the glyphs and positioning data
  def layout(pid, text) do
    GenServer.call(pid, {:layout, text})
  end

  # get the font structure (for additional analysis)
  def font_structure(pid) do
    GenServer.call(pid, :ttf)
  end

  def handle_cast({:parse, filename}, ttf) do
    parsed = TrueType.parse(ttf, filename)
    {:noreply, parsed}
  end
  def handle_call(:ttf, _from, ttf) do
    {:reply, ttf, ttf}
  end
  def handle_call({:layout, text}, _from, ttf) do
    output = TrueType.layout_text(ttf, text)
    {:reply, output, ttf}
  end
end

defmodule Gutenex.PDF.TrueType do
  use Bitwise, only_operators: true
  require Logger

  # empty structure with sensible defaults
  def new do
    %{
      :version => 0, :tables => [], :name => nil, :bbox => [],
      :ascent => 0, :descent => 0, :capHeight => 0, :unitsPerEm => 0,
      :usWeightClass => 500, :stemV => 0, :italicAngle => 0, :flags => 0,
      :glyphWidths => [], :defaultWidth => 0,
      "SubType" => {:name, "Type0"}, :embed => nil,
      :cid2gid => %{}, 
      :gid2cid => %{}, 
      :substitutions => nil,
      :positions => nil
    }
  end

  # entry point for parsing a file
  # TODO: we should probably take raw data
  # instead (or in addition to?) a filename
  # see what Elixir best practice is
  def parse(ttf, filename) do
    f = File.open!(filename)
    data = IO.binread f, :all
    ttf
    |> extractVersion(data)
    |> readHeader(data)
    |> extractName(data)
    |> extractMetrics(data)
    |> markEmbeddedPart(data)
    |> extractCMap(data)
    |> extractFeatures(data)
  end

  # returns a list of glyphs and positioning information
  def layout_text(ttf, text, features \\ ["liga", "kern"]) do
    glyphs = text
    |> String.to_charlist
    |> Enum.map(fn(cid) -> Map.get(ttf.cid2gid, cid, 0) end)

    # see spec for required, never disabled, and recommended
    # features
    # TODO: have a way to set or detect these
    #TODO: pass in (or detect) script/lang combo
    script = "latn"
    lang = nil
    #DEBUG
    features = features ++ ["frac"]

    glyphs
    |> handle_substitutions(ttf, script, lang, features)
    |> position_glyphs(ttf, script, lang, features)
  end

  defp handle_substitutions(glyphs, ttf, script, lang, active_features) do
    # use data in GSUB to do any substitutions
    {subS, subF, subL} = ttf.substitutions
    # see spec for required, never disabled, and recommended
    #availFeatures = subS[script][lang]
    #           |> Enum.map(fn x -> Enum.at(subF, x) end)
    #           |> Enum.map(fn {tag, _} -> tag end)
    #           |> Enum.uniq
    #IO.inspect availFeatures
    # combine indices, apply in order given in LookupList table
    lookups = subS[script][lang]
               |> Enum.map(fn x -> Enum.at(subF, x) end)
               |> Enum.filter_map(fn {tag, _} -> tag in active_features end, fn {_, l} -> l end)
               |> List.flatten
               |> Enum.sort
    # apply the lookups
    Enum.reduce(lookups, glyphs, fn (x, acc) -> applyLookupGSUB(Enum.at(subL, x), acc) end)
  end

  # ==============================================
    # GSUB glyph substitutions
    # Used for ligatures, swashes, alternate forms
    # if a lookup type is as-yet unsupported
    # simply passes through the input
  # ==============================================
  # GSUB 1 -- single substitution (one-for-one)
  defp applyLookupGSUB({1, _flag, table}, glyphs) do
    <<format::16, covOff::16, rest::binary>> = table
    coverage = parseCoverage(binary_part(table, covOff, byte_size(table) - covOff))
    replace = case format do
      1 ->
        <<delta::16, _::binary>> = rest
        fn g -> applySingleSub(g, coverage, delta) end
      2 -> 
        <<nGlyphs::16, ga::binary-size(nGlyphs)-unit(16), _::binary>> = rest
        replacements = for << <<x::16>> <- ga >>, do: x
        fn g -> applySingleSub(g, coverage, replacements) end
      _ ->
        Logger.debug "Unknown GSUB 1 subtable format #{format}"
        fn g -> g end
    end
    Enum.map(glyphs, replace)
  end
  #GSUB 2 - multiple substitution (expand one glyph into several
  defp applyLookupGSUB({2, _flag, _table}, glyphs) do
    Logger.debug "GSUB 2 - multiple substitution"
    glyphs
  end
  # GSUB type 3 -- alternate substitution
  defp applyLookupGSUB({3, _flag, _offsets, table}, glyphs) do
    <<1::16, covOff::16, nAltSets::16, aoff::binary-size(nAltSets)-unit(16), _::binary>> = table
    coverage = parseCoverage(binary_part(table, covOff, byte_size(table) - covOff))
    # alternate set tables
    altOffsets = for << <<x::16>> <- aoff >>, do: x
    alts = Enum.map(altOffsets, fn altOffset -> parseAlts(table, altOffset) end)
    Enum.map(glyphs, fn g -> applyRandomAlt(g, coverage, alts) end)
  end
  # GSUB type 4 -- ligature substition (single glyph replaces multiple glyphs)
  defp applyLookupGSUB({4, _flag, table}, input) do
    #parse ligature table
    <<1::16, covOff::16, nLigSets::16, lsl::binary-size(nLigSets)-unit(16), _::binary>> = table
    # ligature set tables
    ls = for << <<x::16>> <- lsl >>, do: x
    # coverage table
    coverage = parseCoverage(binary_part(table, covOff, byte_size(table) - covOff))
    # ligatures
    ligaOff = Enum.map(ls, fn lsOffset -> parseLigatureSet(table, lsOffset) end)
    applyLigature(coverage, ligaOff, input, []) 
  end
  defp applyLookupGSUB({5, _flag, _table}, glyphs) do
    Logger.debug "GSUB 5 - contextual substitution"
    glyphs
  end
  defp applyLookupGSUB({6, _flag, _table}, glyphs) do
    Logger.debug "GSUB 6 - chaining substitution"
    glyphs
  end
  
  #unhandled type; log and leave input untouched
  defp applyLookupGSUB({type, _flag, _table}, glyphs) do
    Logger.debug "Unknown GSUB lookup type #{type}"
    glyphs
  end

  # GSUB type 7 -- extended table
  defp applyLookupGSUB({7, flag, offsets, table}, glyphs) do
    subtables = offsets 
            |> Enum.map(fn x -> 
            <<1::16, lt::16, off::32>> = binary_part(table, x, 8)
            {lt, binary_part(table, x + off, byte_size(table) - (x + off))}
              end)
    #for each subtable
    Enum.reduce(subtables, glyphs, fn ({type, tbl}, input) -> applyLookupGSUB({type, flag, tbl}, input) end) 
  end
  defp applyLookupGSUB({type, flag, offsets, table}, glyphs) do
    #for each subtable
    Enum.reduce(offsets, glyphs, fn (offset, input) -> applyLookupGSUB({type, flag, binary_part(table, offset, byte_size(table) - offset)}, input) end) 
  end

   
  # parse coverage tables
  defp parseCoverage(<<1::16, nrecs::16, glyphs::binary-size(nrecs)-unit(16), _::binary>>) do
    for << <<x::16>> <- glyphs >>, do: x
  end
  defp parseCoverage(<<2::16, nrecs::16, ranges::binary-size(nrecs)-unit(48), _::binary>>) do
    for << <<startg::16, endg::16, covindex::16>> <- ranges >>, do: {startg, endg, covindex}
  end

  # given a glyph, find out the coverage index (can be nil)
  defp findCoverageIndex(cov, g) when is_integer(hd(cov)) do
    Enum.find_index(cov, fn i -> i == g end)
  end
  defp findCoverageIndex(cov, g) when is_tuple(hd(cov)) do
    r = Enum.find(cov, fn {f,l,_} -> f <= g and g <= l end)
    if r != nil do
      {s,_,i} = r
      i + g - s
    else
      nil
    end
  end
  # catch-all
  defp findCoverageIndex(_cov, _g) do
    nil
  end

  defp parseAlts(table, altOffset) do
    <<nAlts::16, alts::binary-size(nAlts)-unit(16), _::binary>> = binary_part(table, altOffset, byte_size(table) - altOffset)
    for << <<x::16>> <- alts >>, do: x
  end

  defp parseLigatureSet(table, lsOffset) do
    <<nrecs::16, ligat::binary-size(nrecs)-unit(16), _::binary>> = binary_part(table, lsOffset, byte_size(table) - lsOffset)
    ligaOff = for << <<x::16>> <- ligat >>, do: x
    ligaOff
    |> Enum.map(fn x -> binary_part(table, lsOffset + x, byte_size(table) - (lsOffset + x)) end)
    |> Enum.map(fn <<g::16, nComps::16, rest::binary>> ->  {g, nComps-1, rest} end)
    |> Enum.map(fn {g, n, data} -> 
      <<recs::binary-size(n)-unit(16), _::binary>> = data
      gg = for << <<x::16>> <- recs >>, do: x
      {g, gg}
    end)
  end

  defp applySingleSub(g, coverage, delta) when is_integer(delta) do
    coverloc = findCoverageIndex(coverage, g)
    if coverloc != nil, do: g + delta, else: g
  end
  defp applySingleSub(g, coverage, replacements) do
    coverloc = findCoverageIndex(coverage, g)
    if coverloc != nil, do: Enum.at(replacements, coverloc), else: g
  end
  defp applyRandomAlt(g, coverage, alts) do
    coverloc = findCoverageIndex(coverage, g)
    if coverloc != nil do 
      candidates = Enum.at(alts, coverloc)
      Enum.random(candidates)
    else
      g
    end
  end
  defp applyLigature(_coverage, _ligatures, [], output), do: output
  defp applyLigature(coverage, ligatures, [g | glyphs], output) do
    # get the index of a ligature set that might apply
    coverloc = findCoverageIndex(coverage, g)
    {output, glyphs} = if coverloc != nil do
      # find first match in this ligature set (if any)
      lig = Enum.find(Enum.at(ligatures, coverloc), fn {_replacement, match} -> Enum.take(glyphs, length(match)) == match end)
      if lig != nil do
        # replace the current glyph
        {rep, m} = lig
        # skip over any matched glyphs
        # TODO: handle flags correctly!!
        remaining = Enum.slice(glyphs, length(m), length(glyphs))
        {output ++ [rep], remaining}
      end
    else
      {output ++ [g], glyphs}
    end
    applyLigature(coverage, ligatures, glyphs, output)
  end

  defp position_glyphs(glyphs, ttf, script, lang, active_features) do
    # initially just use glyph width as xadvance
    # this is sufficient if kerning information is missing
    positions = glyphs
                |> Enum.map(fn g -> Enum.at(ttf.glyphWidths, g, ttf.defaultWidth) end)
                |> Enum.map(fn advance -> {0, 0, advance, 0} end)

    #TODO: if no GPOS, fallback to kern table
    #
    # use data in the GPOS and BASE table
    # to kern, position, and join
    {scripts, features, lookups} = ttf.positions
    # each feature provides lookup indices
    # combine indices, apply in order given in LookupList table
    # for each lookup, apply to each glyph in order
    indices = scripts[script][lang]
               |> Enum.map(fn x -> Enum.at(features, x) end)
               |> Enum.filter_map(fn {tag, _} -> tag in active_features end, fn {_, l} -> l end)
               |> List.flatten
               |> Enum.sort
    # apply the lookups
    Enum.reduce(indices, {glyphs, positions}, fn (x, acc) -> applyLookupGPOS(Enum.at(lookups, x), acc) end)
  end

  #add two positions together, treat nils as zeroed structure
  defp addPos(p, nil), do: p
  defp addPos({a,b,c,d}, {e,f,g,h}), do: {a+e, b+f, c+g, d+h}

  # type 9 - when 32-bit offsets are used for subtables
  defp applyLookupGPOS({9, flag, offsets, table}, {glyphs, pos}) do
    subtables = offsets 
            |> Enum.map(fn x -> 
            <<1::16, actual_type::16, off::32>> = binary_part(table, x, 8)
            {actual_type, binary_part(table, x + off, byte_size(table) - (x + off))}
              end)
    #for each subtable
    Enum.reduce(subtables, {glyphs, pos}, fn ({type, tbl}, input) -> applyLookupGPOS({type, flag, tbl}, input) end) 
  end

  # all other types 
  defp applyLookupGPOS({type, flag, offsets, table}, {glyphs, pos}) do
    #for each subtable
    Enum.reduce(offsets, {glyphs, pos}, fn (offset, input) -> applyLookupGPOS({type, flag, binary_part(table, offset, byte_size(table) - offset)}, input) end) 
  end

  # type 2 - pair positioning (ie, kerning)
  defp applyLookupGPOS({2, _flag, table}, {glyphs, pos}) do
    #IO.puts "GPOS kern(2)"
    <<fmt::16, covOff::16, record1::16, record2::16, rest::binary>> = table
    kerning = case fmt do
      1 ->
        # FMT 1 - identifies individual glyphs
        # pair set table
        <<nPairs::16, pairOff::binary-size(nPairs)-unit(16), _::binary>> = rest
        pairsetOffsets = for << <<x::16>> <- pairOff >>, do: x
        # coverage table
        coverage = parseCoverage(binary_part(table, covOff, byte_size(table) - covOff))
        # parse the pair sets
        pairSets = Enum.map(pairsetOffsets, fn off -> parsePairSet(table, off, record1, record2) end)
        applyKerning(coverage, pairSets, glyphs, [])
      2 ->
        #FMT 2
        # offset to classdef, offset to classdef
        # nClass1Records, nClass2Records
        <<class1Off::16, class2Off::16, nClass1Records::16, nClass2Records::16, records::binary>> = rest
        IO.puts "kern table format #{fmt}"
        #IO.puts "Class1 #{nClass1Records} Class2 #{nClass2Records}"

        #read in the class definitions
        # fmt==1, startglyph, nglyphs, array of ints (each int is a class)
        # fmt==2, nRanges, {startGlyph, endGlyph, class}

        #read in the actual positioning pairs
        sizeA = valueRecordSize(record1)
        sizeB = valueRecordSize(record2)
        class2size = sizeA + sizeB
        class1size = nClass2Records * class2size
        c1recs = binary_part(records, 0, nClass1Records * class1size)
        c1Recs = for << <<c2recs::binary-size(class1size)>> <- c1recs >>, do: c2recs
        pairSets = Enum.map(c1Recs, fn c2recs ->
          c2Recs = for << <<c2Rec::binary-size(class2size)>> <- c2recs>>, do: c2Rec 
          c2Recs
          |> Enum.map(fn c2Rec -> for << <<v1::binary-size(sizeA), v2::binary-size(sizeB)>> <- c2Rec >>, do: {v1, v2} end)
          |> Enum.map(fn [{v1, v2}] -> {readPositioningValueRecord(record1, v1), readPositioningValueRecord(record2, v2)} end)
        end)

        #apply the kerning
        #classify both glyphs
        #get pairSet[c1][c2]
        #position

        #temporary - produce no kerns
        Enum.map(pos, fn _ -> nil end)
    end
    positioning = Enum.zip(pos, kerning) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end
  #unhandled type; log and leave input untouched
  defp applyLookupGPOS({type, _flag, _table}, {glyphs, pos}) do
    IO.puts "Unknown GPOS lookup type #{type}"
    {glyphs, pos}
  end

  defp applyKerning(_coverage, _pairSets, [], output), do: output
  defp applyKerning(_coverage, _pairSets, [_], output), do: output ++ [nil]
  defp applyKerning(coverage, pairSets, [g | glyphs], output) do
    # get the index of a pair set that might apply
    coverloc = findCoverageIndex(coverage, g)
    {output, glyphs} = if coverloc != nil do
      pairSet = Enum.at(pairSets, coverloc)
      nextChar = hd(glyphs)
      pair = Enum.find(pairSet, fn {g, _, _} -> g == nextChar end)
      if pair != nil do
        {_, v1, v2} = pair
        oo = output ++ [v1]
        IO.inspect {g, v1}
        if v2 != nil do
          {oo ++ [v2], tl(glyphs)}
        else
          {oo, glyphs}
        end
      else
        {output ++ [nil], glyphs}
      end
    else
      {output ++ [nil], glyphs}
    end
    applyKerning(coverage, pairSets, glyphs, output)
  end

  defp parsePairSet(table, offset, fmtA, fmtB) do
    sizeA = valueRecordSize(fmtA)
    sizeB = valueRecordSize(fmtB)
    data = binary_part(table, offset, byte_size(table) - offset)
    # valueRecordSize returns size in bytes
    pairSize = (2 + sizeA + sizeB)
    <<nPairs::16, pairdata::binary>> = data
    pairs = for << <<glyph::16, v1::binary-size(sizeA), v2::binary-size(sizeB)>> <- binary_part(pairdata, 0, pairSize * nPairs) >>, do: {glyph, v1, v2}
    pairs = pairs
      |> Enum.map(fn {g,v1,v2} -> {g, readPositioningValueRecord(fmtA, v1), readPositioningValueRecord(fmtB, v2)} end)
    pairs
  end
  # ValueRecord in spec
  defp readPositioningValueRecord(0, _), do: nil
  defp readPositioningValueRecord(format, bytes) do
    # format is bitset of fields to read for each records
    {xPlace, xprest} = extractValueRecordVal(format &&& 0x0001, bytes)
    {yPlace, yprest} = extractValueRecordVal(format &&& 0x0002, xprest)
    {xAdv, xarest} = extractValueRecordVal(format &&& 0x0004, yprest)
    {yAdv, _xyrest} = extractValueRecordVal(format &&& 0x0008, xarest)

    #TODO: also offsets to device table
    {xPlace, yPlace, xAdv, yAdv}
  end

  defp extractValueRecordVal(_flag, ""), do: {0, ""}
  defp extractValueRecordVal(flag, data) do
    if flag do
      <<x::signed-16, r::binary>> = data
      {x, r}
    else
      {0, data}
    end
  end

  defp valueRecordSize(format) do
    flags = for << x::1 <- <<format>> >>, do: x
    # record size is 2 bytes per set flag in the format spec
    Enum.count(flags, fn x -> x == 1 end) * 2
  end

  defp markEmbeddedPart(ttf, data) do
    raw_cff = rawTable(ttf, "CFF ", data)
    embedded = if raw_cff do
      raw_cff
    else
      data
    end
    %{ttf | :embed => embedded}
  end
  defp extractVersion(ttf, <<version :: size(32), data :: binary>>) do
    {%{ttf | :version => version}, data}
  end
  defp readHeader({%{version: 0x00010000}=ttf, data}, _full) do
    <<numTables::16, 
    _searchRange::16,
    _entrySelector ::16,
    _rangeShift ::16,
    remainder :: binary>> = data
    {tables, _} = readTables([], remainder, numTables)
    %{ttf | :tables => tables}
  end
  defp readHeader({%{version: 0x74727565}=ttf, data}, _full) do
    <<numTables::16, 
    _searchRange::16,
    _entrySelector :: size(16),
    _rangeShift :: size(16),
    remainder :: binary>> = data
    {tables, _} = readTables([], remainder, numTables)
    %{ttf | :tables => tables}
  end
  defp readHeader({%{version: 0x74727566}=ttf, data}, full_data) do
    #TODO: read in TTC header info, subfont 0
    <<_ttcVersion::32,
    numSubfonts::32, rem::binary>> = data
    #read in 32-bit subfont offsets
    {offsets, _remaining} = readOffset([], rem, numSubfonts)
    subfont = binary_part(full_data, offsets[0], byte_size(full_data)-offsets[0]) 
    <<_ttfVersion::32, numTables::16, 
    _searchRange::16,
    _entrySelector :: size(16),
    _rangeShift :: size(16),
    remainder :: binary>> = subfont
    #IO.puts "Subfont 0 has #{numTables} tables"
    {tables, _} = readTables([], remainder, numTables)
    %{ttf | :tables => tables}
  end
  defp readHeader({%{version: 0x4F54544F}=ttf, data}, _) do
    <<numTables::16, 
    _searchRange::16,
    _entrySelector :: size(16),
    _rangeShift :: size(16),
    remainder :: binary>> = data
    #IO.puts "Has #{numTables} tables"
    {tables, _} = readTables([], remainder, numTables)
    %{ttf | :tables => tables}
  end
  defp readHeader({ttf, _data}, _) do
    #IO.puts "TODO: unknown TTF version"
    ttf
  end
  defp readOffset(offsets, data, 0), do: {offsets, data}
  defp readOffset(offsets, <<offset::32, rem::binary>>, count) do
    readOffset([offset | offsets], rem, count-1)
  end
  defp readTables(tables, data, 0) do
    {tables, data}
  end
  defp readTables(tables, <<tag::binary-size(4), checksum::32, offset::32, length::32, data::binary>>, numTables) do
    #for each table
    table = %{name: tag, checksum: checksum, offset: offset, length: length}
    #4-char tag, checksum, offset, length
    readTables([table | tables], data, numTables-1)
  end
  defp extractName(ttf, data) do
    raw = rawTable(ttf, "name", data)
    <<_fmt::16, nRecords::16, strOffset::16, r::binary>> = raw
    #IO.puts "Name table format #{fmt}"
    recs = readNameRecords([], r, nRecords)
    # TODO: pick best supported platform/encoding
    # and just parse that one
    names = Enum.map(recs, fn(r)->recordToName(r, strOffset, raw) end)
    #prefer PS name
    name6 = case Enum.find(names, fn({id, _}) -> id == 6 end) do
      {_, val} -> val
      _ -> nil
    end
    name4 = case Enum.find(names, fn({id, _}) -> id == 4 end) do
      {_, val} -> val
      _ -> nil
    end
    name1 = case Enum.find(names, fn({id, _}) -> id == 1 end) do
      {_, val} -> val
      _ -> nil
    end
    psName = cond do
      name6 -> name6
      name4 -> name4
      name1 -> name1
      true -> "NO-VALID-NAME"
    end
    #replace spaces in psName with dashes
    #self.familyName = names[1] or psName
    #self.styleName = names[2] or 'Regular'
    #self.fullName = names[4] or psName
    #self.uniqueFontID = names[3] or psName
    %{ttf | name: psName}
  end
  defp scale(x, unitsPerEm) do
    x * 1000.0 / unitsPerEm
  end
  defp extractMetrics(ttf, data) do
            _ = """
            *flags        Font flags
            *ascent       Typographic ascender in 1/1000ths of a point
            *descent      Typographic descender in 1/1000ths of a point
            *capHeight    Cap height in 1/1000ths of a point (0 if not available)
            *bbox         Glyph bounding box [l,t,r,b] in 1/1000ths of a point
            *unitsPerEm   Glyph units per em
            *italicAngle  Italic angle in degrees ccw
            *stemV        stem weight in 1/1000ths of a point (approximate)
        
            defaultWidth   default glyph width in 1/1000ths of a point
            charWidths     dictionary of character widths for every supported UCS character
                           code
                           """

    raw_head = rawTable(ttf, "head", data)
    <<_major::16, _minor::16, _rev::32, _checksumAdj::32,
    0x5F, 0x0F, 0x3C, 0xF5, _flags::16, unitsPerEm::16, 
    _created::signed-64, _modified::signed-64,
    minx::signed-16, miny::signed-16, maxx::signed-16, maxy::signed-16,
    _macStyle::16, _lowestPPEM::16, _fontDirectionHint::signed-16,
    _glyphMappingFmt::signed-16, _glyphDataFmt::signed-16>> = raw_head

    bbox = Enum.map([minx, miny, maxx, maxy], fn(x) -> scale(x, unitsPerEm) end)

    raw_os2 = rawTable(ttf, "OS/2", data)
    measured = if raw_os2 do
      # https://www.microsoft.com/typography/otspec/os2.htm
      # match version 0 struct, extract additional fields as needed
      # usWidthClass = Condensed < Normal < Expanded
      # fsType = flags that control embedding
      # unicode range 1-4 are bitflags that identify charsets
      # selFlags = italic, underscore, bold, strikeout, outlined...
      # TODO: conform to fsType restrictions
      <<os2ver::16, avgCharWidth::signed-16, usWeightClass::16,
      usWidthClass::16, fsType::16,
      subXSize::signed-16,subYSize::signed-16,
      subXOffset::signed-16,subYOffset::signed-16,
      superXSize::signed-16,superYSize::signed-16,
      superXOffset::signed-16,superYOffset::signed-16,
      strikeoutSize::signed-16, strikeoutPos::signed-16,
      familyClass::signed-16, panose::80,
      unicodeRange1::32, unicodeRange2::32, unicodeRange3::32, unicodeRange4::32,
      vendorID::32, selFlags::16, firstChar::16, lastChar::16,
      typoAscend::signed-16,typoDescend::signed-16,
      typoLineGap::signed-16, winAscent::16, winDescent::16,
      v0rest::binary>> = raw_os2
      #IO.puts("OS/2 ver #{os2ver} found")
      ascent = scale(typoAscend, unitsPerEm)
      descent = scale(typoDescend, unitsPerEm)

      # os2ver 1 or greater has code page range fields
      v1rest = if os2ver > 0 do
        <<_cp1::32, _cp2::32, v1rest::binary>> = v0rest
        v1rest
      else
        nil
      end

      # if we have a v2 or higher struct we can read out
      # the xHeight and capHeight
      capHeight = if os2ver > 1 and v1rest do
        <<xHeight::signed-16, capHeight::signed-16, 
        defaultChar::16, breakChar::16, maxContext::16,
        v2rest::binary>> = v1rest
        scale(capHeight, unitsPerEm)
      else
        scale(0.7 * unitsPerEm, unitsPerEm)
      end

      # for osver > 4 also fields:
      # lowerOpticalPointSize::16, upperOpticalPointSize::16

      %{ttf | ascent: ascent, descent: descent, capHeight: capHeight, usWeightClass: usWeightClass}
    else
      Logger.debug "No OS/2 info, synthetic data"
      %{ttf | ascent: bbox[3], descent: bbox[1], capHeight: bbox[3], usWeightClass: 500}
    end

    # There's no way to get stemV from a TTF file short of analyzing actual outline data
    # This fuzzy formula is taken from pdflib sources, but we could just use 0 here
    stemV = 50 + trunc((measured.usWeightClass / 65.0) * (measured.usWeightClass / 65.0))

    extractMoreMetrics(%{measured | bbox: bbox, unitsPerEm: unitsPerEm, stemV: stemV}, data)
  end
  defp extractMoreMetrics(ttf, data) do
    #flags, italic angle, default width
    raw_post = rawTable(ttf, "post", data)
    <<_verMajor::16, _verMinor::16,
    italicMantissa::signed-16, italicFraction::16,
    _underlinePosition::signed-16, _underlineThickness::signed-16,
    isFixedPitch::32, _rest::binary>> = raw_post
    # this is F2DOT14 format defined in OpenType standard:
    italic_angle = italicMantissa + italicFraction / 16384.0
    #TODO: these should be const enum somewhere
    flagFIXED    = 0b0001
    #flagSERIF    = 0b0010
    flagSYMBOLIC = 0b0100
    #flagSCRIPT   = 0b1000
    flagITALIC = 0b1000000
    #flagALLCAPS = 1 <<< 16
    #flagSMALLCAPS = 1 <<< 17
    flagFORCEBOLD = 1 <<< 18
    
    # if SEMIBOLD or heavier, set forcebold flag
    forcebold = if ttf.usWeightClass >= 600, do: flagFORCEBOLD, else: 0

    # a non-zero angle sets the italic flag
    itals = if italic_angle != 0, do: flagITALIC, else: 0
    
    # mark it fixed pitch if needed
    fixed = if isFixedPitch > 0, do: flagFIXED, else: 0

    #TODO: figure out values of other flags (SERIF, etc)
    # SERIF and SCRIPT can be derived from sFamilyClass in OS/2 table
    flags = flagSYMBOLIC ||| itals ||| forcebold ||| fixed

    #hhea
    raw_hhea = rawTable(ttf, "hhea", data)
    <<_verMajor::16, _verMinor::16,
    _ascender::signed-16, _descender::signed-16,
    _linegap::signed-16, _advanceWidthMax::16,
    _minLeftBearing::signed-16, _minRightBearing::signed-16,
    _xMaxExtent::signed-16, _caretSlopeRise::16, _caretSlopeRun::16,
    _caretOffset::signed-16, _reserved::64, _metricDataFormat::signed-16,
    numMetrics::16>> = raw_hhea
    #maxp
    #number of glyphs -- will need to subset if more than 255
    #hmtx (glyph widths)
    raw_hmtx = rawTable(ttf, "hmtx", data)
    range = 1..numMetrics
    gw = Enum.map(range, fn(x) -> scale(getGlyphWidth(raw_hmtx, x-1), ttf.unitsPerEm) end)

    %{ttf | italicAngle: italic_angle, flags: flags, glyphWidths: gw, defaultWidth: Enum.at(gw, 0)}
  end
  defp getGlyphWidth(hmtx, index) do
    <<width::16>> = binary_part(hmtx, index*4, 2)
    width
  end
  defp readNameRecords(recs, _data, 0), do: recs
  defp readNameRecords(recs, data, nRecs) do
    #TODO: we only need to read our preferred platform/encoding
    <<platform::16, encoding::16, language::16, nameID::16, length::16, offset::16, remaining::binary>> = data
    r = %{platform: platform, encoding: encoding, lang: language, nameID: nameID, length: length, offset: offset}
    readNameRecords([r | recs], remaining, nRecs-1)
  end


  # Platform 3 (Windows) -- encoding 1 (UCS-2) and 10 (UCS-4)
  defp recordToName(%{platform: 3} = record, offset, data) do
    readUTF16Name(record, offset, data)
  end
  # Platform 0 (Unicode)
  defp recordToName(%{platform: 0} = record, offset, data) do
    readUTF16Name(record, offset, data)
  end
  # Platform 2 (deprecated; identical to platform 0)
  defp recordToName(%{platform: 2, encoding: 1} = record, offset, data) do
    readUTF16Name(record, offset, data)
  end
  # ASCII(UTF-8) for most platform/encodings
  defp recordToName(record, offset, data) do
    raw = binary_part(data, record.offset + offset, record.length)
    {record.nameID, to_string(raw)}
  end
  # handle the unicode (UTF-16BE) names
  defp readUTF16Name(record, offset, data) do
    raw = binary_part(data, record.offset + offset, record.length)
    chars = :unicode.characters_to_list(raw, {:utf16, :big})
    {record.nameID, to_string(chars)}
  end

  defp rawTable(ttf, name, data) do
    t = Enum.find(ttf.tables, fn(x) -> x.name == name end)
    cond do
      t -> binary_part(data, t.offset, t.length)
      true -> nil
    end
  end

  defp lookupTable(ttf, name) do
    t = Enum.find(ttf.tables, fn(x) -> x.name == name end)
    cond do
      t -> {t.offset, t.length}
      true -> nil
    end
  end
  #cmap header
  defp extractCMap(ttf, data) do
    raw_cmap = rawTable(ttf, "cmap", data)
    # version, numTables
    <<_version::16, numtables::16, cmaptables::binary>> = raw_cmap
    # read in tableoffsets (plat, enc, offset)
    {cmapoffsets, _cmapdata} = readCMapOffsets([], cmaptables, numtables)
    # IO.inspect cmapoffsets
    #we need the table's offset and length to find subtables
    {raw_off, raw_len} = lookupTable(ttf, "cmap")
    # ideal is 3.10, fmt 12 (full MS unicode support)
    # next is 3.1, fmt 4 (standard MS unicode support)
    # 3.0 is a Windows symbol font
    # 1.0 is a Macintosh font
    # TODO: if we have a preferred format, only need to parse that rather than all formats
    raw_cmaps = Enum.map(cmapoffsets, fn({plat, enc, off}) -> {plat, enc, binary_part(data, raw_off + off, raw_len - off)} end) 
    cid2gid = Enum.reduce(raw_cmaps, %{}, fn({plat, enc, raw_data}, acc) -> readCMapData(plat, enc, raw_data, acc) end) 
    # reverse the lookup as naive ToUnicode map
    gid2cid = Enum.map(cid2gid, fn ({k, v}) -> {v, k} end) |> Map.new
    %{ttf | :cid2gid => cid2gid, :gid2cid => gid2cid}
  end

  # read in the platform, encoding, offset triplets
  defp readCMapOffsets(tables, data, 0) do
    {tables, data}
  end
  defp readCMapOffsets(tables, data, nTables) do
    <<platform::16, encoding::16, offset::32, remaining::binary>> = data
    t = {platform, encoding, offset}
    readCMapOffsets([t | tables], remaining, nTables-1) 
  end

  # read CMap format 4 (5.2.1.3.3: Segment mapping to delta values)
  # this is the most useful one for the majority of modern fonts
  # used for Windows Unicode mappings (platform 3 encoding 1) for BMP
  defp readCMapData(_platform, _encoding, <<4::16, _length::16, _lang::16, subdata::binary>>, cmap) do
    <<doubleSegments::16,
    _searchRange::16,
    _entrySelector::16,
    _rangeShift::16, 
    segments::binary>> = subdata
    #IO.puts "READ UNICODE TABLE #{platform} #{encoding}"
    segmentCount = div doubleSegments, 2
    # segment end values
    {endcodes, ecDone} = readSegmentData([], segments, segmentCount)
    #reserved::16
    <<_reserved::16, skipRes::binary>> = ecDone
    # segment start values
    {startcodes, startDone} = readSegmentData([], skipRes, segmentCount)
    # segment delta values
    {deltas, deltaDone} = readSignedSegmentData([], startDone, segmentCount)
    # segment range offset values
    {offsets, _glyphData} = readSegmentData([], deltaDone, segmentCount)
    # combine the segment data we've read in
    segs = List.zip([startcodes, endcodes, deltas, offsets])
           |> Enum.reverse
    # generate the character-to-glyph map
    # TODO: also generate glyph-to-character map
    segs
    |> Enum.with_index
    |> Enum.reduce(%{}, fn({x, index}, acc) -> mapSegment(x, acc, index, deltaDone) end)
    |> Map.merge(cmap)
  end

  # read CMap format 12 (5.2.1.3.7 Segmented coverage)
  # This is required by Windows fonts (Platform 3 encoding 10) that have UCS-4 characters
  # and is a SUPERSET of the data stored in format 4
  defp readCMapData(_platform, _encoding, <<12::16, _::16, _length::32, _lang::32, groups::32, subdata::binary>>, cmap) do
    readCMap12Entry([], subdata, groups)
    |> Enum.reduce(%{}, fn({s,e,g}, acc) -> mapCMap12Entry(s,e,g,acc) end)
    |> Map.merge(cmap)
  end

  #unknown formats we ignore for now
  defp readCMapData(_platform, _encoding, <<_fmt::16, _subdata::binary>>, cmap) do
    #IO.inspect {"READ", fmt, platform, encoding}
    cmap
  end
  
  defp mapCMap12Entry(startcode, endcode, glyphindex, charmap) do
    offset = glyphindex-startcode
    startcode..endcode
        |> Map.new(fn(x) -> {x, x + offset} end)
        |> Map.merge(charmap)
  end
  defp readCMap12Entry(entries, _, 0), do: entries
  defp readCMap12Entry(entries, data, count) do
    <<s::32, e::32, g::32, remaining::binary>> = data
    readCMap12Entry([{s,e,g} | entries], remaining, count - 1)
  end

  defp mapSegment({0xFFFF, 0xFFFF, _, _}, charmap, _, _) do
    charmap
  end
  defp mapSegment({first, last, delta, 0}, charmap, _, _) do
    first..last
     |> Map.new(fn(x) -> {x, (x + delta) &&& 0xFFFF} end)
     |> Map.merge(charmap)
  end
  defp mapSegment({first, last, delta, offset}, charmap, segment_index, data) do
    first..last
     |> Map.new(fn(x) ->
       offsetx = (x - first) * 2 + offset + 2 * segment_index
       <<glyph::16>> = binary_part(data, offsetx, 2)
       g = if glyph == 0 do 0 else glyph + delta end
       {x, g &&& 0xFFFF}
     end)
     |> Map.merge(charmap)
  end

  defp readSegmentData(vals, data, 0) do
    {vals, data}
  end
  defp readSegmentData(vals, <<v::16, rest::binary>>, remaining) do
    readSegmentData([v | vals], rest, remaining-1)
  end
  defp readSignedSegmentData(vals, data, 0) do
    {vals, data}
  end
  defp readSignedSegmentData(vals, <<v::signed-16, rest::binary>>, remaining) do
    readSegmentData([v | vals], rest, remaining-1)
  end

  def extractFeatures(ttf, data) do
    {subS, subF, subL} = extractOffHeader("GSUB", ttf, data)
    {posS, posF, posL} = extractOffHeader("GPOS", ttf, data)
    %{ttf | substitutions: {subS, subF, subL},
   positions: {posS, posF, posL}}
  end

  #returns script/language map, feature list, lookup tables
  defp extractOffHeader(table, ttf, data) do
    raw = rawTable(ttf, table, data)
    if raw == nil do
      Logger.debug "No #{table} table"
    end
    <<versionMaj::16, versionMin::16, 
    scriptListOff::16, featureListOff::16, 
    lookupListOff::16, _::binary>> = raw
    #if 1.1, also featureVariations::u32
    #Logger.debug "#{table} Header #{versionMaj}.#{versionMin}"

    lookupList = binary_part(raw, lookupListOff, byte_size(raw) - lookupListOff)
    <<nLookups::16, ll::binary-size(nLookups)-unit(16), _::binary>> = lookupList
    # this actually gives us offsets to lookup tables
    lookups = for << <<x::16>> <- ll >>, do: x
    lookupTables = lookups
         |> Enum.map(fn x -> getLookupTable(x, lookupList) end)

    # get the feature array
    featureList = binary_part(raw, featureListOff, byte_size(raw) - featureListOff)
    features = getFeatures(featureList)

    scriptList = binary_part(raw, scriptListOff, byte_size(raw) - scriptListOff)
    <<nScripts::16, sl::binary-size(nScripts)-unit(48), _::binary>> = scriptList
    scripts = for << <<tag::binary-size(4), offset::16>> <- sl >>, do: {tag, offset}
    scripts = scripts
              |> Enum.map(fn {tag, off} -> readScriptTable(tag, scriptList, off) end)
              |> Map.new

    {scripts, features, lookupTables}
  end
  defp getFeatures(data) do
    <<nFeatures::16, fl::binary-size(nFeatures)-unit(48), _::binary>> = data
    features = for << <<tag::binary-size(4), offset::16>> <- fl >>, do: {tag, offset}
    features 
    |> Enum.map(fn {t, o} -> readLookupIndices(t, o, data) end)
  end
  #returns {lookupType, lookupFlags, [subtable offsets], <<raw table bytes>>}
  defp getLookupTable(offset, data) do
      tbl = binary_part(data, offset, byte_size(data) - offset)
      <<lookupType::16, flags::16, nsubtables::16, st::binary-size(nsubtables)-unit(16), _::binary>> = tbl
      subtables = for << <<y::16>> <- st >>, do: y
      {lookupType, flags, subtables, tbl}
  end
  defp readScriptTable(tag, data, offset) do
    script_table =  binary_part(data, offset, byte_size(data) - offset)
    <<defaultOff::16, nLang::16, langx::binary-size(nLang)-unit(48), _::binary>> = script_table
    langs = for << <<tag::binary-size(4), offset::16>> <- langx >>, do: {tag, offset}
    langs = langs 
            |> Enum.map(fn {tag, offset} -> readFeatureIndices(tag, offset, script_table) end)
            |> Map.new
    langs = if defaultOff == 0 do
      langs
    else
      {_, indices} = readFeatureIndices(nil, defaultOff, script_table)
      Map.put(langs, nil, indices)
    end
    {tag, langs}
  end
  defp readFeatureIndices(tag, offset, data) do
    feature_part = binary_part(data, offset, byte_size(data) - offset)
    <<reorderingTable::16, req::16, nFeatures::16, fx::binary-size(nFeatures)-unit(16), _::binary>> = feature_part
    if reorderingTable != 0 do
      Logger.debug "Lang #{tag} has a reordering table"
    end
    indices = for << <<x::16>> <- fx >>, do: x
    indices = if req == 0xFFFF, do: indices, else: [req | indices]
    {tag, indices}
  end
  defp readLookupIndices(tag, offset, data) do
    lookup_part = binary_part(data, offset, byte_size(data) - offset)
    <<featureParamsOffset::16, nLookups::16, fx::binary-size(nLookups)-unit(16), _::binary>> = lookup_part
    #if featureParamsOffset != 0 do
      #  Logger.debug "Feature #{tag} has feature params"
      #end
    indices = for << <<x::16>> <- fx >>, do: x
    {tag, indices}
  end
end

