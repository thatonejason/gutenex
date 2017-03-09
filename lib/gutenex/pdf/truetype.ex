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
  def layout(pid, text, features \\ nil) do
    GenServer.call(pid, {:layout, text, features})
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
  def handle_call({:layout, text, features}, _from, ttf) do
    output = TrueType.layout_text(ttf, text, features)
    {:reply, output, ttf}
  end
end

defmodule Gutenex.PDF.TrueType do
  use Bitwise, only_operators: true
  require Logger
  alias Gutenex.OpenType.Parser
  alias Gutenex.OpenType.Substitutions

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
      :substitutions => nil, #GSUB
      :positions => nil, #GPOS
      :definitions => nil, #GDEF
      :isCFF => false, :familyClass => 0,
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
    |> Parser.extractVersion(data)
    |> Parser.readHeader(data)
    |> Parser.extractName(data)
    |> Parser.extractMetrics(data)
    |> Parser.markEmbeddedPart(data)
    |> Parser.extractCMap(data)
    |> Parser.extractFeatures(data)
  end

  # returns a list of glyphs and positioning information
  def layout_text(ttf, text, features \\ nil, script \\ "latn", lang \\ nil) do

    # use the font CMAP to convert the initial text 
    # into a series of glyphs
    glyphs = text
    |> String.to_charlist
    |> Enum.map(fn(cid) -> Map.get(ttf.cid2gid, cid, 0) end)

    # detect script if not passed in
    # script = layout.detect_script(text)
    # shaper = layout.selectshaper(script)
    # {glyphs, glyph_features} = shaper.markLocalFeatures(glyphs)
    # opentype.substitutions(glyphs, font, script, lang, features, glyph_features)

    # see OpenType feature registry for required, never disabled,
    # and recommended features
    features = if features == nil do
      [
        "ccmp", "locl", # preprocess (compose/decompose, local forms)
        "mark", "mkmk", "mset", # marks (mark-to-base, mark-to-mark, mark position via substitution)
        "clig", "liga", "rlig", # ligatures (contextual, standard, required)
        "calt", "rclt", # contextual alts (standard, required)
        "kern", "palt", # when kern enabled, also enable palt
        #"opbd", "lfbd", "rtbd", # optical bounds -- requires app support to identify bounding glyphs?
        "curs", # cursive (required; best tested with arabic font)
        "isol", "fina", "medi", "init", # positional forms (required; best tested with arabic font)
      ]
    else
      features
    end

    # per OpenType spec, enable "palt" when "kern" is enabled
    features = if "kern" in features, do: ["palt" | features], else: features

    glyphs
    |> handle_substitutions(ttf, script, lang, features)
    |> position_glyphs(ttf, script, lang, features)
  end

  # discover the supported opentype features
  def discover_features(ttf) do
    #TODO: add kern if there is a 'kern' table but no GPOS
    {_, gsub_features, _} = ttf.substitutions
    {_, gpos_features, _} = ttf.positions
    gsub_features ++ gpos_features
    |> Enum.map(fn {tag, _} -> tag end)
    |> Enum.uniq
  end

  # is there a particular font table?
  def hasTable?(ttf, name) do
    Enum.any?(ttf.tables, fn(x) -> x.name == name end)
  end

  # use this when the length of the actual subtable is unknown
  def subtable(table, offset) do
    binary_part(table, offset, byte_size(table) - offset)
  end


  # subtitute ligatures, positional forms, stylistic alternates, etc
  # based upon the script, language, and active OpenType features
  defp handle_substitutions(glyphs, ttf, script, lang, active_features) do
    # use data in GSUB to do any substitutions
    {scripts, subF, subL} = ttf.substitutions

    # features actually provided by the font
    availableFeatures = getFeatures(scripts, script, lang)

    # combine indices, apply in order given in LookupList table
    lookups = availableFeatures
               |> Enum.map(fn x -> Enum.at(subF, x) end)
               |> Enum.filter_map(fn {tag, _} -> tag in active_features end, fn {_, l} -> l end)
               |> List.flatten
               |> Enum.sort
               |> Enum.uniq

    # apply the lookups and return the selected glyphs
    Enum.reduce(lookups, glyphs, fn (x, acc) -> Substitutions.applyLookupGSUB(Enum.at(subL, x), ttf.definitions, subL, acc) end)
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
  # adjusts positions of glyphs based on script, language, and OpenType features
  # Used for kerning, optical alignment, diacratics, etc
  defp position_glyphs(glyphs, ttf, script, lang, active_features) do
    # initially just use glyph width as xadvance
    # this is sufficient if kerning information is missing
    # TODO: handle vertical writing
    positions = glyphs
                |> Enum.map(fn g -> Enum.at(ttf.glyphWidths, g, ttf.defaultWidth) end)
                |> Enum.map(fn advance -> {:std_width, 0, 0, advance, 0} end)

    #TODO: if no GPOS, fallback to kern table
    #
    # use data in the GPOS and BASE table
    # to kern, position, and join
    {scripts, features, lookups} = ttf.positions

    availableFeatures = getFeatures(scripts, script, lang)

    # each feature provides lookup indices
    # combine indices, apply in order given in LookupList table
    indices = availableFeatures
               |> Enum.map(fn x -> Enum.at(features, x) end)
               |> Enum.filter_map(fn {tag, _} -> tag in active_features end, fn {_, l} -> l end)
               |> List.flatten
               |> Enum.sort
               |> Enum.uniq
    # apply the lookups
    Enum.reduce(indices, {glyphs, positions}, fn (x, acc) -> applyLookupGPOS(Enum.at(lookups, x), ttf.definitions, acc) end)
  end

  # given a script and language, get the appropriate features
  # (falling back as appropriate)
  def getFeatures(scripts, script, lang) do
    # Fallback to "DFLT" or "latn" script; else ignore all
    selected_script = scripts[script] || scripts["DFLT"] || scripts["latn"] || %{}
    # Fallback to nil (default) language for script
    selected_script[lang] || selected_script[nil] || []
  end

  # add two positions together, treat nils as zeroed structure
  # category std_width, kern, pos can be used to optimize PDF output
  defp addPos(p, nil), do: p
  defp addPos(p, {0,0,0,0}), do: p
  defp addPos({:std_width, a,b,c,d}, {0,0,g,0}), do: {:kern, a, b, c+g, d}
  defp addPos({:std_width, a,b,c,d}, {e,f,g,h}), do: {:pos, a+e, b+f, c+g, d+h}
  defp addPos({type, a,b,c,d}, {e,f,g,h}), do: {type, a+e, b+f, c+g, d+h}

  # ==============================================
    # GPOS glyph positioning
    # Used for kerning, optical alignment, diacratics, etc
    # if a lookup type is as-yet unsupported
    # simply passes through the input
  # ==============================================

  # type 9 - when 32-bit offsets are used for subtables
  defp applyLookupGPOS({9, flag, offsets, table}, gdef, {glyphs, pos}) do
    subtables = offsets
            |> Enum.map(fn x ->
            <<1::16, actual_type::16, off::32>> = binary_part(table, x, 8)
            {actual_type, subtable(table, x + off)}
              end)
    #for each subtable
    Enum.reduce(subtables, {glyphs, pos}, fn ({type, tbl}, input) -> applyLookupGPOS({type, flag, tbl}, gdef, input) end)
  end

  # all other types
  defp applyLookupGPOS({type, flag, offsets, table}, gdef, {glyphs, pos}) do
    #for each subtable
    Enum.reduce(offsets, {glyphs, pos}, fn (offset, input) -> applyLookupGPOS({type, flag, subtable(table, offset)}, gdef, input) end)
  end

  #type 1 - single positioning
  defp applyLookupGPOS({1, _flag, table}, _gdef, {glyphs, pos}) do
    <<fmt::16, covOff::16, valueFormat::16, rest::binary>> = table
    coverage = parseCoverage(subtable(table, covOff))
    valSize = valueRecordSize(valueFormat)
    adjusted = case fmt do
    1 ->
      <<val::binary-size(valSize), _::binary>> = rest
      val = readPositioningValueRecord(valueFormat, val)
      Enum.map(glyphs, fn g ->
        coverloc = findCoverageIndex(coverage, g)
        if coverloc != nil, do: val, else: nil
      end)
    2 ->
      <<nVals::16, _::binary>> = rest
      recs = binary_part(rest, 16, nVals * valSize)
      values = for << <<val::binary-size(valSize)>> <- recs >>, do: readPositioningValueRecord(valueFormat, val)
      Enum.map(glyphs, fn g ->
        coverloc = findCoverageIndex(coverage, g)
        if coverloc != nil, do: Enum.at(values, coverloc), else: nil
      end)
    end
    positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end

  # type 2 - pair positioning (ie, kerning)
  defp applyLookupGPOS({2, _flag, table}, _gdef, {glyphs, pos}) do
    <<fmt::16, covOff::16, record1::16, record2::16, rest::binary>> = table
    kerning = case fmt do
      1 ->
        # FMT 1 - identifies individual glyphs
        # pair set table
        <<nPairs::16, pairOff::binary-size(nPairs)-unit(16), _::binary>> = rest
        pairsetOffsets = for << <<x::16>> <- pairOff >>, do: x
        # coverage table
        coverage = parseCoverage(subtable(table, covOff))
        # parse the pair sets
        pairSets = Enum.map(pairsetOffsets, fn off -> parsePairSet(table, off, record1, record2) end)
        applyKerning(coverage, pairSets, glyphs, [])
      2 ->
        #FMT 2
        # offset to classdef, offset to classdef
        # nClass1Records, nClass2Records
        <<class1Off::16, class2Off::16, nClass1Records::16, nClass2Records::16, records::binary>> = rest

        #read in the class definitions
        class1 = Parser.parseGlyphClass(subtable(table, class1Off))
        class2 = Parser.parseGlyphClass(subtable(table, class2Off))
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
        applyKerning2(class1, class2, pairSets, glyphs, [])
    end
    positioning = Enum.zip(pos, kerning) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end

  # type 3 - cursive positioning
  defp applyLookupGPOS({3, _flag, _table}, _gdef, {glyphs, pos}) do
    #  flag:
    #  last glyph in sequence positioned on baseline (GPOS3 only)
    #  x2 ignore base glyphs (see GDEF)
    #  x4 ignore ligatures (see GDEF)
    #  x8 ignore marks (see GDEF)
    #  x10 useMarkFilteringSet (MarkFilteringSet field in lookup, xref GDEF)
    #  0xFF00 MarkAttachmentType (skip all but specified mark type, xref GDEF)
    Logger.debug "GPOS 3 - cursive"
    {glyphs, pos}
  end

  # type 4 - mark-to-base positioning
  defp applyLookupGPOS({4, flag, table}, gdef, {glyphs, pos}) do
    <<_fmt::16, markCoverageOff::16, baseCoverageOff::16, nClasses::16, 
    markArrayOffset::16, baseArrayOffset::16, _::binary>> = table
    
    # coverage definitions
    markCoverage = parseCoverage(subtable(table, markCoverageOff))
    baseCoverage = parseCoverage(subtable(table, baseCoverageOff))

    # baseArray table
    baseTbl = subtable(table, baseArrayOffset)
    <<nRecs::16, records::binary>> = baseTbl
    # 2 bytes per class
    recordSize = nClasses * 2
    records = binary_part(records, 0, nRecs * recordSize)
    records = for << <<record::binary-size(recordSize)>> <- records >>, do: record
    # each record is array of offsets
    baseArray = records
              |> Enum.map(fn r -> for << <<offset::16>> <- r>>, do: offset end)
              |> Enum.map(&Enum.map(&1, fn o -> 
                                parseAnchor(subtable(baseTbl, o)) end) 
                        )

    # markArray table
    markArrayTbl = subtable(table, markArrayOffset)
    markArray = parseMarkArray(markArrayTbl)

    # mark attachment classes
    adjusted = applyMarkToBase(markCoverage, baseCoverage, 
                               baseArray, markArray, 
                               # skip flags and GDEF info
                               flag, gdef, 
                               #prev and next details
                               [hd(glyphs)], tl(glyphs), pos, [nil])

    # apply the adjustments to positioning
    positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end

  # type 5 - mark to ligature positioning
  defp applyLookupGPOS({5, _flag, _table}, _gdef, {glyphs, pos}) do
    Logger.debug "GPOS 5 - mark to ligature"
    {glyphs, pos}
  end

  # type 6 - mark to mark positioning
  defp applyLookupGPOS({6, flag, table}, gdef, {glyphs, pos}) do
    # same as format 4, except "base" is another mark
    <<_fmt::16, markCoverageOff::16, baseCoverageOff::16, nClasses::16, 
    markArrayOffset::16, baseArrayOffset::16, _::binary>> = table
    
    markCoverage = parseCoverage(subtable(table, markCoverageOff))
    baseCoverage = parseCoverage(subtable(table, baseCoverageOff))
    # baseArray table
    baseTbl = binary_part(table, baseArrayOffset, byte_size(table) - baseArrayOffset)
    <<nRecs::16, records::binary>> = baseTbl
    # 2 bytes per class
    recordSize = nClasses * 2
    records = binary_part(records, 0, nRecs * recordSize)
    records = for << <<record::binary-size(recordSize)>> <- records >>, do: record
    # each record is array of offsets
    # 6 bytes is a bit of a cheat, can be 6-10 bytes 
    baseArray = records
              |> Enum.map(fn r -> for << <<offset::16>> <- r>>, do: offset end)
              |> Enum.map(&Enum.map(&1, fn o -> 
                                parseAnchor(binary_part(baseTbl, o, 6)) end) 
                        )

    markArrayTbl = binary_part(table, markArrayOffset, byte_size(table) - markArrayOffset)
    markArray = parseMarkArray(markArrayTbl)

    adjusted = applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, flag, gdef, [hd(glyphs)], tl(glyphs), pos, [nil])
    #Logger.debug "MKMK #{inspect glyphs} #{inspect adjusted}"
    positioning = Enum.zip(pos, adjusted) |> Enum.map(fn {v1, v2} -> addPos(v1,v2) end)
    {glyphs, positioning}
  end

  # type 7 - contextual positioning
  defp applyLookupGPOS({7, _flag, _table}, _gdef, {glyphs, pos}) do
    Logger.debug "GPOS 7 - context"
    {glyphs, pos}
  end

  # type 8 - chained contextual positioning
  defp applyLookupGPOS({8, _flag, _table}, _gdef, {glyphs, pos}) do
    Logger.debug "GPOS 8 - chained context"
    {glyphs, pos}
  end

  #unhandled type; log and leave input untouched
  defp applyLookupGPOS({type, _flag, _table}, _gdef, {glyphs, pos}) do
    Logger.debug "Unknown GPOS lookup type #{type}"
    {glyphs, pos}
  end

  defp parseAnchor(<<_fmt::16, xCoord::signed-16, yCoord::signed-16, _rest::binary>>) do
    # anchorTable (common table)
    # coords are signed!
    # fmt = 1, xCoord::16, yCoord::16
    # fmt = 2, xCoord::16, yCoord::16, index to glyph countour point::16
    # fmt = 3, xCoord::16, yCoord::16, device table offset (for x)::16, device table offset (for y)::16
    {xCoord, yCoord}
  end

  defp parseMarkArray(table) do
    <<nRecs::16, records::binary-size(nRecs)-unit(32), _::binary>> = table
    markArray = for << <<markClass::16, anchorTableOffset::16>> <- records >>, do: {markClass, anchorTableOffset}
    markArray
    |> Enum.map(fn {c,o} -> {c, parseAnchor(binary_part(table, o, byte_size(table) - o))} end)
  end

  defp classifyGlyph(_g, nil), do: 0
  defp classifyGlyph(g, classes) when is_map(classes) do
    Map.get(classes, g, 0)
  end
  defp classifyGlyph(g, ranges) do
    r = Enum.find(ranges, fn {f,l,_} -> f <= g and g <= l end)
    # if no match, class 0
    if r == nil do
      0
    else
      {_,_,class} = r
      class
    end
  end

  # returns true when the lookup flag is set to a value
  def should_skip_glyph(g, flag, gdef) do
    # decompose the flag
    <<attachmentType::8, _::3, _useMark::1, ignoreMark::1, ignoreLig::1, ignoreBase::1, _rtl::1>> = <<flag::16>>

    cond do
      # short circuit - if no flags, we aren't skipping anything
      flag == 0 -> false
      # skip if ignore is set and we match the corresponding GDEF class
      ignoreBase == 1 and classifyGlyph(g, g.classes) == 1 -> true
      ignoreLig  == 1 and classifyGlyph(g, g.classes) == 2 -> true
      ignoreMark == 1 and classifyGlyph(g, g.classes) == 3 -> true
      # skip if we don't match a non-zero attachment type
      attachmentType != 0 and classifyGlyph(g, gdef.attachments) != attachmentType -> true
      # default is DO NOT SKIP
      true -> false
    end
  end

  defp applyMarkToBase(_markCoverage, _baseCoverage, _baseArray, _markArray, _lookupFlag, _gdef, _prev, [], _, output), do: Enum.reverse(output)
  defp applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, lookupFlag, gdef, prev, [g | glyphs], [prev_pos | pos], output) do
    # should we skip this glyph?
    skipMark = should_skip_glyph(g, lookupFlag, gdef)

    # TODO: this code assumes prev is a base
    markloc = findCoverageIndex(markCoverage, g)
    baseloc = findCoverageIndex(baseCoverage, hd(prev))
    mark_offset = if markloc != nil and baseloc != nil and !skipMark do
      b = Enum.at(baseArray, baseloc)
      {class, {mark_x, mark_y}} = Enum.at(markArray, markloc)
      {base_x, base_y} = Enum.at(b, class)
      # align the anchors
      {off_x, off_y} = {base_x - mark_x, base_y - mark_y}
      # TODO: possibly do the base calculations later when actually rendering?
      #   doing so requires us to save mark/base relationship somewhere
      #   but potentially gives more accurate results
      # get the base positioning
      {_, box, boy, bax, bay} = prev_pos
      # mark_offset = (base_offset + offset - base_advance)
      #Logger.debug "OFFSET #{off_x} base #{base_x} mark #{mark_x} less #{bax}"
      {box + off_x - bax, boy + off_y - bay, 0, 0}
    else
      nil
    end
    applyMarkToBase(markCoverage, baseCoverage, baseArray, markArray, lookupFlag, gdef, [g | prev], glyphs, pos, [mark_offset | output])
  end

  defp applyKerning2(_classDef1, _clasDef2, _pairSets, [], output), do: output
  defp applyKerning2(_classDef1, _clasDef2, _pairSets, [_], output), do: output ++ [nil]
  defp applyKerning2(classDef1, classDef2, pairsets, [g1, g2 | glyphs], output) do
    c1 = classifyGlyph(g1, classDef1)
    c2 = classifyGlyph(g2, classDef2)
    pair = pairsets |> Enum.at(c1) |> Enum.at(c2)
    {output, glyphs} = if pair != nil do
      {v1, v2} = pair
      oo = output ++ [v1]
      if v2 != nil do
        {oo ++ [v2], glyphs}
      else
        {oo, [g2 | glyphs]}
      end
    else
      {output ++ [nil], [g2 | glyphs]}
    end
    applyKerning2(classDef1, classDef1, pairsets, glyphs, output)
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
    if flag != 0 do
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

end

