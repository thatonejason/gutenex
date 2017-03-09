defmodule Gutenex.OpenType.Parser do
  use Bitwise, only_operators: true
  require Logger

  # extract the TTF (or TTC) version
  def extractVersion(ttf, <<version :: size(32), data :: binary>>) do
    {%{ttf | :version => version}, data}
  end

  # read in the font header
  def readHeader({%{version: 0x00010000}=ttf, data}, _full) do
    <<numTables::16,
    _searchRange::16,
    _entrySelector ::16,
    _rangeShift ::16,
    remainder :: binary>> = data
    {tables, _} = readTables([], remainder, numTables)
    isCFF = Enum.any?(tables, fn(x) -> x.name == "CFF " end)
    %{ttf | :tables => tables, :isCFF => isCFF}
  end
  def readHeader({%{version: 0x74727565}=ttf, data}, _full) do
    <<numTables::16,
    _searchRange::16,
    _entrySelector :: size(16),
    _rangeShift :: size(16),
    remainder :: binary>> = data
    {tables, _} = readTables([], remainder, numTables)
    isCFF = Enum.any?(tables, fn(x) -> x.name == "CFF " end)
    %{ttf | :tables => tables, :isCFF => isCFF}
  end
  def readHeader({%{version: 0x74727566}=ttf, data}, full_data) do
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
    isCFF = Enum.any?(tables, fn(x) -> x.name == "CFF " end)
    %{ttf | :tables => tables, :isCFF => isCFF}
  end
  def readHeader({%{version: 0x4F54544F}=ttf, data}, _) do
    <<numTables::16,
    _searchRange::16,
    _entrySelector :: size(16),
    _rangeShift :: size(16),
    remainder :: binary>> = data
    #IO.puts "Has #{numTables} tables"
    {tables, _} = readTables([], remainder, numTables)
    isCFF = Enum.any?(tables, fn(x) -> x.name == "CFF " end)
    %{ttf | :tables => tables, :isCFF => isCFF}
  end
  def readHeader({ttf, _data}, _) do
    #IO.puts "TODO: unknown TTF version"
    ttf
  end

  # raw data for a given table
  def rawTable(ttf, name, data) do
    t = Enum.find(ttf.tables, fn(x) -> x.name == name end)
    cond do
      t -> binary_part(data, t.offset, t.length)
      true -> nil
    end
  end

  # lookup a table by name
  def lookupTable(ttf, name) do
    t = Enum.find(ttf.tables, fn(x) -> x.name == name end)
    cond do
      t -> {t.offset, t.length}
      true -> nil
    end
  end

  # is there a particular font table?
  def hasTable?(ttf, name) do
    Enum.any?(ttf.tables, fn(x) -> x.name == name end)
  end

  # use this when the length of the actual subtable is unknown
  def subtable(table, offset) do
    binary_part(table, offset, byte_size(table) - offset)
  end

  # read in the name table and select a name
  def extractName(ttf, data) do
    raw = rawTable(ttf, "name", data)
    <<_fmt::16, nRecords::16, strOffset::16, r::binary>> = raw
    #IO.puts "Name table format #{fmt}"
    recs = readNameRecords([], r, nRecords)
    # pick best supported platform/encoding
    selected = recs
               |> Enum.map(fn r -> {r.platform, r.encoding} end)
               |> findPreferredEncoding
    recs = if selected != nil do
      Enum.filter(recs, fn r -> {r.platform, r.encoding} == selected end)
    else
      recs
    end
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

  # read in the font tables
  defp readTables(tables, data, 0) do
    {tables, data}
  end
  defp readTables(tables, <<tag::binary-size(4), checksum::32, offset::32, length::32, data::binary>>, numTables) do
    #for each table
    table = %{name: tag, checksum: checksum, offset: offset, length: length}
    #4-char tag, checksum, offset, length
    readTables([table | tables], data, numTables-1)
  end
  defp readOffset(offsets, data, 0), do: {offsets, data}
  defp readOffset(offsets, <<offset::32, rem::binary>>, count) do
    readOffset([offset | offsets], rem, count-1)
  end

  defp readNameRecords(recs, _data, 0), do: recs
  defp readNameRecords(recs, data, nRecs) do
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

  def findPreferredEncoding(candidates) do
    # Select a Unicode CMAP by preference
    preferred = [
      # 32-bit Unicode formats
      {3,10}, {0, 6}, {0, 4},
      # 16-bit Unicode formats
      {3,1}, {0,3}, {0,2}, {0, 1}, {0, 0},
      # Windows symbol font (usually unicode)
      {3, 0}
    ]
      preferred |> Enum.find(fn {plat, enc} -> {plat, enc} in candidates end)
  end

  def extractMetrics(ttf, data) do
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

      %{ttf | ascent: ascent, descent: descent, capHeight: capHeight, usWeightClass: usWeightClass, familyClass: familyClass}
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
    # this is F2DOT14 format defined in OpenType standard
    italic_angle = italicMantissa + italicFraction / 16384.0
    #TODO: these should be const enum somewhere
    flagFIXED    = 0b0001
    flagSERIF    = 0b0010
    flagSYMBOLIC = 0b0100
    flagSCRIPT   = 0b1000
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

    # SERIF and SCRIPT can be derived from sFamilyClass in OS/2 table
    class = ttf.familyClass >>> 8
    serif = if Enum.member?(1..7, class), do: flagSERIF, else: 0
    script = if class == 10, do: flagSCRIPT, else: 0
    flags = flagSYMBOLIC ||| itals ||| forcebold ||| fixed ||| serif ||| script

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
  defp scale(x, unitsPerEm) do
    x * 1000.0 / unitsPerEm
  end

  # mark what portion of the font is embedded
  # this may get more complex when we do proper subsetting
  def markEmbeddedPart(ttf, data) do
    embedded = if ttf.isCFF do
      #rawTable(ttf, "CFF ", data)
      data
    else
      data
    end
    %{ttf | :embed => embedded}
  end

  #cmap header
  def extractCMap(ttf, data) do
    raw_cmap = rawTable(ttf, "cmap", data)
    # version, numTables
    <<_version::16, numtables::16, cmaptables::binary>> = raw_cmap
    # read in tableoffsets (plat, enc, offset)
    {cmapoffsets, _cmapdata} = readCMapOffsets([], cmaptables, numtables)

    # find the best available format
    selected = cmapoffsets
               |> Enum.map(fn {plat, enc, _} -> {plat, enc} end)
               |> findPreferredEncoding

    # if we found a preferred format, locate it
    {plat, enc, off} = if selected != nil do
      Enum.find(cmapoffsets, fn {plat, enc, _} -> {plat, enc} == selected end)
    else
      # no preferred format available, just handle the first one
      hd(cmapoffsets)
    end

    #we need the table's offset and length to find subtables
    {raw_off, raw_len} = lookupTable(ttf, "cmap")
    raw_cmap = binary_part(data, raw_off + off, raw_len - off)
    cid2gid = readCMapData(plat, enc, raw_cmap, %{})

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
    {subS, subF, subL} = if hasTable?(ttf, "GSUB"), do: extractOffHeader("GSUB", ttf, data), else: {[], [], []}
    {posS, posF, posL} = if hasTable?(ttf, "GPOS"), do: extractOffHeader("GPOS", ttf, data), else: {[], [], []}
    #read in definitions
    gdef = rawTable(ttf, "GDEF", data)
    definitions = if gdef != nil, do: extractGlyphDefinitionTable(gdef), else: nil

    %{ttf | 
      substitutions: {subS, subF, subL},
      positions: {posS, posF, posL},
      definitions: definitions
    }
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
    if {versionMaj, versionMin} != {1, 0} do
      Logger.debug "#{table} Header #{versionMaj}.#{versionMin}"
    end

    lookupList = subtable(raw, lookupListOff)
    <<nLookups::16, ll::binary-size(nLookups)-unit(16), _::binary>> = lookupList
    # this actually gives us offsets to lookup tables
    lookups = for << <<x::16>> <- ll >>, do: x
    lookupTables = lookups
         |> Enum.map(fn x -> getLookupTable(x, lookupList) end)

    # get the feature array
    featureList = subtable(raw, featureListOff)
    features = parseFeatures(featureList)

    scriptList = subtable(raw, scriptListOff)
    <<nScripts::16, sl::binary-size(nScripts)-unit(48), _::binary>> = scriptList
    scripts = for << <<tag::binary-size(4), offset::16>> <- sl >>, do: {tag, offset}
    scripts = scripts
              |> Enum.map(fn {tag, off} -> readScriptTable(tag, scriptList, off) end)
              |> Map.new

    {scripts, features, lookupTables}
  end
  defp extractGlyphDefinitionTable(table) do
    <<_versionMaj::16, _versionMin::16,
    glyphClassDef::16, _attachList::16,
    _ligCaretList::16, markAttachClass::16,
    _::binary>> = table
    # 1.2 also has 16-bit offset to MarkGlyphSetsDef
    # 1.3 also has 32-bit offset to ItemVarStore

    # predefined classes for use with GSUB/GPOS flags
    # 1 = Base, 2 = Ligature, 3 = Mark, 4 = Component
    glyphClassDef = if glyphClassDef, do: parseGlyphClass(subtable(table, glyphClassDef)), else: nil
    # mark attachment class (may be NULL; used with flag in GPOS/GSUB lookups)
    markAttachClass = if markAttachClass, do: parseGlyphClass(subtable(table, markAttachClass)), else: nil
    %{attachments: markAttachClass, classes: glyphClassDef}
  end
  defp parseFeatures(data) do
    <<nFeatures::16, fl::binary-size(nFeatures)-unit(48), _::binary>> = data
    features = for << <<tag::binary-size(4), offset::16>> <- fl >>, do: {tag, offset}
    features
    |> Enum.map(fn {t, o} -> readLookupIndices(t, o, data) end)
  end
  #returns {lookupType, lookupFlags, [subtable offsets], <<raw table bytes>>}
  defp getLookupTable(offset, data) do
      tbl = subtable(data, offset)
      <<lookupType::16, flags::16, nsubtables::16, st::binary-size(nsubtables)-unit(16), _::binary>> = tbl
      #TODO: if flag bit for markfilteringset, also markFilteringSetIndex::16
      subtables = for << <<y::16>> <- st >>, do: y
      {lookupType, flags, subtables, tbl}
  end
  defp readScriptTable(tag, data, offset) do
    script_table =  subtable(data, offset)
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
    feature_part = subtable(data, offset)
    <<reorderingTable::16, req::16, nFeatures::16, fx::binary-size(nFeatures)-unit(16), _::binary>> = feature_part
    if reorderingTable != 0 do
      Logger.debug "Lang #{tag} has a reordering table"
    end
    indices = for << <<x::16>> <- fx >>, do: x
    indices = if req == 0xFFFF, do: indices, else: [req | indices]
    {tag, indices}
  end
  defp readLookupIndices(tag, offset, data) do
    lookup_part = subtable(data, offset)
    <<featureParamsOffset::16, nLookups::16, fx::binary-size(nLookups)-unit(16), _::binary>> = lookup_part
    #if featureParamsOffset != 0 do
      #  Logger.debug "Feature #{tag} has feature params"
      #end
    indices = for << <<x::16>> <- fx >>, do: x
    {tag, indices}
  end

  def parseGlyphClass(<<1::16, start::16, nGlyphs::16, classes::binary-size(nGlyphs)-unit(16), _::binary>>) do
    classes = for << <<class::16>> <- classes >>, do: class
    classes
    |> Enum.with_index(start)
    |> Map.new(fn {class, glyph} -> {glyph, class} end)
  end
  def parseGlyphClass(<<2::16, nRanges::16, ranges::binary-size(nRanges)-unit(48), _::binary>>) do
    ranges = for << <<first::16, last::16, class::16>> <- ranges >>, do: {first, last, class}
    ranges
  end

  # parse coverage tables
  def parseCoverage(<<1::16, nrecs::16, glyphs::binary-size(nrecs)-unit(16), _::binary>>) do
    for << <<x::16>> <- glyphs >>, do: x
  end
  def parseCoverage(<<2::16, nrecs::16, ranges::binary-size(nrecs)-unit(48), _::binary>>) do
    for << <<startg::16, endg::16, covindex::16>> <- ranges >>, do: {startg, endg, covindex}
  end

  def parseAlts(table, altOffset) do
    <<nAlts::16, alts::binary-size(nAlts)-unit(16), _::binary>> = subtable(table, altOffset)
    for << <<x::16>> <- alts >>, do: x
  end

  def parseLigatureSet(table, lsOffset) do
    <<nrecs::16, ligat::binary-size(nrecs)-unit(16), _::binary>> = subtable(table, lsOffset)
    ligaOff = for << <<x::16>> <- ligat >>, do: x
    ligaOff
    |> Enum.map(fn x -> subtable(table, lsOffset + x) end)
    |> Enum.map(fn <<g::16, nComps::16, rest::binary>> ->  {g, nComps-1, rest} end)
    |> Enum.map(fn {g, n, data} ->
      <<recs::binary-size(n)-unit(16), _::binary>> = data
      gg = for << <<x::16>> <- recs >>, do: x
      {g, gg}
    end)
  end
  def parseContextSubRule1(rule) do
    <<nGlyphs::16, substCount::16, rest::binary>> = rule
    # subtract one since initial glyph handled by coverage
    glyphCount = nGlyphs - 1
    <<input::binary-size(glyphCount)-unit(16), 
      substRecs::binary-size(substCount)-unit(32), 
      _::binary>> = rest

    input_glyphs = for << <<g::16>> <- input >>, do: g
    substRecords = for << <<x::16, y::16>> <- substRecs >>, do: {x, y}
    {input_glyphs, substRecords}
  end

  def parseChainedSubRule2(rule) do
    <<btCount::16, bt::binary-size(btCount)-unit(16),
    nGlyphs::16, rest::binary>> = rule
    # subtract one since initial glyph handled by coverage
    glyphCount = nGlyphs - 1
    <<input::binary-size(glyphCount)-unit(16), 
      laCount::16, la::binary-size(laCount)-unit(16),
      substCount::16,
      substRecs::binary-size(substCount)-unit(32), 
      _::binary>> = rest

    backtrack = for << <<g::16>> <- bt >>, do: g
    lookahead = for << <<g::16>> <- la >>, do: g
    input_glyphs = for << <<g::16>> <- input >>, do: g
    substRecords = for << <<x::16, y::16>> <- substRecs >>, do: {x, y}
    {backtrack, input_glyphs, lookahead, substRecords}
  end

end

