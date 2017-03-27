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
  alias Gutenex.OpenType.Parser
  alias Gutenex.OpenType.Substitutions
  alias Gutenex.OpenType.Positioning
  alias Gutenex.OpenType.Layout

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
  def layout_text(ttf, text, features \\ nil, script \\ nil, lang \\ nil) do

    # use the font CMAP to convert the initial text 
    # into a series of glyphs
    glyphs = text
    |> String.to_charlist
    |> Enum.map(fn(cid) -> Map.get(ttf.cid2gid, cid, 0) end)

    # detect script if not passed in
    script = if script == nil do
      Layout.detect_script(text)
    else
      script
    end
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

    # mark any per-glyph features
    # TODO: shaper needs to work with glyphs
    per_glyph_features = Layout.shape_glyphs(script, text)

    glyphs
    |> handle_substitutions(ttf, script, lang, features, per_glyph_features)
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

  # subtitute ligatures, positional forms, stylistic alternates, etc
  # based upon the script, language, and active OpenType features
  defp handle_substitutions(glyphs, ttf, script, lang, active_features, {per_glyph_features, per_glyph_assignments}) do
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

    # per-glyph lookups
    pgl = availableFeatures
          |> Enum.map(fn x -> Enum.at(subF, x) end)
          |> Enum.filter_map(fn {tag, _} -> tag in per_glyph_features end, fn {tag, l} -> for i <- l, do: {i,tag} end)
          |> List.flatten
          |> Map.new

    # apply the lookups and return the selected glyphs
    Enum.reduce(lookups, glyphs, fn (x, acc) -> 
                if pgl != nil and Map.has_key?(pgl, x) do
                  Substitutions.applyLookupGSUB(Enum.at(subL, x), ttf.definitions, subL, Map.get(pgl, x), per_glyph_assignments, acc)
                else
                  Substitutions.applyLookupGSUB(Enum.at(subL, x), ttf.definitions, subL, nil, nil, acc)
                end
    end)
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
    {g, p} = Enum.reduce(indices, {glyphs, positions}, fn (x, acc) -> Positioning.applyLookupGPOS(Enum.at(lookups, x), ttf.definitions, acc) end)

    #if script is RTL, reverse
    if script == "arab" do
      {Enum.reverse(g), Enum.reverse(p)}
    else
      {g, p}
    end
  end

  # given a script and language, get the appropriate features
  # (falling back as appropriate)
  def getFeatures(scripts, script, lang) do
    # Fallback to "DFLT", "dflt", or "latn" script; else ignore all
    selected_script = scripts[script]  || scripts["DFLT"] || scripts["dflt"] || scripts["latn"] || %{}
    # Fallback to nil (default) language for script
    selected_script[lang] || selected_script[nil] || []
  end

end

