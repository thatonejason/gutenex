defmodule Gutenex.OpenType.Layout do
  @external_resource scripts_path = Path.join([__DIR__, "Scripts.txt"])
  lines = File.stream!(scripts_path, [], :line)
          |> Stream.filter(&String.match?(&1, ~r/^[0-9A-F]+/))
  for line <- lines do
    [r, s, _] = String.split(line, ~r/[\#;]/)
    r = String.trim(r) 
        |> String.split("..")
        |> Enum.map(&Integer.parse(&1, 16))
        |> Enum.map(fn {x, _} -> x end)
    s = String.trim(s)
    case r do
      [x] ->
        def script_from_codepoint(unquote(x)) do
          unquote(s)
        end
      [a, b] ->
        def script_from_codepoint(n) when n in unquote(a)..unquote(b) do
          unquote(s)
        end
    end
  end

  def script_to_tag(script) do
  # This maps the Unicode Script property to an OpenType script tag
  # Data from http=>//www.microsoft.com/typography/otspec/scripttags.htm
  # and http=>//www.unicode.org/Public/UNIDATA/PropertyValueAliases.txt.
  unicode_scripts = %{
  "Adlam" => "adlm",
  "Ahom" => "ahom",
  "Anatolian_Hieroglyphs" => "hluw",
  "Caucasian_Albanian" => "aghb",
  "Arabic" => "arab",
  "Imperial_Aramaic"=> "armi",
  "Armenian"=> "armn",
  "Avestan"=> "avst",
  "Balinese"=> "bali",
  "Bamum"=> "bamu",
  "Bassa_Vah"=> "bass",
  "Batak"=> "batk",
  "Bengali"=> ["bng2", "beng"],
  "Bopomofo"=> "bopo",
  "Brahmi"=> "brah",
  "Braille"=> "brai",
  "Buginese"=> "bugi",
  "Buhid"=> "buhd",
  "Byzantine_Music"=> "byzm",
  "Chakma"=> "cakm",
  #"Canadian_Syllabics"=> "cans",
  "Canadian_Aboriginal"=> "cans",
  "Carian"=> "cari",
  "Cham"=> "cham",
  "Cherokee"=> "cher",
  "Coptic"=> "copt",
  "Cypriot"=> "cprt",
  "Cyrillic"=> "cyrl",
  "Devanagari"=> ["dev2", "deva"],
  "Deseret"=> "dsrt",
  "Duployan"=> "dupl",
  "Egyptian_Hieroglyphs"=> "egyp",
  "Elbasan"=> "elba",
  "Ethiopic"=> "ethi",
  "Georgian"=> "geor",
  "Glagolitic"=> "glag",
  "Gothic"=> "goth",
  "Grantha"=> "gran",
  "Greek"=> "grek",
  "Gujarati"=> ["gjr2", "gujr"],
  "Gurmukhi"=> ["gur2", "guru"],
  "Hangul"=> "hang",
  "Han"=> "hani",
  "Hanunoo"=> "hano",
  "Hebrew"=> "hebr",
  "Hiragana"=> "hira",
  "Pahawh_Hmong"=> "hmng",
  "Katakana_Or_Hiragana"=> "hrkt",
  "Old_Italic"=> "ital",
  "Javanese"=> "java",
  "Kayah_Li"=> "kali",
  "Katakana"=> "kana",
  "Kharoshthi"=> "khar",
  "Khmer"=> "khmr",
  "Khojki"=> "khoj",
  "Kannada"=> ["knd2", "knda"],
  "Kaithi"=> "kthi",
  "Tai_Tham"=> "lana",
  "Lao"=> "lao ",
  "Latin"=> "latn",
  "Lepcha"=> "lepc",
  "Limbu"=> "limb",
  "Linear_A"=> "lina",
  "Linear_B"=> "linb",
  "Lisu"=> "lisu",
  "Lycian"=> "lyci",
  "Lydian"=> "lydi",
  "Mahajani"=> "mahj",
  "Mandaic"=> "mand",
  "Manichaean"=> "mani",
  "Mende_Kikakui"=> "mend",
  "Meroitic_Cursive"=> "merc",
  "Meroitic_Hieroglyphs"=> "mero",
  "Malayalam"=> ["mlm2", "mlym"],
  "Modi"=> "modi",
  "Mongolian"=> "mong",
  "Mro"=> "mroo",
  "Meetei_Mayek"=> "mtei",
  "Myanmar"=> ["mym2", "mymr"],
  "Old_North_Arabian"=> "narb",
  "Nabataean"=> "nbat",
  "Nko"=> "nko ",
  "Ogham"=> "ogam",
  "Ol_Chiki"=> "olck",
  "Old_Turkic"=> "orkh",
  "Odia"=> "orya",
  "Osmanya"=> "osma",
  "Palmyrene"=> "palm",
  "Pau_Cin_Hau"=> "pauc",
  "Old_Permic"=> "perm",
  "Phags_Pa"=> "phag",
  "Inscriptional_Pahlavi"=> "phli",
  "Psalter_Pahlavi"=> "phlp",
  "Phoenician"=> "phnx",
  "Miao"=> "plrd",
  "Inscriptional_Parthian"=> "prti",
  "Rejang"=> "rjng",
  "Runic"=> "runr",
  "Samaritan"=> "samr",
  "Old_South_Arabian"=> "sarb",
  "Saurashtra"=> "saur",
  "Shavian"=> "shaw",
  "Sharada"=> "shrd",
  "Siddham"=> "sidd",
  "Khudawadi"=> "sind",
  "Sinhala"=> "sinh",
  "Sora_Sompeng"=> "sora",
  "Sundanese"=> "sund",
  "Syloti_Nagri"=> "sylo",
  "Syriac"=> "syrc",
  "Tagbanwa"=> "tagb",
  "Takri"=> "takr",
  "Tai_Le"=> "tale",
  "New_Tai_Lue"=> "talu",
  "Tamil"=> "taml",
  "Tai_Viet"=> "tavt",
  "Telugu"=> ["tel2", "telu"],
  "Tifinagh"=> "tfng",
  "Tagalog"=> "tglg",
  "Thaana"=> "thaa",
  "Thai"=> "thai",
  "Tibetan"=> "tibt",
  "Tirhuta"=> "tirh",
  "Ugaritic"=> "ugar",
  "Vai"=> "vai ",
  "Warang_Citi"=> "wara",
  "Old_Persian"=> "xpeo",
  "Cuneiform"=> "xsux",
  "Yi"=> "yi  ",
  "Inherited"=> "zinh",
  "Common"=> "zyyy",
  "Unknown"=> "zzzz"
}

    Map.get(unicode_scripts, script, "zzzz")
  end

  def detect_script(text) do
    x = String.codepoints(text)
    |> Stream.map(fn <<x::utf8>> -> x end)
    |> Stream.map(&script_from_codepoint(&1))
    |> Stream.filter(fn x -> !(x in ["Common", "Inherited", "Unknown"]) end)
    |> Enum.to_list
    script = if x == [], do: "Unknown", else: hd(x)
    script_to_tag(script)
  end


  # need to implement universal shaping engine
  # https://www.microsoft.com/typography/OpenTypeDev/USE/intro.htm
  # clearly supports indic scripts
  # probably best to split out arabic and/or BIDI
  # dnom/numr seems to need special handling by a shaping engine
  # some fonts seem to have issues with combining marks
  # what about hangul? Usually seems to have own shaping rules

  # arabic shaper -- use ArabicShaping.txt to pick correct form
  # for each glyph
  def shape_glyphs("arab", glyphs) do
    # look up shaping type of current glyph
    # use previous glyph state + shaping type
    # determine current state, possibly update previous state
    # for example previous might change from fina to medi
    # in order to join with current
    output = glyphs
    |> Enum.map(fn _ -> "isol" end)
    features = ["isol", "medi", "init", "fina", "med2", "fin2", "fin3"]
    {features, output}
  end

  # default shaper does nothing
  def shape_glyphs(_script, glyphs) do
    {[], []}
  end
end
