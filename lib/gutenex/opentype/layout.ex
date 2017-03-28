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

  # used for cursive scripts -- Arabic, Syriac, N'Ko, Mongolian, etc
  @external_resource shaping_path = Path.join([__DIR__, "ArabicShaping.txt"])
  lines = File.stream!(shaping_path, [], :line)
          |> Stream.filter(&String.match?(&1, ~r/^[0-9A-F]+/))
  for line <- lines do
    [cp, _, t, join_group] = String.split(line, ~r/[\#;]/)
    cp = cp
         |> String.trim
         |> Integer.parse(16)
         |> elem(0)
    join_group = String.trim(join_group)

    # use the join type unless in special-cased join group
    t = if join_group in ["ALAPH", "DALATH RISH"] do
      join_group
    else
      String.trim(t)
    end
    def arabic_shaping_from_codepoint(unquote(cp)) do
      unquote(t)
    end
  end
  def arabic_shaping_from_codepoint(_cp) do
    "U"
  end

  #  Cursive scripts have the following join types:
  #  R Right_Joining
  #  L Left_Joining
  #  D Dual_Joining
  #  C Join_Causing
  #  U Non_Joining
  #  T Transparent
  #
  #  special joining rules for:
  #  ALAPH
  #  DALATH RISH
  #  
  def arabic_shaping([], _prev, output), do: Enum.reverse(output)

  # start
  def arabic_shaping([type | types], nil, []) do
    curr = if type == "U", do: nil, else: "isol"
    arabic_shaping(types, type, [curr])
  end

  # case A: previous is U
  def arabic_shaping([type | types], "U", [prev | output]) do
    curr = if type == "U", do: nil, else: "isol"
    arabic_shaping(types, type, [curr, prev | output])
  end

  # case B: previous is DALATH RISH
  def arabic_shaping([type | types], "DALATH RISH", [prev | output]) do
    curr = case type do
      "U" -> nil
      "ALAPH" -> "fin3"
      _ -> "isol"
    end
    arabic_shaping(types, type, [curr, prev | output])
  end

  # case C: previous is R
  def arabic_shaping([type | types], "R", [prev | output]) do
    curr = case type do
      "U" -> nil
      "ALAPH" -> "fin2"
      _ -> "isol"
    end
    arabic_shaping(types, type, [curr, prev | output])
  end

  # case C.2: previous is ALAPH in isol form
  def arabic_shaping([type | types], "ALAPH", ["isol" | output]) do
    curr = case type do
      "U" -> nil
      "ALAPH" -> "fin2"
      _ -> "isol"
    end
    arabic_shaping(types, type, [curr, "isol" | output])
  end

  # case D: previous is ALAPH in fina form
  def arabic_shaping([type | types], "ALAPH", ["fina" | output]) do
    {prev, curr} = case type do
      "U" -> {"fina", nil}
      "L" -> {"fina", "isol"}
      "ALAPH" -> {"med2", "fin2"}
      _ -> {"med2", "isol"}
    end
    arabic_shaping(types, type, [curr, prev | output])
  end

  # case E: previous is D or C in fina form
  def arabic_shaping([type | types], prev_type, ["fina" | output]) when prev_type in ["D", "C"] do
    {prev, curr} = case type do
      "U" -> {"fina", nil}
      "L" -> {"fina", "isol"}
      _ -> {"medi", "fina"}
    end
    arabic_shaping(types, type, [curr, prev | output])
  end

  # case F: previous is D, C, or L in isol form
  def arabic_shaping([type | types], prev_type, ["isol" | output]) when prev_type in ["D", "C", "L"] do
    {prev, curr} = case type do
      "U" -> {"isol", nil}
      "L" -> {"isol", "isol"}
      _ -> {"init", "fina"}
    end
    arabic_shaping(types, type, [curr, prev | output])
  end

  # case G: previous is ALAPH in fin2/fin3 form
  def arabic_shaping([type | types], "ALAPH", [prev | output]) when prev in ["fin2", "fin3"] do
    {prev, curr} = case type do
      "U" -> {prev, nil}
      "L" -> {prev, "isol"}
      "ALAPH" -> {"isol", "fin2"}
      _ -> {"isol", "isol"}
    end
    arabic_shaping(types, type, [curr, prev | output])
  end

  # transparent -- should be ignored by joining rules
  # combining marks are an example as they are zero-width
  def arabic_shaping(["T" | types], prev_type, [prev | output]) do
    arabic_shaping(types, prev_type, [nil, prev | output])
  end

  # fallback -- do no shaping for current glyph
  def arabic_shaping([type | types], prev_type, [prev | output]) do
    arabic_shaping(types, type, [nil, prev | output])
  end

  # shape arabic and other cursive scripts
  def shape_glyphs("arab", text) do
    # TODO: use glyphs as input rather than text
    # convert back to unicode codepoints
    features = ["isol", "medi", "init", "fina", "med2", "fin2", "fin3"]

    # look up shaping types
    x = text
        |> String.codepoints
        |> Stream.map(fn <<x::utf8>> -> x end)
        |> Stream.map(&arabic_shaping_from_codepoint(&1))
        |> Enum.to_list

    # record the locations of transparent (do not affect joining)
    glyphs_T = x
               |> Enum.with_index
               |> Enum.filter(fn {n, _} -> n == "T" end)
               |> Enum.map(fn {"T", i} -> i end)

    # remove the transparent glyphs
    trimmed = x
              |> Enum.filter(fn n -> n != "T" end)

    # do the shaping
    shaped = arabic_shaping(trimmed, nil, [])

    # transparent glyphs have no shape class, insert nil at appropriate locations
    shaped = Enum.reduce(glyphs_T, shaped, fn i, acc -> List.insert_at(acc, i, nil) end)

    {features, shaped}
  end

  # default shaper does nothing
  def shape_glyphs(_script, glyphs) do
    {[], []}
  end
end
