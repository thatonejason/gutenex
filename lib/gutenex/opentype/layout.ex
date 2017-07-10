defmodule Gutenex.OpenType.Layout do
  alias UnicodeData

  def detect_script(text) do
    x = String.codepoints(text)
    |> Stream.map(&UnicodeData.script_from_codepoint(&1))
    |> Stream.filter(fn x -> !(x in ["Common", "Inherited", "Unknown"]) end)
    |> Enum.to_list
    script = if x == [], do: "Unknown", else: hd(x)
    UnicodeData.script_to_tag(script)
  end

  def join_type(cp) do
    join_group = UnicodeData.joining_group(cp)
    if join_group in ["ALAPH", "DALETH RISH"] do
      join_group
    else
      UnicodeData.joining_type(cp)
    end
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
  def arabic_shaping([type | types], _prev_type, [prev | output]) do
    arabic_shaping(types, type, [nil, prev | output])
  end

  # shape arabic and other cursive scripts
  # arabic, mongolian, syriac, n'ko, phags pa,
  # mandiac, manichaean, psalter pahlavi
  def shape_glyphs(script, text) when script in ["arab", "mong", "syrc", "nko ", "phag", "mand", "mani", "phlp"] do
    # TODO: use glyphs as input rather than text
    # convert back to unicode codepoints
    features = ["isol", "medi", "init", "fina", "med2", "fin2", "fin3"]

    # look up shaping types
    x = text
        |> String.codepoints
        |> Stream.map(&join_type(&1))
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
  def shape_glyphs(_script, _glyphs) do
    {[], []}
  end
end
