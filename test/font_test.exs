defmodule GutenexFontTest do
  use ExUnit.Case
  alias Gutenex.PDF.TrueType, as: TrueType
  alias Gutenex.PDF.OpenTypeFont

  test "parse Truetype font metrics" do
    ttf = TrueType.new
          |> TrueType.parse("./test/support/fonts/NotoSansCJKjp-Bold.otf")
    assert 733 == ttf.capHeight
    assert -120 == ttf.descent
    assert 880 == ttf.ascent
    assert [-1013.0, -1046.0, 2926.0, 1806.0] == ttf.bbox
    assert 0 == ttf.italicAngle
    assert 1000 == ttf.unitsPerEm
    assert 1000 == ttf.defaultWidth
    assert 700 == ttf.usWeightClass
  end

  test "register parsed font" do
    {:ok, pid} = Gutenex.start_link
    ttf = TrueType.new
          |> TrueType.parse("./test/support/fonts/NotoSansCJKjp-Bold.otf")

    # sanity check that we do not have font before registration
    ctx = Gutenex.context(pid)
    assert not Map.has_key?(ctx.fonts, ttf.name)

    Gutenex.register_font(pid, ttf.name, ttf)

    # now font exists
    ctx = Gutenex.context(pid)
    assert Map.has_key?(ctx.fonts, ttf.name)
  end

  test "Basic CMAP support" do
    ttf = TrueType.new
          |> TrueType.parse("./test/support/fonts/NotoSans-Bold.ttf")
    {glyphs, _} = TrueType.layout_text(ttf, "ABC")
    assert glyphs == [36 , 37 ,38]
  end

  test "Apply OpenType substitutions (GSUB 4) - exercise ligature" do
    ttf = TrueType.new
          |> TrueType.parse("./test/support/fonts/NotoSans-Bold.ttf")
    {glyphs, _} = TrueType.layout_text(ttf, "ffl", ["liga"])
    assert glyphs == [603]
  end

  test "Apply OpenType substitutions (GSUB 6) - exercise chained" do
    ttf = TrueType.new
          |> TrueType.parse("./test/support/fonts/SourceSansPro-Regular.otf")
    {glyphs, _} = TrueType.layout_text(ttf, "1/2", ["liga", "frac"])
    assert glyphs == [1617, 1726, 1604]
  end

  @tag :integration
  test "embed font" do
    File.rm("./tmp/embed.pdf")
    {:ok, ttf} = OpenTypeFont.start_link
    OpenTypeFont.parse(ttf, "./test/support/fonts/NotoSans-Italic.ttf")
    {:ok, ssp} = OpenTypeFont.start_link
    OpenTypeFont.parse(ssp, "./test/support/fonts/SourceSansPro-Regular.otf")

    {:ok, pid} = Gutenex.start_link
    Gutenex.register_font(pid, "NotoSans", ttf)
         |> Gutenex.register_font("SourceSansPro", ssp)
      |> Gutenex.begin_text
      |> Gutenex.text_leading(48)
      |> Gutenex.set_font("Helvetica", 48)
      |> Gutenex.text_position(40, 180)
      |> Gutenex.text_render_mode(:fill)
      |> Gutenex.write_text_br("ABC")
      |> Gutenex.set_font("NotoSans", 32)
      |> Gutenex.text_render_mode(:fill)
      |> Gutenex.write_text_br("Noto Sans")
      |> Gutenex.set_font("SourceSansPro", 32)
      |> Gutenex.write_text_br("kern AWAY difficult waffle 1/2")
      |> Gutenex.write_text_br("Japanese \u713C")
      |> Gutenex.end_text
      |> Gutenex.export("./tmp/embed.pdf")
  end
end
