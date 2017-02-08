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


  test "embed font" do
    File.rm("./tmp/embed.pdf")
    #ttf = TrueType.new
    #      |> TrueType.parse("./test/support/fonts/NotoSans-Bold.ttf")
    {:ok, ttf} = OpenTypeFont.start_link
    OpenTypeFont.parse(ttf, "./test/support/fonts/NotoSans-Bold.ttf")

    {:ok, pid} = Gutenex.start_link
    Gutenex.register_font(pid, "NotoSans", ttf)
      |> Gutenex.begin_text
      |> Gutenex.text_leading(48)
      |> Gutenex.set_font("Helvetica", 48)
      |> Gutenex.text_position(40, 180)
      |> Gutenex.text_render_mode(:fill)
      |> Gutenex.write_text_br("ABC")
      |> Gutenex.set_font("NotoSans", 32)
      |> Gutenex.text_render_mode(:fill)
      |> Gutenex.write_text_br("Noto Sans")
      |> Gutenex.write_text_br("kern AWAY and ligature waffle wa\uFB04e")
      |> Gutenex.write_text("Japanese \u713C")
      |> Gutenex.end_text
      |> Gutenex.export("./tmp/embed.pdf")
  end
end
