# The tests in this suite originate in the HarfBuzz project
# which is licensed under the "Old MIT" license
# 
# See https://github.com/behdad/harfbuzz

defmodule GutenexHarfbuzzTest do
  use ExUnit.Case
  alias Gutenex.PDF.TrueType, as: TrueType

  # generate a HarfBuzz-style string
  # this allows us to re-use HarfBuzz shaping tests
  def harfbuzz(glyphs, pos) do
    Enum.zip(glyphs, pos)
    |> Enum.map_join("|", fn {g, {_, xoff, yoff, xadv, _}} ->
      if xoff != 0 or yoff != 0 do
        "#{g}@#{xoff},#{yoff}+#{xadv}"
      else
        "#{g}+#{xadv}"
      end
    end)
  end

  def harfbuzz_test(font, text, expected) do
    filename = "./test/support/fonts/sha1sum/" <> font
    ttf = TrueType.new
          |> TrueType.parse(filename)
    {glyphs, pos} = TrueType.layout_text(ttf, text)
    hb = harfbuzz(glyphs, pos)
    assert hb == expected
  end

  test "simple text" do
    # expect kerning of VA pair
    harfbuzz_test("49c9f7485c1392fa09a1b801bc2ffea79275f22e.ttf",
                  "VABEabcd",
                  "4+1142|1+1295|2+1295|3+1123|5+1126|6+1164|7+1072|8+1164")
  end

  test "cursive tests (arabic shaping with GPOS 3)" do
    harfbuzz_test("c4e48b0886ef460f532fb49f00047ec92c432ec0.ttf",
                  "\u0643\u0645\u0645\u062B\u0644",
                  "8+738|5@441,1197+0|6@0,432+405|9@0,477+452|9@0,977+452|10@20,1577+207")

    harfbuzz_test("298c9e1d955f10f6f72c6915c3c6ff9bf9695cec.ttf",
                  "\u0643\u0645\u0645\u062B\u0644",
                  "8+738|5@441,1197+0|6@0,432+405|9@0,477+500|9@0,577+452|10@20,1177+207")

    # \u06E1 is a non-spacing combining mark
    # the font incorrectly gives it an advance width of 1000 instead of zero
    # this is a GPOS4 test, not sure why in HB cursive tests
    # harfbuzz_test("07f054357ff8638bac3711b422a1e31180bba863.ttf",
                  # "\u0606\u06E1", # ARABIC CUBE ROOT, COMBINING MARK (DOTLESS HEAD OF KHAH)
                  # "2@40,502+0|1+1000")
  end

  test "language-specific glyphs" do
    # we expect different glyphs from this font depending on the language specified
    # language-tags.tests
  end

  test "mark filtering sets" do
    # make sure marks are filtered correctly
    # mark filtering happens in GPOS table lookup 34 (type 8, chained context pos)
    harfbuzz_test("f22416c692720a7d46fadf4af99f4c9e094f00b9.ttf",
                  "\u062A\u062E\u062A\u0629",
                  "75@299,1170+0|28+502|75@149,690+0|58+532|74@-51,1259+0|73+196|75@655,1751+0|34@0,-358+905")

    harfbuzz_test("f22416c692720a7d46fadf4af99f4c9e094f00b9.ttf",
                  "\u062A\u062E\u0646\u0629",
                  "75@299,1170+0|28+502|74@149,690+0|58+532|74@-51,1259+0|73+196|75@655,1751+0|34@0,-358+905")

    harfbuzz_test("f22416c692720a7d46fadf4af99f4c9e094f00b9.ttf",
                  "\u062A\u062E\u0626\u0629",
                  "75@299,1170+0|28+502|78@149,690+0|58+532|74@-51,1259+0|73+196|75@655,1751+0|34@0,-358+905")

    harfbuzz_test("f22416c692720a7d46fadf4af99f4c9e094f00b9.ttf",
                  "\u062A\u062E\u062B\u0629",
                  "75@299,1520+0|28+502|76@149,690+0|58+532|74@-51,1259+0|73+196|75@655,1751+0|34@0,-358+905")

    harfbuzz_test("f22416c692720a7d46fadf4af99f4c9e094f00b9.ttf",
                  "\u062A\u062E\u0679\u0629",
                  "75@299,1520+0|28+502|77@149,690+0|58+532|74@-51,1259+0|73+196|75@655,1751+0|34@0,-358+905")

  end

  test "ligID" do
    # is this REALLY about shaping or is it about deriving glyph names?
  end

end
