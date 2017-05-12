# The tests in this suite originate in the HarfBuzz project
# which is licensed under the "Old MIT" license
# 
# See https://github.com/behdad/harfbuzz

defmodule GutenexHarfbuzzTest do
  use ExUnit.Case
  alias Gutenex.PDF.TrueType, as: TrueType
  alias Gutenex.OpenType.Layout

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

  test "simple kerning of latin text (GPOS 2)" do
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

  test "arabic-like joining (positional shaping)" do
    # handle Adlam; RTL, arabic shaper, Unicode 9.0, and above BMP
    harfbuzz_test("5dfad7735c6a67085f1b90d4d497e32907db4c78.ttf",
                  "\u{1E922}\u{1E923}\u{1E924}\u{1E925}\u{1E926}\u{1E927}\u{1E928}\u{1E929}\u{1E92A}\u{1E92B}\u{1E92C}\u{1E92D}\u{1E92E}\u{1E92F}\u{1E930}\u{1E931}\u{1E932}\u{1E933}\u{1E934}\u{1E935}\u{1E936}\u{1E937}\u{1E938}\u{1E939}\u{1E93A}\u{1E93B}\u{1E93C}\u{1E93D}\u{1E93E}\u{1E93F}\u{1E940}\u{1E941}\u{1E942}\u{1E943}",
                  "117+711|87+573|141+773|69+594|84+686|129+621|96+555|123+772|102+577|66+552|111+664|72+600|51+662|78+781|126+648|135+553|81+778|99+531|132+651|138+674|57+674|105+640|75+641|63+590|60+628|114+599|48+594|108+492|120+777|45+655|93+525|90+554|54+600|42+597")
  end

  test "ligID" do
    # is this REALLY about shaping or is it about deriving glyph names?
  end

end
