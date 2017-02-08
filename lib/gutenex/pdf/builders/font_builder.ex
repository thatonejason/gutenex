defmodule Gutenex.PDF.Builders.FontBuilder do
  alias Gutenex.PDF.Context
  alias Gutenex.PDF.RenderContext
  alias Gutenex.PDF.OpenTypeFont

  # Builds each font object, adding the font objects and references to the
  # render context. Returns {render_context, context}
  def build({%RenderContext{}=render_context, %Context{}=context}) do
    updated_render_context = build_fonts(render_context, Map.to_list(context.fonts))
    {updated_render_context, context}
  end

  defp build_fonts(%RenderContext{}=render_context, []) do
    %RenderContext{
      render_context |
      font_objects: Enum.reverse(render_context.font_objects)
    }
  end

  # This handles embedding a Type0 composite font per the 1.7 spec
  defp build_fonts(%RenderContext{}=render_context, [{font_alias, pid } | fonts]) when is_pid(pid) do
    ttf = OpenTypeFont.font_structure(pid)
    # add stream, add descriptor, add descfont, add tounicodemap, add font
    # font =  {:dict, font_definition}
    fo = RenderContext.current_object(render_context)
    fr = RenderContext.current_reference(render_context)
    # CIDFont = {:dict, CID}
    cidc = RenderContext.next_index(render_context)
    cido = RenderContext.current_object(cidc)
    cidr = RenderContext.current_reference(cidc)
    # descriptor = {:dict, desc}
    dec = RenderContext.next_index(cidc)
    deo = RenderContext.current_object(dec)
    der = RenderContext.current_reference(dec)
    # embedded = {:stream, CFF}
    ec = RenderContext.next_index(dec)
    eo = RenderContext.current_object(ec)
    er = RenderContext.current_reference(ec)
    # CMap = {:stream, details}
    cmapc = RenderContext.next_index(ec)
    cmapo = RenderContext.current_object(cmapc)
    cmapr = RenderContext.current_reference(cmapc)
    # set up info
    base_font = %{
      "Type" => {:name, "Font"},
      "Encoding" => {:name, "Identity-H"},
      "Subtype" => {:name, "Type0"},
      "BaseFont" => {:name, ttf.name},
      "DescendantFonts" => {:array, [cidr]},
      "ToUnicode" => cmapr
    }
    cid_font = %{
      "Type" => {:name, "Font"},
      "Subtype" => {:name, "CIDFontType0"},
      "BaseFont" => {:name, ttf.name},
      "CIDSystemInfo" => {:dict, %{"Ordering" => "Identity", "Registry" => "Adobe", "Supplement" => 0} },
      "FontDescriptor" => der,
      "DW" => ttf.defaultWidth,
      "W" => glyph_widths(ttf)
    }
    metrics = %{
      "Type" => {:name, "FontDescriptor"},
      "FontName" => {:name, ttf.name},
      "FontWeight" => ttf.usWeightClass,
      "Flags" => ttf.flags,
      "FontBBox" => {:rect, ttf.bbox},
      "ItalicAngle" => ttf.italicAngle,
      "Ascent" => ttf.ascent,
      "Descent" => ttf.descent,
      "CapHeight" => ttf.capHeight,
      "StemV" => ttf.stemV,
      "FontFile3" => er  #TODO: handle different composite types
    }

    z = :zlib.open()
    :zlib.deflateInit(z)
    zout = :zlib.deflate(z, ttf.embed)
    compressed = IO.iodata_to_binary(zout)
    :zlib.close(z)

    embed_bytes = {:stream, {:dict, %{"Subtype" => {:name, "CIDFontType0C"}, "Length" => byte_size(compressed), "Filter" => {:name, "FlateDecode"}}}, compressed}

    rc = %RenderContext{RenderContext.next_index(cmapc) |
     font_aliases: Map.put(ec.font_aliases, font_alias, fr),
     font_objects: [
       { fo, {:dict, base_font} },
       { cido, {:dict, cid_font} },
       { deo, {:dict, metrics} },
       { eo, embed_bytes },
       { cmapo, identity_tounicode_cmap() }
       | ec.font_objects
     ]
    }
    build_fonts(rc, fonts)
  end


  defp build_fonts(%RenderContext{}=render_context, [{font_alias, font_definition} | fonts]) do
    render_context = %RenderContext{
      RenderContext.next_index(render_context) |
      font_aliases: add_font_alias(render_context, font_alias),
      font_objects: add_font_object(render_context, font_definition)
    }
    build_fonts(render_context, fonts)
  end

  defp add_font_alias(render_context, font_alias) do
    reference = RenderContext.current_reference(render_context)
    Map.put(render_context.font_aliases, font_alias, reference)
  end

  defp add_font_object(render_context, font_definition) do
    [
      {
        RenderContext.current_object(render_context),
        {:dict, font_definition}
      }
      | render_context.font_objects
    ]
  end

  defp glyph_widths(ttf) do
    {b, _, :end} = ttf.glyphWidths ++ [:end]
    |> Enum.with_index
    |> Enum.reduce({[], 0, 0}, fn({w, gid}, {buckets, lg, lw}) -> bucketWidth(gid, w, buckets, lg, lw) end)
    cw = b |> Enum.reduce([], fn(x, acc) -> fmtb(x, acc) end)
    {:array, cw}
  end

  def fmtb({s,w}, output) do
    [s, {:array, w}] ++ output
  end
  def fmtb({s,e,w}, output) do
    [s,e,w] ++ output
  end

  def bucketWidth(gid, width, [], 0, _) do
    {[], gid, width}
  end
  def bucketWidth(gid, width, [], 1, width) do
    {[{1, 1, width}], gid, width}
  end
  def bucketWidth(gid, width, [], 1, lw) do
    {[{1, [lw]}], gid, width}
  end
  def bucketWidth(gid, width, [{start, widths} | tail], lastgid, width) do
    {[{lastgid, lastgid, width} | [{start, widths} | tail]], gid, width}
  end
  def bucketWidth(gid, width, [{start, widths} | tail], _lastgid, lastWidth) do
    {[{start, widths ++ [lastWidth]} | tail], gid, width}
  end
  def bucketWidth(gid, width, [{s, e, w} | tail], lastgid, w) do
    {[{s, lastgid, w} | tail], gid, width}
  end
  def bucketWidth(gid, width, [{s, e, w} | tail], lastgid, width) do
    {[{lastgid, lastgid, width} | [{s, e, w} | tail]], gid, width}
  end
  def bucketWidth(gid, width, [{s, e, w} | tail], lastgid, lw) do
    {[{lastgid, [lw]} | [{s, e, w} | tail]], gid, width}
  end

  defp identity_tounicode_cmap() do
    {:stream, """
/CIDInit /ProcSet findresource begin
12 dict begin
begincmap
/CIDSystemInfo
<< /Registry (Adobe)
/Ordering (UCS)
/Supplement 0
>> def
/CMapName /Adobe−Identity−UCS def
/CMapType 2 def
1 begincodespacerange
<0000> <FFFF>
endcodespacerange
2 beginbfrange
<0000> <005E> <0020>
<005F> <0061> [<00660066> <00660069> <00660066006C>]
endbfrange
1 beginbfchar
<3A51> <D840DC3E>
endbfchar
endcmap
CMapName currentdict /CMap defineresource pop
end
end
    """}
  end
end


