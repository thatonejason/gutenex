defmodule Gutenex.PDF.Builders.FontBuilder do
  alias Gutenex.PDF.Context
  alias Gutenex.PDF.RenderContext

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

  # probably embed_fonts
  defp build_fonts(%RenderContext{}=render_context, [{font_alias, %{ "SubType" => {:name, "Type0"} }=ttf } | fonts]) do
    # add stream, add descriptor, add descfont, add tounicodemap, add font
    # font =  {:dict, font_definition}
    fo = RenderContext.current_object(render_context)
    fr = RenderContext.current_reference(render_context)
    # CIDFont = {:dict, CID}
    cidc = RenderContext.next_index(render_context)
    cido = RenderContext.current_object(cidc)
    cidr = RenderContext.current_reference(cidc)
    # decriptor = {:dict, desc}
    dec = RenderContext.next_index(cidc)
    deo = RenderContext.current_object(dec)
    der = RenderContext.current_reference(dec)
    # embedded = {:stream, CFF}
    ec = RenderContext.next_index(dec)
    eo = RenderContext.current_object(ec)
    er = RenderContext.current_reference(ec)

    # CMap = {:stream, details}
    #rc = %RenderContext{RenderContext.next_index(render_context)}
    #fo = RenderContext.current_object(rc)
    #fr = RenderContext.current_reference(rc)
    # set up info
    base_font = %{
      "Type" => {:name, "Font"},
      "Encoding" => {:name, "Identity-H"},
      "Subtype" => {:name, "Type0"},
      "BaseFont" => {:name, ttf.name},
      "DescendantFonts" => {:array, [cidr]}
    }
    cid_font = %{
      "Type" => {:name, "Font"},
      "Subtype" => {:name, "CIDFontType0"},
      "BaseFont" => {:name, ttf.name},
      "CIDSystemInfo" => {:dict, %{"Ordering" => "Identity", "Registry" => "Adobe", "Supplement" => 0} },
      "FontDescriptor" => der,
      "DW" => ttf.defaultWidth,
      "W" => {:array, [ 1, {:array, [ 227 ]}, 27, {:array, [ 325 ]}, 47, {:array, [ 749 ]}, 66, {:array, [ 591 ]}, 70, {:array, [ 581 ]}, 74,
          {:array, [ 304, 305 ]}, 79, {:array, [ 641, 626, 644 ]}, 84, {:array, [ 495, 420 ]}]}
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

    rc = %RenderContext{RenderContext.next_index(ec) |
     font_aliases: Map.put(ec.font_aliases, font_alias, fr),
     font_objects: [
       { fo, {:dict, base_font} },
       { cido, {:dict, cid_font} },
       { deo, {:dict, metrics} },
       { eo, embed_bytes }
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

end
