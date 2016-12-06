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
  defp build_fonts(%RenderContext{}=render_context, [{font_alias, %{ "SubType" => {:name, "Type0"} }=embedded } | fonts]) do
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
    base_font = %{}
    cid_font = %{}
    metrics = %{}
    embed_bytes = <<>>

    %RenderContext{ec |
     font_aliases: Map.put(ec.font_aliases, font_alias, fr),
     font_objects: [
       { fo, {:dict, base_font} },
       { cido, {:dict, cid_font} },
       { deo, {:dict, metrics} },
       { eo, {:stream, embed_bytes} }
       | ec.font_objects
     ]
    }
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
