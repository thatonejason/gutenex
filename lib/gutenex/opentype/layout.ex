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

  def detect_script(text) do
    x = String.codepoints(text)
    |> Enum.map(fn <<x::utf8>> -> x end)
    |> Enum.map(&script_from_codepoint(&1))
    IO.puts("#{text} #{inspect x}")
    'latn'
  end
end
