defmodule Gutenex.Mixfile do
  use Mix.Project

  def project do
    [
      app: :gutenex,
      name: "Gutenex",
      version: "0.2.0",
      source_url: "https://github.com/SenecaSystems/gutenex",
      elixir: "~> 1.5",
      deps: deps(),
      description: description(),
      package: package()
    ]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps() do
    [
      {:imagineer, github: "jbowtie/imagineer", ref: "fix_1_6_exceptions"},
      # {:opentype, "~> 0.3.0" },
      {:earmark, "~> 1.2", only: :dev},
      {:ex_doc, "~> 0.16.4", only: :dev}
    ]
  end

  defp description() do
    """
    PDF Generation in Elixir
    """
  end

  defp package() do
    [
      licenses: ["MIT"],
      links: %{github: "https://github.com/SenecaSystems/gutenex"},
      contributors: ["Chris Maddox", "John C Barstow"]
    ]
  end
end
