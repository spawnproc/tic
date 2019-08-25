defmodule TIC.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :tic,
      version: "0.8.0",
      description: "TIC Crypto Liquidity Integrator",
      package: package(),
      elixir: "~> 1.7",
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(src include mix.exs LICENSE),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :tic,
      links: %{"GitHub" => "https://github.com/enterprizing/tic"}
    ]
  end


  def application() do
    [mod: {:tic, []}]
  end

  def deps() do
    [
      {:ex_doc, "~> 0.11", only: :dev},
      {:rest, "~> 1.5.0"},
      {:cowboy, "~> 2.5.0"},
      {:n2o, "~> 6.6.0"},
    ]
  end
end
