defmodule TwoBuyerMaty1.MixProject do
  use Mix.Project

  def project do
    [
      app: :two_buyer,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {TwoBuyerMaty1.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end
end
