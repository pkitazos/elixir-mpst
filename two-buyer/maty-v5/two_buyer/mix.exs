defmodule TwoBuyerMaty5.MixProject do
  use Mix.Project

  def project do
    [
      app: :two_buyer,
      version: "0.5.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {TwoBuyerMaty5.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end
end
