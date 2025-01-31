defmodule TwoBuyerProtocol.MixProject do
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
      extra_applications: [:logger],
      mod: {TwoBuyerProtocol.Application, []}
    ]
  end

  defp deps do
    []
  end
end
