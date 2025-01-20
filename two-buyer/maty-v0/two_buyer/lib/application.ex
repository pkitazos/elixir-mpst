defmodule TwoBuyerProtocol.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: BuyerRegistry}
    ]

    opts = [strategy: :one_for_one, name: TwoBuyerProtocol.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
