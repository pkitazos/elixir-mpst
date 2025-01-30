defmodule TwoBuyerMaty5.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = []

    opts = [strategy: :one_for_one, name: TwoBuyerMaty5.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
