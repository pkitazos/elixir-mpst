defmodule TwoBuyerMaty2.Application do
  use Application

  @impl true
  def start(_type, _args) do
    # If you want to supervise your Buyer1, Buyer2, Seller, etc. you can add them here.
    # For now, we’ll keep it empty and just manually start processes from an IEx session or Main module.
    children = [
      # {Buyer1, []},
      # {Buyer2, []},
      # {Seller, []},
      # ...
    ]

    opts = [strategy: :one_for_one, name: TwoBuyerMaty2.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
