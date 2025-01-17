defmodule TwoBuyerMaty1.Application do
  use Application

  def start(_type, _args) do
    Supervisor.start_link(children, opts)
  end
end
