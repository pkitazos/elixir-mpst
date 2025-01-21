defmodule TwoBuyerMaty2.Logger do
  # use Logger

  def log(role, msg) do
    IO.puts("[#{role}] #{msg}")
  end

  def log(role, handler, msg) do
    IO.puts("[#{role}] (#{handler}) #{msg}")
  end
end
