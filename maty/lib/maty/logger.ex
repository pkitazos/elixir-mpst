defmodule Maty.Logger do
  def log(role, msg) do
    IO.puts("[#{role}] #{msg}")
  end

  def log(role, handler, msg) do
    IO.puts("[#{role}] (#{handler}) #{msg}")
  end
end
