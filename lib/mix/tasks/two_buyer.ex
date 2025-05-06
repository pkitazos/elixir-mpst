defmodule Mix.Tasks.TwoBuyer do
  use Mix.Task

  def run(_) do
    TwoBuyer.Main.start()
  end
end
