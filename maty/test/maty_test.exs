defmodule MatyTest do
  use ExUnit.Case
  doctest Maty

  test "greets the world" do
    assert Maty.hello() == :world
  end
end
