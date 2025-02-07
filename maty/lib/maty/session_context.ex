defmodule Maty.SessionContext do
  @callback participants() :: [atom()]
  @callback new(map()) :: struct()
end
