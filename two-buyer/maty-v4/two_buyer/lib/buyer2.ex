defmodule TwoBuyerMaty4.Buyer2 do
  alias TwoBuyerMaty4.{MatyActor, Logger}

  use MatyActor

  @name __MODULE__
  @role :buyer2

  # -----------------------------------------------------------------
  defp get_address(), do: "18 Lilybank Gardens"

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
