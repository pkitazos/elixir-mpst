defmodule TwoBuyerMaty4.Seller do
  alias TwoBuyerMaty4.{MatyActor, Logger}

  use MatyActor

  @name __MODULE__
  @role :seller

  # -----------------------------------------------------------------
  defp lookup_price(_title_str), do: 155
  defp shipping_date(_addr), do: "2025-02-07"

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
