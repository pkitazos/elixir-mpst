defmodule TwoBuyer.Main do
  alias TwoBuyer.Participants.{Seller, Buyer1, Buyer2}

  def start do
    {:ok, ap} = Maty.AccessPoint.start_link(TwoBuyer.SessionContext)

    Seller.start_link(ap)

    spawn_buyers(ap, "Types and Programming Languages")
    spawn_buyers(ap, "Compiling with Continuations")
  end

  defp spawn_buyers(ap, title) do
    Buyer1.start_link({ap, title})
    Buyer2.start_link(ap)
  end
end
