defmodule Main do
  alias MV6.{AccessPoint, Seller, Buyer1, Buyer2}

  def main do
    {:ok, ap} = AccessPoint.start_link()

    Seller.start_link(ap)

    spawn_buyers(ap, "Types and Programming Languages")
    spawn_buyers(ap, "Compiling with Continuations")
  end

  defp spawn_buyers(ap, title) do
    Buyer1.start_link({ap, title})
    Buyer2.start_link(ap)
  end
end
