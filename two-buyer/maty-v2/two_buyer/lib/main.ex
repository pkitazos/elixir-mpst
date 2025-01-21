defmodule TwoBuyerMaty2.Main do
  alias TwoBuyerMaty2.Seller
  alias TwoBuyerMaty2.Buyer1
  alias TwoBuyerMaty2.Buyer2
  alias TwoBuyerMaty2.SessionContext

  def start_two_buyer(title \\ "Types and Programming Languages") do
    ap =
      newAP([
        {:seller, Seller},
        {:buyer1, Buyer1},
        {:buyer2, Buyer2}
      ])

    seller(ap)

    buyer1(ap, title)
    buyer2(ap)
  end

  defp seller(ap) do
    Seller.init_role(ap)
    # can choose to spawn new seller process here
    :ok = Seller.install()
  end

  defp buyer1(ap, title) do
    Buyer1.init_role(ap, title)
  end

  defp buyer2(ap) do
    Buyer2.init_role(ap)
  end

  defp newAP(participants, context_module \\ SessionContext) do
    # spawn each role process; store PIDs in a context struct.
    Enum.map(participants, fn {role, module} ->
      {:ok, pid} = module.start_link()
      {role, pid}
    end)
    |> Map.new()
    |> then(&struct(context_module, &1))
  end
end
