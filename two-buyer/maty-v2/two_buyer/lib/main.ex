defmodule TwoBuyerMaty2.Main do
  def start_two_buyer(title \\ "Types and Programming Languages") do
    ap =
      newAP([
        {:seller, TwoBuyerMaty2.Seller},
        {:buyer1, TwoBuyerMaty2.Buyer1},
        {:buyer2, TwoBuyerMaty2.Buyer2}
      ])

    seller(ap)

    buyer1(ap, title)
    buyer2(ap)
  end

  defp seller(ap) do
    TwoBuyerMaty2.Seller.init_role(ap)
    # can choose to spawn new seller process here
    :ok = TwoBuyerMaty2.Seller.install()
  end

  defp buyer1(ap, title) do
    TwoBuyerMaty2.Buyer1.init_role(ap, title)
  end

  defp buyer2(ap) do
    TwoBuyerMaty2.Buyer2.init_role(ap)
  end

  defp newAP(participants, context_module \\ TwoBuyerMaty2.SessionContext) do
    # spawn each role process; store PIDs in a context struct.
    Enum.map(participants, fn {role, module} ->
      {:ok, pid} = module.start_link()
      {role, pid}
    end)
    |> Map.new()
    |> then(&struct(context_module, &1))
  end
end
