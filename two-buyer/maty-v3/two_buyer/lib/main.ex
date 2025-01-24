defmodule TwoBuyerMaty3.Main do
  alias TwoBuyerMaty3.{Seller, Buyer1, Buyer2, SessionContext}

  @participants [{:seller, Seller}, {:buyer1, Buyer1}, {:buyer2, Buyer2}]

  def start_sessions do
    _session1 = create_session("Types and Programming Languages")

    :done
  end

  def create_session(title) do
    @participants
    |> create_context()
    |> initialize_participants(title)
  end

  defp initialize_participants(context, title) do
    with :ok <- init_seller(context),
         :ok <- init_buyer1(context, title),
         :ok <- init_buyer2(context) do
      {:ok, context}
    end
  end

  defp init_seller(ap) do
    Seller.init_role(ap)
    :ok = Seller.setup()
  end

  defp init_buyer1(ap, title) do
    Buyer1.init_role(ap)
    :ok
  end

  defp init_buyer2(ap) do
    Buyer2.init_role(ap)
    :ok
  end

  # -----------------------------------------------------------------

  # this would be the equivalent of newAP
  defp create_context(participants, context_module \\ SessionContext) do
    participants
    |> start_processes()
    |> build_context(context_module)
  end

  defp start_processes(participants) do
    Enum.map(participants, fn {role, module} ->
      {:ok, pid} = module.start_link()
      {role, pid}
    end)
  end

  defp build_context(processes, context_module) do
    processes
    |> Map.new()
    |> then(&struct(context_module, &1))
  end
end
