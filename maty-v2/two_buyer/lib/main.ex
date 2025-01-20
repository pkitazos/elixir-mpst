defmodule TwoBuyerMaty2.Main do
  def start_two_buyer(title \\ "Types and Programming Languages") do
    # Spawn each role process; store PIDs in a context struct.
    {:ok, seller_pid} = TwoBuyerMaty2.Seller.start_link()
    {:ok, buyer1_pid} = TwoBuyerMaty2.Buyer1.start_link()
    {:ok, buyer2_pid} = TwoBuyerMaty2.Buyer2.start_link()

    # seller_ref = Process.monitor(seller_pid)
    # buyer2_ref = Process.monitor(buyer2_pid)

    session = %TwoBuyerMaty2.SessionContext{
      seller: seller_pid,
      buyer1: buyer1_pid,
      buyer2: buyer2_pid
    }

    # Initialize each role with references to the other roles
    TwoBuyerMaty2.Seller.init_role(session)
    TwoBuyerMaty2.Buyer1.init_role(session)
    TwoBuyerMaty2.Buyer2.init_role(session)

    TwoBuyerMaty2.Buyer1.send_title(title)

    # wait_for_completion([seller_ref, buyer2_ref])
  end

  defp wait_for_completion([]), do: :ok

  defp wait_for_completion(refs) do
    receive do
      {:DOWN, ref, :process, _pid, :normal} ->
        wait_for_completion(List.delete(refs, ref))

      {:DOWN, _ref, :process, _pid, reason} ->
        {:error, reason}
    end
  end
end
