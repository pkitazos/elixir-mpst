defmodule TwoBuyerMaty1.Main do
  def start_two_buyer() do
    # Spawn each role process; store PIDs in a context struct.
    {:ok, seller_pid} = TwoBuyerMaty1.Seller.start_link()
    {:ok, buyer1_pid} = TwoBuyerMaty1.Buyer1.start_link()
    {:ok, buyer2_pid} = TwoBuyerMaty1.Buyer2.start_link()

    session = %TwoBuyerMaty1.SessionContext{
      seller: seller_pid,
      buyer1: buyer1_pid,
      buyer2: buyer2_pid
    }

    # Initialize each role with references to the other roles
    TwoBuyerMaty1.Seller.init_role(session)
    TwoBuyerMaty1.Buyer1.init_role(session)
    TwoBuyerMaty1.Buyer2.init_role(session)

    TwoBuyerMaty1.Buyer1.send_title(session.seller, "Types and Programming Languages")

    :ok
  end
end
