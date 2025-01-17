defmodule Main do
  def start_two_buyer() do
    # Spawn each role process; store PIDs in a context struct.
    seller_pid = Seller.start_link()
    buyer1_pid = Buyer1.start_link()
    buyer2_pid = Buyer2.start_link()

    session = %SessionContext{
      seller: seller_pid,
      buyer1: buyer1_pid,
      buyer2: buyer2_pid
    }

    # Initialize each role with references to the other roles
    Seller.init_role(session)
    Buyer1.init_role(session)
    Buyer2.init_role(session)

    Buyer1.send_title("Types and Programming Languages")

    :ok
  end
end
