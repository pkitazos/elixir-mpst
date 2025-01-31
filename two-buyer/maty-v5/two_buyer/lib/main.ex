defmodule Main do
  alias TwoBuyerMaty5.{AccessPoint, Seller, Buyer1, Buyer2}

  def main do
    {:ok, ap} = AccessPoint.start_link()

    # this should also register the seller in the session
    {:ok, seller_pid} = Seller.start_link(ap)
    {:ok, id} = AccessPoint.create_session(ap)
    :ok = Seller.register(seller_pid, id)

    {:ok, buyer1_pid} = Buyer1.start_link(ap)
    {:ok, buyer2_pid} = Buyer2.start_link(ap)

    # IO.puts("[DEBUG] here is the session id: #{id}")
    # not sure how to have the seller register in infinite sessions yet
    :ok = Buyer1.register(buyer1_pid, id)
    :ok = Buyer2.register(buyer2_pid, id)
    # at this point, ap knows about all the actors and should inform them of who acts as who
    :ok = Seller.fetch_session(seller_pid, id)
    :ok = Buyer1.fetch_session(buyer1_pid, id)
    :ok = Buyer2.fetch_session(buyer2_pid, id)

    # now internally all of the participants should have a map that looks like this:
    # %{
    #   seller: pid,
    #   buyer1: pid,
    #   buyer2: pid
    # }
    # which is stored in actor_state under [:sessions, session_id, :participants]
    # they can use this to communicate with each other and route messages to the correct actor

    # now we can start the session

    # recursively call the seller to start new sessions (somehow)
    # Seller.instal()
    Buyer1.send_title(buyer1_pid, id, "Types and Programming Languages")
    :ok
  end
end
