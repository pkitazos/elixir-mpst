defmodule Main do
  alias TwoBuyerMaty5.{AccessPoint, Seller, Buyer1, Buyer2}

  def main do
    {:ok, ap} = AccessPoint.start_link()

    {:ok, seller_pid} = Seller.start_link(ap)
    {:ok, buyer1_pid} = Buyer1.start_link(ap)
    {:ok, buyer2_pid} = Buyer2.start_link(ap)

    {:ok, id} = AccessPoint.create_session(ap)
    IO.puts("[DEBUG] here is the session id: #{id}")
    # not sure how to have the seller register in infinite sessions yet
    Seller.register(seller_pid, id)
    Buyer1.register(buyer1_pid, id)
    Buyer2.register(buyer2_pid, id)
    # at this point, ap knows about all the actors and should inform them of who acts as who
    Seller.fetch_session(seller_pid, id)
    Buyer1.fetch_session(buyer1_pid, id)
    Buyer2.fetch_session(buyer2_pid, id)
    # ! need this to be synchronous

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
    # ! currently this fires off a message to the seller before I know that all participants have all the required information
    Buyer1.send_title(buyer1_pid, id, "Types and Programming Languages")
    :ok
  end
end
