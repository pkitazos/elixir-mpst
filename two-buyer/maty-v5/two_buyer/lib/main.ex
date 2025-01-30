defmodule Main do
  def main do
    {:ok, ap} = AccessPoint.start_link()

    Seller.init(_)
    Buyer1.init(_)
    Buyer2.init(_)

    # sellers should be able to register once and be automatically installed in all sessions
    {:ok, _} = Seller.register(ap)
    # buyers should be able to register for specific sessions
    {:ok, _} = Buyer1.register(ap, "session1")
    {:ok, _} = Buyer2.register(ap, "session1")

    # at this point, ap knows about all the actors and should inform them of who acts as who
    Seller.fetch_session(ap, "session1")
    Buyer1.fetch_session(ap, "session1")
    Buyer2.fetch_session(ap, "session1")

    # now internally all of the participants should have a map that looks like this:
    # %{
    #   seller: pid,
    #   buyer1: pid,
    #   buyer2: pid
    # }
    # which they can use to communicate with each other and route messages to the correct actor
  end
end
