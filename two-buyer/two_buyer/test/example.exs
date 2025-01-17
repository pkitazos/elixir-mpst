# Start the processes
{:ok, seller} = Seller.start_link()
{:ok, _buyer1} = Buyer.start_link(:buyer1, seller)
{:ok, _buyer2} = Buyer.start_link(:buyer2, seller)

# Example flow
Buyer.request_title(:buyer1, "Elixir Programming")

Buyer.propose_split(:buyer1, :buyer2, 50)

Buyer.accept_quote(:buyer2, "123 Main St")
# or
Buyer.reject_quote(:buyer2)
