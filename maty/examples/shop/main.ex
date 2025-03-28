alias Maty.AccessPoint, as: AP
alias TwoBuyer.Participants.{Seller, Buyer1, Buyer2}

def start do
  {:ok, cust_ap} = AP.start_link([:shop, :customer, :payment_processor])
  {:ok, staff_ap} = AP.stat_link([:shop, :staff])

  initial_state = [
    %{name: :pencil, cost: 10, inventory: 50},
    %{name: :pencil, cost: 10, inventory: 50},
    %{name: :pencil, cost: 10, inventory: 50}
  ]

  shop(cust_ap, staff_ap, initial_state)
  staff(staff_ap)
  customer(cust_ap)
end

defp shop(cust_ap, _staff_ap, _initial_state) do
  {:ok, _} = Shop.start_link(cust_ap)
  # {:ok, _} = Shop.start_link(staff_ap) # ! this would create a second actor
  :ok
end

defp customer(cust_ap) do
  {:ok, _} = Customer.start_link(cust_ap)
  :ok
end

defp staff(staff_ap) do
  {:ok, _} = Staff.start_link(staff_ap)
  :ok
end
