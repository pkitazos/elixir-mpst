defmodule Shop.Main do
  alias Shop.Participants.{Customer,Shop,PaymentProcessor, Staff}

  def start do
    {:ok, customer_ap} = Maty.AccessPoint.start_link([:customer, :shop, :payment_processor])
    {:ok, staff_ap} = Maty.AccessPoint.start_link([:shop, :staff])

    initial_stock = %{}

    Shop.start_link({customer_ap, staff_ap, initial_stock})

    Staff.start_link(staff_ap)
    Customer.start_link(customer_ap)
    PaymentProcessor.start_link(customer_ap)
  end
end
