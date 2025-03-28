defmodule Shop.Participants.Shop do
  @st {:item_req_handler, ""}

  @impl true
  @spec init_actor(pid()) :: {:ok, maty_actor_state()}
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    register(
      ap_pid,
      :shop,
      &install(&1, &2, ap_pid),
      initial_state
    )
  end

  @spec install(session(), maty_actor_state(), pid()) :: suspend()
  def install(_session, state, ap_pid) do
    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        &install(&1, &2, ap_pid),
        state
      )

    {:suspend, {&__MODULE__.item_req_handler/4, :customer}, updated_state}
  end

  @handler :item_req_handler
  @spec item_req_handler({:request_items, :unit}, role(), session_ctx(), maty_actor_state()) ::
          suspend()
  def item_req_handler({:request_items, :unit}, :customer, session, state) do
    inventory = get_inventory()
    maty_send(session, :customer, {:item_summary, summary(inventory)})
    {:suspend, {&__MODULE__.cust_req_handler/4, :customer}, state}
  end

  @handler :cust_req_handler
  @spec cust_req_handler({:get_item_info, binary()}, role(), session_ctx(), maty_actor_state()) ::
          suspend()
  def cust_req_handler({:get_item_info, itemID}, :customer, session, state) do
    inventory = get_inventory()
    maty_send(session, :customer, {:item_info, lookup_item(itemID, inventory)})

    {:suspend, {&__MODULE__.cust_req_handler/4, :customer}, state}
  end

  @handler :cust_req_handler
  @spec cust_req_handler(
          {:checkout, {list(binary()), binary()}},
          role(),
          session_ctx(),
          maty_actor_state()
        ) ::
          suspend()
  def cust_req_handler({:checkout, {itemIDs, details}}, :customer, session, state) do
    # ? I assume this `get` gets data from the actor state?
    inventory = get_inventory()

    if in_stock(itemsIDs, inventory) do
      maty_send(session, :customer, {:payment_processing, :unit})

      total = cost(itemIDs, inventory)

      # ? and the set in the `decrease_stock` function would update the actor state?
      _updated_inventory = decrease_stock(itemIDs, inventory)
      maty_send(session, :payment_processor, {:buy, {total, details}})

      {:suspend, {&__MODULE__.payment_response_handler/4, :payment_processor}, state}
    else
      maty_send(session, :customer, {:out_of_stock, :unit})
      {:suspend, {&__MODULE__.cust_req_handler/4, :customer}, state}
    end
  end

  @doc """
    # ? How am I supposed to pass items to a handler like this?
    paymentResponseHandler(itemIDs) === handler PaymentProcessor {
      ok() |->
          date = deliveryDate(itemIDs)
          Customer!ok(date)
          suspend custReqHandler

      paymentDeclined() |->
          Customer!paymentDeclined()
          items = get()
          set increaseStock(itemIDs, items);
          suspend custReqHandler
    }
  """
  @handler :payment_response_handler
  @spec payment_response_handler(
          {:ok, :unit},
          role(),
          session_ctx(),
          maty_actor_state()
        ) :: suspend()
  def payment_response_handler({:ok, :unit}, :payment_processor, session, state) do
    itemIDs = ["I01", "I01", "I02", "I03"]

    date = delivery_date(itemIDs)
    maty_send(session, :customer, {:ok, date})
    {:suspend, {&__MODULE__.cust_req_handler/4, :customer}, state}
  end

  @handler :payment_response_handler
  @spec payment_response_handler(
          {:payment_declined, :unit},
          role(),
          session_ctx(),
          maty_actor_state()
        ) :: suspend()
  @def payment_response_handler({:payment_declined, :unit}, :payment_processor, session, state) do
    itemIDs = ["I01", "I01", "I02", "I03"]

    maty_send(session, :customer, {:payment_declined, :unit})
    inventory = get_inventory()
    _updated_inventory = increase_stock(itemIDs, inventory)
    {:suspend, {&__MODULE__.cust_req_handler/4, :customer}, state}
  end

  # ----

  defp get_inventory(_state) do
    %{
      "I01" => %{ID: "I01", name: "foo", cost: 10, stock: 30},
      "I02" => %{ID: "I02", name: "bar", cost: 25, stock: 20},
      "I03" => %{ID: "I03", name: "baz", cost: 50, stock: 20}
    }
  end

  defp summary(_) do
    "ID: I01 - foo: £10\nID: I02 - bar: £25\nID: I03 - baz: £50"
  end

  defp lookup_item(itemID, inventory) do
    inventory[itemID]
  end

  defp in_stock(_), do: true

  defp cost(_), 95
end
