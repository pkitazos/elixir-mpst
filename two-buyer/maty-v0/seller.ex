defmodule Seller do
  @type role :: :buyer1 | :buyer2 | :seller

  @type seller() :: term()
  @type buyer1() :: term()
  @type buyer2() :: term()

  @type ap :: %{role => pid()}

  def seller() do
    {:suspend, &titleHandler/1}
  end

  def titleHandler(ap) do
    receive do
      {:buyer1, {:title, title}} ->
        getPid(:buyer1) |> send({:quote, lookupPrice(title)})
        {:suspend, &decisionHandler/1}
    end
  end

  def decisionHandler(ap) do
    receive do
      {:buyer2, {:address, address}} ->
        getPid(:buyer2) |> send({:date, calculateDeliveryDate(address)})
        {:done}

      {:buyer2, {:quit}} ->
        {:done}
    end
  end

  defp getPid(:buyer1, %{buyer1: pid}), do: pid
  defp getPid(:buyer2, %{buyer2: pid}), do: pid
  defp getPid(:seller, %{seller: pid}), do: pid

  # business logic

  defp lookupPrice(title) do
    String.length(title) * 5
  end

  def calculateDeliveryDate(address) do
    Date.utc_today()
    |> Date.add(String.length(address))
  end
end
