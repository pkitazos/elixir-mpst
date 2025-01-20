defmodule Buyer do
  @type role :: :buyer1 | :buyer2 | :seller

  @type seller() :: term()
  @type buyer1() :: term()
  @type buyer2() :: term()

  @type ap :: %{role => pid()}

  # Buyer1 handlers

  def buyer1(ap, title) do
    seller = getPid(:seller, ap)
    send(seller, {:title, title})
    {:suspend, &quoteHandler/1}
  end

  # quoteHandler
  def quoteHandler(ap) do
    receive do
      {:seller, {:quote, amount}} -> getPid(:buyer2, ap) |> send({:share, amount / 2})
    end
  end

  # Buyer2 handlers

  def buyer2() do
    {:suspend, &shareHandler/1}
  end

  def shareHandler(ap) do
    receive do
      {:buyer1, {:share, amount}} ->
        if amount > 100 do
          getPid(:seller, ap) |> send({:quit})
          {:done}
        else
          getPid(:seller, ap) |> send({:address, getAddress()})
          {:suspend, &date_handler/1}
        end
    end
  end

  def date_handler(ap) do
    receive do
      {:seller, {:date, date}} ->
        IO.puts(date)
        {:done}
    end
  end

  # helpers - potentially behaviour functions

  @spec newAP(seller(), buyer1(), buyer2()) :: ap()
  defp newAP(seller, buyer1, buyer2) do
    %{:seller => seller, :buyer1 => buyer1, :buyer2 => buyer2}
  end

  defp getPid(:buyer1, %{buyer1: pid}), do: pid
  defp getPid(:buyer2, %{buyer2: pid}), do: pid
  defp getPid(:seller, %{seller: pid}), do: pid

  # business logic

  defp getAddress() do
    "My Address"
  end
end
