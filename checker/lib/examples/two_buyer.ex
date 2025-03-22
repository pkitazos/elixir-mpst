defmodule Examples.PingPong do
  use Checker

  @st {:title_handler, ["B1&title(string).B1!quote(number).decision_handler"]}

  @st {:decision_handler, ["B2&address(string).B2!date(date).end", "B2&quit(unit).end"]}

  @handler :title_handler
  def title_handler({:title, title}, :B1, session, state) do
    amount = lookup_price(title)

    maty_send(session, :B1, {:quote, amount})
    {:suspend, {&__MODULE__.decision_handler/4, :B2}, state}
  end

  @handler :decision_handler
  def decision_handler({:address, addr}, :B2, session, state) do
    date = shipping_date(addr)

    maty_send(session, :B2, {:date, date})
    {:done, :unit, state}
  end

  @handler :decision_handler
  def decision_handler({:quit, _}, :B2, _session, state) do
    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  defp lookup_price(_title_str), do: 150
  defp shipping_date(_addr_str), do: "2021-12-31"

  defp maty_send({session, from}, to, msg) do
    send(session.participants[to], {:maty_message, session.id, to, from, msg})
  end
end
