defmodule TwoBuyer.SessionContext do
  @behaviour Maty.SessionContext

  defstruct [:seller, :buyer1, :buyer2]

  @impl true
  def participants do
    [:seller, :buyer1, :buyer2]
  end

  @impl true
  def new(participants) do
    struct!(__MODULE__, participants)
  end
end
