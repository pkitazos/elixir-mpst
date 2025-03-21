defmodule Maty.ST do
  @type t :: SIn.t() | SOut.t() | SEnd.t() | SHandler.t()

  defmodule SIn do
    @enforce_keys [:from, :message, :continue_as]
    defstruct [:from, :message, :continue_as]
    # from :: atom()
    # message :: {label :: atom(), type :: atom()}
    # continue_as :: [SOut | SEnd | SHandler]

    @type t :: %__MODULE__{
            from: atom(),
            message: {atom(), atom()},
            continue_as: list(SOut.t() | SEnd.t() | SHandler.t())
          }
  end

  defmodule SOut do
    @enforce_keys [:to, :message, :continue_as]
    defstruct [:to, :message, :continue_as]
    # to :: atom()
    # message :: {label :: atom(), type :: atom()}
    # continue_as :: [SOut | SEnd | SHandler]

    @type t :: %__MODULE__{
            to: atom(),
            message: {atom(), atom()},
            continue_as: list(SOut.t() | SEnd.t() | SHandler.t())
          }
  end

  defmodule SEnd do
    defstruct []

    @type t :: %__MODULE__{}
  end

  defmodule SHandler do
    @enforce_keys [:handler]
    defstruct [:handler]

    @type t :: %__MODULE__{
            # Adjust this type as needed
            handler: any()
          }
  end
end

# examples

# @st "buyer1&share(float).{ seller!address(string).date_handler, seller!quit(unit).end }"
# %SIn{
#   from: :buyer1,
#   message: {:share, some_float},
#   continue_as: [
#     %SOut{
#       to: :seller,
#       message: {:address, some_string},
#       continue_as: [%SHandler{handler: :date_handler}]
#     },
#     %SOut{
#       to: :seller,
#       message: {:quit, some_unit},
#       continue_as: [%SEnd{}]
#     }
#   ]
# }

# @st "buyer1&title(string) . buyer1!quote(float) . decision_handler"
# # {:buyer1, :&, {:title, some_string}, [{:buyer1, :!, {:quote, some_float}, [:decision_handler]}]}

# %SIn{
#   from: :buyer1,
#   message: {:title, some_string},
#   continue_as: [
#     %SOut{
#       to: :buyer1,
#       message: {:quote, some_float},
#       continue_as: [%SHandler{handler: :decision_handler}]
#     }
#   ]
# }
