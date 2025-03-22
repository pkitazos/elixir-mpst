defmodule Maty.ST do
  @type t :: SIn.t() | SOut.t() | SEnd.t() | SName.t() | SBranch.t()

  defmodule SBranch do
    @enforce_keys [:label, :payload, :continue_as]
    defstruct [:label, :payload, :continue_as]

    @type t :: %__MODULE__{
            label: atom(),
            payload: atom(),
            continue_as: SOut.t() | SEnd.t() | SName.t()
          }
  end

  defmodule SIn do
    @enforce_keys [:from, :branches]
    defstruct [:from, :branches]

    @type t :: %__MODULE__{
            from: atom(),
            branches: [Branch.t()]
          }
  end

  defmodule SOut do
    @enforce_keys [:to, :branches]
    defstruct [:to, :branches]

    @type t :: %__MODULE__{
            to: atom(),
            branches: [Branch.t()]
          }
  end

  defmodule SEnd do
    defstruct []

    @type t :: %__MODULE__{}
  end

  defmodule SName do
    @enforce_keys [:handler]
    defstruct [:handler]

    @type t :: %__MODULE__{
            handler: atom()
          }
  end
end

# examples

# @st "buyer1&{share(number).seller!{address(string).date_handler, quit(unit).end }}"
# %ST.SIn{
#   from: :buyer1,
#   branches: [
#     %ST.SBranch{
#       label: :share,
#       payload: :number,
#       continue_as: %ST.SOut{
#         to: :seller,
#         branches: [
#           %ST.SBranch{
#             label: :address,
#             payload: :string,
#             continue_as: %ST.SName{handler: :date_handler}
#           },
#           %ST.SBranch{
#             label: :quit,
#             payload: :unit,
#             continue_as: %ST.SEnd{}
#           }
#         ]
#       }
#     }
#   ]
# }

# @st "buyer1&{title(string).buyer1!{quote(number).decision_handler}}"
# %ST.SIn{
#   from: :buyer1,
#   branches: [
#     %ST.SBranch{
#       label: :title,
#       payload: :string,
#       continue_as: %ST.SOut{
#         to: :buyer1,
#         branches: [
#           %ST.SBranch{
#             label: :quote,
#             payload: :number,
#             continue_as: %ST.SName{handler: :decision_handler}
#           }
#         ]
#       }
#     }
#   ]
# }
