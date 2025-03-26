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
