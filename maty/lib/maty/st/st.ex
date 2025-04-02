defmodule Maty.ST do
  @type t :: SIn.t() | SOut.t() | SEnd.t() | SName.t() | SBranch.t() | SRec.t() | SRecRef.t()

  defmodule SBranch do
    @moduledoc false
    @enforce_keys [:label, :payload, :continue_as]
    defstruct [:label, :payload, :continue_as]

    @type t :: %__MODULE__{
            label: atom(),
            payload: atom(),
            continue_as: SOut.t() | SEnd.t() | SName.t() | SRecRef.t()
          }
  end

  defmodule SIn do
    @moduledoc false
    @enforce_keys [:from, :branches]
    defstruct [:from, :branches]

    @type t :: %__MODULE__{
            from: atom(),
            branches: [SBranch.t()]
          }
  end

  defmodule SOut do
    @moduledoc false
    @enforce_keys [:to, :branches]
    defstruct [:to, :branches]

    @type t :: %__MODULE__{
            to: atom(),
            branches: [SBranch.t()]
          }
  end

  defmodule SEnd do
    @moduledoc false
    defstruct []

    @type t :: %__MODULE__{}
  end

  defmodule SName do
    @moduledoc false
    @enforce_keys [:handler]
    defstruct [:handler]

    @type t :: %__MODULE__{
            handler: atom()
          }
  end

  defmodule SRec do
    @moduledoc false
    @enforce_keys [:id, :body]
    defstruct [:id, :body, outer_recurse: false]

    @type t :: %__MODULE__{
            id: atom(),
            body: SIn.t() | SOut.t(),
            outer_recurse: boolean()
          }
  end

  defmodule SRecRef do
    @moduledoc false
    @enforce_keys [:id]
    defstruct [:id]

    @type t :: %__MODULE__{id: atom()}
  end
end
