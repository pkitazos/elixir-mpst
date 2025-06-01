defmodule Maty.Typechecker.Error.Internal do
  defstruct [:title, :opts, :message]

  @type t :: %__MODULE__{
          title: binary(),
          opts: binary(),
          message: binary()
        }
end
