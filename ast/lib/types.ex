defmodule Maty.Typechecker.Types do
  # Haskell: data BaseType = TUnit | TInt | TBool | TString
  # Elixir (tagged tuple version)
  @type base_type ::
          :t_unit
          | :t_int
          | :t_bool
          | :t_string

  # Haskell: data ST = End | SOut OutputST | SIn InputST
  # Elixir
  @type st ::
          :end
          # SOut (OutST p msgs)
          | {:out, participant, [{label, type, st}]}
          # SIn  (InST p msgs)
          | {:in, participant, [{label, type, st}]}
          # If you really need SAnnotation
          | {:annotation, String.t()}

  @type type ::
          {:base, base_type}
          | {:func, type, st, st | nil, type | nil}
          | {:ap_type, [{participant, st}]}
          | {:handler, st}

  @type val ::
          {:v_var, name}
          | {:v_unit}
          | {:v_bool, boolean}
          | ...
          | {:v_lam, name, type, st, st, expr}
          | {:v_handler, participant, [{label, name, type, st, expr}]}

  @type expr ::
          {:e_return, val}
          | {:e_cont, expr, expr}
          | ...
          | {:e_if, val, expr, expr}
          | ...
end
