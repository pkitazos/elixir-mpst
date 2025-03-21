defmodule Maty.Typechecker.Test do
  # # any()
  # @spec test(:hello) :: nil
  # def test(_a), do: nil

  # # 0..255
  # @spec test(arity()) :: nil
  # def test(_a), do: nil

  # # 	t
  # @spec test(as_boolean(t)) :: nil
  # def test(_a), do: nil

  # # <<_::_*8>>
  # @spec test(binary()) :: nil
  # def test(_a), do: nil

  # # <<_::8, _::_*8>>
  # @spec test(nonempty_binary()) :: nil
  # def test(_a), do: nil

  # # <<_::_*1>>
  # @spec test(bitstring()) :: nil
  # def test(_a), do: nil

  # # <<_::1, _::_*1>>
  # @spec test(nonempty_bitstring()) :: nil
  # def test(_a), do: nil

  # # true | false
  # @spec test(boolean()) :: nil
  # def test(_a), do: nil

  # # 0..255
  # @spec test(byte()) :: nil
  # def test(_a), do: nil

  # # 0..0x10FFFF
  # @spec test(char()) :: nil
  # def test(_a), do: nil

  # # [char()]
  # @spec test(charlist()) :: nil
  # def test(_a), do: nil

  # # [char(), ...]
  # @spec test(nonempty_charlist()) :: nil
  # def test(_a), do: nil

  # # (... -> any)
  # @spec test(fun()) :: nil
  # def test(_a), do: nil

  # # fun()
  # @spec test(function()) :: nil
  # def test(_a), do: nil

  # # pid() | port() | reference()
  # @spec test(identifier()) :: nil
  # def test(_a), do: nil

  # # iolist() | binary()
  # @spec test(iodata()) :: nil
  # def test(_a), do: nil

  # # maybe_improper_list(byte() | binary() | iolist(), binary() | [])
  # @spec test(iolist()) :: nil
  # def test(_a), do: nil

  # # [{atom(), any()}]
  # @spec test(keyword()) :: nil
  # def test(_a), do: nil

  # # 	[{atom(), t}]
  # @spec test(keyword(t)) :: nil
  # def test(_a), do: nil

  # # [any()]
  # @spec test(list()) :: nil
  # def test(_a), do: nil

  # # nonempty_list(any())
  # @spec test(nonempty_list()) :: nil
  # def test(_a), do: nil

  # # maybe_improper_list(any(), any())
  # @spec test(maybe_improper_list()) :: nil
  # def test(_a), do: nil

  # # nonempty_maybe_improper_list(any(), any())
  # @spec test(nonempty_maybe_improper_list()) :: nil
  # def test(_a), do: nil

  # # {module(), atom(), arity()}
  # @spec test(mfa()) :: nil
  # def test(_a), do: nil

  # # atom()
  # @spec test(module()) :: nil
  # def test(_a), do: nil

  # # none()
  # @spec test(no_return()) :: nil
  # def test(_a), do: nil

  # # atom()
  # @spec test(node()) :: nil
  # def test(_a), do: nil

  # # integer() | float()
  # @spec test(number()) :: nil
  # def test(_a), do: nil

  # # %{:__struct__ => atom(), optional(atom()) => any()}
  # @spec test(struct()) :: nil
  # def test(_a), do: nil

  # # :infinity | non_neg_integer()
  # @spec test(timeout()) :: nil
  # def test(_a), do: nil
end
