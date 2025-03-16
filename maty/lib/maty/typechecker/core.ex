defmodule Maty.Typechecker.Core do
  alias Maty.ST, as: ST
  alias Maty.Typechecker.Error, as: Err

  def typecheck_header(module, handler, args, _body) do
    arity = length(args)

    cond do
      arity > 4 -> IO.puts("too many args passed to handler")
      arity < 4 -> IO.puts("too few args passed to handler")
      true -> :ok
    end

    [msg, from | _] = args
    {label, val} = msg

    all_sts = Module.get_attribute(module, :st) |> Enum.into(%{})
    keys = all_sts[handler]

    bs =
      for key <- keys do
        branch = ST.Lookup.get(key)

        cond do
          # for this first check any branch should yield the same result
          from != branch.from ->
            {:error, &Err.participant_mismatch/0}

          # from here on is where there's a difference
          !is_tuple(msg) ->
            {:error, &Err.malformed_message/0}

          is_var(msg) ->
            # todo make distinct from non-tuple message
            {:error, &Err.malformed_message/0}

          label != elem(branch.message, 0) ->
            {:error, &Err.label_mismatch/0}

          !is_some_val(val) ->
            # todo somehow use dialyzer types
            {:error, fn -> IO.puts("Type mismatch") end}

          true ->
            {:ok, {label, key}}
        end
      end

    bs |> monad_sum()
  end

  @type ast_var :: {atom(), list(), nil}

  defp is_var({tag, ctx, children}), do: is_atom(tag) and is_list(ctx) and is_nil(children)
  defp is_var(_), do: false

  defp is_some_val(var) do
    # todo make this list exhaustive
    is_var(var) or
      is_atom(var) or
      is_binary(var) or
      is_number(var) or
      is_bitstring(var) or
      is_boolean(var)
  end

  defp extract_body(do: {:__block__, [], block}), do: block
  defp extract_body(do: expr), do: expr

  defp monad_sum(bs) do
    case Enum.find(bs, fn {atom, _} -> atom == :ok end) do
      {:ok, id} ->
        {:ok, id}

      _ ->
        # * handle different errors
        # may be a non-issue once I start consuming session types from my annotations
        List.first(bs)
    end
  end
end
