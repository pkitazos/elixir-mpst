defmodule Maty.Typechecker.SessionChecker do
  alias Maty.ST, as: ST
  alias Maty.Typechecker.Error, as: Err

  def process_handler_annotation(env, {name, args, body}) do
    sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})
    handler = Module.get_attribute(env.module, :handler)
    Module.delete_attribute(env.module, :handler)

    if handler != nil do
      case Map.fetch(sts, handler) do
        {:ok, _pre} ->
          case typecheck_header(env, handler, args, body) do
            {:error, error} ->
              # IO.inspect(env.line)
              # IO.inspect(env.module)
              # IO.inspect(env.file)
              IO.puts(error)

            {:ok, {label, key}} ->
              # todo find better name for key
              Module.put_attribute(
                env.module,
                :pairs,
                {{name, length(args), label}, {handler, key}}
              )

              updated_sts =
                Module.get_attribute(env.module, :st)
                |> Enum.into(%{})
                |> Map.update(handler, [], &Enum.filter(&1, fn x -> x != key end))
                |> Map.to_list()

              Module.delete_attribute(env.module, :st)

              for {k, v} <- updated_sts do
                Module.put_attribute(env.module, :st, {k, v})
              end
          end

        :error ->
          Message.handler_annotation(name, handler)
      end
    end
  end

  def typecheck_header(env, handler, args, _body) do
    arity = length(args)

    cond do
      arity > 4 -> IO.puts("too many args passed to handler")
      arity < 4 -> IO.puts("too few args passed to handler")
      true -> :ok
    end

    [msg, from | _] = args

    if tuple_size(msg) != 2 do
      error = Err.malformed_message(handler, env.line, tuple_size: tuple_size(msg))
      {:error, error}
    else
      {label, val} = msg

      all_sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})
      keys = all_sts[handler]

      bs =
        for key <- keys do
          branch = ST.Lookup.get(key)
          expected_label = elem(branch.message, 0)

          cond do
            # for this first check any branch should yield the same result
            from != branch.from ->
              error =
                Err.participant_mismatch(handler, env.line, expected: branch.from, got: from)

              {:error, error}

            label != expected_label ->
              error = Err.label_mismatch(handler, env.line, expected: expected_label, got: label)
              {:error, error}

            not is_some_val(val) ->
              # todo somehow use dialyzer types
              {:error, fn -> IO.puts("Type mismatch") end}

            true ->
              {:ok, {label, key}}
          end
        end

      bs |> monad_sum()
    end
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
