defmodule Maty.Hook do
  alias Maty.Typechecker, as: TC
  alias Maty.Typechecker.Core, as: TC_Core
  alias Maty.Typechecker.Message, as: TC_Msg

  @debug_handler :title_handler

  defmacro __using__(_) do
    quote do
      import Maty.Hook

      Module.register_attribute(__MODULE__, :st, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :handler, accumulate: false)
      Module.register_attribute(__MODULE__, :pairs, accumulate: true, persist: true)

      Module.register_attribute(__MODULE__, :type_specs, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :invalid_collection, accumulate: true, persist: true)
      @compile :debug_info

      @on_definition Maty.Hook
      @before_compile Maty.Hook
    end
  end

  def __on_definition__(env, _kind, name, args, _guards, body) do
    process_handler_annotation(env, {name, args, body})
    IO.puts("My name is #{name}")
    process_type_annotation(env, {name, args, body})
  end

  def __before_compile__(env) do
    pairs = Module.get_attribute(env.module, :pairs)

    for {k, v} <- pairs do
      IO.puts("#{inspect(k)} --> \n#{inspect(v)}\n\n")
    end

    IO.puts("-----------------------------")
  end

  defp remove_branch(module, handler, key) do
    updated_sts =
      Module.get_attribute(module, :st)
      |> Enum.into(%{})
      |> Map.update(handler, [], &Enum.filter(&1, fn x -> x != key end))
      |> Map.to_list()

    Module.delete_attribute(module, :st)

    for {k, v} <- updated_sts do
      Module.put_attribute(module, :st, {k, v})
    end
  end

  def __after_compile__(env, bytecode) do
    try do
      process_bytecode(bytecode)
    catch
      :error, error ->
        IO.puts(:oops)
    end
  end

  defp process_handler_annotation(env, {name, args, body}) do
    sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})
    handler = Module.get_attribute(env.module, :handler)
    Module.delete_attribute(env.module, :handler)

    if handler != nil do
      case Map.fetch(sts, handler) do
        {:ok, _pre} ->
          case TC_Core.typecheck_header(env, handler, args, body) do
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

              remove_branch(env.module, handler, key)
          end

        :error ->
          TC_Msg.handler_annotation(name, handler)
      end
    end
  end

  defp process_type_annotation(env, {name, args, body}) do
    spec = Module.get_attribute(env.module, :spec)

    if not is_nil(spec) and length(spec) > 0 do
      current_line = env.line
      # IO.puts("---->> #{current_line}")

      arity = length(args)

      {:spec, {:"::", _, [{spec_name, _, args_types}, return_type]}, _module} = hd(spec)

      args_types = args_types || []

      # args_types_converted = TC.Expr.get_type(args_types)
      args_types_converted = []

      IO.puts(
        "function #{name}\nspec: #{inspect(args_types)} -> #{inspect(return_type)}\nargs: #{inspect(args)}\nbody: #{inspect(body)}"
      )

      # [
      #   {:title,
      #    {{:., [line: 70, column: 38], [{:__aliases__, [line: 70, column: 32], [:String]}, :t]},
      #     [line: 70, column: 39], []}},
      #   {:role, [line: 70, column: 45], []},
      #   {:session_ctx, [line: 70, column: 53], []},
      #   {:maty_actor_state, [line: 70, column: 68], []}
      # ]
      # {:{}, [line: 65, column: 11],
      #  [
      #    :suspend,
      #    {{:function, [line: 65, column: 23], []}, {:role, [line: 65, column: 35], []}},
      #    {:maty_actor_state, [line: 65, column: 44], []}
      #  ]}

      # [
      #   do:
      #     {:__block__, [],
      #      [
      #        {:=, [line: 67, column: 12],
      #         [
      #           {:amount, [line: 67, column: 5], nil},
      #           {:lookup_price, [line: 67, column: 14], [{:title, [line: 67, column: 27], nil}]}
      #         ]},
      #        {:maty_send, [line: 69, column: 5],
      #         [
      #           {:session, [line: 69, column: 15], nil},
      #           :buyer1,
      #           {:quote, {:amount, [line: 69, column: 42], nil}}
      #         ]},
      #        {:{}, [line: 70, column: 5],
      #         [
      #           :suspend,
      #           {{:&, [line: 70, column: 17],
      #             [
      #               {:/, [line: 70, column: 45],
      #                [
      #                  {{:., [line: 70, column: 28],
      #                    [{:__MODULE__, [line: 70, column: 18], nil}, :decision_handler]},
      #                   [no_parens: true, line: 70, column: 29], []},
      #                  4
      #                ]}
      #             ]}, :buyer2},
      #           {:state, [line: 70, column: 59], nil}
      #         ]}
      #      ]}
      # ]

      return_type_converted = TC.Expr.get_type(return_type)

      if Enum.member?(args_types_converted, :error) or return_type_converted == :error do
        error_message =
          "Problem with @spec for #{spec_name}/#{length(args_types)} " <>
            Macro.to_string(args_types) <>
            " :: " <>
            Macro.to_string(return_type)

        Module.put_attribute(env.module, :invalid_collection, {{name, arity}, error_message})
      else
        types = {spec_name, length(args_types)}

        case types do
          {^name, ^arity} ->
            # Spec describes the current function
            Module.put_attribute(
              env.module,
              :type_specs,
              {{name, arity}, {args_types_converted, return_type_converted}}
            )

          _ ->
            # No spec match
            :ok
        end
      end
    end
  end

  defp process_bytecode(bytecode) do
    try do
      # Gets debug_info chunk from BEAM file
      chunks =
        case :beam_lib.chunks(bytecode, [:debug_info]) do
          {:ok, {_mod, chunks}} -> chunks
          {:error, _, error} -> throw({:error, inspect(error)})
        end

      # Gets the (extended) Elixir abstract syntax tree from debug_info chunk
      dbgi_map =
        case chunks[:debug_info] do
          {:debug_info_v1, :elixir_erl, metadata} ->
            case metadata do
              {:elixir_v1, map, _} ->
                IO.inspect(map)
                map

              {version, _, _} ->
                throw({:error, "Found version #{version} but expected :elixir_v1."})
            end

          x ->
            throw({:error, inspect(x)})
        end
    catch
      _ -> IO.puts(:oops)
    end
  end
end
