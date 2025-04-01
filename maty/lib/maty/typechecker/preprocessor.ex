defmodule Maty.Typechecker.Preprocessor do
  alias Maty.Typechecker.Tc, as: TC
  alias Maty.Typechecker.{Error}
  alias Maty.{ST, Utils}
  require Logger

  def process_handler_annotation(module, kind, name, arity, handler, meta) do
    sts = Module.get_attribute(module, :st) |> Enum.into(%{})
    annotated_handlers = Module.get_attribute(module, :annotated_handlers)

    cond do
      kind == :defp ->
        error = Error.no_private_handlers(meta)
        Logger.error(error)
        {:error, error}

      true ->
        with {:ok, st_key} <- Map.fetch(sts, handler) do
          st = ST.Lookup.get(st_key)

          case Enum.find(annotated_handlers, fn {_, v} -> v == st end) do
            {{^name, ^arity}, ^st} ->
              :ok

            # todo: also store the handler label
            nil ->
              Module.put_attribute(
                module,
                :annotated_handlers,
                {{name, arity}, st}
              )

              Module.put_attribute(
                module,
                :handler_defs,
                {{name, arity}, handler}
              )

            {{prev_fn_name, prev_fn_arity}, _} ->
              prev = "#{to_string(prev_fn_name)}/#{prev_fn_arity}"
              curr = "#{to_string(name)}/#{arity}"

              error = Error.handler_already_taken(handler, prev, curr)

              Logger.error(error)
              {:error, error}
          end
        else
          :error ->
            error = Error.missing_handler(handler)
            Logger.error(error)
            {:error, error}
        end
    end

    Module.delete_attribute(module, :handler)
  end

  def process_type_annotation(env, {name, args}) do
    var_env =
      Maty.Types.payload_types()
      |> Enum.map(&{&1, &1})
      |> Enum.into(%{})
      |> Map.merge(Maty.Types.map())

    case Module.get_attribute(env.module, :spec) do
      [{:spec, {:"::", _, [{spec_name, _, args_types}, return_type]}, _module} | _] ->
        arity = length(args)

        with {:info, {^name, ^arity}} <- {:info, {spec_name, length(args_types)}},
             {:args, {:ok, {:list, typed_args}, _}} <- {:args, TC.typecheck(var_env, args_types)},
             {:return, {:ok, typed_return, _}} <- {:return, TC.typecheck(var_env, return_type)} do
          Utils.ModAttr.append_to_key(
            env,
            :type_specs,
            {name, arity},
            {typed_args, typed_return}
          )

          Module.delete_attribute(env.module, :spec)
        else
          {:info, _} ->
            error =
              "Spec info: {#{spec_name}, #{length(args_types)}} doesn't match function info: {#{name}, #{arity}}"

            Logger.error(error)
            Module.put_attribute(env.module, :spec_errors, {{name, arity}, error})

          {:args, {:error, _, _}} ->
            error = Error.spec_args_not_well_typed(spec_name, args_types)
            Logger.error(error)
            Module.put_attribute(env.module, :spec_errors, {{name, arity}, error})

          {:return, {:error, _, _}} ->
            error = Error.spec_return_not_well_typed(spec_name, return_type)
            Logger.error(error)
            Module.put_attribute(env.module, :spec_errors, {{name, arity}, error})
        end

      _ ->
        :ok
    end
  end
end
