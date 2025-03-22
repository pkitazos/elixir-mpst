defmodule Maty.Typechecker.Preprocessor do
  alias Maty.Typechecker.{CoreChecker, Error}
  alias Maty.Utils
  require Logger

  def process_handler_annotation(module, kind, name, arity, handler) do
    sts = Module.get_attribute(module, :st) |> Enum.into(%{})
    annotated_handlers = Module.get_attribute(module, :annotated_handlers)

    cond do
      kind == :defp ->
        error = Error.no_private_handlers()
        Logger.error(error)
        {:error, error}

      true ->
        with {:ok, st_key} <- Map.fetch(sts, handler) do
          case Enum.find(annotated_handlers, fn {k, v} -> v == st_key end) do
            {{^name, ^arity}, ^st_key} ->
              :ok

            nil ->
              Module.put_attribute(module, :annotated_handlers, {{name, arity}, st_key})

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

  def process_type_annotation(env, {name, args, body}) do
    case Module.get_attribute(env.module, :spec) do
      [{:spec, {:"::", _, [{spec_name, _, args_types}, return_type]}, _module} | _] ->
        arity = length(args)

        args_types_checked = CoreChecker.get_type(args_types || [])
        return_type_checked = CoreChecker.get_type(return_type)

        cond do
          Utils.deep_contains?([args_types_checked], :error) ->
            error = Error.spec_args_not_well_typed(spec_name, args_types)
            Logger.error(error)
            Module.put_attribute(env.module, :spec_errors, {{name, arity}, error})

          Utils.deep_contains?([return_type_checked], :error) ->
            error = Error.spec_return_not_well_typed(spec_name, return_type)
            Logger.error(error)
            Module.put_attribute(env.module, :spec_errors, {{name, arity}, error})

          true ->
            types = {spec_name, length(args_types)}

            case types do
              {^name, ^arity} ->
                Utils.ModAttr.append_to_key(
                  env,
                  :type_specs,
                  {name, arity},
                  {args_types_checked, return_type_checked}
                )

              _ ->
                error = "Spec name: #{spec_name} doesn't match function name: #{name}"
                Logger.error(error)
                Module.put_attribute(env.module, :spec_errors, {{name, arity}, error})
            end
        end

      _ ->
        :ok
    end
  end
end
