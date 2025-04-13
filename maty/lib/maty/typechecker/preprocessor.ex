defmodule Maty.Typechecker.Preprocessor do
  alias Maty.Typechecker.Tc, as: TC
  alias Maty.Typechecker.Error
  alias Maty.Utils
  require Logger

  def process_handler_annotation(
        module: module,
        function: {name, arity},
        handler_label: handler_label,
        session_types: session_types,
        store: handler_store,
        kind: handler_kind,
        meta: meta
      ) do
    with {:ok, session_type} <- Map.fetch(session_types, handler_label),
         {:ok, st} <- Maty.Parser.parse(session_type) do
      Utils.Env.add_at_key(
        module,
        handler_store,
        handler_label,
        %{function: {name, arity}, st: st}
      )
    else
      :error ->
        error = Error.missing_handler(handler_label, meta)
        Logger.error(error)
        {:error, error}

      {:error, _} ->
        error = Error.invalid_session_type_annotation(handler_label)
        Logger.error(error)
        {:error, error}
    end

    Module.delete_attribute(module, handler_kind)
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
          Utils.Env.prepend_to_key(
            env.module,
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
