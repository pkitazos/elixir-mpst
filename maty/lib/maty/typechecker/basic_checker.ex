defmodule Maty.Typechecker.BasicChecker do
  def spec_to_type() do
  end

  def process_type_annotation(env, {name, args, body}) do
    spec = Module.get_attribute(env.module, :spec)

    if not is_nil(spec) and length(spec) > 0 do
      current_line = env.line
      # IO.puts("---->> #{current_line}")

      arity = length(args)

      {:spec, {:"::", _, [{spec_name, _, args_types}, return_type]}, _module} = hd(spec)

      args_types = args_types || []

      # args_types_converted = TC.Expr.get_type(args_types)
      args_types_converted = []
      # return_type_converted = TC.Expr.get_type(return_type)
      return_type_converted = :some_type

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
end
