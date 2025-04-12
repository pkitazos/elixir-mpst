defmodule Maty.Macros do
  @type role :: Maty.Types.role()
  @type session_id :: Maty.Types.session_id()
  @type init_token :: Maty.Types.init_token()
  @type session :: Maty.Types.session()
  @type session_ctx :: Maty.Types.session_ctx()
  @type maty_actor_state :: Maty.Types.maty_actor_state()
  @type suspend :: Maty.Types.suspend()
  @type done :: Maty.Types.done()

  @moduledoc """
  Provides user-friendly macros for the Maty framework.

  These macros simplify the process of writing session-typed actors by providing
  a more declarative syntax for handlers and communication operations.
  """

  defmacro __using__(_opts) do
    quote do
      import Maty.Macros, only: [handler: 5]
      alias Maty.MatyDSL, as: MatyDSL
    end
  end

  @doc """
  Defines a handler function with proper type annotations and pattern matching.

  ## Parameters

  * `handler_name` - The name of the handler function to define
  * `role` - The role that this handler receives messages from
  * `pattern` - The message pattern to match, with type annotations
  * `state_var` - The name to use for the state variable
  * `do_block` - The body of the handler function

  ## Examples

      handler :title_handler, :buyer1, {:title, title :: binary()}, state do
        amount = lookup_price(title)
        Maty.send(:buyer1, {:quote, amount})
        Maty.suspend(:decision_handler, state)
      end
  """
  defmacro handler(handler_name, role, pattern, state_var, do: body) do
    # first verify we have a proper tagged tuple pattern
    # the pattern should be a 2-tuple with an atom tag as the first element
    case pattern do
      {tag, _payload} when is_atom(tag) ->
        # Process the pattern to extract the cleaned pattern and type specification
        {clean_pattern, type_spec} = process_pattern(pattern)

        quote do
          @handler unquote(handler_name)
          @spec unquote(handler_name)(
                  unquote(role),
                  unquote(type_spec),
                  session_ctx(),
                  maty_actor_state()
                ) :: suspend() | done()
          def unquote(handler_name)(
                unquote(role),
                unquote(clean_pattern),
                session_ctx,
                unquote(state_var)
              ) do
            try do
              unquote(body)
            catch
              {:suspend, next_handler, new_state} ->
                {:suspend, {next_handler, unquote(role)}, new_state}
            end
          end
        end

      _ ->
        # ERROR: Expected a tagged tuple pattern like {:tag, payload}
        # for now, we'll just raise a compile-time error
        raise ArgumentError, "Expected a tagged tuple pattern like {:tag, payload}"
    end
  end

  # Process the pattern to strip type annotations and build a type spec
  defp process_pattern({tag, payload}) when is_atom(tag) do
    {clean_payload, payload_type} = process_payload(payload)
    {{tag, clean_payload}, quote(do: {unquote(tag), unquote(payload_type)})}
  end

  # Case 1: Payload with type annotation
  defp process_payload({:"::", _, [var, type]}) do
    {var, type}
  end

  # Case 2: 2-tuple of variables
  defp process_payload({a, b}) do
    {a_clean, a_type} = process_var_with_type(a)
    {b_clean, b_type} = process_var_with_type(b)
    {{a_clean, b_clean}, quote(do: {unquote(a_type), unquote(b_type)})}
  end

  # Case 3: n-tuple of variables
  defp process_payload({:{}, context, elements}) do
    {clean_elements, element_types} = Enum.unzip(Enum.map(elements, &process_var_with_type/1))
    {{:{}, context, clean_elements}, quote(do: {unquote_splicing(element_types)})}
  end

  # Case 4: List with type annotation
  defp process_payload({:"::", _, [[_ | _] = list, list_type]}) do
    clean_vars =
      Enum.map(list, fn
        {var, _, ctx} when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) -> {var, [], ctx}
        # Handle literals or other values
        var -> var
      end)

    {clean_vars, list_type}
  end

  # Case 5: List without type annotation (default to list(any()))
  defp process_payload(list) when is_list(list) do
    clean_vars =
      Enum.map(list, fn
        {var, _, ctx} when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) -> {var, [], ctx}
        # Handle literals or other values
        var -> var
      end)

    {clean_vars, quote(do: list(any()))}
  end

  # Case 6: Simple variable without type annotation
  defp process_payload({var, meta, ctx}) when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    {{var, meta, ctx}, quote(do: any())}
  end

  # Case 7: Literal value (number, string, atom, boolean)
  defp process_payload(literal)
       when is_number(literal) or is_binary(literal) or is_atom(literal) or is_boolean(literal) do
    {literal, infer_type_from_literal(literal)}
  end

  # Default case - for any other pattern, we'll just leave it as is and use any() type
  # pin - this would be an error case in a stricter implementation
  defp process_payload(other) do
    {other, quote(do: any())}
  end

  # Helper to process a variable that might have a type annotation
  defp process_var_with_type({:"::", _, [var, type]}) do
    {var, type}
  end

  defp process_var_with_type({var, meta, ctx})
       when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    {{var, meta, ctx}, quote(do: any())}
  end

  defp process_var_with_type(literal)
       when is_number(literal) or is_binary(literal) or is_atom(literal) or is_boolean(literal) do
    {literal, infer_type_from_literal(literal)}
  end

  defp process_var_with_type(other) do
    # This would be an error case in a stricter implementation
    {other, quote(do: any())}
  end

  # Helper to infer a type from a literal value
  defp infer_type_from_literal(literal) do
    cond do
      is_binary(literal) -> quote(do: binary())
      is_integer(literal) -> quote(do: number())
      is_float(literal) -> quote(do: number())
      is_boolean(literal) -> quote(do: boolean())
      is_atom(literal) -> quote(do: atom())
      true -> quote(do: any())
    end
  end
end
