defmodule Maty.Hook do
  alias Maty.Typechecker

  defmacro __using__(_) do
    quote do
      import Maty.Hook

      Module.register_attribute(__MODULE__, :st, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :handler, accumulate: false)
      Module.register_attribute(__MODULE__, :pairs, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :fn_st_keys, accumulate: true, persist: true)

      Module.register_attribute(__MODULE__, :type_specs, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :spec_errors, accumulate: true, persist: true)

      @compile :debug_info

      @on_definition Maty.Hook
      @before_compile Maty.Hook
      # @after_compile Maty.Hook
    end
  end

  def __on_definition__(env, kind, name, args, guards, body) do
    Typechecker.handle_on_definition(env, kind, name, args, guards, body)
  end

  def __before_compile__(env) do
    Typechecker.handle_before_compile(env)
  end

  def __after_compile__(env, bytecode) do
    Typechecker.handle_after_compile(env, bytecode)
  end
end
