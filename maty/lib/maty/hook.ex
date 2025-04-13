defmodule Maty.Hook do
  alias Maty.{Typechecker, Utils}

  defmacro __using__(_) do
    quote do
      import Maty.Hook

      # ephemeral annotations
      Module.register_attribute(__MODULE__, :init_handler, accumulate: false, persist: false)
      Module.register_attribute(__MODULE__, :handler, accumulate: false, persist: false)
      Module.register_attribute(__MODULE__, :spec, accumulate: false, persist: false)

      # attribute stores
      Utils.Env.setup(__MODULE__, :st)
      Utils.Env.setup(__MODULE__, :delta)
      Utils.Env.setup(__MODULE__, :annotated_init_handlers)
      Utils.Env.setup(__MODULE__, :type_specs)
      Utils.Env.setup(__MODULE__, :spec_errors)

      @compile :debug_info

      @on_definition Maty.Hook
      @before_compile Maty.Hook
      @after_compile Maty.Hook
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
