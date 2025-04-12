defmodule Maty.Utils do
  def deep_contains?(data, key) when is_list(data) do
    Enum.any?(data, fn item -> deep_contains?(item, key) end)
  end

  def deep_contains?(data, key) when is_tuple(data) do
    data
    |> Tuple.to_list()
    |> deep_contains?(key)
  end

  def deep_contains?(data, key) do
    data == key
  end

  def to_func({name, arity}), do: "#{name}/#{arity}"

  defmodule ModAttr do
    def get_map(module, key) do
      Module.get_attribute(module, key) |> Enum.into(%{})
    end

    def append_to_key(env, attr, key, val) do
      updated_attr =
        Module.get_attribute(env.module, attr)
        |> Enum.into(%{})
        |> Map.update(key, [val], fn vals -> [val | vals] end)
        |> Map.to_list()

      Module.delete_attribute(env.module, attr)

      for entry <- updated_attr do
        Module.put_attribute(env.module, attr, entry)
      end
    end

    def remove_from_key(env, attr, key, val) do
      updated_attr =
        Module.get_attribute(env.module, attr)
        |> Enum.into(%{})
        |> Map.update(key, [], &Enum.filter(&1, fn x -> x != val end))
        |> Map.to_list()

      Module.delete_attribute(env.module, attr)

      for entry <- updated_attr do
        Module.put_attribute(env.module, attr, entry)
      end
    end
  end
end
