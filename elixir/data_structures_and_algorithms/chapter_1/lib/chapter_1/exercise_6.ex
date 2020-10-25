defmodule Chapter1.Exercise6 do
  defstruct values: []

  @range Enum.to_list(1..31)

  def new(value) when is_integer(value), do: new([value])
  def new([]), do: %__MODULE__{values: []}
  def new(values) when is_list(values) do
    if validate?(values) do
      %__MODULE__{values: Enum.uniq(values)}
    else
      raise ArgumentError
    end
  end

  defp validate?(values), do: Enum.all?(values, &Enum.member?(@range, &1))

  def make_null(%__MODULE__{}), do: new([])

  def union(%__MODULE__{values: values1}, %__MODULE__{values: values2}) do
    values = values1 ++ values2
    new(values)
  end

  def size(%__MODULE__{values: values}), do: length(values)
end
