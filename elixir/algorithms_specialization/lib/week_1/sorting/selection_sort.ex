defmodule AlgorithmsSpecialization.Week1.Sorting.SelectionSort do
  @moduledoc false

  @spec sort(list) :: list
  def sort(list) when is_list(list) and length(list) < 2, do: list

  def sort(list) when is_list(list) do
    do_sort(list, [])
  end

  defp do_sort([], acc), do: acc

  defp do_sort(list, acc) do
    max = Enum.max(list)
    new_list = List.delete(list, max)
    do_sort(new_list, [max | acc])
  end
end
