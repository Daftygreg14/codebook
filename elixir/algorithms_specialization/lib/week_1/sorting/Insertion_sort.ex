defmodule AlgorithmsSpecialization.Week1.Sorting.InsertionSort do
  @moduledoc false

  @spec sort(list) :: list
  def sort(list) when is_list(list) and length(list) < 2, do: list

  def sort(list) do
    do_sort([], list)
  end

  defp do_sort(sorted_list, []), do: sorted_list

  defp do_sort(sorted_list, _unsorted_list = [h | tail]) do
    insert_elem(h, sorted_list) |> do_sort(tail)
  end

  defp insert_elem(elem, []), do: [elem]

  defp insert_elem(elem, list = [min | _tail]) when elem <= min do
    [elem | list]
  end

  defp insert_elem(elem, [min | tail]) do
    [min | insert_elem(elem, tail)]
  end
end
