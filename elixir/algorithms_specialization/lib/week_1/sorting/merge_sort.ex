defmodule AlgorithmsSpecialization.Week1.Sorting.MergeSort do
  @moduledoc false

  @spec sort(list) :: list
  def sort(list) when is_list(list) and length(list) < 2, do: list

  def sort(list) when is_list(list) do
    splitter = length(list) |> div(2)
    {r, l} = Enum.split(list, splitter)
    merge(sort(r), sort(l))
  end

  defp merge(left_list, right_list) do
    do_merge(left_list, right_list, [])
  end

  defp do_merge([], right, acc), do: acc ++ right
  defp do_merge(left, [], acc), do: acc ++ left

  defp do_merge([left_head | left_tail], right = [right_head | _], acc)
       when left_head < right_head do
    do_merge(left_tail, right, acc ++ [left_head])
  end

  defp do_merge(left, [right_head | right_tail], acc) do
    do_merge(left, right_tail, acc ++ [right_head])
  end
end
