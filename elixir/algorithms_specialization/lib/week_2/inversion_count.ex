defmodule AlgorithmsSpecialization.Week2.InversionCount do
  @moduledoc false

  @spec count(list) :: integer
  def count(list) when is_list(list) do
    {_, count} = sort_and_count(list)
    count
  end

  defp sort_and_count([]), do: {[], 0}
  defp sort_and_count([el]), do: {[el], 0}

  defp sort_and_count(list) do
    n = length(list) |> div(2) |> round()
    {l, r} = Enum.split(list, n)

    # left inversions
    {list_b, count_x} = sort_and_count(l)
    # right inversions
    {list_c, count_y} = sort_and_count(r)
    # split inversions
    {list_d, count_z} = merge_and_count(list_b, list_c)

    {list_d, count_x + count_y + count_z}
  end

  defp merge_and_count(left, right), do: do_merge_and_count(left, right, {[], 0})

  defp do_merge_and_count([], right, {acc, count}), do: {acc ++ right, count}
  defp do_merge_and_count(left, [], {acc, count}), do: {acc ++ left, count}

  defp do_merge_and_count([left_head | left_tail], right = [right_head | _], {acc, count})
       when left_head <= right_head do
    do_merge_and_count(left_tail, right, {acc ++ [left_head], count})
  end

  defp do_merge_and_count(left, [right_head | right_tail], {acc, count}) do
    do_merge_and_count(left, right_tail, {acc ++ [right_head], count + length(left)})
  end
end
