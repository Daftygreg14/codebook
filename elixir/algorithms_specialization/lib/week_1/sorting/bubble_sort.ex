defmodule AlgorithmsSpecialization.Week1.Sorting.BubbleSort do
  @moduledoc false

  # performance optimization as prepending is faster than appending
  import Enum, only: [reverse: 1]

  @spec sort(list) :: list
  # dont need to sort empty or one elem array
  def sort(list) when is_list(list) and length(list) < 2, do: list

  def sort(list) when is_list(list) do
    do_sort(list, [], :passed) |> reverse()
  end

  # when there is no more cases and status is :passed return acc.
  defp do_sort([], acc, :passed), do: acc
  # when there is no more cases and status is not :passed start second loop with acc as and arg.
  defp do_sort([], acc, _status), do: do_sort(reverse(acc), [], :passed)

  # when there is only one element prepend it to acc and pass previous status
  defp do_sort([h], acc, status), do: do_sort([], [h | acc], status)

  # when there is >=2 elements in list and first is bigger than second, prepend second to acc and change
  # status to :unpassed as we don't know list status
  defp do_sort([h1, h2 | tail], acc, _status) when h1 > h2 do
    do_sort([h1 | tail], [h2 | acc], :unpassed)
  end

  # when h1 <= h2 we can prepend h1 to acc and pass previous status to the function
  defp do_sort([h1, h2 | tail], acc, status) do
    do_sort([h2 | tail], [h1 | acc], status)
  end
end
