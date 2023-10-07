defmodule NQueens do
  @moduledoc """
  N-Queens problem solver.
  """

  def run(n), do: do_run(n, [])

  # -----------------------------------------------------------------------------
  defp do_run(size, board) when length(board) == size, do: board
  defp do_run(size, board) do
    Enum.reduce(0..size, board, fn k, board_acc ->
      if Enum.member?(board_acc, k) do
        board_acc
      else
        new_board = List.insert_at(board, -1, k)
        IO.inspect("k: #{k}")
        IO.inspect("board_acc: #{inspect(board_acc)}")

        checked_board = check_board(size, new_board)

        IO.inspect("Before pop: #{inspect(board_acc)}")
        {_, val} = List.pop_at(checked_board, -1)
        val
      end
    end)
  end

  defp check_board(size, board) do
    if is_safe?(board) do
      IO.inspect("Safe: #{inspect(board)}")
      do_run(size, board)
    else
      board
    end
  end

  # -----------------------------------------------------------------------------
  defp is_safe?(board) when length(board) == 1, do: true
  defp is_safe?(board) do
    col = length(board) - 1
    cols = 0..col

    !Enum.any?(cols, fn row ->
      col - row == abs(Enum.at(board, row) - Enum.at(board, col))
    end)
  end
end
