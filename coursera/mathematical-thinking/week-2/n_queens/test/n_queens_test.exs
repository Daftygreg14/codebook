defmodule NQueensTest do
  use ExUnit.Case
  doctest NQueens

  test "greets the world" do
    assert NQueens.run(4) == :world
  end
end
