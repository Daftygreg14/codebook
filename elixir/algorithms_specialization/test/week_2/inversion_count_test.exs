defmodule AlgorithmsSpecialization.Week2.InversionCountTest do
  use ExUnit.Case, async: true

  alias AlgorithmsSpecialization.Week2.InversionCount

  describe "count/1" do
    for %{list: list, count: count} <- [
          %{list: [], count: 0},
          %{list: [1], count: 0},
          %{list: [1, 3, 5, 2, 4, 6], count: 3},
          %{list: [4, 3, 2, 1], count: 6},
          %{list: [1, 20, 6, 4, 5], count: 5},
          %{list: [5, 4, 3, 2, 1], count: 10},
          %{list: [1, 2, 3, 4], count: 0},
          %{list: [1, 1, 1, 1], count: 0}
        ] do
      test "for list #{list} result should be: #{count}" do
        assert unquote(count) == InversionCount.count(unquote(list))
      end
    end
  end
end
