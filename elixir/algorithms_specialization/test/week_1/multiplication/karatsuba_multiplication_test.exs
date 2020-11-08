defmodule AlgorithmsSpecialization.Week1.Multiplication.KaratsubaMultiplicationTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias AlgorithmsSpecialization.Week1.Multiplication.KaratsubaMultiplication

  describe "multi/2" do
    test "returns course example result" do
      assert 5678 * 1234 == KaratsubaMultiplication.multi(5678, 1234)
    end

    test "returns 0 when first arg is 0" do
      assert 0 == KaratsubaMultiplication.multi(0, 1)
    end

    test "returns 0 when second arg is 0" do
      assert 0 == KaratsubaMultiplication.multi(1, 0)
    end

    test "multiplicities two integer" do
      check all(int1 <- positive_integer(), int2 <- positive_integer(), max_runs: 100) do
        assert int1 * int2 == KaratsubaMultiplication.multi(int1, int2)
      end
    end
  end
end
