defmodule AlgorithmsSpecialization.Week2.MatrixMultiplicationTest do
  use ExUnit.Case, async: true

  alias AlgorithmsSpecialization.Week2.MatrixMultiplication

  describe "multi/2" do
    test "returns matrix multiplication for size 1" do
      matrix_a = [[3]]
      matrix_b = [[2]]

      assert [[6]] == MatrixMultiplication.multi(matrix_a, matrix_b)
    end

    test "returns matrix multiplication for size 2" do
      matrix_a = [
        [1, 2],
        [3, 4]
      ]

      matrix_b = [
        [5, 6],
        [7, 8]
      ]

      expected_result = [
        [19, 22],
        [43, 50]
      ]

      assert expected_result == MatrixMultiplication.multi(matrix_a, matrix_b)
    end

    test "returns matrix multiplication for size 4" do
      matrix_a = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
      ]

      matrix_b = [
        [13, 14, 15, 16],
        [9, 10, 11, 12],
        [5, 6, 7, 8],
        [1, 2, 3, 4]
      ]

      expected_result = [
        [50, 60, 70, 80],
        [162, 188, 214, 240],
        [274, 316, 358, 400],
        [386, 444, 502, 560]
      ]

      assert expected_result == MatrixMultiplication.multi(matrix_a, matrix_b)
    end

    test "raises error when dimensions does not matches" do
      matrix_a = [
        [2, 1],
        [3, 4]
      ]

      matrix_b = [
        [3, 4],
        [5, 3],
        [6, 7]
      ]

      assert_raise RuntimeError, "Can not multiply matrices; matrices must be square", fn ->
        MatrixMultiplication.multi(matrix_a, matrix_b)
      end
    end

    test "raises error when sizes does not matches" do
      matrix_a = [
        [2, 1],
        [3, 4]
      ]

      matrix_b = [
        [3, 4, 5],
        [5, 3, 2],
        [6, 7, 3]
      ]

      assert_raise RuntimeError,
                   "Can not multiply matrices; matrices must be equal of size",
                   fn ->
                     MatrixMultiplication.multi(matrix_a, matrix_b)
                   end
    end

    test "raises error when sizes are not pow of 2" do
      matrix_a = [
        [3, 4, 5],
        [5, 3, 2],
        [6, 7, 3]
      ]

      matrix_b = [
        [3, 4, 5],
        [5, 3, 2],
        [6, 7, 3]
      ]

      assert_raise RuntimeError,
                   "Can not multiply matrices; matrices size must be pow of 2",
                   fn ->
                     MatrixMultiplication.multi(matrix_a, matrix_b)
                   end
    end

    test "raises error when matrix is invalid" do
      matrix_a = [
        [2, 1],
        [3, 4]
      ]

      matrix_b = [
        [3, 4],
        [5, 3, 4]
      ]

      assert_raise MatchError, fn ->
        MatrixMultiplication.multi(matrix_a, matrix_b)
      end
    end
  end
end
