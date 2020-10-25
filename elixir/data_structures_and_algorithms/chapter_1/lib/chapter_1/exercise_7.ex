defmodule Chapter1.Exercise7 do
  def calculate(p, q) when is_integer(p) and is_integer(q) and p > q do
    case rem(p, q) do
      0 -> q
      r -> calculate(p, r)
    end
  end
end
