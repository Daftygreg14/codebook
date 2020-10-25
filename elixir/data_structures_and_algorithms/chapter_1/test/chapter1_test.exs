defmodule Chapter1Test do
  use ExUnit.Case
  doctest Chapter1

  test "greets the world" do
    assert Chapter1.hello() == :world
  end
end
