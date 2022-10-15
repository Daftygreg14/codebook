#!/usr/bin/env ruby

class Solution
  attr_reader :invariant_checks, :invariant_checks_succeeded

  def initialize(input_size)
    @invariant_checks = 0
    @invariant_checks_succeeded = 0
    @balls = prepare_balls(input_size)
  end

  def run
    raise StandardError unless balls_num_is_odd?

    do_solution
  end

  private

  def prepare_balls(n)
    whites = [:white] * n
    blacks = n.odd? ? [:black] * n : [:black] * (n + 1)
    whites + blacks
  end

  def do_solution
    return @balls.first if @balls.size <= 1
    invariant_check

    # Drops two samples from array
    sample_1 = fetch_sample
    sample_2 = fetch_sample

    # Recursion
    if sample_1 == sample_2
      do_solution
    else
      @balls.push(:black)
      do_solution
    end
  end

  def fetch_sample
    idx = rand(@balls.size)
    @balls.delete_at(idx)
  end

  def invariant_check
    @invariant_checks += 1
    @invariant_checks_succeeded += 1 if balls_num_is_odd?
  end

  def balls_num_is_odd?
    @balls.filter { |el| el.eql?(:black) }.size.odd?
  end
end

m = (n = rand(1_000)).odd? ? n : n + 1
solution = Solution.new(m)
result = solution.run()

puts "Result: #{result}"
puts "Invariants Checks: #{solution.invariant_checks}"
puts "Invariants Checks Succeeded: #{solution.invariant_checks_succeeded}"
