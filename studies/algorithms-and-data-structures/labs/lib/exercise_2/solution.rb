module Exercise2
  class Solution
    attr_reader :numbers, :iterations

    def initialize(number, with_print = false)
      @n = number
      @numbers = [prepare_output_digits(number)]
      @iterations = 0
      @with_print = with_print
    end

    def run
      while @n != 495 && @n != 0 do
        n0 = @n
        min, max = calculate_min_max(@n)
        @n1 = max
        @n2 = min
        @n = @n1 - @n2
        @iterations += 1

        if @with_print
          puts "#{@iterations}- Number: #{prepare_output_digits(n0)}: #{prepare_output_digits(max)} - #{prepare_output_digits(min)} = #{prepare_output_digits(@n)}"
        end

        @numbers << prepare_output_digits(@n)
      end

      @n
    end

    private

    def calculate_min_max(number)
      calculate_permutation(number).minmax
    end

    def calculate_permutation(number)
      digits = calculate_digits(number)
      digits.permutation.to_a.map { |e| e.join("").to_i }
    end

    def calculate_digits(number)
      digits = number.digits.reverse

      case digits.length
      when 3 then digits
      when 2 then digits.unshift(0)
      when 1 then digits.unshift(0).unshift(0)
      else raise StandardError, "Invalid Integer"
      end
    end

    def prepare_output_digits(number)
      calculate_digits(number).join("")
    end
  end
end
