#

class NumDivision:
    def __init__(self, num):
        self.num = num

    def division(self):
        if self.num % 3 and self.num % 5 == 0:
            return "The number is divisible by 3 and 5"
        elif self.num % 3 or self.num % 5 == 0:
            return "The number is divisible by 3 or 5"
        else:
            return "The number is not divisible by 3 or 5"


num = NumDivision(15)
print(num.division())