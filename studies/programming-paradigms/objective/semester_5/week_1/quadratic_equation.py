# python3 quadratic_equation.py

class QuadraticEquation:
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c

    def delta(self):
        return self.b**2 - 4 * self.a * self.c

    def roots(self):
        if self.delta() < 0:
            return None
        elif self.delta() == 0:
            return -self.b / (2 * self.a)
        else:
            return (-self.b + self.delta()**0.5) / (2 * self.a), (-self.b - self.delta()**0.5) / (2 * self.a)

equ = QuadraticEquation(1, 3, 1)

print(equ.delta())
print(equ.roots())