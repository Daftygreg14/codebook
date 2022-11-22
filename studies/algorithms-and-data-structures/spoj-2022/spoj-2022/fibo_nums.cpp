#include <string>
#include <iostream>
#include <map>

const long moduloVal = 1000000007;
// Map to keep results. Baisc calculation doesnt work with large numbers
std::map<long long, long long> results;

long long fibonacciModulo(long long n) {
	if (n < 3) return 1;

	try {
		return results.at(n);
	}
	catch (std::out_of_range) {
		results[n] = (fibonacciModulo(n - 1) + fibonacciModulo(n - 2)) % moduloVal;
		return results[n];
	};
}

int fiboNums() {
	std::string value;
	getline(std::cin, value);
	short int testsNumber = std::stoi(value);

	for (short int i = 0; i < testsNumber; i++) {
		getline(std::cin, value);
		long long currentNumber = std::stoll(value);
		long long fibonacciNumber = fibonacciModulo(currentNumber);

		std::cout << fibonacciNumber << std::endl;
	}

	return 0;
}