#include <iostream>
#include <vector>
#include <tuple>

using relationPair = std::tuple<int, int>;
using relationsVector = std::vector<relationPair>;

using matrixRow = std::vector<int>;
using matrixType = std::vector<matrixRow>;

// used https://slideplayer.pl/slide/806912/
// 1 0 0 
// 0 1 0
// 0 0 1
bool isReflexive(matrixType matrix, int maxNum)
{
	for (int rc = 0; rc < maxNum; rc++) {
		if (matrix[rc][rc] == 0) { return false; }
	}

	return true;
}

// 0 1 1
// 1 0 1
// 1 1 0
bool isIreflexive(matrixType matrix, int maxNum)
{
	for (int rc = 0; rc < maxNum; rc++) {
		if (matrix[rc][rc]) { return false; };
	}

	return true;
}

// 0 1 0
// 1 0 1
// 0 1 0
bool isSymmetric(matrixType matrix, int maxNum)
{
	for (int r = 0; r < maxNum - 1; r++) {
		for (int c = r + 1; c < maxNum; c++) {
			if (matrix[r][c] != matrix[c][r]) { return false; };
		}
	}

	return true;
}

// 0 1 0
// 0 0 1
// 1 0 0
bool isAsymmetric(matrixType matrix, int maxNum) {
	for (int r = 0; r < maxNum - 1; r++) {
		for (int c = r + 1; c < maxNum; c++) {
			if (matrix[r][c] && matrix[c][r]) { return false; };
		}
	}

	return true;
}

bool isTransistive(matrixType matrix, int maxNum) {
	for (int r = 0; r < maxNum; r++) for (int c = 0; c < maxNum; c++) {
		int value = 0;

		for (int k = 0; (!value) && (k < maxNum); k++) {
			value = matrix[r][k] && matrix[k][c];
		}

		if (matrix[r][c] < value) { return false; };
	}

	return true;
}

bool isConnected(matrixType matrix, int maxNum) {
	for (int r = 0; r < maxNum - 1; r++) {
		for (int c = r + 1; c < maxNum; c++) {
			if (!(matrix[r][c] || matrix[c][r])) { return false; };
		}
	}

	return true;
}



int main()
{
	int numOne, numTwo;
	int maxNum = 0;
	relationsVector relations;

	// Build relations vector
	while (std::cin >> numOne >> numTwo) {
		if (numOne > maxNum) { maxNum = numOne; };
		if (numTwo > maxNum) { maxNum = numTwo; };
		relationPair pair = relationPair(numOne, numTwo);
		relations.push_back(pair);
	}

	// Build binary matrix
	// 1 = true, 0 = false
	int matrixSize = maxNum + 1;
	matrixType relationMatrix = matrixType(matrixSize);

	// Build matrix with default 0
	for (int i = 0; i < matrixSize; i++) {
		relationMatrix[i] = matrixRow(matrixSize);
	}

	// Fill matrix with relations
	for (int i = 0; i < relations.size(); i++) {
		relationPair pair = relations[i];
		int rowIdx = std::get<0>(pair) - 1;
		int colIdx = std::get<1>(pair) - 1;

		relationMatrix[rowIdx][colIdx] = 1;
	}

	// Check binary matrix
	bool ref = isReflexive(relationMatrix, maxNum);
	bool iref = isIreflexive(relationMatrix, maxNum);
	bool sym = isSymmetric(relationMatrix, maxNum);
	bool asym = isAsymmetric(relationMatrix, maxNum);
	bool conn = isConnected(relationMatrix, maxNum);
	bool tran = isTransistive(relationMatrix, maxNum);

	// Print Line One
	if (ref) { std::cout << "Z" << " "; };
	if (iref) { std::cout << "PZ" << " "; };
	if (sym) { std::cout << "S" << " "; };
	if (asym) { std::cout << "AS" << " "; };
	if (tran) { std::cout << "P" << " "; };
	if (conn) { std::cout << "SP" << " "; }

	if (!ref && !iref && !sym && !asym && !tran && !conn) {
		std::cout << "X";
		return 0;
	}
	// Start Second Line
	std::cout << "\n";
	// Check relation types combinations
	bool rr = ref && sym && tran;
	bool rpl = ref && asym && tran && conn;
	bool rpcz = ref && asym && tran;
	// Print Line Two
	if (rr) { std::cout << "RR" << " "; };
	if (rpl) { std::cout << "RPL" << " "; };
	if (!rpl && rpcz) { std::cout << "RPCz"; };
	if (!rr && !rpl && !rpcz) { std::cout << "X"; };

	return 0;
}