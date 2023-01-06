using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TicTacToeCore
{
    internal class GameBoard
    {
        private Player[,] _board;

        internal GameBoard(int size)
        {
            if (size < 3) { throw new ArgumentOutOfRangeException("size"); }

            initializeBoard(size);
        }

        public Player GetField(int row, int col)
        {
            return _board[row, col];
        }

        public void UpdateField(int row, int col, Player player)
        {
            if (row < 0 || row > GetSize() - 1) { throw new ArgumentException($"Invalid value {row}"); }
            if (col < 0 || col > GetSize() - 1) { throw new ArgumentException($"Invalid value {col}"); }
            if (_board[row, col] != null) { throw new ArgumentException("Field already taken"); }

            _board[row, col] = player;
        }

        public int GetSize()
        {
            return _board.GetLength(0);
        }

        public bool IsFilled()
        {
            bool isFilled = true;
            for(int i = 0; i < GetSize(); i++)
            {
                if ( GetField(i, i) != null ) { isFilled = false; break; } 
            }
            return isFilled;
        }

        public Player GetWinner()
        {
            Player player = null;
            if (maybeColumnWinner() != null)
            {
                player = maybeColumnWinner();
            } else if (maybeRowWinner() != null)
            {
                player = maybeRowWinner();
            } else if (maybeDiagonalWinner() != null)
            {
                player = maybeDiagonalWinner();
            };
            
            return player;
        }

        private Player maybeRowWinner()
        {
            Player player = null;
            for(int rowNum = 0; rowNum < GetSize(); rowNum++)
            {
                Player[] row = getRow(rowNum);
                if (isWinnerSet(row)) { player = row[0]; break; }
            }

            return player;
        }

        private Player maybeColumnWinner()
        {
            Player player = null;
            for (int colNum = 0; colNum < GetSize(); colNum++)
            {
                Player[] col = getColumn(colNum);
                if (isWinnerSet(col)) { player = col[0]; break; }
            }

            return player;
        }

        private Player maybeDiagonalWinner()
        {
            Player[] diagonalOne = getDiagonal(Enumerable.Range(0, GetSize()));
            if (isWinnerSet(diagonalOne)) { return diagonalOne[0]; }

            Player[] diagonalTwo = getDiagonal(Enumerable.Range(0, GetSize()).Reverse());
            if (isWinnerSet(diagonalTwo)) { return diagonalTwo[0]; }

            return null;
        }

        private void initializeBoard(int size)
        {
            _board = new Player[size, size];
        }
        private Player[] getColumn(int columnNumber)
        {
            return Enumerable.Range(0, GetSize()).Select(x => _board[x, columnNumber]).ToArray();
        }

        private Player[] getRow(int rowNumber)
        {
            return Enumerable.Range(0, GetSize()).Select(x => _board[rowNumber, x]).ToArray();
        }

        private Player[] getDiagonal(IEnumerable<int> range)
        {
            return range.Select(x => _board[x, x]).ToArray();
        }

        private bool isWinnerSet(Player[] values)
        {
            if (values[0] == null) { return false; };
            if (values.Distinct().Count() == 1) { return true; };

            return false;
        }
    }
}
