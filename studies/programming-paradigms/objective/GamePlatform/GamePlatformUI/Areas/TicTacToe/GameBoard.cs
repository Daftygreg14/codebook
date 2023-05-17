using System.Text.Json;

namespace GamePlatformUI.Areas.TicTacToe
{
    public class GameBoard
    {
        private Player[,] _board;

        public GameBoard()
        {
            initializeBoard(3);
        }

        public string toJsonString()
        {
            int length = _board.GetLength(0);
            
            string[][] board = new string[length][];

            for (int i = 0; i < length; i++)
            {
                board[i] = new string[length];
                for (int j = 0; j < length; j++)
                {
                    if (_board[i, j] != null)
                    {
                        board[i][j] = _board[i, j].playerId;
                    }
                    else
                    {
                        board[i][j] = "";
                    }
                }
            }

            var v = new { board = board };
            return JsonSerializer.Serialize(v);
        }

        public static GameBoard FromJsonString(string jsonString)
        {
            var boardData = JsonSerializer.Deserialize<string[][]>(jsonString);
            int length = boardData.Length;
            var _board = new Player[length, length];

            for (int i = 0; i < length; i++)
            {
                for (int j = 0; j < length; j++)
                {
                    if (!string.IsNullOrEmpty(boardData[i][j]))
                    {
                        _board[i, j] = new Player(boardData[i][j]);
                    }
                    else
                    {
                        _board[i, j] = null;
                    }
                }
            }

            return new GameBoard{
                _board = _board
            };
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

            for (int row = 0; row < GetSize(); row++)
            {
                for (int col = 0; col < GetSize(); col++)
                {
                    if (GetField(row, col) != null) { isFilled = false; break; }
                }
            }
            return isFilled;
        }

        public Player GetWinner()
        {
            Player player = null;
            if (maybeColumnWinner() != null)
            {
                player = maybeColumnWinner();
            }
            else if (maybeRowWinner() != null)
            {
                player = maybeRowWinner();
            }
            else if (maybeDiagonalWinner() != null)
            {
                player = maybeDiagonalWinner();
            };

            return player;
        }

        private Player maybeRowWinner()
        {
            Player player = null;
            for (int rowNum = 0; rowNum < GetSize(); rowNum++)
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
