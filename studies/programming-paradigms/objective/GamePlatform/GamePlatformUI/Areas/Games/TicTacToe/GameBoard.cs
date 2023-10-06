using Microsoft.EntityFrameworkCore.Metadata.Internal;
using NuGet.Protocol;
using System.Drawing;
using System.Text.Json;

namespace GamePlatformUI.Areas.Games.TicTacToe
{
    public class GameBoard
    {
        private Player[,] _board;

        public GameBoard()
        {
            _board = new Player[3, 3];
        }

        // Serialization API
        public string ToJsonString()
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

            return System.Text.Json.JsonSerializer.Serialize(board);
        }
        
        public static GameBoard FromJsonString(string jsonString)
        {
            var boardData = JsonSerializer.Deserialize<string[][]>(jsonString);
            if (boardData == null) { throw new ArgumentException("Invalid JSON string"); }
            
            int length = boardData.Length;
            var board = new Player?[length, length];

            for (int i = 0; i < length; i++)
            {
                for (int j = 0; j < length; j++)
                {
                    if (!string.IsNullOrEmpty(boardData[i][j]))
                    {
                        board[i, j] = new Player(boardData[i][j]);
                    }
                    else
                    {
                        board[i, j] = null;
                    }
                }
            }

            return new GameBoard { _board = board };
        }

        // Game API
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
            int size = GetSize();

            for (int row = 0; row < size; row++)
            {
                for (int col = 0; col < size; col++)
                {
                    if (GetField(row, col) == null) 
                    { 
                        isFilled = false;
                        break; 
                    }
                }
            }

            return isFilled;
        }

        public Player? GetWinner()
        {
            Player? player = null;
            if (_maybeColumnWinner() != null)
            {
                player = _maybeColumnWinner();
            }
            else if (_maybeRowWinner() != null)
            {
                player = _maybeRowWinner();
            }
            else if (_maybeDiagonalWinner() != null)
            {
                player = _maybeDiagonalWinner();
            };

            return player;
        }

        private Player? _maybeRowWinner()
        {
            Player? player = null;
            for (int rowNum = 0; rowNum < GetSize(); rowNum++)
            {
                Player?[] row = _getRow(rowNum);
                if (_isWinnerSet(row)) { player = row[0]; break; }
            }

            return player;
        }

        private Player? _maybeColumnWinner()
        {
            Player? player = null;
            for (int colNum = 0; colNum < GetSize(); colNum++)
            {
                Player?[] col = _getColumn(colNum);
                if (_isWinnerSet(col)) { player = col[0]; break; }
            }

            return player;
        }

        private Player? _maybeDiagonalWinner()
        {
            Player?[] diagonalOne = _getDiagonal(Enumerable.Range(0, GetSize()));
            if (_isWinnerSet(diagonalOne)) { return diagonalOne[0]; }

            Player?[] diagonalTwo = _getDiagonal(Enumerable.Range(0, GetSize()).Reverse());
            if (_isWinnerSet(diagonalTwo)) { return diagonalTwo[0]; }

            return null;
        }

        private Player?[] _getColumn(int columnNumber)
        {
            return Enumerable.Range(0, GetSize()).Select(x => _board[x, columnNumber]).ToArray();
        }

        private Player?[] _getRow(int rowNumber)
        {
            return Enumerable.Range(0, GetSize()).Select(x => _board[rowNumber, x]).ToArray();
        }

        private Player?[] _getDiagonal(IEnumerable<int> range)
        {
            return range.Select(x => _board[x, x]).ToArray();
        }

        private bool _isWinnerSet(Player?[] values)
        {
            if (values[0] == null) { return false; };
            if (values.Select(val => val?.playerId).Distinct().Count() == 1) { return true; };

            return false;
        }
    }
}
