using GamePlatformUI.Models;
using System.Text.Json;

namespace GamePlatformUI.Areas.Games.TicTacToe
{
    public class TicTacToeMatch : Match
    {
        public enum GameStateEnum
        {
            Init,
            PlayerOneRegistrated,
            PlayerTwoRegistrated,
            PlayerOneTurn,
            PlayerTwoTurn,
            Finished
        };

        private Player? _playerOne, _playerTwo;
        private GameBoard _board;
        private GameStateEnum _state;

        public TicTacToeMatch() : base()
        {
            _state = GameStateEnum.Init;
            _board = new GameBoard();
        }

        // Game API
        public override void Start()
        {
            _state = GameStateEnum.PlayerOneTurn;
        }

        public override void JoinGame(Game game, string userId)
        {
            if (game.GamePlayers?.FirstOrDefault(gp => gp.PlayerId == userId) == null)
            {
                Player player = new Player(userId);
                this.RegisterPlayerTwo(player);
            }
        }

        public void RegisterPlayerOne(Player player)
        {
            _playerOne = player;
            _state = GameStateEnum.PlayerOneRegistrated;
        }

        public void RegisterPlayerTwo(Player player)
        {
            _playerTwo = player;
            _state = GameStateEnum.PlayerTwoRegistrated;
        }

        public void TakeShot(CellPosition cell)
        {
            _board.UpdateField(cell.Row, cell.Col, _getCurrentPlayer());
            Console.WriteLine("HERE \n\r");
            Console.WriteLine($"{_board.IsFilled()}");
            Console.WriteLine($"Player One: #{_playerOne.playerId}");
            Console.WriteLine($"Player Two: #{_playerTwo.playerId}");
            
            Console.WriteLine($"{_board.GetWinner()}");

            if (_board.IsFilled() || _board.GetWinner() != null)
            {
                FinishGame();
            }
            else
            {
                _togglePlayer();
            }
        }

        public void FinishGame()
        {
            _state = GameStateEnum.Finished;
        }

        public int BoardSize()
        {
            return _board.GetSize();
        }

        public Player? GetFieldAssigment(int row, int col)
        {
            return _board.GetField(row, col);
        }

        public Player? GetWinner()
        {
            return _board.GetWinner();
        }

        public Player? GetLoser()
        {
            if (PlayerOne == GetWinner()) { return PlayerTwo; }
            if (PlayerTwo == GetWinner()) { return PlayerOne; }

            return null;
        }

        // Serializers
        public override string ToJsonString()
        {
            var v = new MatchJsonData
            {
                playerOneId = this.PlayerOne?.playerId,
                playerTwoId = this.PlayerTwo?.playerId,
                board = this.Board.ToJsonString(),
                state = this.State().ToString()
            };

            return JsonSerializer.Serialize(v);
        }

        public new static TicTacToeMatch FromJsonString(string jsonString)
        {
            var gameData = JsonSerializer.Deserialize<MatchJsonData>(jsonString);
            if (gameData == null) { throw new System.ArgumentException("Invalid JSON string"); }
            
            var playerOne = gameData.playerOneId != null ? new Player(gameData.playerOneId) : null;
            var playerTwo = gameData.playerTwoId != null ? new Player(gameData.playerTwoId) : null;
            var board = gameData.board != null ? GameBoard.FromJsonString(gameData.board) : null;
            var state = gameData.state != null ? (GameStateEnum)Enum.Parse(typeof(GameStateEnum), gameData.state) : GameStateEnum.Init;

            return new TicTacToeMatch()
            {
                _playerOne = playerOne,
                _playerTwo = playerTwo,
                _board = board,
                _state = state
            };

        }

        // Data Accessors
        public override string State()
        {
            return _state.ToString();
        }
        public override string CurrentPlayerId()
        {
            return _getCurrentPlayer().playerId;
        }
        public GameBoard Board
        {
            get { return _board; }
        }
        public Player? PlayerOne
        {
            get { return _playerOne; }
        }

        public Player? PlayerTwo
        {
            get { return _playerTwo; }
        }


        // Helper Functions
        private Player _getCurrentPlayer()
        {
            if (_state == GameStateEnum.PlayerTwoTurn && _playerTwo != null)
            {
                return _playerTwo;
            }
            else if (_state == GameStateEnum.PlayerOneTurn && _playerOne != null)
            {
                return _playerOne;
            } else
            {
                throw new ApplicationException("Invalid Match State");
            }
        }
        private void _togglePlayer()
           
        {
            if (_state == GameStateEnum.PlayerOneTurn)
            {
                _state = GameStateEnum.PlayerTwoTurn;
            }
            else
            {
                _state = GameStateEnum.PlayerOneTurn;
            }
        }

    }
}