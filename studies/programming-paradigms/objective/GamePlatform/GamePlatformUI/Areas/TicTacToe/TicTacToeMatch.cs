using System.Text.Json;

namespace GamePlatformUI.Areas.TicTacToe
{
    public class TicTacToeMatch
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
        private GameBoard? _board;
        private GameStateEnum _state;

        public TicTacToeMatch() : base()
        {
            _state = GameStateEnum.Init;
        }

        // Game API
        public void Start()
        {
            _state = GameStateEnum.PlayerOneTurn;
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

        public void InitializeBoard()
        {
            _board = new GameBoard();
        }

        public void TakeShot(int row, int col)
        {
            _board.UpdateField(row, col, GetCurrentPlayer());

            if (_board.IsFilled() || _board.GetWinner() != null)
            {
                FinishGame();
            }
            else
            {
                togglePlayer();
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

        public Player GetFieldAssigment(int row, int col)
        {
            return _board.GetField(row, col);
        }

        public Player GetCurrentPlayer()
        {
            if (_state == GameStateEnum.PlayerTwoTurn)
            {
                return _playerTwo;
            }
            else
            {
                return _playerOne;
            }
        }

        public Player GetWinner()
        {
            return _board.GetWinner();
        }

        public Player GetLoser()
        {
            if (PlayerOne == GetWinner()) { return PlayerTwo; }

            return PlayerOne;
        }

        // Props
        public GameStateEnum State { get { return _state; } }
        public Player PlayerOne
        {
            get { return _playerOne; }
        }

        public Player PlayerTwo
        {
            get { return _playerTwo; }
        }

        // Serializers
        public string ToJsonString()
        {
            return JsonSerializer.Serialize(this);
        }

        public static TicTacToeMatch FromJsonString(string json)
        {
            return JsonSerializer.Deserialize<TicTacToeMatch>(json);
        }

        // Helper Functions
        private void togglePlayer()
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