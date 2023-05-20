using GamePlatformUI.Areas.Games.TicTacToe;
using GamePlatformUI.Models;

namespace GamePlatformUI.Presenters
{
    public class TicTacToePresenter : GamePresenter
    {
        public readonly string playerOneName, playerTwoName;
        private TicTacToeMatch _match;
        private string? _userId;

        public TicTacToePresenter(Game game, string? userId) : base(game)
        {
            _userId = userId;
            _match = TicTacToeMatch.FromJsonString(game.GameMatchJson);
            playerOneName = _getPlayerName(game, _match.PlayerOne);
            playerTwoName = _getPlayerName(game, _match.PlayerTwo);
        }
        
        public override bool canJoin()
        {
            bool validState = _match.State() == TicTacToeMatch.GameStateEnum.PlayerOneRegistrated.ToString();
            bool validPlayer = _match.PlayerOne.playerId != _userId;
            bool validUser = _userId != null;
            return validUser && validState && validPlayer;
        }

        public override bool canStart()
        {
            bool validState = _match.State() == TicTacToeMatch.GameStateEnum.PlayerTwoRegistrated.ToString();
            bool validPlayer = _match.PlayerOne.playerId == _userId;
            bool validUser = _userId != null;
            return validUser && validState && validPlayer;
        }

        public override bool canDelete()
        {
            bool validUser = _userId != null;
            bool validPlayer = _match.PlayerOne.playerId == _userId;
            return validUser && validPlayer;
        }
        
        public override bool canPlay()
        {
            bool validUser = _userId != null;
            bool validPlayerOne = _match.PlayerOne?.playerId == _userId;
            bool validPlayerTwo = _match.PlayerTwo?.playerId == _userId;
            bool validState = _activeMatch();
            return validUser && validState && (validPlayerOne || validPlayerTwo);
        }

        public override string controllerName()
        {
            return "TicTacToeMatch";
        }
        public bool playerTurn()
        {
            bool validUser = _userId != null;
            bool playerTurn = _match.CurrentPlayerId() == _userId;
            bool validState = _activeMatch();

            return validUser && playerTurn && validState;
        }
            
        public bool activeField(int row, int col)
        {
            bool validUser = _userId != null;
            bool emptyField = _match.GetFieldAssigment(row, col) == null;
            bool validState = _activeMatch();
            return validUser && validState && emptyField;
        }

        public bool shouldBeStarted()
        {
            return _match.State() == TicTacToeMatch.GameStateEnum.PlayerTwoRegistrated.ToString();
        }
        
        public string getBoardField(int row, int col)
        {
            var player = _match.GetFieldAssigment(row, col);
            if (player == null) {  return ""; }
            if (player.playerId == _match.PlayerOne.playerId) { return "X"; }
            return "O";
        }

        private bool _activeMatch()
        {
            bool playerOneTurn = _match.State() == TicTacToeMatch.GameStateEnum.PlayerOneTurn.ToString();
            bool playerTwoTurn = _match.State() == TicTacToeMatch.GameStateEnum.PlayerTwoTurn.ToString();

            return playerOneTurn || playerTwoTurn;
        }

        private string _getPlayerName(Game game, Player? player)
        {
            if(player == null)
            {
                return "Not Registrated";
            }

            var gamePlayer = game.GamePlayers.FirstOrDefault(gp => gp.PlayerId == player.playerId);
            
            if(gamePlayer == null)
            {
                return "Not Known";
            }
            
            return gamePlayer.Player.DisplayName();
        }
    }
}
