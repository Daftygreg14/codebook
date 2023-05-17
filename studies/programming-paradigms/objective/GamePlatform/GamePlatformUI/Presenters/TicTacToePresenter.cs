using GamePlatformUI.Areas.TicTacToe;
using GamePlatformUI.Models;

namespace GamePlatformUI.Presenters
{
    public class TicTacToePresenter : GamePresenter
    {
        public string playerOneName, playerTwoName;
        private TicTacToeMatch _match;
        private string? _userId;

        public TicTacToePresenter(Game game, string? userId) : base(game)
        {
            _userId = userId;
            Console.WriteLine(game.GameMatchJson);
            _match = TicTacToeMatch.FromJsonString(game.GameMatchJson);
            Console.WriteLine(_match.ToJsonString());

            playerOneName = getPlayerName(game, _match.PlayerOne);
            playerTwoName = getPlayerName(game, _match.PlayerTwo);
        }

        public override bool canJoin()
        {
            bool validState = _match.State == TicTacToeMatch.GameStateEnum.PlayerOneRegistrated;
            bool validPlayer = _match.PlayerOne.playerId != _userId;
            return validState && validPlayer;
        }

        public override bool canStart()
        {
            bool validState = _match.State == TicTacToeMatch.GameStateEnum.PlayerTwoRegistrated;
            bool validPlayer = _match.PlayerOne.playerId == _userId;
            return validState && validPlayer;
        }

        public override bool canDelete()
        {
            return _match.PlayerOne.playerId == _userId;
        }

        public bool shouldBeStarted()
        {
            return _match.State == TicTacToeMatch.GameStateEnum.PlayerTwoRegistrated;
        }
        
        public string getBoardField(int row, int col)
        {
            var player = _match.GetFieldAssigment(row, col);
            return getFieldSign(player);
        }

        private string getFieldSign(Player? player)
        {
            if (player == _match.PlayerOne) { return "X"; };
            if (player == _match.PlayerTwo) { return "O"; };

            return "";
        }

        private string getPlayerName(Game game, Player? player)
        {
            if(player == null)
            {
                return "Not Registrated";
            }

            var gamePlayer = game.GamePlayers.Where(gp => gp.PlayerId == player.playerId).First();
            
            if(gamePlayer == null)
            {
                return "Not Known";
            }
            
            return gamePlayer.Player.DisplayName();
        }
    }
}
