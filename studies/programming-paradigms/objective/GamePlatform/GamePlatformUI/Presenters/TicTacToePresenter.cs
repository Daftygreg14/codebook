using GamePlatformUI.Areas.TicTacToe;
using GamePlatformUI.Models;

namespace GamePlatformUI.Presenters
{
    public class TicTacToePresenter : GamePresenter
    {
        public string playerOneName, playerTwoName;
        private TicTacToeMatch _match;
        
        public TicTacToePresenter(Game game) : base(game)
        {
            _match = TicTacToeMatch.FromJsonString(game.GameMatchJson);
            playerOneName = getPlayerName(game, _match.PlayerOne);
            playerTwoName = getPlayerName(game, _match.PlayerTwo);
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
