namespace GamePlatformUI.Areas.TicTacToe
{
    public class Player
    {
        public string playerId;
        private string _whenWon, _whenLose;
        public Player(string playerId, string whenWon, string whenLose)
        {
            playerId = playerId;
            _whenWon = whenWon;
            _whenLose = whenLose;
        }

        public string TextWhenWon()
        {
            return _whenWon;
        }

        public string TextWhenLose()
        {
            return _whenLose;
        }
    }
}