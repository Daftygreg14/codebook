using GamePlatformUI.Abstracts;

namespace GamePlatformUI.Areas.TicTacToe
{
    public class HumanPlayer : Player
    {
        private string _whenWon, _whenLose;
        public HumanPlayer(string name, string whenWon, string whenLose) : base(name)
        {
            _whenWon = whenWon;
            _whenLose = whenLose;
        }

        public override string TextWhenWon()
        {
            return _whenWon;
        }

        public override string TextWhenLose()
        {
            return _whenLose;
        }
    }
}