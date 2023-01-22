using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TicTacToeCore
{
    public class HumanPlayer : Player
    {
        private string _whenWon, _whenLose;
        public HumanPlayer(string name, string whenWon, string whenLose) : base(name) { 
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
