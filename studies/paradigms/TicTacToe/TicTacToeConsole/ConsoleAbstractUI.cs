using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TicTacToeCore;

namespace TicTacToeConsole
{
    internal abstract class ConsoleAbstractUI
    {
        protected Game _game;
        public ConsoleAbstractUI(Game game)
        {
            _game = game;
        }
        public abstract void RenderTitle();
    }
}
