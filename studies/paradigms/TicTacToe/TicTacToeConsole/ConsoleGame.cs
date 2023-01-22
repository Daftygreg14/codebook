using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;
using TicTacToeCore;

namespace TicTacToeConsole
{
    internal class ConsoleGame
    {
        private Game _game;
        private ConsoleGameUI _gameUI;
        private ConsoleRegistrationUI _registrationUI;
        public ConsoleGame()
        {
            _game = new Game();
            _gameUI = new ConsoleGameUI(_game);
            _registrationUI = new ConsoleRegistrationUI(_game);
        }

        /// <summary>
        /// Main Game loop.
        /// This function is responsible for UI orchestration.
        /// </summary>
        public void Play()
        {
            while (_game.State != Game.GameStateEnum.Finished) {
                Console.Clear();

                switch (_game.State)
                {
                    case Game.GameStateEnum.Init:
                        _registrationUI.RenderTitle();
                        _registrationUI.PlayerOneRegistration();
                        break;
                    case Game.GameStateEnum.PlayerOneRegistrated:
                        _registrationUI.RenderTitle();
                        _registrationUI.PlayerTwoRegistration();
                        break;
                    case Game.GameStateEnum.PlayerTwoRegistrated:
                        _gameUI.RenderTitle();
                        _gameUI.SetupBoard();
                        break;
                    case Game.GameStateEnum.PlayerOneTurn: case Game.GameStateEnum.PlayerTwoTurn:
                        _gameUI.RenderTitle();
                        _gameUI.RenderBoard();
                        _gameUI.PlayTurn();
                        break;
                }
            }

            Console.Clear();
            _gameUI.RenderTitle();
            _gameUI.RenderBoard();
            _gameUI.RenderResult();
        }
    }
}
