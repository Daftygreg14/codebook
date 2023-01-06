using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Remoting.Metadata.W3cXsd2001;
using System.Text;
using System.Threading.Tasks;

namespace TicTacToeCore
{
    public class Game
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

        private string _uuid;
        private Player _playerOne, _playerTwo;
        private GameBoard _board;
        private GameStateEnum _state;

        public Game()
        {
            _uuid = Guid.NewGuid().ToString();
            _state = GameStateEnum.Init;
        }

        // Game API
        public void Initialized()
        {
            _state = GameStateEnum.PlayerOneTurn;
        }

        public void RegisterPlayerOne(HumanPlayer player)
        {
            _playerOne = player;
            _state = GameStateEnum.PlayerOneRegistrated;
        }

        public void RegisterPlayerTwo(Player player)
        {
            _playerTwo = player;
            _state = GameStateEnum.PlayerTwoRegistrated;
        }

        public void InitializeBoard(int size)
        {
            _board = new GameBoard(size);
            _state = GameStateEnum.PlayerOneTurn;
        }

        public void TakeShot(int row, int col)
        {
            _board.UpdateField(row, col, GetCurrentPlayer());

            if(_board.IsFilled() || _board.GetWinner() != null)
            {
                FinishGame();
            } else
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

        public Player FieldValue(int row, int col)
        {
            return _board.GetField(row, col);
        }

        public Player GetCurrentPlayer()
        {
            if (_state == GameStateEnum.PlayerTwoTurn)
            {
                return _playerTwo;
            } else
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
            if(PlayerOne == GetWinner()) { return PlayerTwo; }

            return PlayerOne;
        }

        // Props
        public GameStateEnum State { get { return _state; } }
        public string Uuid { get { return _uuid; } }
        public Player PlayerOne
        {
            get { return _playerOne; }
        }

        public Player PlayerTwo 
        {
            get { return _playerTwo; }
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
