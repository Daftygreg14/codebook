using GamePlatformUI.Models;

namespace GamePlatformUI.Presenters
{
    public abstract class GamePresenter
    {
        public readonly Int64 id;
        private string type, hostName;
        private string createdAt;
        protected Game _game;
        public GamePresenter(Game game)
        {
            id = game.Id;
            _game = game;
        }
        public abstract string GameState { get; }
        public string HostName { get { return getHostName(); } }
        public string Type { get { return _game.GameType; } }
        public string CreatedAt { get { return _game.CreatedAt.ToShortDateString(); } }

        public abstract bool CanStart();

        public abstract bool CanJoin();

        public abstract bool CanPlay();

        public abstract bool CanDelete();

        public abstract string ControllerName();

        private string getHostName()
        {
            if (_game?.Host() != null && _game.Host()?.DisplayName() != null)
            {
                return _game.Host().DisplayName();
            }
            return string.Empty;
        }
        
    }
}
