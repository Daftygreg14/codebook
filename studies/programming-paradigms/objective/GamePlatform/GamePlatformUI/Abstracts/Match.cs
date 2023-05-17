namespace GamePlatformUI.Abstracts
{
    public abstract class Match
    {
        private Player _playerOne;
        private Int64 _gameId;

        public Match(Int64 gameId)
        {
            _gameId = gameId;
        }
        public abstract void Start();
    
        public void RegisterPlayerOne(Player player)
        {
            _playerOne = player;
        }
        public abstract void FinishGame();

    }
}
