namespace GamePlatformUI.Abstracts
{
    public abstract class Player
    {
        private string _name;
        public Player(string name)
        {
            validateName(name);
            _name = name;
        }

        public string Name { get { return _name; } }

        protected void validateName(string name)
        {
            if (name == string.Empty)
            {
                throw new ArgumentException("name cannot be an empty string", "name");
            }
        }

        public abstract string TextWhenWon();
        public abstract string TextWhenLose();
    }
}