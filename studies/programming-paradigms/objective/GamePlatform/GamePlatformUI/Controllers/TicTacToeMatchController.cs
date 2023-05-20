using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Identity;

using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Services;
using GamePlatformUI.Factories;
using GamePlatformUI.Areas.Games.TicTacToe;
using NuGet.Protocol;

namespace GamePlatformUI.Controllers
{
    public class TicTacToeMatchController : Controller
    {
        private readonly IGameRepository _gameRepo;
        private readonly UserManager<User> _userManager;
        private readonly GamePresenterFactory _presenterFactory;
        private readonly MatchFactory _matchFactory;

        public TicTacToeMatchController(IGameRepository repo, UserManager<User> userManager)
        {
            _gameRepo = repo;
            _userManager = userManager;
            _presenterFactory = new GamePresenterFactory();
            _matchFactory = new MatchFactory();
        }

        // GET: Match/Edit/5
        public IActionResult Details(long id)
        {
            var game = _gameRepo.GetGame(id);
            var currentUserId = _userManager.GetUserId(User);

            if (game == null) { return NotFound(); }
            if (currentUserId == null) { return NotFound(); }

            var presenter = _presenterFactory.CreateGamePresenter(game, currentUserId);
            return View(presenter);
        }
        
        // POST: Match/Start/5
        public IActionResult Start(long id)
        {
            var game = _gameRepo.GetGame(id);
            var currentUserId = _userManager.GetUserId(User);

            if (game == null) { return NotFound(); }
            if (currentUserId == null ) { return NotFound(); }

            var match = _matchFactory.LoadMatch(game);
            match.StartGame(game, currentUserId);
            game.StoreMatch(match);
            _gameRepo.UpdateGame(game);

            return RedirectToAction(nameof(Edit), new { id });
        }

        // GET: Match/Edit/5
        public IActionResult Edit(long id)
        {
            var game = _gameRepo.GetGame(id);
            var currentUserId = _userManager.GetUserId(User);

            if (game == null) { return NotFound();}
            if (currentUserId == null) { return NotFound(); }

            var presenter = _presenterFactory.CreateGamePresenter(game, currentUserId);
            return View(presenter);
        }

        // POST: Match/Edit/5
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Edit(long id, [Bind("Row,Col")] CellPosition cell)
        {
            var game = _gameRepo.GetGame(id);
            var currentUserId = _userManager.GetUserId(User);

            if (game == null) { return NotFound(); }
            if (currentUserId == null) { return NotFound(); }

            TicTacToeMatch match = _matchFactory.LoadMatch(game) as TicTacToeMatch;
            Console.WriteLine(cell.ToJson());
            
            match.TakeShot(cell);
            game.StoreMatch(match);
            _gameRepo.UpdateGame(game);
            return RedirectToAction(nameof(Edit), new { id });
        }

    }
}
