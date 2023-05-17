using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Identity;

using GamePlatformUI.Models;
using GamePlatformUI.Services;
using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Presenters;
using Microsoft.CodeAnalysis.VisualBasic.Syntax;

namespace GamePlatformUI.Controllers
{
    public class GamesController : Controller
    {
        private readonly IGameRepository _gameRepo;
        private readonly IGameTypeRepository _gameTypeRepo;
        private readonly UserManager<User> _userManager;

        public GamesController(IGameRepository repo, IGameTypeRepository typeRepo, UserManager<User> userManager)
        {
            _gameRepo = repo;
            _gameTypeRepo = typeRepo;
            _userManager = userManager;
        }

        // GET: Games
        public IActionResult Index()
        {
            var games = _gameRepo.GetGames().Select(game => new GamePresenter(game)).ToList();
            return View(games);
        }

        // GET: Games/Details/5
        public IActionResult Details(long id)
        {
            var game = _gameRepo.GetGame(id);
            if (game == null)
            {
                return NotFound();
            }

            return View(game);
        }

        // GET: Games/Create
        public IActionResult Create()
        {
            ViewBag.GameTypes = _gameTypeRepo.GetAvailableGameTypes().ToList();
            return View();
        }

        // POST: Games/Join/5
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Join(long id)
        {
            string currentUserId = _userManager.GetUserId(User);
            Game game = _gameRepo.GetGame(id);

            if (game != null && currentUserId != null)
            {
                Console.WriteLine(game.ToString());
                game.JoinGame(currentUserId);
                _gameRepo.UpdateGame(game);
                return RedirectToAction(nameof(Index));
            }

            debugModelState();
            return RedirectToAction(nameof(Index));
        }

        // POST: Games/Create
        // [TODO] Refactor to create GamePlayer in GameRepository
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Create([Bind("GameType")] Game game)
        {
            string currentUserId = _userManager.GetUserId(User);

            if (ModelState.IsValid && currentUserId != null)
            {
                _gameRepo.AddGame(game, currentUserId);
                return RedirectToAction(nameof(Index));
            }
            debugModelState();
            ViewBag.GameTypes = _gameTypeRepo.GetAvailableGameTypes().ToList();
            return View(game);
        }

        // POST: Games/Delete/5
        [HttpPost, ActionName("Delete")]
        [ValidateAntiForgeryToken]
        public IActionResult Delete(long id)
        {
            var game = _gameRepo.GetGame(id);
            if (game == null)
            {
                return NotFound();
            }
            if (game.Host().Id != _userManager.GetUserId(User))
            {
                return BadRequest();
            }
            _gameRepo.DeleteGame(id);
            return RedirectToAction(nameof(Index));
        }

        private void debugModelState()
        {
            {
                var errors = ModelState
                    .Where(x => x.Value.Errors.Count > 0)
                    .Select(x => new { x.Key, x.Value.Errors })
                    .ToArray();

                foreach (var error in errors)
                {
                    foreach (var subError in error.Errors)
                    {
                        Console.WriteLine($"Key: {error.Key}, Error: {subError.ErrorMessage}");
                    }
                }
            }
        }
    }
}
