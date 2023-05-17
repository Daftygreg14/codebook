using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using GamePlatformUI.Models;
using GamePlatformUI.Repository;
using Microsoft.AspNetCore.Identity;

namespace GamePlatformUI.Controllers
{
    public class GamesController : Controller
    {
        private readonly GameRepository _repo;
        private readonly UserManager<IdentityUser> _userManager;

        public GamesController(GameRepository repo, UserManager<IdentityUser> userManager)
        {
            _repo = repo;
            _userManager = userManager;
        }

        // GET: Games
        public IActionResult Index()
        {
            return View(_repo.GetGames());
        }

        // GET: Games/Details/5
        public IActionResult Details(long id)
        {
            var game = _repo.GetGame(id);
            if (game == null)
            {
                return NotFound();
            }

            return View(game);
        }

        // GET: Games/Create
        public IActionResult Create()
        {
            return View();
        }

        // POST: Games/Create
        // [TODO] Refactor to create GamePlayer in GameRepository
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Create([Bind("Id,GameType,CreatedAt,UpdatedAt")] Game game)
        {
            string currentUserId = _userManager.GetUserId(User);

            if (ModelState.IsValid && currentUserId != null)
            {
                _repo.AddGame(game, currentUserId);
                return RedirectToAction(nameof(Index));
            }
            return View(game);
        }

        // DELETE: Games/Delete/5
        [HttpDelete, ActionName("Delete")]
        [ValidateAntiForgeryToken]
        public IActionResult DeleteConfirmed(long id)
        {
            _repo.DeleteGame(id);
            return RedirectToAction(nameof(Index));
        }
    }
}
