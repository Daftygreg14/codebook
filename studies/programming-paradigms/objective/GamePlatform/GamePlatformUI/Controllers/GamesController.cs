using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Rendering;
using GamePlatformUI.Models;
using GamePlatformUI.Repository;

namespace GamePlatformUI.Controllers
{
    public class GamesController : Controller
    {
        private readonly GameRepository _repo;

        public GamesController(GameRepository repo)
        {
            _repo = repo;
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
        [HttpPost]
        [ValidateAntiForgeryToken]
        public IActionResult Create([Bind("Id,GameType,CreatedAt,UpdatedAt")] Game game)
        {
            if (ModelState.IsValid)
            {
                _repo.AddGame(game);
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
