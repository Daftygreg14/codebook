using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using GamePlatformUI.Models;
using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Services;
using Microsoft.AspNetCore.Identity;

namespace GamePlatformUI.Controllers
{
    public class MatchController : Controller
    {
        private readonly IGameRepository _gameRepo;
        private readonly UserManager<User> _userManager;

        public MatchController(IGameRepository repo, UserManager<User> userManager)
        {
            _gameRepo = repo;
            _userManager = userManager;
        }

        // POST: Match/StartGame/5
        public async Task<IActionResult> Start(long id)
        {
            var game = _gameRepo.GetGame(id);
            var currentUserId = _userManager.GetUserId(User);
            
            if(game != null && currentUserId != null)
            {
                game.StartGame(currentUserId);
                _gameRepo.UpdateGame(game);
            }
            return RedirectToAction(nameof(Edit));
        }

        // GET: Match/Edit/5
        public async Task<IActionResult> Edit(long id)
        {
            var game = _gameRepo.GetGame(id);
            return View(game);
        }

        // POST: Match/Edit/5
        [HttpPost]
        [ValidateAntiForgeryToken]
        public async Task<IActionResult> Edit(long id, [Bind("Id,GameType,GameState,GameMatchJson,CreatedAt,UpdatedAt")] Game game)
        {
            return RedirectToAction(nameof(Index));
        }

    }
}
