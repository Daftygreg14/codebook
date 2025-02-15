using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;
using GamePlatformUI.Models;
using Microsoft.AspNetCore.Identity;

namespace GamePlatformUI.Areas.Identity.Data;

// Add profile data for application users by adding properties to the User class
public class User : IdentityUser
{
    public ICollection<GamePlayer>? GamePlayers { get; set; }
    public string? DisplayName()
    {
        if(UserName != null)
        {
            return UserName;
        } 
        return Email;
    }
}

