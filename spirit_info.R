# Base game --------------------------------------------------------------------

spirits <- list(
  "Spirit Island"=list("River Surges in Sunlight",
                       "Vital Strength of the Earth",
                       "Lightning's Swift Strike",
                       "Shadows Flicker Like Flame",
                       "Thunderspeaker",
                       "A Spread of Rampant Green",
                       "Ocean's Hungry Grasp",
                       "Bringer of Dreams and Nightmares"
  ))

aspect_list <- list("River Surges in Sunlight"=c("None"),
                    "Vital Strength of the Earth"=c("None"),
                    "Lightning's Swift Strike"=c("None"),
                    "Shadows Flicker Like Flame"=c("None"))

boards <- c("A", "B", "C", "D")

adversaries <- c("None",
                 "Brandenburg-Prussia",
                 "England",
                 "Sweden")

scenarios <- c("None",
               "Blitz",
               "Guard the Isle's Heart",
               "Rituals of Terror",
               "Dahan Insurrection"
)

# Expansion packs --------------------------------------------------------------

# Branch and Claw
bc_spirits <- list("Keeper of the Forbidden Wild",
                   "Sharp Fangs behind the Leaves")

bc_adversaries <- c("France")

bc_scenarios <- c("Second Wave",
                  "Powers Long Forgotten",
                  "Ward the Shores",
                  "Rituals of the Destroying Flame")

# Jagged Earth
je_spirits <- list("Stone's Unyielding Defiance",
                   "Shifting Memory of Ages",
                   "Grinning Trickster Stirs Up Trouble",
                   "Lure of the Deep Wilderness",
                   "Many Minds Move as One",
                   "Volcano Looming High",
                   "Shroud of Silent Mist",
                   "Vengeance as a Burning Plague",
                   "Starlight Seeks Its Form",
                   "Fractured Days Split the Sky")

je_adversaries <- c("Hapsburg", "Russia")

je_aspects <- list("River Surges in Sunlight"=c("Sunshine"),
                   "Vital Strength of the Earth"=c("Resilience"),
                   "Lightning's Swift Strike"=c("Pandemonium", "Wind"),
                   "Shadows Flicker Like Flame"=c("Madness", "Reach"))

je_scenarios <- c()

# Feather and Flame
ff_spirits <- list("Heart of the Wildfire",
                   "Serpent Slumbering Beneath the Island",
                   "Downpour Drenches the World",
                   "Finder of Paths Unseen")

ff_adversaries <- c("Scotland")

ff_aspects <- list("River Surges in Sunlight"=c("Travel"),
                   "Vital Strength of the Earth"=c("Might"),
                   "Lightning's Swift Strike"=c("Immense"),
                   "Shadows Flicker Like Flame"=c("Amorphous", "Foreboding"))