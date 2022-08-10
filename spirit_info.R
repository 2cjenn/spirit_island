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

adversaries <- list("None"=c(0),
                 "Brandenburg-Prussia"=c(0, 2, 4, 6, 7, 9, 10),
                 "England"=c(0, 3, 4, 6, 7, 9, 11),
                 "Sweden"=c(0, 2, 3, 5, 6, 7, 8))

scenarios <- list("None"=0,
                  "Blitz"=0,
                  "Guard the Isle's Heart"=0,
                  "Rituals of Terror"=3,
                  "Dahan Insurrection"=4)

# Expansion packs --------------------------------------------------------------

# Branch and Claw
bc_spirits <- list("Keeper of the Forbidden Wilds",
                   "Sharp Fangs Behind the Leaves")

bc_adversaries <- list("France"=c(0, 3, 5, 7, 8, 9, 10))

bc_scenarios <- list("Second Wave"=1,
                     "Powers Long Forgotten"=1,
                     "Ward the Shores"=2,
                     "Rituals of the Destroying Flame"=3)

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

je_aspects <- list("River Surges in Sunlight"=c("Sunshine"),
                   "Vital Strength of the Earth"=c("Resilience"),
                   "Lightning's Swift Strike"=c("Pandemonium", "Wind"),
                   "Shadows Flicker Like Flame"=c("Madness", "Reach"))

je_adversaries <- list("Hapsburg"=c(0, 3, 5, 6, 8, 9, 10),
                       "Russia"=c(0, 3, 4, 6, 7, 9, 11))

je_scenarios <- list("Elemental Invocation"=1,
                     "Despicable Theft"=2,
                     "The Great River"=3)

# Feather and Flame
ff_spirits <- list("Heart of the Wildfire",
                   "Serpent Slumbering Beneath the Island",
                   "Downpour Drenches the World",
                   "Finder of Paths Unseen")

ff_aspects <- list("River Surges in Sunlight"=c("Travel"),
                   "Vital Strength of the Earth"=c("Might"),
                   "Lightning's Swift Strike"=c("Immense"),
                   "Shadows Flicker Like Flame"=c("Amorphous", "Foreboding"))

ff_adversaries <- list("Scotland"=c(0, 3, 4, 6, 7, 8, 10))

ff_scenarios <- list("A Diversity of Spirits"=0,
                     "Varied Terrains"=2)


# Shorthands -------------------------------------------------------------------

abbreviations <- list("River Surges in Sunlight" = "RSS",
                      "Vital Strength of the Earth" = "VSE",
                      "Lightning's Swift Strike" = "LSS",
                      "Shadows Flicker Like Flame" = "SFF",
                      "Thunderspeaker" = "THS",
                      "A Spread of Rampant Green" = "SRG",
                      "Ocean's Hungry Grasp" = "OHG",
                      "Bringer of Dreams and Nightmares" = "BDN",
                      "Keeper of the Forbidden Wilds" = "KFW",
                      "Sharp Fangs Behind the Leaves" = "SFL",
                      "Stone's Unyielding Defiance" = "SUD",
                      "Shifting Memory of Ages" = "SMA",
                      "Grinning Trickster Stirs Up Trouble" = "GTT",
                      "Lure of the Deep Wilderness" = "LDW",
                      "Many Minds Move as One" = "MMM",
                      "Volcano Looming High" = "VLH",
                      "Shroud of Silent Mist" = "SSM",
                      "Vengeance as a Burning Plague" = "VBP",
                      "Starlight Seeks Its Form" = "SSF",
                      "Fractured Days Split the Sky" = "FDS",
                      "Heart of the Wildfire" = "HWF",
                      "Serpent Slumbering Beneath the Island" = "SSB",
                      "Downpour Drenches the World" = "DDW",
                      "Finder of Paths Unseen" = "FPU"
)