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

aspect_list <- list()

powerprog_list <- list("River Surges in Sunlight",
                       "Vital Strength of the Earth",
                       "Lightning's Swift Strike",
                       "Shadows Flicker Like Flame")

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

# Horizons
ho_spirits <- list("Devouring Teeth Lurk Underfoot",
                   "Eyes Watch From the Trees",
                   "Fathomless Mud of the Swamp",
                   "Rising Heat of Stone and Sand",
                   "Sun-Bright Whirlwind")

ho_powerprog <- list("Devouring Teeth Lurk Underfoot",
                     "Eyes Watch From the Trees",
                     "Fathomless Mud of the Swamp",
                     "Rising Heat of Stone and Sand",
                     "Sun-Bright Whirlwind")

# Nature Incarnate
ni_spirits <- list("Ember-Eyed Behemoth",
                   "Towering Roots of the Jungle",
                   "Hearth-Vigil",
                   "Breath of Darkness Down Your Spine",
                   "Relentless Gaze of the Sun",
                   "Wandering Voice Keens Delirium",
                   "Wounded Waters Bleeding",
                   "Dances Up Earthquakes")

ni_aspects <- list("Bringer of Dreams and Nightmares"=c("Violence", "Enticing"),
                   "Heart of the Wildfire"=c("Transforming"),
                   "Lightning's Swift Strike"=c("Sparking"),
                   "Lure of the Deep Wilderness"=c("Lair"),
                   "Ocean's Hungry Grasp"=c("Deeps"),
                   "Serpent Slumbering Beneath the Island"=c("Locus"),
                   "Shadows Flicker Like Flame"=c("Dark Fire"),
                   "Shifting Memory of Ages"=c("Intensify", "Mentor"),
                   "Vital Strength of the Earth"=c("Nourishing"),
                   "Shroud of Silent Mist"=c("Stranded"),
                   "A Spread of Rampant Green"=c("Tangles", "Regrowth"),
                   "Thunderspeaker"=c("Warrior", "Tactician"),
                   "River Surges in Sunlight"=c("Haven"),
                   "Keeper of the Forbidden Wilds"=c("Spreading Hostility"),
                   "Sharp Fangs Behind the Leaves"=c("Encircle", "Unconstrained"))

ni_adversaries <- list("Habsburg Mining Expedition"=c(0, 3, 4, 5, 7, 9, 10))

ni_scenarios <- list("Surges of Colonization"=2,
                     "Larger Surges of Colonization"=7,
                     "Destiny Unfolds"=-1)


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
                      "Finder of Paths Unseen" = "FPU",
                      "Devouring Teeth Lurk Underfoot" = "DTL",
                      "Eyes Watch From the Trees" = "EWT",
                      "Fathomless Mud of the Swamp" = "FMS",
                      "Rising Heat of Stone and Sand" = "RHS",
                      "Sun-Bright Whirlwind" = "SBW",
                      "Ember-Eyed Behemoth"="EEB",
                      "Towering Roots of the Jungle"="TRJ",
                      "Hearth-Vigil"="HVG",
                      "Breath of Darkness Down Your Spine"="BDS",
                      "Relentless Gaze of the Sun"="RGS",
                      "Wandering Voice Keens Delirium"="WVK",
                      "Wounded Waters Bleeding"="WWB",
                      "Dances Up Earthquakes"="DUP"
)

unbreviations <- names(abbreviations)
names <- unlist(abbreviations)
names(names) <- NULL
names(unbreviations) <- names

# Colours ----------------------------------------------------------------------

# https://www.datanovia.com/en/blog/awesome-list-of-657-r-color-names/
# https://bookdown.org/hneth/ds4psy/D-3-apx-colors-basics.html
spirit_colours <- c(
  "River Surges in Sunlight" = "darkturquoise",
  "Vital Strength of the Earth" = "khaki4",
  "Lightning's Swift Strike" = "gold",
  "Shadows Flicker Like Flame" = "darkslategray",
  "Thunderspeaker" = "black",
  "A Spread of Rampant Green" = "darkgreen",
  "Ocean's Hungry Grasp" = "darkblue",
  "Bringer of Dreams and Nightmares" = "darkorchid4",
  "Keeper of the Forbidden Wilds" = "lightgoldenrod3",
  "Sharp Fangs Behind the Leaves" = "firebrick2",
  "Stone's Unyielding Defiance" = "slategray2",
  "Shifting Memory of Ages" = "rosybrown4",
  "Grinning Trickster Stirs Up Trouble" = "mediumaquamarine",
  "Lure of the Deep Wilderness" = "lightgoldenrod1",
  "Many Minds Move as One" = "papayawhip",
  "Volcano Looming High" = "grey20",
  "Shroud of Silent Mist" = "lightcyan1",
  "Vengeance as a Burning Plague" = "olivedrab1",
  "Starlight Seeks Its Form" = "darkslateblue",
  "Fractured Days Split the Sky" = "deepskyblue",
  "Heart of the Wildfire" = "darkorange1",
  "Serpent Slumbering Beneath the Island" = "darkolivegreen4",
  "Downpour Drenches the World" = "darkorchid1",
  "Finder of Paths Unseen" = "lightsteelblue3",
  "Devouring Teeth Lurk Underfoot" = "tomato4",
  "Eyes Watch From the Trees" = "seagreen",
  "Fathomless Mud of the Swamp" = "salmon4",
  "Rising Heat of Stone and Sand" = "red4",
  "Sun-Bright Whirlwind" = "darkolivegreen2",
  "Ember-Eyed Behemoth"="darkgoldenrod",
  "Towering Roots of the Jungle"="sienna",
  "Hearth-Vigil"="magenta2",
  "Breath of Darkness Down Your Spine"="azure4",
  "Relentless Gaze of the Sun"="lemonchiffon",
  "Wandering Voice Keens Delirium"="tan2",
  "Wounded Waters Bleeding"="tomato1",
  "Dances Up Earthquakes"="palevioletred"
)

# https://community.plotly.com/t/plotly-colours-list/11730/3
adversary_colours <- c(
  "None" = "grey",
  "Brandenburg-Prussia" = "orange",
  "England" = "red",
  "Sweden" = "green",
  "Scotland" = "blue",
  "France" = "yellow",
  "Hapsburg" = "purple",
  "Russia" = "black",
  "Habsburg Mining Expedition" = "khaki2"
)

# https://plotly-r.com/working-with-symbols.html
scenario_symbols <- c(
  "None" = "circle-dot",
  "Blitz" = "diamond-tall-dot",
  "Guard the Isle's Heart" = "square-dot",
  "Rituals of Terror" = "cross-dot",
  "Dahan Insurrection" = "x-dot",
  "Second Wave" = "triangle-down-dot",
  "Powers Long Forgotten" = "pentagon-dot",
  "Ward the Shores" = "star-triangle-up-dot",
  "Rituals of the Destroying Flame" = "star-dot",
  "Elemental Invocation" = "hexagram-dot",
  "Despicable Theft" = "star-square-dot",
  "The Great River" = "diamond-wide-dot",
  "A Diversity of Spirits" = "triangle-right-dot",
  "Varied Terrain" = "hexagon-dot",
  "Surges of Colonization" = "diamond-x",
  "Larger Surges of Colonization" = "diamond-tall",
  "Destiny Unfolds" = "diamond-tall-dot"
)
