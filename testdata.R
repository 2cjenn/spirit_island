game <- data.table(id = as.POSIXct("2022/05/20"),
                   date = as.Date("20/05/22", format="%d/%m/%y"),
                   n_players = 2,
                   adversary = "None",
                   level = 0,
                   scenario = "None",
                   difficulty = 0,
                   victory = TRUE,
                   invader_cards = 5,
                   dahan = 16,
                   blight = 2,
                   score = 27,
                   # invisible
                   branch_claw = FALSE,
                   jagged_earth = FALSE,
                   feather_flame = FALSE,
                   blighted_island = FALSE,
                   fear_level=2
)
players <- data.table(id = rep(as.POSIXct("2022/05/20"),6),
                      n = 1:6,
                      name = c("Thomas", "Jennifer", "", "", "", ""),
                      spirit = c("Vital Strength of the Earth",
                                 "River Surges in Sunlight",
                                 NA, NA, NA, NA),
                      aspect = rep(NA, 6),
                      board = c("A", "B", NA, NA, NA, NA),
                      power_prog = c(FALSE, FALSE, NA, NA, NA, NA)
)

players_wide <- dcast(players, formula = id ~ n,
                      value.var=list("name", "spirit", "aspect", "board", "power_prog"))

mydata <- merge(players_wide, game, by.x="id", by.y="id")