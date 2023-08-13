paste_noNA <- function(x, sep=", ") {
  gsub(", " , sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) )
}
  
  
# Create data for each of 6 players
# Where player data is given from user input, use that
# Otherwise, use empty strings and NAs as appropriate
gen_players <- function(input, unique_id) {
  players <- data.table(
    id = as.POSIXct(character()),
    n = numeric(),
    name = character(),
    spirit = character(),
    aspect = character(),
    board = character(),
    powerprog = character(),
    toptrack = numeric(),
    bottomtrack = numeric(),
    destroyed = numeric()
  )
  for (i in 1:6){
    if(i <= input$player_n) {
      player_i <- data.table(
        id = unique_id,
        n = i,
        name = input[[paste0("name", i)]],
        spirit = input[[paste0("spirit", i)]],
        aspect = ifelse(
          is.null(input[[paste0("aspect", i)]]),
          "",
          input[[paste0("aspect", i)]]
        ),
        board = input[[paste0("board", i)]],
        powerprog = input[[paste0("powerprog", i)]],
        toptrack = input[[paste0("toptrack", i)]],
        bottomtrack = input[[paste0("bottomtrack",i)]],
        destroyed = input[[paste0("destroyed",i)]]
      )
    } else {
      player_i <- data.table(
        id = unique_id,
        n = i,
        name = "",
        spirit = "",
        aspect = "",
        board = "",
        powerprog = FALSE,
        toptrack = NA,
        bottomtrack = NA,
        destroyed = NA
      )
    }
    players <- rbind(players, player_i)
    players_wide <- dcast(players, formula = id ~ n, 
                          value.var=list("name", "spirit", "aspect", "board", 
                                         "powerprog", "toptrack", 
                                         "bottomtrack", "destroyed"))
    
  }
  return(players_wide)
}


# Merge the player data with the general game data to form one row
# Wide format for storage
gen_datarow <- function(input, victory, score, difficulty){
  unique_id <- Sys.time()
  
  players <- gen_players(input, unique_id)
  
  newrow = data.table(id = unique_id,
                      date = input$date,
                      n_players = input$player_n,
                      adversary = input$adversary,
                      level = input$adv_level,
                      scenario = input$scenario,
                      difficulty = difficulty,
                      archipelago_scenario = ifelse(input$archipelago, input$arc_scenario, ""),
                      victory = victory,
                      invader_cards = input$invader_cards,
                      dahan = input$dahan,
                      blight = input$blight,
                      score = score,
                      #invisible
                      blighted_island = input$blighted_island,
                      fear_level = input$fear_level,
                      branch_claw = input$branch_claw,
                      jagged_earth = input$jagged_earth,
                      feather_flame = input$feather_flame,
                      horizons = input$horizons,
                      nature_incarnate = input$nature_incarnate,
                      archipelago = input$archipelago
  )
  
  row = merge.data.table(players, newrow, by.x=c("id"), by.y=c("id"))
  return(row)
}


# Abbreviate and concat spirit names, boards etc to make table narrower
# Choose which columns to hide
arrange_scoretable <- function(data) {
  data <- data %>%
    mutate(across(spirit_1:spirit_6, ~ifelse(.x=="", .x, abbreviations[.x])),
           branch_claw = ifelse(branch_claw == TRUE, "BC", NA),
           jagged_earth = ifelse(jagged_earth == TRUE, "JE", NA),
           feather_flame = ifelse(feather_flame == TRUE, "FF", NA))
  
  
  for(i in 1:6) {
    powerprog_col <- paste0("powerprog_", i)
    spirit_col <- paste0("spirit_", i)
    powerprog <- data[[powerprog_col]] == TRUE
    if(any(powerprog)) {
      data[powerprog,][[spirit_col]] <- paste0(data[powerprog,][[spirit_col]], "+")
    }
  }
  
  data$spirits <- apply( data[, paste0("spirit_", c(1:6)) ], 1, paste_noNA, sep=", ")
  data$names <- apply( data[, paste0("name_", c(1:6)) ], 1, paste_noNA, sep=", ")
  data$boards <- apply( data[, paste0("board_", c(1:6)) ], 1, paste_noNA, sep=", ")
  data$expansions <- apply( data[, c("branch_claw", "jagged_earth", "feather_flame")],
                            1, paste_noNA, sep=", ")
  data <- data %>% 
    dplyr::relocate(names, .after=date) %>%
    dplyr::relocate(spirits, .after=names) %>%
    dplyr::relocate(boards, .after=spirits) %>%
    select(-c(name_1:name_6, spirit_1:spirit_6, aspect_1:aspect_6, 
              powerprog_1:powerprog_6, board_1:board_6,
              toptrack_1:toptrack_6, bottomtrack_1:bottomtrack_6,
              destroyed_1:destroyed_6,
              branch_claw, jagged_earth, feather_flame)) %>%
    arrange(desc(id))
}


# Rearrange data into long format by players
# In order to filter by player to generate plots
players_long <- function(data){
  
  player_data <- data %>%
    arrange(desc(id)) %>%
    mutate(game = seq.int(nrow(.)),
           across(powerprog_1:destroyed_6, ~as.character(.x))) %>%
    pivot_longer(
      cols = c(
        paste0("name_", 1:6),
        paste0("spirit_", 1:6),
        paste0("aspect_", 1:6),
        paste0("board_", 1:6),
        paste0("powerprog_", 1:6),
        paste0("toptrack_", 1:6),
        paste0("bottomtrack_", 1:6),
        paste0("destroyed_", 1:6)
      ),
      names_to = c("type", "num"),
      names_sep = "_",
      values_to = "value"
    ) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(powerprog = as.logical(powerprog),
           across(c(toptrack, bottomtrack, destroyed), ~as.numeric(.x))) %>%
    filter(!is.na(name) & name != "")
  
  return(player_data)
  
}


# Combine lists of aspects
map_aspects <- function(aspect_list, aspects) {
  for (aspect in names(aspects)) {
    if (aspect %in% names(aspect_list)) {
      aspect_list[[aspect]] <- c(aspect_list[[aspect]], aspects[[aspect]])
    }
    else {
      aspect_list[[aspect]] <- c("None", aspects[[aspect]])
    }
  }
  return(aspect_list)
}

# None to NA
ifnone <- function(x) {
  ifelse(x=="None", NA, x)
}
