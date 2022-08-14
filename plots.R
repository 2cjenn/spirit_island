library(dplyr)
library(tidyr)
library(plotly)
library(forcats)

# loadcsv <- function(filepath) {
#   mydata <- read.csv(filepath, na.strings = "NA")
#   mydata$date <- as.Date(mydata$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
#   mydata$id <- as.POSIXct(mydata$id, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
#   return(mydata)
# }
# 
# mydata <- loadcsv("data.csv")

#-------------------------------------------------------------------------------

# Most popular spirit

popular_spirit <- function(player_data) {
  spirits <- player_data %>% 
    filter(!is.na(spirit) & spirit != "") %>%
    count(spirit) %>%
    mutate(spirit = fct_reorder(spirit, n))
  
  plot_ly(
    data = spirits,
    x = ~spirit, y = ~n,
    color = ~spirit, colors = spirit_colours,
    type = "bar",
    showlegend=FALSE
  ) %>%
    layout(title = "Popularity of each spirit")
}

#-------------------------------------------------------------------------------

# Score

difficulty_vs_score <- function(player_data) {
  score <- player_data %>%
    mutate(score = score - ifelse(victory, 5*difficulty, 2*difficulty) - ifelse(victory, 10, 0)) %>%
    mutate(score = ifelse(victory, score, -score)) %>%
    select(difficulty, adversary, level, scenario, score)
  
  plot_ly(data = score, y = ~score, x = ~difficulty, 
          type = "scatter", mode = "markers",
          color = ~adversary, colors = adversary_colours,
          symbol = ~scenario, symbols = scenario_symbols,
          hoverinfo = 'text', text = ~paste0(adversary, " L", level, ", ", scenario),
          showlegend=FALSE) %>%
    layout(title = "Score by difficulty",
           yaxis = list(title = paste0("Score from invader cards, dahan and blight\n",
                                       "Defeat                              Victory")),
           xaxis = list(title = "Difficulty"))
}


#-------------------------------------------------------------------------------

# Haven't used in a while

games_since_spirit <- function(player_data) {
  games <- player_data %>%
    filter(!is.na(spirit) & spirit != "") %>%
    group_by(spirit) %>%
    slice_min(game, n=1, with_ties=FALSE) %>%
    ungroup %>%
      mutate(spirit = fct_reorder(spirit, game))
  
  plot_ly(
    data = games,
    y = ~spirit, x = ~game,
    color = ~spirit, colors = spirit_colours,
    type = "bar",
    orientation = "h",
    showlegend=FALSE
  ) %>%
    layout(title = "Time since playing each spirit")
  
}

games_since_adversary <- function(player_data) {
  games <- player_data %>%
    select(id, adversary, game) %>%
    filter(adversary != "None") %>%
    group_by(adversary) %>%
    slice_min(game, n=1, with_ties=FALSE) %>%
    ungroup %>%
    mutate(adversary = fct_reorder(adversary, game))
  
  plot_ly(
    data = games,
    y = ~adversary, x = ~game,
    color = ~adversary, colors = adversary_colours,
    type = "bar",
    orientation = "h",
    showlegend=FALSE
  ) %>%
    layout(title = "Time since playing each adversary")
  
}

#-------------------------------------------------------------------------------

# How often are different spirits played together

spirit_friends <- function(player_data) {
  data <- player_data %>%
    select(game, spirit, n_players) %>%
    filter(spirit != "") %>%
    mutate(spirit_short = abbreviations[spirit])
  
  spirit_rows <- list()
  text_rows <- list()
  
  for (s in unique(data$spirit_short)) {
    games <- data$game[data$spirit_short == s]
    
    friends <- data %>% 
      filter(game %in% games & spirit_short != s & spirit_short != "") %>%
      mutate(weight = 1/factorial(n_players)) %>%
      group_by(spirit_short, spirit) %>%
      summarize(weight.sum = sum(weight), .groups="drop_last") %>%
      ungroup %>%
      mutate(label = paste0("x: ", spirit, 
                            "\ny: ", unbreviations[[s]], 
                            "\nz: ", round(weight.sum, 2)))
    
    weight_row <- as.list(friends$weight.sum)
    names(weight_row) <- friends$spirit_short
    spirit_rows[[s]] <- as.data.frame(weight_row)
    
    text_row <- as.list(friends$label)
    names(text_row) <- friends$spirit_short
    text_rows[[s]] <- as.data.frame(text_row)
  }
  
  heatmatrix <- bind_rows(spirit_rows, .id="id") %>%
    arrange(id)
  colorder <- heatmatrix$id
  rownames(heatmatrix) <- heatmatrix$id
  
  heatmatrix <- as.matrix(heatmatrix[,colorder])
  
  textmatrix <- bind_rows(text_rows, .id="id") %>%
    arrange(id)
  colorder <- textmatrix$id
  rownames(textmatrix) <- textmatrix$id
  
  textmatrix <- as.matrix(textmatrix[,colorder])
  

  
  plot_ly(
    x = rownames(heatmatrix), 
    y = colnames(heatmatrix),
    z = heatmatrix, type = "heatmap",
    hoverinfo='text', text=textmatrix
  ) %>%
    layout(title = "How often are spirits played together?")
  
}


