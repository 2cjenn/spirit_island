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

popular_spirit <- function(mydata) {
  spirits <- mydata %>% 
    select(id, starts_with("spirit"), adversary) %>%
    pivot_longer(cols=paste0("spirit_", 1:6), values_to="spirit") %>%
    filter(!is.na(spirit) & spirit != "") %>%
    count(spirit) %>%
    mutate(spirit = fct_reorder(spirit, n))
  
  plot_ly(
    data = spirits,
    x = ~spirit, y = ~n,
    color = ~spirit, colors = spirit_colours,
    type = "bar"
  ) %>%
    layout(title = "Popularity of each spirit")
}

#-------------------------------------------------------------------------------

# Score

difficulty_vs_score <- function(mydata) {
  score <- mydata %>%
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

games_since_spirit <- function(mydata) {
  time <- mydata %>%
    mutate(time = difftime(Sys.Date(), date)) %>%
    select(id, starts_with("spirit"), time) %>%
    pivot_longer(cols=paste0("spirit_", 1:6), values_to="spirit") %>%
    filter(!is.na(spirit) & spirit != "") %>%
    group_by(spirit) %>%
    slice_min(time, n=1, with_ties=FALSE) %>%
    ungroup %>%
    mutate(spirit = fct_reorder(spirit, time))
  
  plot_ly(
    data = time,
    y = ~spirit, x = ~time,
    color = ~spirit, colors = spirit_colours,
    type = "bar",
    orientation = "h"
  ) %>%
    layout(title = "Time since playing each spirit")
  
}

games_since_adversary <- function(mydata) {
  time <- mydata %>%
    mutate(time = difftime(Sys.Date(), date)) %>%
    select(id, adversary, time) %>%
    filter(adversary != "None") %>%
    group_by(adversary) %>%
    slice_min(time, n=1, with_ties=FALSE) %>%
    ungroup %>%
    mutate(adversary = fct_reorder(adversary, time))
  
  plot_ly(
    data = time,
    y = ~adversary, x = ~time,
    color = ~adversary, colors = adversary_colours,
    type = "bar",
    orientation = "h",
    showlegend=FALSE
  ) %>%
    layout(title = "Time since playing each adversary")
  
}



