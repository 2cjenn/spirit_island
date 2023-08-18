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

fixed_jitter <- function (x, factor = 1, amount = NULL) {
  set.seed(42)
  jitter(x, factor, amount)
}

#-------------------------------------------------------------------------------

# Most popular spirit

popular_spirit <- function(player_data) {
  spirits <- player_data %>% 
    filter(!is.na(spirit) & spirit != "") %>%
    add_count(spirit) %>%
    mutate(spirit = fct_reorder(spirit, n),
           name = factor(name)) %>%
    count(spirit, name) %>%
    mutate(text = paste0(name, ": ", n))
  
  plot_ly(data = spirits) %>%
    add_trace(
      x = ~spirit, y = ~n,
      # text = ~text,
      hoverinfo = 'text',
      hovertext = paste(spirits$name,
                        ": ", spirits$n),
      color = ~spirit, colors = spirit_colours,
      type = "bar",
      showlegend=FALSE,
      marker=list(line=list(color="white", width=1))
  ) %>%
    layout(barmode = "stack",
           title = "Popularity of each spirit")
}

#-------------------------------------------------------------------------------

# Score

difficulty_vs_score <- function(player_data) {
  scores <- player_data %>%
    mutate(score = score - ifelse(victory, 5*difficulty, 2*difficulty) - ifelse(victory, 10, 0)) %>%
    mutate(score = ifelse(victory, score, -score)) %>%
    select(id, difficulty, adversary, level, scenario, score) %>%
    group_by(id) %>%
    filter(row_number()==1) %>%
    ungroup
  
  plot_ly(data = scores, y = ~score, x = ~jitter(difficulty), 
          type = "scatter", mode = "markers",
          color = ~adversary, colors = adversary_colours,
          symbol = ~scenario, symbols = scenario_symbols,
          marker = list(size = 10, opacity=0.8),
          hoverinfo = 'text', text = ~paste0(adversary, " L", level, ", ", scenario),
          showlegend=FALSE) %>%
    layout(title = "Score by difficulty",
           yaxis = list(title = paste0("Score from invader cards, dahan and blight\n",
                                       "Defeat                              Victory")),
           xaxis = list(title = "Difficulty"))
}


#-------------------------------------------------------------------------------

# Score over time

time_score <- function(player_data) {
  scores <- player_data %>%
    mutate(game = max(game) + 1 - game) %>%
    select(id, date, game, score, difficulty, adversary, victory) %>%
    group_by(id) %>%
    filter(row_number()==1) %>%
    ungroup
  
  plot_ly(scores, x = ~game, y = ~score, 
          color = ~adversary, colors = adversary_colours, 
          size = ~difficulty, sizes=c(10,100), fill=~'',
          text = ~paste0("Difficulty: ", difficulty),
          type = 'scatter', mode = 'markers',
          symbol = ~victory, symbols = c("x", "circle"),
          marker = list(opacity = 0.8),
          showlegend = FALSE
  ) %>%
    layout(title = "Score over time, by difficulty")
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

# Average stats per adversary

avgstat_by_adv <- function(player_data) {
  games <- player_data %>% 
    group_by(id) %>%
    filter(row_number()==1) %>%
    mutate(time_taken = as.difftime(paste0(time_taken, ":00"), units = "hours")) %>%
    group_by(adversary) %>%
    summarise(Level = mean(level),
              Difficulty = mean(difficulty),
              # Score = mean(score),
              Duration = mean(as.numeric(time_taken), na.rm=TRUE),
              Blight = mean(blight/n_players, na.rm=TRUE),
              `Fear Level` = mean(fear_level, na.rm=TRUE),
              `Invader Cards` = mean(invader_cards, na.rm=TRUE),
              .groups="drop") %>%
    pivot_longer(cols = !adversary, names_to="stat", values_to="mean") %>%
    mutate(mean = round(mean, 2),
           stat = factor(stat, levels=c("Level", "Difficulty", 
                                        "Duration", "Invader Cards", 
                                        "Fear Level", "Blight")),
           adversary = factor(adversary, levels=names(adversary_colours)))
  
 
    plot_ly(data=games, x = ~stat, y = ~mean, type = 'bar', 
            color = ~adversary,
            colors = adversary_colours) %>%
      layout(title = "Average (mean) stats by adversary")
}


# Rates

rates_by_adv <- function(player_data) {
  games <- player_data %>% 
    group_by(id) %>%
    filter(row_number()==1) %>%
    group_by(adversary) %>%
    summarise(`Win` = 100*sum(victory==TRUE)/n(),
              `Blighted Island` = 100*sum(blighted_island==TRUE)/n(),
              `With Scenario` = 100*sum(scenario!="None")/n(),
              `Total Wipe` = 100*sum(total_wipe)/n(),
              .groups="drop") %>%
    pivot_longer(cols = !adversary, names_to="stat", values_to="rate") %>%
    mutate(rate = round(rate, 2),
           stat = factor(stat, levels=c("Win", "Blighted Island", 
                                        "With Scenario", "Total Wipe")),
           adversary = factor(adversary, levels=names(adversary_colours)))
  
  
  plot_ly(data=games, x = ~stat, y = ~rate, type = 'bar', 
          color = ~adversary,
          colors = adversary_colours) %>%
    layout(title = "Rate (%) by adversary")
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


