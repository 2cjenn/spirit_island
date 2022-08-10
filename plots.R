library(dplyr)
library(tidyr)
library(plotly)
library(forcats)

#-------------------------------------------------------------------------------

# Most popular spirit

popular_spirit <- function(mydata) {
  spirits <- mydata %>% 
    select(id, starts_with("spirit"), adversary) %>%
    pivot_longer(cols=paste0("spirit_", 1:6), values_to="spirit") %>%
    filter(!is.na(spirit) & spirit != "") %>%
    count(spirit) %>%
    mutate(spirit = fct_reorder(spirit, n))
  
  g <- ggplot(spirits, aes(x=spirit, y=n, fill = spirit)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values = spirit_colours, limits = names(spirit_colours))
  return(ggplotly(g))
}

#-------------------------------------------------------------------------------

# Score

difficulty_vs_score <- function(mydata) {
  score <- mydata %>%
    mutate(score = score - ifelse(victory, 5*difficulty, 2*difficulty) - ifelse(victory, 10, 0)) %>%
    mutate(score = ifelse(victory, score, -score)) %>%
    select(difficulty, adversary, scenario, score)
  
  g <- ggplot(score, aes(x=difficulty, y=score, color=adversary, shape=scenario)) +
    geom_point() +
    scale_color_manual(values = adversary_colours, limits = names(adversary_colours))
  return(ggplotly(g))
}







