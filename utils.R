paste_noNA <- function(x, sep=", ") {
  gsub(", " , sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) )
}

players_long <- function(data){
  
  player_data <- data %>%
    arrange(desc(date)) %>%
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