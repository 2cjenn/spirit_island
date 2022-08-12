paste_noNA <- function(x, sep=", ") {
  gsub(", " , sep, toString(abbreviations[x[!is.na(x) & x!="" & x!="NA"]] ) )
}

players_long <- function(data){
  
  player_data <- data %>%
    arrange(desc(date)) %>%
    mutate(game = seq.int(nrow(.))) %>%
    pivot_longer(
      cols = c(
        paste0("name_", 1:6),
        paste0("spirit_", 1:6),
        paste0("aspect_", 1:6),
        paste0("board_", 1:6)
      ),
      names_to = c("type", "num"),
      names_sep = "_",
      values_drop_na = TRUE
    ) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    filter(!is.na(name) & name != "")
  
  return(player_data)
  
}