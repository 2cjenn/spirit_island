
arc_log <- read.csv("archipelago/arc_log.csv",
                    na.strings = c("NA", ""),
                    colClasses = c("artifact_unlocked" = "character"))
scen_list <- read.csv("archipelago/scenario_list.csv", 
                      na.strings = c("NA", ""))
unlock_list <- read.csv("archipelago/unlock_list.csv", na.strings = c("NA", ""))

# Check for completed scenarios
get_complete <- function(arc_log) {
  victory <- arc_log %>% 
    filter(victory=="y") %>%
    pull(scenario) %>%
    unique()
  
  played_twice <- arc_log %>%
    group_by(scenario) %>%
    tally %>%
    filter(n==2) %>%
    pull(scenario)
  
  complete <- unique(c(0, victory, played_twice))
  
  return(complete)
}

# Find currently available scenarios
get_available <- function(scen_list, arc_log) {
  complete <- get_complete(arc_log)
  
  unavailable <- scen_list %>%
    filter(! prereq %in% complete) %>%
    pull(ID) %>%
    unique()
  
  available <- scen_list %>% 
    mutate(spirits = ifelse(!is.na(spirits), 
                            strsplit(spirits, split=", "), NA),
           except_spirits = ifelse(!is.na(except_spirits),
                             strsplit(except_spirits, split=", "), NA)) %>%
    filter(! ID %in% unavailable,
           prereq %in% complete,
           ! ID %in% complete,
           (is.na(spirits) | spirits %in% arc_log[["spirit_unlocked"]]),
           (is.na(except_spirits) | any(!arc_log[["spirit_unlocked"]] %in% except_spirits)),
           (is.na(annex4) | TRUE %in% arc_log[["annex4"]])) %>%
    pull(ID) %>%
    unique()
  
  return(available)
}

# get_available(scen_list, arc_log)

# Get available artifacts
get_artifacts <- function(arc_log) {
  unlocked <- arc_log$artifact_unlocked
  used <- arc_log$artifact
  available <- unlocked[!is.na(unlocked) & !(unlocked %in% used)]
  available <- unlist(strsplit(available, split=", "))
  return(sort(available))
}

# Get available flags
get_flags <- function(arc_log) {
  unlocked <- arc_log$flag_unlocked
  used <- arc_log$flag[arc_log$victory=="y"]
  available <- unlocked[!is.na(unlocked) & !(unlocked %in% used)]
  available <- unlist(strsplit(available, split=", "))
  return(sort(available))
}

# Generate a row for the Archipelago Log
gen_arcrow <- function(input, victory, arc_log) {
  influence <- arc_log %>% 
    slice_max(game) %>% 
    pull(influence)
  
  if(input$victory == TRUE & 
     !(input$scenario %in% arc_log$scenario)) {
    influence <- influence + 4
  } else if (input$victory == TRUE){
    influence <- influence + 2
  }
  if(!is.na(input$unlock_spirit)) {
    influence <- influence + 10
  }
  if(!is.na(input$use_artifact)) {
    influence <- influence - 6
  }
  if(!is.na(input$use_flag) &
     !(input$use_flag %in% arc_log$flag)) {
    influence <- influence - 10
  } # If flag was used in a game that was lost, can use again
  
  newrow <- data.table(
    scenario = input$scenario,
    artifact = ifelse(input$use_artifact=="None", NA, input$use_artifact),
    flag = ifelse(input$use_flag=="None", NA, input$use_flag),
    victory = input$victory,
    influence = influence,
    spirit_unlocked = input$unlock_spirit,
    artifact_unlocked = input$unlock_artifacts,
    flag_unlocked = input$unlock_flags,
    annex4 = input$annex4,
    annex5 = input$annex5
  )
}

