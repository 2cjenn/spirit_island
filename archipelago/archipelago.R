
arc_log <- read.csv("archipelago/arc_log.csv",
                    na.strings = c("NA", ""),
                    colClasses = c("artifact_unlocked" = "character"))
scen_list <- read.csv("archipelago/scenario_list.csv", 
                      na.strings = c("NA", ""))
# artifact_list <- read.csv("archipelago/artifact_list.csv", na.strings = "NA")
# flag_list <- read.csv("archipelago/flag_list.csv", na.strings = "NA")
unlock_list <- read.csv("archipelago/unlock_list.csv", na.strings = c("NA", ""))

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

get_available(scen_list, arc_log)



get_artifacts <- function(arc_log) {
  unlocked <- arc_log$artifact_unlocked
  used <- arc_log$artifact
  available <- unlocked[!is.na(unlocked) & !(unlocked %in% used)]
  return(sort(available))
}

get_flags <- function(arc_log) {
  unlocked <- arc_log$flag_unlocked
  used <- arc_log$flag[arc_log$victory=="y"]
  available <- unlocked[!is.na(unlocked) & !(unlocked %in% used)]
  return(sort(available))
}

obtain_artifact <- function(artifact_list, arc_log) {
  complete <- get_complete(arc_log)
  used_artifacts <- arc_log$artifact[arc_log$victory=="y"]
  
  available_artifacts <- artifact_list[artifact_list$Scenario %in% complete & 
                                 !(artifact_list$Artifact %in% used_artifacts)]
  return(sort(available_artifacts))
}

obtain_flag <- function(flag_list, arc_log) {
  complete <- get_complete(arc_log)
  used_flags <- arc_log$flag[arc_log$victory=="y"]
  
  available_flags <- flag_list$Flag[flag_list$Scenario %in% complete & 
                                 !(flag_list$Flag %in% used_flags)]
  return(paste0(sort(available_flags), collapse=", "))
}


gen_arcrow <- function(input, arc_log) {
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

