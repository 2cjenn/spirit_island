
scen_list <- read.csv("archipelago/scenario_list.csv", 
                      na.strings = c("NA", ""))
unlock_list <- read.csv("archipelago/unlock_list.csv", na.strings = c("NA", ""))

artifacts_csv <- read.csv("archipelago/artifacts.csv")
flags_csv <- read.csv("archipelago/flags.csv")

# From https://docs.google.com/spreadsheets/d/1Cy4_-0aJF41YPp2FJwYWPRiO5hJZKvx03589FJg_pEw/edit#gid=1650379336
scen_details <- read.csv("archipelago/scenario_details.csv", na.strings=c("NA", ""))

# Check for completed scenarios
get_complete <- function(arc_log) {
  victory <- arc_log %>% 
    filter(victory==TRUE) %>%
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
get_available <- function(arc_log, scen_list) {
  complete <- get_complete(arc_log)
  
  unavailable <- scen_list %>%
    filter(! prereq %in% complete) %>%
    pull(ID) %>%
    unique()
  
  unlocked_spirits <- arc_log %>%
    drop_na(spirit_unlocked) %>%
    pull(spirit_unlocked)
  
  available <- scen_list %>%
    separate(spirits, into=c("req1", "req2"), sep=", ", remove=FALSE) %>%
    filter(! ID %in% unavailable,
           prereq %in% complete,
           ! ID %in% complete,
           (is.na(annex4) | TRUE %in% arc_log[["annex4"]]),
           (is.na(spirits) | 
              if_all(starts_with("req"), 
                     function(x){x %in% unlocked_spirits}))) %>%
    pull(ID) %>%
    unique()
  
  return(available)
}

# get_available(scen_list, arc_log)

# Get available artifacts
get_artifacts <- function(arc_log) {
  if(any(!is.na(arc_log$flag_unlocked))) {
    unlocked <- strsplit(arc_log$artifact_unlocked, split=", |,") %>%
      unlist() %>%
      as.numeric()
  } else {
    unlocked <- c()
  }
  used <- arc_log$artifact
  available <- unlocked[!is.na(unlocked) & !(unlocked %in% used)]
  return(sort(available))
}

# Get available flags
get_flags <- function(arc_log) {
  if(any(!is.na(arc_log$flag_unlocked))) {
    unlocked <- strsplit(arc_log$flag_unlocked, split=", |,") %>%
      unlist() %>%
      as.numeric()
  } else {
    unlocked <- c()
  }
  used <- arc_log$flag[arc_log$victory==TRUE]
  available <- unlocked[!is.na(unlocked) & !(unlocked %in% used)]
  return(sort(available))
}


gen_arclog <- function(data) {
  arc_log <- data %>%
    filter(!is.na(archipelago_scenario)) %>%
    select(game, date, archipelago_scenario, artifact, flag,
           victory, influence, spirit_unlocked, aspect_unlocked,
           artifact_unlocked, flag_unlocked, annex4, annex5) %>%
    rename(scenario = archipelago_scenario)
}

