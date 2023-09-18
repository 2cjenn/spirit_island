
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
  
  unlocked_spirits <- get_spirits(arc_log)
  
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

# Get available spirits
get_spirits <- function(arc_log) {
  unlocked <- c()
  if(any(!is.na(arc_log$spirit_unlocked))) {
    unlocked <- arc_log %>%
      drop_na(spirit_unlocked) %>%
      pull(spirit_unlocked)
  }
  aspect_unlocks <- c()
  if(any(!is.na(arc_log$aspect_unlocked))) {
    aspects <- arc_log %>%
      drop_na(aspect_unlocked) %>%
      pull(aspect_unlocked)
    aspect_unlocks <- sapply(aspects, function(y) 
      names(all_aspects[sapply(all_aspects, function(x) y %in% x)]))
  }
  scenario_unlocks <- c()
  if(33 %in% arc_log$scenario[arc_log$victory==TRUE]){
    scenario_unlocks <- c(scenario_unlocks, "Heart of the Wildfire")
  }
  if(55 %in% arc_log$scenario[arc_log$victory==TRUE]){
    scenario_unlocks <- c(scenario_unlocks, "Serpent Slumbering Beneath the Island")
  }
  available <- unique(c(unlocked, aspect_unlocks, scenario_unlocks))
  return(available)
}

# Get mastered spirits
get_mastery <- function(arc_log) {
  mastered <- c()
  if(any(!is.na(arc_log$spirit_mastered))) {
    mastered <- arc_log %>%
      drop_na(spirit_mastered) %>%
      pull(spirit_mastered)
  }
  return(mastered)
}

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
    select(game, date, archipelago_scenario, level, artifact, flag,
           victory, influence, spirit_unlocked, spirit_mastered, aspect_unlocked,
           artifact_unlocked, flag_unlocked, annex4, annex5) %>%
    rename(scenario = archipelago_scenario,
           adv_level = level)
}

