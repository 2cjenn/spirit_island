#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyvalidate)
library(dplyr)
library(tidyr)
library(DT)
library(data.table)
library(bslib)
library(plotly)
library(stringr)
library(shinyTime)
library(shinydashboard)

# Spirit info ------------------------------------------------------------------

# source("define_players.R")

source("spirit_info.R")

source("archipelago/archipelago.R")

# source("testdata.R")

source("plots.R")

source("utils.R")

# Data functions ---------------------------------------------------------------
# https://deanattali.com/blog/shiny-persistent-data-storage/

saveData <- function(data, filename="data.rds") {
  saveRDS(data, filename)
  }

loadData <- function(filename="data.rds") {
  data <- readRDS(filename)
  return(data)
}

loadcsv <- function(filepath) {
  mydata <- read.csv(filepath, na.strings = c("NA", ""),
                     colClasses = c("artifact_unlocked" = "character"))
  mydata$date <- as.Date(mydata$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
  mydata$id <- as.POSIXct(mydata$id, format = c("%Y-%m-%d %H:%M:%S"))
  return(mydata)
}

mydata <- loadData()
# mydata <- loadcsv("data.csv")
# saveData(mydata)

players <- mydata %>%
  select(id, starts_with("name"), archipelago, adversary, scenario) %>%
  pivot_longer(cols=paste0("name_", 1:6)) %>%
  filter(!is.na(value) & value != "")

  
# Begin ------------------------------------------------------------------------


# Shiny app
# https://shiny.rstudio.com/articles/layout-guide.html - layouts
# https://shiny.rstudio.com/articles/html-tags.html - text formatting
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/ - widget types
# https://github.com/rstudio/shiny/issues/1927 - keyboard in select on mobile
ui <- function(req) {
  fluidPage(
  # theme = bs_theme(version = 5, bootswatch = "minty"),
  tabsetPanel(
    tabPanel("Enter results",
             titlePanel("Spirit Island"),
             strong("Expansions:"),
             fluidRow(
               id="expansions",
               column(width=3,
                      checkboxInput(inputId="branch_claw",
                                    label="Branch and Claw?",
                                    value=TRUE)),
               column(width=3,
                      checkboxInput(inputId="jagged_earth",
                                    label="Jagged Earth?",
                                    value=TRUE)),
               column(width=3,
                      checkboxInput(inputId="feather_flame",
                                    label="Feather and Flame?",
                                    value=TRUE)),
               column(width=3,
                      checkboxInput(inputId="horizons",
                                    label="Horizons of Spirit Island?",
                                    value=FALSE)),
               column(width=3,
                      checkboxInput(inputId="nature_incarnate",
                                    label="Nature Incarnate?",
                                    value=FALSE)),
               column(width=3,
                      checkboxInput(inputId="archipelago",
                                    label="Archipelago",
                                    value=FALSE))
             ),
             fluidRow(
               id="date_playercount",
               column(width=3,
                      dateInput(inputId="date",
                                label="Date:",
                                value=Sys.Date(),
                                format="dd/mm/yyyy")),
               column(width=6,
                      sliderInput("player_n", "Number of players",
                                  1, 4, 2, step=1, ticks=FALSE))
             ),
             uiOutput("Archipelago"),
             hr(),
             uiOutput("Players"),
             hr(),
             uiOutput("adv_scen"),
             hr(),
             
             fluidRow(
               id="score",
               column(width=2,
                      numericInput(inputId="invader_cards",
                                   label="Invader Cards",
                                   value=0,
                                   min=0, max=15, step=1)),
               column(width=2,
                      numericInput(inputId="dahan",
                                   label="Dahan Remaining",
                                   value=0,
                                   min=0, max=48, step=1)),
               column(width=2,
                      numericInput(inputId="blight",
                                   label="Blight",
                                   value=0,
                                   min=0, max=52, step=1)),
               column(width=2,
                      checkboxInput(inputId="blighted_island",
                                    label="Blighted Island?",
                                    value=FALSE)),
               column(width=2,
                      checkboxInput(inputId="total_wipe",
                                    label="Total board wipe?",
                                    value=FALSE))
               ),
             fluidRow(
               id="notes",
               column(width=2,
                      numericInput(inputId="fear_level",
                                   label="Fear Level",
                                   value=1,
                                   min=1, max=4, step=1)),
               column(width=3,
                      timeInput(inputId="time_taken",
                                label="Duration",
                                seconds=FALSE,
                                minute.steps=5))
             ),
             helpText("For a victory, count the invader cards remaining in the deck.",
                      "For a defeat, count the invader cards *not* in the deck."),
             uiOutput("Archipelago_unlocks"),
             hr(),
             fluidRow(
               id="victory",
               column(width=2, offset=4,
                      actionButton(inputId="victory",
                                   label="VICTORY!",
                                   class="btn-success")),
               column(width=2,
                      actionButton(inputId="defeat",
                                   label="Defeat :(",
                                   class="btn-danger"),)
             )), 
    tabPanel("Score history",
             selectInput(inputId="columns", 
                         label="Select columns to display", 
                         choices=c("date", "names", "spirits", "boards", "n_players",
                                   "adversary", "level", "scenario", "difficulty",
                                   "archipelago_scenario", "board_layout", "time_taken",
                                   "victory", "invader_cards", "dahan", "blight", "score",
                                   "blighted_island", "fear_level", "total_wipe", "expansions"),
                         selected=c("date", "spirits", "adversary", "level", 
                                    "scenario", "difficulty", "victory", "score"),
                         multiple=TRUE),
             
             dataTableOutput("scores")
    ),
    tabPanel("Plots",
             fluidRow(
               id="victory",
               column(width=3,
                      selectInput(inputId="filter_player",
                                  label="Player:",
                                  choices=c("All", "T & J", unique(players$value)),
                                  selected="All",
                                  selectize=FALSE)),
               column(width=3,
                      selectInput(inputId="filter_archipelago",
                                  label="Games:",
                                  choices=c("All", "Archipelago only", "Non-Archipelago"),
                                  selected="All",
                                  selectize=FALSE)),
               column(width=3,
                      selectInput(inputId="filter_adversary",
                                  label="Adversary:",
                                  choices=c("All", unique(players$adversary)),
                                  selected="All",
                                  selectize=FALSE)),
               column(width=3,
                      selectInput(inputId="filter_scenario",
                                  label="Scenario:",
                                  choices=c("All", unique(players$scenario)),
                                  selected="All",
                                  selectize=FALSE))
             ),
             hr(),
             
             # Per-player plots
             plotlyOutput("pop_spirit", inline=TRUE),
             plotlyOutput("diff_vs_score", inline=TRUE), 
             plotlyOutput("time_score", inline=TRUE),
             plotlyOutput("games_since_spirit", inline=TRUE),
             plotlyOutput("games_since_adversary", inline=TRUE),
             plotlyOutput("adversary_pie", inline=TRUE),
             
             plotlyOutput("avgstat_by_adv", inline=TRUE),
             plotlyOutput("rates_by_adv", inline=TRUE),
             
             # Global plots
             # https://community.rstudio.com/t/plotly-fixed-ratio/94447/5
             div(style="width:100%;height:0;padding-top:100%;position:relative;",
                 div(style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                     plotlyOutput("spirit_friends", inline=TRUE, height="60vmin", width="70vmin")
                     )
                 )
    ),
    tabPanel("Archipelago",
             
             uiOutput("available"),
             hr(),
             fluidRow(
               id="art_flag",
               column(width=6,
                      dataTableOutput("artifacts")),
               column(width=6,
                      dataTableOutput("flags"))
             ),
             hr(),
             dataTableOutput("archipelago_log")
    ),
    tabPanel("Backup",
             downloadButton("downloadData", "Download"),
             fileInput("file1", "Choose CSV File",
                       multiple = TRUE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"))
    )
  )
  
)}


server = function(input, output, session) {
  session$allowReconnect(TRUE)
  
  iv <- InputValidator$new()
  
  ######################
  # Spirit Archipelago #
  ######################
  
  output$Archipelago <- renderUI({
    if(input$archipelago) {
      
      arc_log <- gen_arclog(df())
      
      div(
        fluidRow(
          id="arch",
          hr(),
          column(width=2, offset=0,
                 selectInput(inputId="arc_scenario",
                             label="Scenario:",
                             choices=get_available(arc_log, scen_list),
                             selectize=FALSE)),
          column(width=2,
                 selectInput(inputId="use_artifact",
                             label="Use artifact?",
                             choices=c("None", 
                                       get_artifacts(arc_log)),
                             selectize=FALSE)),
          column(width=2,
                 selectInput(inputId="use_flag",
                             label="Use flag?",
                             choices=c("None",
                                       get_flags(arc_log)),
                             selectize=FALSE)),
          column(width=3,
                 checkboxInput(inputId="scen_desc",
                               label="Show description?",
                               value=FALSE))
        ),
        fluidRow(
          id="scen_details",
          box(
            uiOutput("scenario_name"),
            width=6
          ),
          box(
            uiOutput("board_layout"),
            width=4
          )
        )
      )
      
    }
  })
  active_scenario <- reactive(
    scen_details[scen_details$Number == input$arc_scenario, , drop=TRUE]
  ) %>% 
    bindEvent(input$arc_scenario)
  
  
  output$scenario_name <- renderUI({
    scen <- active_scenario()
    arc_log <- gen_arclog(df())
    recent_level <- arc_log %>%
      slice_max(game)
    new_level <- ifelse(recent_level$victory==TRUE,
                        min(recent_level$adv_level + 1, 6), recent_level$adv_level - 2)
    
    text <- paste0("<b>Name</b>: ", scen$Title,
                   "<br/>",
                   "<b>Adversary</b>: ", scen$Adversary,
                   "<br/>",
                   "<b>Suggested level</b>: ", new_level,
                   "<br/>",
                   "<b>Game Effects</b>: ", scen$Game_Effects,
                   "<br/>",
                   "<b>Board Layout</b>: ", scen$Board_Setup,
                   "<br/>")
    if(input$scen_desc==TRUE) {
      text <- paste0(text,
                     "<br/>",
                     "<b>Description</b>: ",
                     scen$Description)
    }
    HTML(text)
  })
  
  output$board_layout = renderUI({
    scen <- active_scenario()
    layout <- scen$Board_Setup
    image_name <-  paste0( layout, ".png")
    if(layout %in% layout_options) {
      img(src = image_name, height = '100px')
    }
    
  })
  
  observe({
    scen <- active_scenario()
    arc_log <- gen_arclog(df())
    
    # Available spirits
    allowed_spirits <- list()
    if(scen$Mandatory_Spirits != "(none)") {
      mandatory <- strsplit(scen$Mandatory_Spirits, split=", |,")
      mandatory <- as.list(mandatory[[1]])
      allowed_spirits[["Mandatory"]] <- mandatory
    } else {
      mandatory <- c()
    }
    if(scen$Restricted_Spirits != "All other spirits") {
      
      if(scen$Default_Spirits != "(none)") {
        default <- strsplit(scen$Default_Spirits, split=", |,")[[1]]
        allowed_spirits[["Default"]] <- as.list(default)
      } else {
        default <- c()
      }
      
      if(scen$Restricted_Spirits != "(none)") {
        restricted <- strsplit(scen$Restricted_Spirits, split=", |,")
        restricted <- restricted[[1]]
      } else {
        restricted <- c()
      }
      
      unlocked_spirits <- get_spirits(arc_log)
      unlocked_spirits <- unlocked_spirits[! unlocked_spirits %in% restricted &
                                             ! unlocked_spirits %in% default &
                                             ! unlocked_spirits %in% mandatory]
      
      allowed_spirits[["Unlocked"]] <- as.list(unlocked_spirits)
    }
    
    for(n in seq(1, input$player_n, 1)) {
      updateSelectInput(session, paste0("spirit", n),
                        choices = allowed_spirits,
                        selected = unlist(allowed_spirits)[n])
    }
    
    # Default values for adversary, difficulty, etc
    updateSelectInput(session, "adversary",
                      selected = scen$Adversary)
    
    updateSelectInput(session, "layout",
                      selected = scen$Board_Setup)
    
    recent_level <- arc_log %>%
      slice_max(game)
    new_level <- ifelse(recent_level$victory==TRUE,
                        min(recent_level$adv_level + 1, 6),
                        recent_level$adv_level - 2)
    updateNumericInput(session, "adv_level",
                       value=new_level)
    
  })
  
  # Separate observer so spirit doesn't update recursively
  observe({
    scen <- active_scenario()
    arc_log <- gen_arclog(df())
    
    # Aspect
    unlocked_aspects <- arc_log %>% 
      drop_na(aspect_unlocked) %>%
      pull(aspect_unlocked)
    
    mandatory_aspect <- scen$Mandatory_Aspect
    restricted_aspect <- scen$Restricted_Aspect
    
    for(n in seq(1, input$player_n, 1)) {
      if((input$jagged_earth | input$feather_flame | input$nature_incarnate) &
         input[[paste0("spirit", n)]] %in% names(aspects)) {
        choices <- aspects[[input[[paste0("spirit", n)]]]]
        if(!is.na(mandatory_aspect) & mandatory_aspect %in% choices) {
          choices <- c(mandatory_aspect)
        } else if (!is.na(restricted_aspect)) {
          choices <- choices[choices %in% c("None", unlocked_aspects) &
                               choices != restricted_aspect]
        } else if (is.na(restricted_aspect)) {
          choices <- choices[choices %in% c("None", unlocked_aspects)]
        }
        updateSelectInput(session, paste0("aspect", n),
                          choices=choices)
      }
    }
  })
  
  ######################
  # Spirits and Boards #
  ######################
  
  output$Players <- renderUI({
    
    # Branch and Claw expansion
    if(input$branch_claw) {
      spirits[["Branch and Claw"]] <- bc_spirits
    }
    # Jagged Earth expansion
    if(input$jagged_earth) {
      spirits[["Jagged Earth"]] <- je_spirits
      aspect_list <- map_aspects(aspect_list, je_aspects)
      boards <- c(boards, "E", "F")
      
      updateSliderInput(session, "player_n", value = input$player_n,
                        min = 1, max = 6, step = 1)
    } else {
      updateSliderInput(session, "player_n", value = input$player_n,
                        min = 1, max = 4, step = 1)
    }
    # Feather and Flame expansion
    if(input$feather_flame) {
      spirits[["Feather and Flame"]] <- ff_spirits
      aspect_list <- map_aspects(aspect_list, ff_aspects)
    }
    # Horizons expansion
    if(input$horizons) {
      spirits[["Horizons of Spirit Island"]] <- ho_spirits
      powerprog_list <- c(powerprog_list, ho_powerprog)
      print(powerprog_list)
    }
    # Nature Incarnate expansion
    if(input$nature_incarnate) {
      spirits[["Nature Incarnate"]] <- ni_spirits
      aspect_list <- map_aspects(aspect_list, ni_aspects)
    }
    
    spirit_list <<- spirits
    aspects <<- aspect_list
    
    
    interaction <- lapply(seq_len(input$player_n), function(x) {
      column(width=floor(12/input$player_n),
             # Player name
             textInput(inputId=paste0("name", x),
                       label="Name:",
                       value=switch(x,
                                    "Thomas", "Jennifer", 
                                    "", "", "", "")
             ),
             # Spirit
             selectInput(inputId=paste0("spirit", x),
                         label="Spirit:",
                         choices=spirits,
                         selected=unlist(spirits)[x],
                         selectize=FALSE),
             # Aspects
             renderUI({
               if((input$jagged_earth | input$feather_flame | input$nature_incarnate) & 
                  input[[paste0("spirit", x)]] %in% names(aspect_list)){
                 selectInput(inputId=paste0("aspect", x),
                             label="Aspect:",
                             choices=aspect_list[[input[[paste0("spirit", x)]]]],
                             selectize=FALSE
                 )
               }}),
             # Board
             selectInput(inputId=paste0("board", x),
                         label="Board:",
                         choices=boards,
                         selected=boards[x],
                         selectize=FALSE),
             # Power progressions?
             renderUI({
               if(input[[paste0("spirit", x)]] %in% powerprog_list){
                 checkboxInput(inputId=paste0("powerprog",x),
                               label="Power Progression?",
                               value=FALSE)
               }
             }),
             # Presence tracks
             strong("Presence removed from tracks:"),
             fluidRow(
               column(width=3,
                      numericInput(inputId=paste0("toptrack", x),
                                   label="Top:",
                                   value=0,
                                   max=13, min=0, step=1)),
               column(width=3,
                      numericInput(inputId=paste0("bottomtrack", x),
                                   label="Bottom:",
                                   value=0,
                                   max=13, min=0, step=1)),
               column(width=3,
                      numericInput(inputId=paste0("destroyed", x),
                                   label="Destroyed:",
                                   value=0,
                                   max=13, min=0, step=1)),
             )
      )
    })
    interaction[["id"]] <- "players"
    do.call(fluidRow, interaction)
  })
  observe(
    for(x in 1:input$player_n) {
      iv$add_rule(paste0("name", x), sv_required())
    }
  )
  
  
  #############################
  # Adversaries and Scenarios #
  #############################
  
  output$adv_scen <- renderUI({
    if(input$branch_claw) {
      adversaries <- c(adversaries, bc_adversaries)
      scenarios <- c(scenarios, bc_scenarios)
    }
    if(input$jagged_earth) {
      adversaries <- c(adversaries, je_adversaries)
      scenarios <- c(scenarios, je_scenarios)
    }
    if(input$feather_flame) {
      adversaries <- c(adversaries, ff_adversaries)
      scenarios <- c(scenarios, ff_scenarios)
    }
    if(input$nature_incarnate) {
      adversaries <- c(adversaries, ni_adversaries)
      scenarios <- c(scenarios, ni_scenarios)
    }
    
    fluidRow(
      # Adversary 
      column(width=2,
             selectInput(inputId="adversary",
                         label="Adversary:",
                         choices=names(adversaries),
                         selectize=FALSE)
      ),
      # Level
      column(width=2,
             numericInput(inputId="adv_level",
                            label="Level:",
                            value=0,
                            min=0, max=6, step=1)
             ),
      # Scenario
      column(width=3,
             selectInput(inputId="scenario",
                         label="Scenario:",
                         choices=names(scenarios),
                         selectize=FALSE)
             ),
      column(width=2,
             selectInput(inputId="layout",
                         label="Layout:",
                         choices=layout_options,
                         selectize=FALSE)
             ),
      # Difficulty calculation
      renderUI({
        adv_diff <- adversaries[[input$adversary]][input$adv_level + 1]
        scen_diff <- scenarios[[input$scenario]]
        difficulty <<- adv_diff + scen_diff
        column(width=2,
               # https://community.rstudio.com/t/fluidrow-and-column-add-border-to-the-respective-block/13187/2
               style = "background-color: whitesmoke;",
               strong("Difficulty:"),
               helpText(difficulty))
      })
    )
  })
  
  #######################
  # Archipelago Results #
  #######################
  
  output$Archipelago_unlocks <- renderUI({
    if(input$archipelago) {
      
      arc_log <- gen_arclog(df())
      
      s <- unlist(spirit_list, use.names=FALSE)
      locked_spirits <- c("None", s[! s %in% get_spirits(arc_log)])
      
      a <- unique(unlist(aspects, use.names=FALSE))
      locked_aspects <- a[! a %in% unique(arc_log$aspect_unlocked)]
      
      div(
        fluidRow(
          id="victory_text",
          hr(),
          column(width=3,
                 checkboxInput(inputId="victory_text",
                               label="Show victory text?",
                               value=FALSE)),
          column(width=9,
                 renderUI({
                   scen <- active_scenario()
                   
                   if(input$victory_text==TRUE) {
                     text <- paste0(scen$Victory_Text,
                                  "<br/><br/>",
                                  "<b>Unlocks</b>: ", scen$Unlocks,
                                  "<br/>")
                     HTML(text)
                   }
                 })
                 )
        ),
        fluidRow(
          id="arch",
          hr(),
          column(width=2, offset=0,
                 selectInput(inputId="unlock_spirit",
                             label="Unlock spirit?",
                             choices=locked_spirits,
                             selectize=FALSE)),
          column(width=2,
                 selectInput(inputId="unlock_aspect",
                             label="Unlock aspect?",
                             choices=locked_aspects,
                             selectize=FALSE)),
          column(width=2,
                 textInput(inputId="unlock_artifacts",
                             label="Unlock artifact?",
                             value=NA,
                             placeholder="Artifact(s) unlocked, comma separated")),
          column(width=2,
                 textInput(inputId="unlock_flags",
                             label="Unlock flag?",
                             value=NA,
                           placeholder="Flag(s) unlocked, comma separated")),
          column(width=3,
                 checkboxInput(inputId="annex4",
                               label="Annex 4?",
                               value=FALSE)),
          column(width=3,
                 checkboxInput(inputId="annex5",
                               label="Annex 5?",
                               value=FALSE))
        )
      )
    }
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$victory, {
    if(input$victory>0){
      score <- (5 * difficulty) + 10 + (2 * input$invader_cards) +
        floor(input$dahan/input$player_n) - floor(input$blight/input$player_n)
      newrow <- gen_datarow(input, data=mydata, victory=TRUE, score=score, difficulty=difficulty)
      
      mydata <<- rbind(mydata, newrow)
    }
    saveData(mydata)
    
    showNotification(paste0("Well Done! Score of ", score, " recorded"))
  })
  
  observeEvent(input$defeat, {
    if(input$defeat>0){
      score <- (2 * difficulty) + input$invader_cards +
        floor(input$dahan/input$player_n) - floor(input$blight/input$player_n)
      newrow <- gen_datarow(input, data=mydata, victory=FALSE, score=score, difficulty=difficulty)
      
      mydata <<- rbind(mydata, newrow)
    }
    saveData(mydata)
    
    showNotification(paste0("Better luck next time! Score of ", score, " recorded"))
  })
  
  #################
  # Reactive data #
  #################
  
  df <- eventReactive(c(input$victory, input$defeat), {
    data.frame(mydata)
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  # https://stackoverflow.com/a/40812507
  output$scores <- DT::renderDataTable({
    data <- df()
    
    data <- arrange_scoretable(data)
    setorder(data, -id)
    
    if (!is.null(input$columns)) {
      columns = input$columns
    }
    data[, columns]
    },
    options=list(
      pageLength=20),
    rownames=FALSE
    )
  
  #########
  # Plots #
  #########
  
  df_player <- reactive({
    data <- players_long(mydata)
    if(input$filter_player == "T & J") {
      data <- data %>%
        filter(name %in% c("Thomas", "Jennifer"))
    } else if(input$filter_player != "All") {
      data <- data %>%
        filter(name == input$filter_player)
    }
    if(input$filter_archipelago == "Archipelago only") {
      data <- data %>%
        filter(archipelago == TRUE)
    } else if (input$filter_archipelago == "Non-Archipelago") {
      data <- data %>%
        filter(archipelago == FALSE)
    }
    if(input$filter_adversary != "All") {
      data <- data %>%
        filter(adversary == input$filter_adversary)
    }
    if(input$filter_scenario != "All") {
      data <- data %>%
        filter(scenario == input$filter_scenario)
    }
    data <- data %>% 
      arrange(desc(id))
    return(data)
  }) %>% 
    bindEvent(input$victory, input$defeat, 
              input$filter_player, input$filter_archipelago,
              input$filter_adversary, input$filter_scenario)
  
  observe({
    players <- df() %>%
      select(id, starts_with("name"), archipelago, adversary, scenario) %>%
      pivot_longer(cols=paste0("name_", 1:6)) %>%
      filter(!is.na(value) & value != "")

    updateSelectInput(session, "filter_player",
                      choices = c("All", "T & J", unique(players$value)),
                      selected = "All")
    
    updateSelectInput(session, "filter_adversary",
                      choices = c("All",  unique(players$adversary)),
                      selected = "All")
    
    updateSelectInput(session, "filter_scenario",
                      choices = c("All", unique(players$scenario)),
                      selected = "All")
    
  })
  
  # Per-player plots
  output$pop_spirit <- renderPlotly(popular_spirit(df_player())) 
  output$diff_vs_score <- renderPlotly(difficulty_vs_score(df_player()))
  output$time_score <- renderPlotly(time_score(df_player()))
  output$games_since_spirit <- renderPlotly(games_since_spirit(df_player()))
  output$games_since_adversary <- renderPlotly(games_since_adversary(df_player()))
  output$adversary_pie <- renderPlotly(adversary_pie(df_player()))
  
  output$avgstat_by_adv <- renderPlotly(avgstat_by_adv(df_player()))
  output$rates_by_adv <- renderPlotly(rates_by_adv(df_player())) 
  
  
  # Global plots
  output$spirit_friends <- renderPlotly(spirit_friends(players_long(df())))
  
  ###############
  # Archipelago #
  ###############
  
  output$available <- renderUI({
    arc_log <- gen_arclog(df())
    
    available_scenarios <- get_available(arc_log, scen_list)
    
    s <- unlist(spirit_list, use.names=FALSE)
    unlocked_spirits <- s[s %in% get_spirits(arc_log)]
    
    a <- unique(unlist(aspects, use.names=FALSE))
    unlocked_aspects <- a[a %in% unique(arc_log$aspect_unlocked)]
    
    fluidRow(
      id="arch",
      renderText(paste0("Available scenarios: ", paste0(available_scenarios, collapse=", "))),
      renderText(paste0("Unlocked spirits: ", paste0(unlocked_spirits, collapse=", "))),
      renderText(paste0("Unlocked aspects: ", paste0(unlocked_aspects, collapse=", "))),
    )
    
    
  })
  
  output$artifacts <- DT::renderDataTable({
    arc_log <- gen_arclog(df())
    available <- get_artifacts(arc_log)
    artifacts_csv %>% 
      filter(Artifact %in% available) %>%
      select(-Description) %>%
      mutate(Effect = str_replace_all(Effect, pattern="\\. ", replacement=".<br/>"))
  },
  options=list(
    pageLength=5),
  rownames=FALSE, escape=FALSE)
  
  output$flags <- DT::renderDataTable({

    arc_log <- gen_arclog(df())
    available <- get_flags(arc_log)
    flags_csv %>% 
      filter(Flag %in% available) %>%
      select(-Description) %>%
      mutate(Effect = str_replace_all(Effect, pattern="\\. ", replacement=".<br/>"))
  },
  options=list(
    pageLength=5),
  rownames=FALSE, escape=FALSE)
  
  output$archipelago_log <- DT::renderDataTable({
    arc_log <- gen_arclog(df())
    arc_log %>% arrange(desc(game)) %>%
      select(-artifact_unlocked, -flag_unlocked, -annex4, -annex5)
  },
  options=list(
    pageLength=5),
  rownames=FALSE
  )
  
  #######################
  # Download and upload #
  #######################
  
  # Download
  output$downloadData <- downloadHandler(
    filename = paste0("spiritisland_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      write.csv(mydata, file, row.names = FALSE)
    }
  )
  
  # Upload
  observeEvent(input$file1, {
    mydata <<- loadcsv(input$file1$datapath)
  })
  
  
  iv$enable()
}


shinyApp(ui, server)

