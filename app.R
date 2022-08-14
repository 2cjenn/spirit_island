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

# Spirit info ------------------------------------------------------------------

# source("define_players.R")

source("spirit_info.R")

# source("testdata.R")

source("plots.R")

source("utils.R")

# Data functions ---------------------------------------------------------------
# https://deanattali.com/blog/shiny-persistent-data-storage/

saveData <- function(data) {
  saveRDS(data, "data.rds")
  }

loadData <- function() {
  data <- readRDS("data.rds")
  return(data)
}

loadcsv <- function(filepath) {
  mydata <- read.csv(filepath, na.strings = "NA")
  mydata$date <- as.Date(mydata$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
  mydata$id <- as.POSIXct(mydata$id, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
  return(mydata)
}

mydata <- loadData()
# mydata <- loadcsv("data.csv")

players <- mydata %>%
  select(id, starts_with("name")) %>%
  pivot_longer(cols=paste0("name_", 1:6)) %>%
  filter(!is.na(value) & value != "")

  
# Begin ------------------------------------------------------------------------


# Shiny app
# https://shiny.rstudio.com/articles/layout-guide.html - layouts
# https://shiny.rstudio.com/articles/html-tags.html - text formatting
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/ - widget types
# https://github.com/rstudio/shiny/issues/1927 - keyboard in select on mobile
ui = fluidPage(
  # theme = bs_theme(version = 5, bootswatch = "minty"),
  tabsetPanel(
    tabPanel("Enter results",
             titlePanel("Spirit Island"),
             strong("Expansions:"),
             fluidRow(
               id="expansions",
               #DT::dataTableOutput("responses", width = 300), tags$hr(),
               column(width=3,
                      checkboxInput(inputId="branch_claw",
                                    label="Branch and Claw?",
                                    value=TRUE)),
               column(width=3,
                      checkboxInput(inputId="jagged_earth",
                                    label="Jagged Earth?",
                                    value=FALSE)),
               column(width=3,
                      checkboxInput(inputId="feather_flame",
                                    label="Feather and Flame?",
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
             hr(),
             uiOutput("Players"),
             hr(),
             uiOutput("adv_scen"),
             hr(),
             
             fluidRow(
               id="invad_scen",
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
                      numericInput(inputId="fear_level",
                                   label="Fear Level",
                                   value=1,
                                   min=1, max=4, step=1))
             ),
             helpText("For a victory, count the invader cards remaining in the deck.",
                      "For a defeat, count the invader cards *not* in the deck."),
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
                                   "victory", "invader_cards", "dahan", "blight", "score",
                                   "blighted_island", "fear_level", "expansions"),
                         selected=c("date", "spirits", "adversary", "level", 
                                    "scenario", "difficulty", "victory", "score"),
                         multiple=TRUE),
             
             dataTableOutput("scores")
    ),
    tabPanel("Plots",
             selectInput(inputId="filter_player",
                         label="Player:",
                         choices=c("All", unique(players$value)),
                         selected="All",
                         selectize=FALSE),
             
             plotlyOutput("pop_spirit", inline=TRUE),
             plotlyOutput("diff_vs_score", inline=TRUE),
             plotlyOutput("games_since_spirit", inline=TRUE),
             plotlyOutput("games_since_adversary", inline=TRUE)
      
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
  
)


server = function(input, output, session) {
  iv <- InputValidator$new()
  
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
      aspect_list <- Map(c, aspect_list, je_aspects)
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
      aspect_list <- Map(c, aspect_list, ff_aspects)
    }
    
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
               if((input$jagged_earth | input$feather_flame) & 
                  input[[paste0("spirit", x)]] %in% names(aspect_list)){
                 selectInput(inputId=paste0("aspect", x),
                             label="Aspect:",
                             choices=aspect_list[input[[paste0("spirit", x)]]],
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
               if(input[[paste0("spirit", x)]] %in% names(aspect_list)){
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
    
    fluidRow(
      # Adversary 
      column(width=3,
             selectInput(inputId="adversary",
                         label="Adversary:",
                         choices=names(adversaries),
                         selectize=FALSE)
      ),
      # Level
      renderUI({
        column(width=2,
               if(input$adversary != "None") {
                 numericInput(inputId="adv_level",
                              label="Level:",
                              value=0,
                              min=0, max=6, step=1)
               } else {
                 numericInput(inputId="adv_level",
                              label="Level:",
                              value=0,
                              min=0, max=0)
               })
      }),
      # Scenario
      column(width=4,
             selectInput(inputId="scenario",
                         label="Scenario:",
                         choices=names(scenarios),
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

  
  gen_players <- function(input, unique_id) {
    players <- data.table(
      id = as.POSIXct(character()),
      n = numeric(),
      name = character(),
      spirit = character(),
      aspect = character(),
      board = character(),
      power_prog = character(),
      top_track = numeric(),
      bottom_track = numeric(),
      destroyed = numeric()
    )
    for (i in 1:6){
      if(i <= input$player_n) {
        player_i <- data.table(
          id = unique_id,
          n = i,
          name = input[[paste0("name", i)]],
          spirit = input[[paste0("spirit", i)]],
          aspect = ifelse(
            is.null(input[[paste0("aspect", i)]]),
            "",
            input[[paste0("aspect", i)]]
          ),
          board = input[[paste0("board", i)]],
          power_prog = input[[paste0("powerprog", i)]],
          top_track = input[[paste0("toptrack", i)]],
          bottom_track = input[[paste0("bottomtrack",i)]],
          destroyed = input[[paste0("destroyed",i)]]
        )
      } else {
        player_i <- data.table(
          id = unique_id,
          n = i,
          name = "",
          spirit = "",
          aspect = "",
          board = "",
          power_prog = FALSE,
          top_track = NA,
          bottom_track = NA,
          destroyed = NA
        )
      }
      players <- rbind(players, player_i)
      players_wide <- dcast(players, formula = id ~ n, 
                            value.var=list("name", "spirit", "aspect", "board", 
                                           "power_prog", "top_track", 
                                           "bottom_track", "destroyed"))
      
    }
    return(players_wide)
  }
  
  gen_datarow <- function(input, victory, score, difficulty){
    unique_id <- Sys.time()
    
    players <- gen_players(input, unique_id)
    
    newrow = data.table(id = unique_id,
                        date = input$date,
                        n_players = input$player_n,
                        adversary = input$adversary,
                        level = input$adv_level,
                        scenario = input$scenario,
                        difficulty = difficulty,
                        victory = victory,
                        invader_cards = input$invader_cards,
                        dahan = input$dahan,
                        blight = input$blight,
                        score = score,
                        #invisible
                        branch_claw = input$branch_claw,
                        jagged_earth = input$jagged_earth,
                        feather_flame = input$feather_flame,
                        blighted_island = input$blighted_island,
                        fear_level = input$fear_level
    )
    
    row = merge.data.table(players, newrow, by.x=c("id"), by.y=c("id"))
    return(row)
  }
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$victory, {
    if(input$victory>0){
      score <- (5 * difficulty) + 10 + (2 * input$invader_cards) +
        floor(input$dahan/input$player_n) - floor(input$blight/input$player_n)
      newrow <- gen_datarow(input, victory=TRUE, score=score, difficulty=difficulty)
      
      mydata <<- rbind(mydata, newrow)
    }
    saveData(mydata)
    
    showNotification(paste0("Well Done! Score of ", score, " recorded"))
  })
  
  observeEvent(input$defeat, {
    if(input$defeat>0){
      score <- (2 * difficulty) + input$invader_cards +
        floor(input$dahan/input$player_n) - floor(input$blight/input$player_n)
      newrow <- gen_datarow(input, victory=FALSE, score=score, difficulty=difficulty)
      
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
    
    data <- data %>%
      mutate(across(spirit_1:spirit_6, ~ifelse(.x=="", .x, abbreviations[.x])),
             branch_claw = ifelse(branch_claw == TRUE, "BC", NA),
             jagged_earth = ifelse(jagged_earth == TRUE, "JE", NA),
             feather_flame = ifelse(feather_flame == TRUE, "FF", NA))
      
    
    for(i in 1:6) {
      power_prog_col <- paste0("power_prog_", i)
      spirit_col <- paste0("spirit_", i)
      power_prog <- data[[power_prog_col]] == TRUE
      if(any(power_prog)) {
        data[power_prog,][[spirit_col]] <- paste0(data[power_prog,][[spirit_col]], "+")
      }
    }
    
    data$spirits <- apply( data[, paste0("spirit_", c(1:6)) ], 1, paste_noNA, sep=", ")
    data$names <- apply( data[, paste0("name_", c(1:6)) ], 1, paste_noNA, sep=", ")
    data$boards <- apply( data[, paste0("board_", c(1:6)) ], 1, paste_noNA, sep=", ")
    data$expansions <- apply( data[, c("branch_claw", "jagged_earth", "feather_flame")],
                              1, paste_noNA, sep=", ")
    data <- data %>% 
      dplyr::relocate(names, .after=date) %>%
      dplyr::relocate(spirits, .after=names) %>%
      dplyr::relocate(boards, .after=spirits) %>%
      select(-c(name_1:name_6, spirit_1:spirit_6, aspect_1:aspect_6, 
                power_prog_1:power_prog_6, board_1:board_6,
                top_track_1:top_track_6, bottom_track_1:bottom_track_6,
                destroyed_1:destroyed_6,
                branch_claw, jagged_earth, feather_flame))
      # select(date, spirits, adversary, level, scenario, difficulty, victory, score)
    if (!is.null(input$columns)) {
      columns = input$columns
    }
    data[, columns]
    },
    options=list(
      order=c(1, 'desc'),
      pageLength=20)
    )
  
  #########
  # Plots #
  #########

  
  df_player <- reactive({
    data <- players_long(mydata)
    if(input$filter_player != "All") {
      data <- data %>%
        filter(name == input$filter_player) %>%
        arrange(desc(date)) %>%
        mutate(game = seq.int(nrow(.)))
    }
    return(data)
  }) %>% 
    bindEvent(input$victory, input$defeat, input$filter_player)
  
  observe({
    players <- df() %>%
      select(id, starts_with("name")) %>%
      pivot_longer(cols=paste0("name_", 1:6)) %>%
      filter(!is.na(value) & value != "")

    updateSelectInput(session, "filter_player",
                      choices = c("All", unique(players$value)),
                      selected = "All")
    
  })


  output$pop_spirit <- renderPlotly(popular_spirit(df_player())) 
  output$diff_vs_score <- renderPlotly(difficulty_vs_score(df_player()))
  output$games_since_spirit <- renderPlotly(games_since_spirit(df_player()))
  output$games_since_adversary <- renderPlotly(games_since_adversary(df_player()))
  
  #######################
  # Download and upload #
  #######################
  
  # Download
  output$downloadData <- downloadHandler(
    filename = "spiritisland_data.csv",
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

