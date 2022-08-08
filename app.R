#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(data.table)

# Load data --------------------------------------------------------------------

source("define_players.R")
source("spirit_info.R")
mydata <- data.table(id = "test",
                     val = "test")

# Load data --------------------------------------------------------------------

# data <- readRDS("SpiritIsland.rds")

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

# Begin ------------------------------------------------------------------------


# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")

# Shiny app
# https://shiny.rstudio.com/articles/layout-guide.html
ui = fluidPage(
  tabsetPanel(
    tabPanel("Enter results",
             titlePanel("Spirit Island"),
             fluidRow(
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
               column(width=2, offset=3,
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
                                   min=0, max=52, step=1))
             ),
             hr(),
             fluidRow(
               column(width=2, offset=4,
                      actionButton(inputId="victory",
                                   label="VICTORY!",
                                   class="btn-success")),
               column(width=2,
                      actionButton(inputId="defeat",
                                   label="Defeat :(",
                                   class="btn-danger"),)
             )), 
    tabPanel("Score history", dataTableOutput("scores"))
  )
  
)


server = function(input, output, session) {
  
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
                         selected=unlist(spirits)[x]),
             # Aspects
             renderUI({
             if((input$jagged_earth | input$feather_flame) & 
                input[[paste0("spirit", x)]] %in% names(aspect_list)){
               selectInput(inputId=paste0("aspect", x),
                           label="Aspect:",
                           choices=aspect_list[input[[paste0("spirit", x)]]]
               )
             }}),
             # Board
             selectInput(inputId=paste0("board", x),
                         label="Board:",
                         choices=boards,
                         selected=boards[x])
      )
    })
    do.call(fluidRow, interaction)
  })
  
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
                         choices=adversaries)
             ),
      # Level
      renderUI({
        column(width=2,
               if(input$adversary != "None") {
                 numericInput(inputId="adv_level",
                              label="Level",
                              value=0,
                              min=0,
                              max=6,
                              step=1)
               })
             }),
      # Scenario
      column(width=4,
             selectInput(inputId="scenario",
                         label="Scenario:",
                         choices=scenarios)
      )
    )
  })
  
  # Whenever a field is filled, aggregate all form data
  # formData <- reactive({
  #   # data <- sapply(fields, function(x) input[[x]])
  #   # data
  #   return(input)
  # })

  # # When the Submit button is clicked, save the form data
  # observeEvent(input$victory, {
  #   saveData(formData())
  # })

  # Show the previous responses
  # (update with current response when Submit is clicked)
  # https://stackoverflow.com/a/40812507
  output$scores <- DT::renderDataTable(df())
  df <- eventReactive(input$victory, {
    if(input$victory>0){
      newrow = data.table(id = input$name1,
                          val = input$spirit1)
      mydata <<- rbind(mydata, newrow)
    }
    mydata
    }, ignoreNULL = FALSE)
}


shinyApp(ui, server)