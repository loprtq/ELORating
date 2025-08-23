## *****************************************************************************
## # Description: 
## R Shiny app to be used for ELO rating related matches. Created for things 
## such as table tennis, football, etc.
## *****************************************************************************

## *****************************************************************************
## # Libraries and helper functions
## *****************************************************************************
# Libraries
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(elo)

# Helper functions
invisible(
  sapply(list.files(path = "funcs", full.names = TRUE), source)
)

# Initial files
data.frame("Player" = NA, "Rating" = NA) %>%
  data_crt(filepath = "player_data.rds")
data.frame("Time" = Sys.time(), "Player1" = NA, "Score1" = NA, "Player2" = NA, "Score2" = NA) %>%
  data_crt(filepath = "match_data.rds")


## *****************************************************************************
## # UI
## *****************************************************************************
ui <- bslib::page_navbar(
  title = "ELO Rating",
  theme = bslib::bs_theme(),
  shiny::includeCSS("www/style.css"),
  id = "nav_tabs",
  
  # Match data
  # * Register matches
  # * See recent matches
  bslib::nav_panel(
    title = "Matches",
    value = "matches_tab",
    bslib::card(
      bslib::card_header(htmltools::h3("Register match")),
      bslib::layout_columns(
        col_widths = c(5, 2, 5),
        htmltools::div(
          class = "d-flex flex-column align-items-start",
          # style = "min-width: 350px;",
          htmltools::p(
            shiny::textOutput(outputId = "player1ELO")
          ),
          shiny::selectInput(
            label = NULL,
            inputId = "player1",
            choices = NULL,
            width = "95%"
          ),
          shiny::selectInput(
            label = NULL,
            inputId = "player1score",
            choices = posscore,
            width = "20%"
          )
        ),
        htmltools::div(
          class = "d-flex flex-column align-items-center",
          br(),
          htmltools::h1("VS")
        ),
        htmltools::div(
          class = "d-flex flex-column align-items-end",
          # style = "min-width: 350px;",
          htmltools::p(
            shiny::textOutput(outputId = "player2ELO")
          ),
          shiny::selectInput(
            label = NULL,
            inputId = "player2",
            choices = NULL,
            width = "95%"
          ),
          shiny::selectInput(
            label = NULL,
            inputId = "player2score",
            choices = posscore,
            width = "20%"
          )
        )
      ),
      htmltools::div(
        style = "display: flex; justify-content: center;",
        shiny::actionButton(
          inputId = "matchregister_btn",
          label = "Submit",
          style = "min-width: 150px; max-width: 250px;"
        )
      ),
      min_height = "300px",
      max_height = "350px"
    ),
    bslib::card(
      bslib::card_header(htmltools::h3("Match history")),
      bslib::card_body(
        shiny::tableOutput(outputId = "match_table")
      )
    )
  ),
  
  # Player data
  # * Register players
  # * See overall player ratings
  bslib::nav_panel(
    title = "Players",
    value = "players_tab",
    bslib::card(
      bslib::card_header(htmltools::h3("Register player")),
      bslib::card_body(
        htmltools::div(
          style = "display: flex; justify-content: center;",
          shiny::textInput(
            inputId = "playerregister",
            label = NULL,
            placeholder = "Name",
            width = "75%"
          )
        ),
        htmltools::div(
          style = "display: flex; justify-content: center;",
          shiny::actionButton(
            inputId = "playerregister_btn",
            label = "Submit",
            style = "min-width: 150px; max-width: 250px;"
          )
        )
      ),
      min_height = "175px",
      max_height = "225px"
    ),
    bslib::card(
      bslib::card_header(htmltools::h3("Player ratings")),
      bslib::card_body(
        shiny::tableOutput(outputId = "player_table")
      )
    )
  ),
  
  
  # Filter button
  bslib::nav_spacer(),
  bslib::nav_item(
    htmltools::div(
      style = "display: flex; justify-content: center;",
      shiny::actionButton(
        inputId = "filter_btn",
        label = "Filter",
        icon = icon("filter")
      )
    )
  )
)


## *****************************************************************************
## # Server
## *****************************************************************************
server <- function(input, output, session) {
# Data --------------------------------------------------------------------
  # Player data
  player_data <- shiny::reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = "player_data.rds",
    readFunc = data_imp
  )
  
  # Match data
  match_data <- shiny::reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = "match_data.rds",
    readFunc = data_imp
  )
  
  # Initial choices for players in a match
  observeEvent(TRUE, {
    shiny::updateSelectInput(
      inputId = "player1",
      choices = player_data() %>%
        dplyr::pull(Player)
    )
    shiny::updateSelectInput(
      inputId = "player2",
      choices = player_data() %>%
        dplyr::pull(Player)
    )
  })
  
  
# Player Tab --------------------------------------------------------------
  # Register player
  shiny::observeEvent(input$playerregister_btn, {
    shiny::req(input$playerregister)
    
    # Export new player info
    data <- player_data() %>%
      dplyr::add_row(
        Player = input$playerregister,
        Rating = startELO
      ) %>%
      tidyr::drop_na()
    data %>%
      data_exp(data = ., filepath = "player_data.rds")
    
    # Update choices
    shiny::updateSelectInput(
      inputId = "player1",
      choices = data %>%
        dplyr::filter(Player != ifelse(is.null(input$player2), "", input$player2)) %>%
        dplyr::pull(Player),
      selected = input$player1
    )
    shiny::updateSelectInput(
      inputId = "player2",
      choices = data %>%
        dplyr::filter(Player != ifelse(is.null(input$player1), "", input$player1)) %>%
        dplyr::pull(Player),
      selected = input$player2
    )
    
    # Reset player register box
    shiny::updateTextInput(
      inputId = "playerregister",
      value = ""
    )
  })
  
  # Show player data
  output$player_table <- shiny::renderTable(
    player_data() %>%
      dplyr::filter(
        if (is.null(input$filter)) {
          TRUE
        } else {
          grepl(tolower(input$filter), tolower(Player)) | grepl(input$filter, Rating)
        }
      ) %>%
      dplyr::rename(Name = Player) %>%
      dplyr::arrange(-Rating)
  )


# Match Tab ---------------------------------------------------------------
  # Register match
  ## Player 1
  ### Update player 1 choices
  shiny::observeEvent(input$player2, {
    # Required inputs
    shiny::req(input$player2)
    
    # Reset inputs
    shiny::updateSelectInput(
      inputId = "player1",
      choices = player_data() %>%
        dplyr::filter(Player != ifelse(is.null(input$player2), "", input$player2)) %>%
        dplyr::pull(Player),
      selected = input$player1
    )
  })
  
  ### Update player 1 ELO
  output$player1ELO <- shiny::renderText("")
  shiny::observeEvent(input$player1, {
    # Required inputs
    shiny::req(input$player1)
    shiny::req(input$player1 != input$player2)
    
    # Reset inputs
    output$player1ELO <- renderText({
      player_data() %>%
        dplyr::filter(Player == input$player1) %>%
        dplyr::pull(Rating) %>%
        unlist()
    })
  })
  
  ## Player 2
  ### Update player 2 choices
  shiny::observeEvent(input$player1, {
    # Required inputs
    shiny::req(input$player1)
    
    # Reset inputs
    shiny::updateSelectInput(
      inputId = "player2",
      choices = player_data() %>%
        dplyr::filter(Player != ifelse(is.null(input$player1), "", input$player1)) %>%
        dplyr::pull(Player),
      selected = input$player2
    )
  })
  
  ### Update player 2 ELO
  output$player2ELO <- shiny::renderText("")
  shiny::observeEvent(input$player2, {
    # Required inputs
    shiny::req(input$player2)
    shiny::req(input$player1 != input$player2)
    
    # Reset inputs
    output$player2ELO <- renderText({
      player_data() %>%
        dplyr::filter(Player == input$player2) %>%
        dplyr::pull(Rating) %>%
        unlist()
    })
  })
  
  ### Register match data
  shiny::observeEvent(input$matchregister_btn, {
    # Required inputs
    shiny::req(input$player1)
    shiny::req(input$player1score)
    shiny::req(input$player2)
    shiny::req(input$player2score)
    shiny::req(input$player1 != input$player2)
    shiny::req(input$player1score != 0 & input$player2score != 0)
    
    # Save data
    match_data() %>%
      dplyr::add_row(
        Time = Sys.time(),
        Player1 = input$player1,
        Score1 = input$player1score,
        Player2 = input$player2,
        Score2 = input$player2score
      ) %>%
      tidyr::drop_na() %>%
      data_exp(data = ., filepath = "match_data.rds")
    
    # Update ratings
    player1ELO <- player_data() %>%
      dplyr::filter(Player == input$player1) %>%
      dplyr::pull(Rating) %>%
      unlist()
    player2ELO <- player_data() %>%
      dplyr::filter(Player == input$player2) %>%
      dplyr::pull(Rating) %>%
      unlist()
    ratings <- ELO_func(player1ELO, input$player1score, player2ELO, input$player2score)
    player_data() %>%
      dplyr::mutate(Rating = case_when(
        Player == input$player1 ~ ratings[[1]],
        Player == input$player2 ~ ratings[[2]],
        TRUE ~ Rating
      )) %>%
      data_exp(data = ., filepath = "player_data.rds")
    
    # Reset inputs
    shiny::updateSelectInput(inputId = "player1", selected = NULL)
    shiny::updateSelectInput(inputId = "player1score", selected = NULL)
    shiny::updateSelectInput(inputId = "player2", selected = NULL)
    shiny::updateSelectInput(inputId = "player2score", selected = NULL)
  })
  
  
  # Show match data
  output$match_table <- shiny::renderTable(
    match_data() %>%
      dplyr::filter(
        if (is.null(input$filter)) {
          TRUE
        } else {
          grepl(tolower(input$filter), tolower(Player1)) | grepl(input$filter, Score1) |
          grepl(tolower(input$filter), tolower(Player2)) | grepl(input$filter, Score2)
        }
      ) %>%
      dplyr::arrange(desc(Time)) %>%
      dplyr::relocate(Score2, .before = Player2) %>%
      dplyr::select(-Time)
  )


# Filter ------------------------------------------------------------------
  # Filter button
  shiny::observeEvent(input$filter_btn, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Filter",
        htmltools::p("Write either names, scores or the likes. The filter is reset each time the button is pressed."),
        htmltools::div(
          style = "display: flex; justify-content: center;",
          shiny::textInput(
            inputId = "filter",
            label = NULL,
            placeholder = "Filter text",
            width = "75%"
          )
        ),
        easyClose = TRUE
      )
    )
  })
}


## *****************************************************************************
## # Run the app
## *****************************************************************************
shiny::shinyApp(ui = ui, server = server)