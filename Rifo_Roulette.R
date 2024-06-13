library(shiny)
library(shinyjs)
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Rifo Roulette"),
  sidebarLayout(
    sidebarPanel(
      textInput("player1_name", "Enter Player 1 Name:", value = "Player 1"),
      textInput("player2_name", "Enter Player 2 Name:", value = "Player 2"),
      actionButton("start_game", "Start Game"),
      actionButton("restart_game", "Restart Game", style = "display:none")
    ),
    mainPanel(
      h3(textOutput("current_turn")),
      textOutput("game_status"),
      tags$hr(),
      htmlOutput("player1_lives"),
      htmlOutput("player2_lives"),
      textOutput("shot_result"),
      br(),
      actionButton("shoot_self", "Shoot Yourself", style = "display:none"),
      actionButton("shoot_opponent", "Shoot Opponent", style = "display:none")
    )
  )
)
server <- function(input, output, session) {
  bullets <- reactiveVal(sample(1:10, 3))
  player1_lives <- reactiveVal(2)
  player2_lives <- reactiveVal(2)
  player_names <- reactiveVal(c("Player 1", "Player 2"))
  current_player <- reactiveVal(1)
  chambers <- reactiveVal(1:10)
  shot_result <- reactiveVal("")
  game_started <- reactiveVal(FALSE)
  
  observeEvent(input$start_game, {
    player_names(sample(c(input$player1_name, input$player2_name)))
    current_player(sample(1:2, 1))
    player1_lives(2)
    player2_lives(2)
    chambers(1:10)
    bullets(sample(1:10, 3))
    game_started(TRUE)
    hide("start_game")
    show("restart_game")
    show("shoot_self")
    show("shoot_opponent")
  })
  
  observeEvent(input$restart_game, {
    player_names(sample(c(input$player1_name, input$player2_name)))
    current_player(sample(1:2, 1))
    player1_lives(2)
    player2_lives(2)
    chambers(1:10)
    bullets(sample(1:10, 3))
    shot_result("")
    game_started(TRUE)
  })
  
  observeEvent(input$shoot_self, {
    if (game_started()) {
      if (current_player() == 1) {
        shot_result(shoot(player1_lives, bullets, chambers, current_player))
      } else {
        shot_result(shoot(player2_lives, bullets, chambers, current_player))
      }
      check_game_status()
    }
  })
  
  observeEvent(input$shoot_opponent, {
    if (game_started()) {
      if (current_player() == 1) {
        shot_result(shoot(player2_lives, bullets, chambers, current_player))
        current_player(2)
      } else {
        shot_result(shoot(player1_lives, bullets, chambers, current_player))
        current_player(1)
      }
      check_game_status()
    }
  })
  
  shoot <- function(player_lives, bullets, chambers, current_player) {
    chamber <- sample(chambers(), 1)
    chambers(setdiff(chambers(), chamber))
    if (chamber %in% bullets()) {
      player_lives(player_lives() - 1)
      return(paste("BANNNGGGGGG!!!! ", player_names()[current_player()], "Shot a bullet!"))
    } else {
      return(paste("PHHHHHSSSS!!!! ", player_names()[current_player()], "Shot a blank!"))
    }
  }
  
  check_game_status <- function() {
    if (player1_lives() <= 0) {
      showModal(modalDialog(
        title = "Game Over",
        paste(player_names()[2], "Wins!"),
        easyClose = TRUE,
        footer = NULL
      ))
      game_started(FALSE)
    } else if (player2_lives() <= 0) {
      showModal(modalDialog(
        title = "Game Over",
        paste(player_names()[1], "Wins!"),
        easyClose = TRUE,
        footer = NULL
      ))
      game_started(FALSE)
    }
  }
  
  output$game_status <- renderText({
    if (game_started()) {
      "Game in Progress"
    } else {
      "Please start the game."
    }
  })
  
  output$current_turn <- renderText({
    if (game_started()) {
      paste("Gun Holder: ", player_names()[current_player()])
    } else {
      "Waiting to start..."
    }
  })
  
  output$player1_lives <- renderUI({
    HTML(paste(player_names()[1], "Lives: ", paste(rep("&#x1F499;", player1_lives()), collapse = " ")))
  })
  
  output$player2_lives <- renderUI({
    HTML(paste(player_names()[2], "Lives: ", paste(rep("&#x1F499;", player2_lives()), collapse = " ")))
  })
  
  output$shot_result <- renderText({
    shot_result()
  })
}

shinyApp(ui = ui, server = server)
