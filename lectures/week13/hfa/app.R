library(shiny)
library(dplyr)
library(ggplot2)
library(nflreadr)
library(purrr)
library(stringr)

ui <- fluidPage(
  titlePanel("NFL Home Field Advantage Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      sliderInput(
        "year_range",
        "Season Range:",
        min = 2000,      # adjust depending on nflreadr data availability
        max = 2024,
        value = c(2000, 2024),
        sep = ""
      ),
      selectInput("team", "Select Team:", choices = NULL),
      selectInput("rival_a", "Rivalry: Team A", choices = NULL),
      selectInput("rival_b", "Rivalry: Team B", choices = NULL),
      br(),
      helpText("Model: margin = Σ_j β_j * indicator_j + ε")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Home Field Advantage", verbatimTextOutput("hfa_text")),
        tabPanel("Team Strength Estimates", plotOutput("coef_plot")),
        tabPanel("Margins by Team", plotOutput("team_margins")),
        tabPanel("Rivalries", plotOutput("rivalry_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  ####################################################################
  # 1. Load and prepare data
  ####################################################################
  
  games <- reactive({
    # Apply year filter
    yr <- input$year_range
    g <- nflreadr::load_schedules() |>
      filter(game_type == "REG", !is.na(home_score), !is.na(away_score)) |>
      filter(season >= yr[1], season <= yr[2]) |>
      mutate(
        margin = home_score - away_score,
      )
    
    teams <- sort(unique(c(g$home_team, g$away_team)))
    
    # Build design matrix
    for (team in teams) {
      g[[team]] <- case_when(
        g$home_team == team ~ 1,
        g$away_team == team ~ -1,
        .default = 0
      )
    }
    
    return(list(data = g, teams = teams))
  })
  
  observe({
    updateSelectInput(session, "team", choices = games()$teams)
    updateSelectInput(session, "rival_a", choices = games()$teams)
    updateSelectInput(session, "rival_b", choices = games()$teams)
  })
  
  ####################################################################
  # 2. Fit model
  ####################################################################
  
  model_fit <- reactive({
    g <- games()$data
    teams <- games()$teams
    lm(margin ~ ., data = g[, c("margin", teams)])
  })
  
  ####################################################################
  # 3. Team strength coefficient plot
  ####################################################################
  
  output$coef_plot <- renderPlot({
    fit <- model_fit()
    teams <- games()$teams
    coefs <- coef(fit)[teams]
    
    df <- tibble(team = teams, strength = coefs)
    
    ggplot(df, aes(x = reorder(team, strength), y = strength)) +
      geom_col() +
      coord_flip() +
      labs(title = "Estimated Team Strength Coefficients",
           x = "Team", y = "Strength (HFA-adjusted effect)") +
      theme_minimal()
  })
  
  ####################################################################
  # 4. Margins by team
  ####################################################################
  
  output$team_margins <- renderPlot({
    g <- games()$data
    req(input$team)
    
    df <- g |>
      filter(home_team == input$team | away_team == input$team) |>
      mutate(location = ifelse(home_team == input$team, "Home", "Away"))
    
    ggplot(df, aes(x = location, y = margin)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.4, width = 0.2) +
      labs(title = paste("Margins for", input$team),
           y = "Home Score – Away Score", x = "Location") +
      theme_minimal()
  })
  
  output$hfa_text <- renderPrint({
    fit <- model_fit()
    intercept <- coef(fit)[1]
    se <- summary(fit)$coefficients[1, 2]
    cat("Estimated Home Field Advantage (intercept):\n")
    cat(sprintf("  %.3f points (SE = %.3f)\n", intercept, se))
  })
  
  ####################################################################
  # 5. Rivalry explorer
  ####################################################################
  
  output$rivalry_plot <- renderPlot({
    g <- games()$data
    req(input$rival_a, input$rival_b)
    
    df <- g |>
      filter(
        (home_team == input$rival_a & away_team == input$rival_b) |
          (home_team == input$rival_b & away_team == input$rival_a)
      ) |>
      mutate(matchup = paste(home_team, "vs", away_team))
    
    ggplot(df, aes(x = as.factor(season), y = margin, color = home_team)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      labs(
        title = paste("Rivalry:", input$rival_a, "vs", input$rival_b),
        x = "Season", y = "Home – Away Margin"
      ) +
      theme_minimal()
  })
  
}

shinyApp(ui, server)