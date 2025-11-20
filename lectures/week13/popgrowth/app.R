# -------------------------------------------------------------
# Complete self-contained Shiny app
# Visualizes population, fertility, GDP (1900–present)
# and allows simple exploratory regressions.
# -------------------------------------------------------------

library(shiny)
library(tidyverse)
library(broom)

load("population.RData")

# -------------------------------------------------------------
# Load your three data frames:
# population, fertility, gdp
# (Assumed already in the environment—replace with read_csv() if needed)
# -------------------------------------------------------------

# Build merged panel
panel <- population %>%
  filter(Year >= 1900) %>%
  left_join(fertility, by = c("Entity", "Code", "Year")) %>%
  left_join(gdp,       by = c("Entity", "Code", "Year")) %>%
  mutate(
    Population_Millions = Population / 1e6
  )

# -------------------------------------------------------------
# UI
# -------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Modern Population, Fertility, and GDP Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "countries",
        "Select countries:",
        choices = sort(unique(panel$Entity)),
        multiple = TRUE,
        options = list(maxItems = 6)
      ),
      sliderInput(
        "years",
        "Year range:",
        min = min(panel$Year, na.rm = TRUE),
        max = max(panel$Year, na.rm = TRUE),
        value = c(1950, 2020),
        step = 1
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization",
                 h3("Population (Millions)"),
                 plotOutput("popPlot"),
                 
                 h3("Fertility Rate"),
                 plotOutput("fertPlot"),
                 
                 h3("Real GDP per Capita"),
                 plotOutput("gdpPlot")
        ),
        
        tabPanel("Regression / Correlation",
                 h3("Correlation Matrix"),
                 tableOutput("corrTable"),
                 
                 h3("Simple Linear Model"),
                 selectInput(
                   "yvar",
                   "Outcome (Y):",
                   choices = c(
                     "Population_Millions",
                     "Fertility_Rate",
                     "GDP"
                   )
                 ),
                 selectInput(
                   "xvar",
                   "Predictor (X):",
                   choices = c(
                     "Population_Millions",
                     "Fertility_Rate",
                     "GDP"
                   ),
                   selected = "GDP"
                 ),
                 verbatimTextOutput("lmOut")
        )
      )
    )
  )
)

# -------------------------------------------------------------
# SERVER
# -------------------------------------------------------------
server <- function(input, output, session) {
  
  filtered <- reactive({
    req(input$countries)
    panel %>%
      filter(
        Entity %in% input$countries,
        Year >= input$years[1],
        Year <= input$years[2]
      )
  })
  
  # -------------------- Viz: Population -----------------------
  output$popPlot <- renderPlot({
    df <- filtered()
    ggplot(df, aes(Year, Population_Millions, color = Entity)) +
      geom_line(size = 1) +
      labs(
        y = "Population (millions)",
        x = "Year"
      ) +
      theme_classic()
  })
  
  # -------------------- Viz: Fertility ------------------------
  output$fertPlot <- renderPlot({
    df <- filtered()
    ggplot(df, aes(Year, Fertility_Rate, color = Entity)) +
      geom_line(size = 1) +
      labs(
        y = "Fertility rate (children per woman)",
        x = "Year"
      ) +
      theme_classic()
  })
  
  # -------------------- Viz: GDP per cap ----------------------
  output$gdpPlot <- renderPlot({
    df <- filtered()
    ggplot(df, aes(Year, GDP, color = Entity)) +
      geom_line(size = 1) +
      labs(
        y = "GDP per capita (real)",
        x = "Year"
      ) +
      theme_classic()
  })
  
  # -------------------- Regression: Correlation matrix --------
  output$corrTable <- renderTable({
    df <- filtered() %>%
      select(Population_Millions, Fertility_Rate, GDP)
    
    if (nrow(df) < 5) return(NULL)
    
    round(cor(df, use = "pairwise.complete.obs"), 3)
  })
  
  # -------------------- Regression: Simple linear model -------
  output$lmOut <- renderPrint({
    df <- filtered()
    
    y <- input$yvar
    x <- input$xvar
    
    if (y == x) {
      cat("Outcome and predictor must differ.")
      return()
    }
    
    # enough data?
    if (sum(!is.na(df[[y]]) & !is.na(df[[x]])) < 10) {
      cat("Insufficient complete data to fit model.")
      return()
    }
    
    mod <- lm(as.formula(paste(y, "~", x)), data = df)
    summary(mod)
  })
}

# -------------------------------------------------------------
# Run the app
# -------------------------------------------------------------
shinyApp(ui, server)