library(shiny)
library(plotly)
library(dplyr)

# Liste des pays de l'Union Européenne
eu_countries <- c("France", "Germany", "Italy", "Spain", "Poland", "Romania", 
                  "Netherlands", "Belgium", "Czechia", "Greece", "Portugal", 
                  "Sweden", "Hungary", "Austria", "Bulgaria", "Denmark", 
                  "Finland", "Slovakia", "Ireland", "Croatia", "Lithuania", 
                  "Slovenia", "Latvia", "Estonia", "Cyprus", "Luxembourg", "Malta")

# Interface utilisateur
ui <- fluidPage(
  titlePanel("COVID-19 : Union Européenne"),
  sidebarLayout(
    sidebarPanel(
      h4("Afficher les données :"),
      checkboxInput("show_confirmed", "Cas confirmés", value = TRUE),
      checkboxInput("show_deaths", "Décès", value = TRUE)
    ),
    mainPanel(
      h4(""),
      plotlyOutput("plot_eu")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Assurer que la colonne Last_Update est bien au format Date
  combined_data$Last_Update <- as.Date(combined_data$Last_Update, format = "%Y-%m-%d")
  
  # Filtrer les données pour l'Union Européenne et après une certaine date
  eu_data <- combined_data %>%
    filter(Country_Region %in% eu_countries) %>%
    filter(Last_Update >= as.Date("2020-08-04")) %>%
    group_by(Last_Update) %>%
    summarise(
      Total_Confirmed = sum(Confirmed),
      Total_Deaths = sum(Deaths)
    ) %>%
    ungroup()
  
  # Générer le graphique Plotly
  output$plot_eu <- renderPlotly({
    p <- plot_ly(eu_data, x = ~Last_Update)
    
    # Ajouter les cas confirmés si activés
    if (input$show_confirmed) {
      p <- p %>% add_lines(y = ~Total_Confirmed, name = "Cas confirmés", 
                           line = list(color = "#FF7F0E", width = 3))
    }
    
    # Ajouter les décès si activés
    if (input$show_deaths) {
      p <- p %>% add_lines(y = ~Total_Deaths, name = "Décès", yaxis = "y2",
                           line = list(color = "#1F77B4", width = 3))
    }
    
    # Mise en page avec deux axes Y
    p <- p %>% layout(
      title = "COVID-19 dans l'Union Européenne",
      xaxis = list(title = "Date"),
      yaxis = list(
        title = "Nombre de cas confirmés",
        rangemode = "tozero"
      ),
      yaxis2 = list(
        title = "Nombre de décès",
        overlaying = "y",
        side = "right",
        rangemode = "tozero",
        range = c(0, max(eu_data$Total_Deaths) * 5)
      ),
      legend = list(orientation = "h", x = 0.5, y = -0.2),
      plot_bgcolor = "#FFFFFF",
      paper_bgcolor = "#FFFFFF"
    )
    
    p
  })
}

# Lancer l'application
shinyApp(ui, server)