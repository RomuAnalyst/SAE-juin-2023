library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(openxlsx)

# Données d'exemple
data <- read.xlsx("bdd_compagnies.xlsx")

data$Revenue <- as.numeric(data$Revenue)
data$Profits <- as.numeric(data$Profits)
data$Assets <- as.numeric(data$Assets)
data$Market.Value <- as.numeric(data$Market.Value)
data$Total.Employees <- as.numeric(data$Total.Employees)
data$Year.Founded <- round(data$Year.Founded)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "SAE VCOD - juin 2023"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Le chiffre...🔟", tabName = "page1", icon = icon("dashboard"),badgeLabel = "fixe", badgeColor = "green"),
      menuItem("Page 2", tabName = "page2",badgeLabel = "dynamique", badgeColor = "purple"),
      menuItem("Page 3", tabName = "page3")
      #,
      # menuItem("Source code", icon = icon("file"),
      #       href = "/Users/romuzami/Desktop/ezfze/app.R")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 16px;
      }
    '))),
    tabItems(
      # Page 1
      tabItem(
        tabName = "page1",
        fluidRow(
          column(width = 4,
                 box(width = 100,
                  title = "Top Industries",
                  status = "success",
                  solidHeader = TRUE,
                 tableOutput("top_industries"),
                 plotlyOutput("graph_industries")
          )),
          column(width = 4,
                 box(width = 100,
                 tableOutput("top_countries"),
                 plotlyOutput("graph_countries")
          )),
        fluidRow(
          column(width = 4,
                 box(width = 100,
                 title = "Top...",
                 status = "success",
                 tableOutput("top_year"),
                 plotlyOutput("graph_year")
          ))
        )
      )),
      
      # Page 2
      tabItem(
        tabName = "page2",
        fluidRow(
          column(width = 4,
                 selectInput("industry", "Industry", choices = unique(data$Industry))
          ),
          column(width = 8,
                 box(
                   title = "Informations sur les organisations",
                   status = "primary",
                   solidHeader = TRUE,
                   tableOutput("org_info")
                 )
          )
        )),

      # Page 3
      tabItem(
        tabName = "page3",
        fluidRow(
          column(width = 12,
                 box(title = "Contenu de la page 3")
          )
        )
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  # Page 1
  output$top_industries <- renderTable({
    data %>% group_by(Industry) %>% count(sort = TRUE) %>% head(10)
  })
  
  output$graph_industries <- renderPlotly({
    data %>%
      count(Industry) %>%
      plot_ly(x = ~Industry, y = ~n, type = "bar")
  })
  
  output$top_countries <- renderTable({
    data %>% group_by(Country) %>% count(sort = TRUE) %>% head(10)
  })
  
  output$graph_countries <- renderPlotly({
    data %>%
      count(Country) %>%
      plot_ly(locations = ~Country, locationmode = "country names", z = ~n, type = "choropleth")
  })
  
  output$top_year <- renderTable({
    data %>% select(Year.Founded) %>% group_by(Year.Founded) %>% arrange(desc(Year.Founded)) %>% head()
  })
  
  output$graph_year <- renderPlotly({
    data %>% filter(between(Year.Founded,2012, 2022)) %>% 
      count(Year.Founded) %>%
      plot_ly(x = ~Year.Founded, y = ~n, type = "scatter", mode = "lines",symbol = "pentagon")
  })
  
  # Page 2
  # Obtenir les informations sur les organisations de l'industry sélectionnée
  org_info <- reactive({
    selected_data <- data %>%
      filter(Industry == input$industry)
    
    total_revenue <- sum(selected_data$Revenue)
    
    selected_data <- selected_data %>%
      mutate(Revenue_Contribution = Revenue / total_revenue * 100) %>% 
      arrange(desc(Revenue_Contribution)) %>% 
      head(10)
    
    selected_data
  })
  
  # Afficher les informations sur les organisations de l'industry sélectionnée
  output$org_info <- renderTable({
    org <- org_info()
    
    if (nrow(org) == 0) {
      return(NULL)
    }
    
    org[, c("Organization.Name", "Year.Founded", "Revenue_Contribution")]
  })
  
  #output$world_map <- renderPlotly({
    #data %>%
      #group_by(Country) %>%
     # summarise(Volume = sum(Industry)) %>%
     # plot_ly(locations = ~Country, locationmode = "country names", z = ~Volume, type = "choropleth")
}

# Exécution de l'application
shinyApp(ui, server)

