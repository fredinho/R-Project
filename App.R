################################################################################
# ANDERSON - CEDRICK - FRED
# David 
# 05/2020
#
################################################################################

################################################################################
# Library
################################################################################

install.packages("anyLib")
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", 
                 "ggplot2", "googleVis", "colourpicker"))

################################################################################
# UI : vue de la page shiny qui va etre afficher
################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Projet R - Deaths of Car Drivers in Great Britain",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem(" Data input ", tabName = "readData", 
                         icon = icon("dashboard")),
                menuItem(" Visualisation", tabName = "visualization", 
                         icon = icon("poll"), badgeColor = "green")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
               background-color: #f4b943;
        }
            /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #f4b943;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ff69b4;
                              }
                              '
        ))),
    
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Data input"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Laod...",
                        placeholder = "No file selected"),
              
              fluidRow(
                column(9,
                       h3("File preview"),
                       dataTableOutput(outputId = "preview")
                ),
                column(3,
                       h3("Parameters"),
                       
                       # Input: Checkbox if file has header
                       radioButtons(inputId = "header", 
                                    label = "Header",
                                    choices = c("Yes" = TRUE,
                                                "No" = FALSE),
                                    selected = TRUE, inline=T),
                       
                       # Input: Select separator ----
                       radioButtons(inputId = "sep", 
                                    label = "Separateur",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = ";", inline=T),
                       
                       # Input: Select quotes ----
                       radioButtons(inputId = "quote", 
                                    label= "Quote",
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Simple Quote" = "'"),
                                    selected = "", inline=T)
                )
              ), 
              tags$br(),
              
              div(actionButton(inputId = "actBtnVisualisation", label = "Displaying",icon = icon("play") ), align = "center")
              
              
              
      ),
      
      # visualization
      tabItem(tabName = "visualization",
              h1("driver death trends"),
              h2("Dashboard exploration"),
              dataTableOutput('dataTable'),
              h2("Graphiques"),
              fluidRow(
                column(12, plotOutput("plotAvecGgplot2")),
                column(6, plotlyOutput("plotAvecPlotly")),
                column(6, htmlOutput("plotAvecGoogle"))
              ),
              tags$br(),
              fluidRow(
                column(4, plotOutput("plotAvecR")),
                column(4, colourpicker::colourInput("colR", "Couleur graphique R", "#31245",allowTransparent = T),
                       sliderInput("cex", "Taille",
                                   min = 0.5, max = 3,
                                   value = 1,step = 0.2
                       )),
                column(4, selectInput(inputId = "pch", choices = 1:20, label = "Type de points",selected = 1),
                       textInput("title", "Titre", "Long Sepal vs Large Petal (R)") )
              ),
              tags$br(), 
      )
    )
  )
)

################################################################################
# Server
################################################################################

server <- function(input, output, session) {
  
  data <- reactiveValues()
  
  #=============================================================================
  # Preview
  #=============================================================================
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=10
    )
  },  options = list(scrollX = TRUE , dom = 't'))
  
  #=============================================================================
  # Lecture
  #=============================================================================
  observeEvent(input$actBtnVisualisation, {
    
    if(!is.null(input$dataFile$datapath)){
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote)
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Reading the file went well",
        type = "PERFECTLY READ FILE"
      )
      
      updateTabItems(session, "tabs", selected = "visualization")
    }
    
  })
  
  #=============================================================================
  # Exploration du tableau
  #=============================================================================
  
  output$dataTable = DT::renderDataTable({
    if(!is.null(data$table)){
      datatable(data$table, filter = 'top') %>% 
        formatStyle('Index', 
                    background = styleColorBar(data$table$Index, 'Index'),
                    #backgroundSize = '100% 90%',
                    #backgroundRepeat = 'no-repeat',
                    #backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Date',
          background = styleColorBar(range(data$table$Date), 'Date'),
          #backgroundSize = '100% 90%',
          #backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Number_Deaths',
          background = styleColorBar(data$table$Number_Deaths, 'Number_Deaths'),
          backgroundSize = '95% 50%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        
        )
    }else {
      NULL
    }
  })
  
  #=============================================================================
  # Graphiques
  #=============================================================================
  # dataTable_tab_rows_selected
  output$plotAvecR <- renderPlot({
    if(!is.null(data$table) && !is.null(input$dataTable_rows_all)){
      plot(data$table$Number_Deaths[input$dataTable_rows_all],
           data$table$Number_Deaths[input$dataTable_rows_all], 
           main = input$Index,
           ylab = "Longueur Sepal",
           xlab = "Longueur Petal",
           pch = as.numeric(input$Date),
           col = input$colR, 
           cex = input$cex)
    }else {
      NULL
    }
  })
  
  output$plotAvecGgplot2 <- renderPlot({
    if(!is.null(data$table)){
      ggplot(data=data$table[input$dataTable_rows_all,], aes(x = Date, y = Number_Deaths)) + 
        geom_line(aes(color=Number_Deaths)) +
        xlab("Index") +  ylab("Number_Deaths") +
        ggtitle("Evolution of mortalities (ggplot2)")
    }else {
      NULL
    }
  })
  
  output$plotAvecPlotly <- renderPlotly({
    if(!is.null(data$table)){
      plot_ly(data = data$table[input$dataTable_rows_all,], x = ~Date, y = ~Number_Deaths)%>%
        layout(title = 'Petal Long-Large (plotly)',
               yaxis = list(title = "Largeur Petal"),
               xaxis = list(title = "Longueur Petal"))
    }else {
      NULL
    }
    
  })
  
  output$plotAvecGoogle <- renderGvis({
    if(!is.null(data$table)){
      gvisLineChart(as.data.frame(data$table$Number_Deaths[data$table$Date]),
                    options=list(title ="Largeur Petal (Google)")
      )
    }else {
      NULL
    }
    
  })
  
}

shinyApp(ui, server)
