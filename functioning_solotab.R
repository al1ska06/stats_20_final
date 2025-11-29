library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage(
                  "UCLA Division of Physical Sciences Major Statistics by Division",
                  tabPanel("Atmospheric and Oceanic",
                           fluidRow(
                             
                             column(
                               width = 3,
                               selectInput(
                                 inputId = "major_select",
                                 label = "Choose a major:",
                                 choices = c("atmo", "climate", "atmo_math"),
                                 selected = "atmo"
                             )),
                             column(width = 6,
                                    plotOutput("major_barplot"),
                                    br(),
                                    radioButtons(
                                      inputId = "grouping",
                                      label = "See courseload by...",
                                      choices = c(
                                        "division" = "division",
                                        "department" = "department",
                                        "value" = "value"
                                      ),
                                      selected = "division"
                                    )),
                             column(
                               width = 3,
                               #title
                               h4("Personal progress tracker!"),
                               
                               # progress circle
                               div(
                                 style = "display: flex; justify-content: center; align-items: center; margin-bottom: 20px;",
                                 plotOutput("progress_circle", height = "250px", width = "250px")
                               ),
                               
                               #checklist
                               div(
                                 style = "height: 300px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px;",
                                 uiOutput("course_checklist")
                               )
                             ),
                          
                           )),
                  tabPanel("Chemistry/Biochemistry", "blank fucking panel"),
                  tabPanel("Earth, Planetary, Space Sciences", "This panel is intentionally left blank"),
                  tabPanel("Environmental Science", "This panel is intentionally left blank"),
                  tabPanel("Mathematics", "This panel is intentionally left blank"),
                  tabPanel("Physics and Astronomy", "This panel is intentionally left blank"),
                  tabPanel("Statistics/Data Science", "This panel is intentionally left blank"),
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  #code for selecting and subsetting a major
  courses <- read.csv("big_total.csv")
  selected <- reactive({ subset(courses, major == input$major_select)})
  
  #code for outputting the second column bar chart
  output$major_barplot <- renderPlot({
  
    df <- selected()
    
    group <- switch(
      input$grouping,
      "division"   = df$division,
      "department" = df$department,
      "value"      = df$value
    )
    
    counts <- table(group)
    
    barplot(
      counts,
      col = "steelblue",
      border = "white",
      main = paste("Courseload grouped by", input$grouping),
      xlab = paste(input$grouping),
      ylab = "Number of Courses"
    )
    
  })

  #code for outputting the progress circle in column 3
  output$progress_circle <- renderPlot({
    
    df <- selected()
    
    total_courses <- nrow(df)
    completed <- length(input$completed_courses)
    pct <- completed / total_courses #used to fill pct*area below
    
    #used to create padding for the circle
    plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
         xlab = "", ylab = "", axes = FALSE, asp = 1)
    symbols(0, 0, circles = 1, inches = FALSE,
            add = TRUE, bg = "white", lwd = 3)
    
    # actual progress
    theta <- seq(pi/2, pi/2 - 2*pi*pct, length.out = 200)
    x <- c(0, cos(theta))
    y <- c(0, sin(theta))
    polygon(x, y, col = "skyblue", border = NA)
    
    # text inside circle
    text(0, 0, paste0(completed, "/", total_courses), cex = 2)
  })
  
  
  output$course_checklist <- renderUI({
    df <- selected()
    
    checkboxGroupInput(
      inputId = "completed_courses",
      label = "Check off completed courses:",
      choices = df$name,
      selected = NULL
    )
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)