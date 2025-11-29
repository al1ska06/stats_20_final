library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("journal"),
                
                navbarPage(
                  "UCLA Division of Physical Sciences Major Statistics by Division",
                  
                  tabPanel("Tab 1",
                           fluidRow( #the container housing 3 columns for the primary app
                             
                             column(
                               width = 3,
                               selectInput( #this input is for department
                                 inputId = "dep_select",
                                 label = "Choose a department within the physical sciences:",
                                 choices = c("Atmospheric and Oceanic", "Chemistry/Biochemistry", "Mathematics", "Physics and Astronomy", "Environmental Science", "Earth, Planetary, and Space Sciences"),
                                 selected = "Atmospheric and Oceanic"
                               ),
                               selectInput( #this input is for major
                                 inputId = "major_select",
                                 label = "Choose a major:",
                                 choices = NULL #choices filled in server function
                               )),
                             
                             
                             column( #this column just has some major statistics bar chart
                               width = 6,
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
                             
                             
                             column( #this column has the progress tracker AND checklist
                               width = 3,
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
                  tabPanel("Tab 2", "maybe you guys have ideas to put smth here? not sure. kept it open as an option")
                ) 
) 


server <- function(input, output, session) {
  

  courses <- read.csv("big_total.csv") #editable link to big_total.csv is on the README

  dept_majors <- list(
    "Atmospheric and Oceanic" = unique(courses$major[courses$tab == "Atmospheric and Oceanic"]),
    "Chemistry/Biochemistry" =  unique(courses$major[courses$tab == "Chemistry/Biochemistry"]),
    "Mathematics" =  unique(courses$major[courses$tab == "Mathematics"]),
    "Physics and Astronomy" =  unique(courses$major[courses$tab == "Physics and Astronomy"]),
    "Environmental Science" =  unique(courses$major[courses$tab == "Environmental Science"])
  )
  
  observe({
    req(input$dep_select)
    majors <- dept_majors[[input$dep_select]]
    updateSelectInput(session, "major_select", choices = majors, selected = majors[1])
  })
  
  # Reactive subset of courses based on department and major
  selected <- reactive({
    req(input$dep_select, input$major_select)  # wait until both inputs are available
    subset(courses, 
           tab == input$dep_select & 
             major == input$major_select)
  })
  
  
  
  
  
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
    
    plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
         xlab = "", ylab = "", axes = FALSE, asp = 1)
    symbols(0, 0, circles = 1, inches = FALSE,
            add = TRUE, bg = "white", lwd = 3)
    
    # actual filled progress
    theta <- seq(pi/2, pi/2 - 2*pi*pct, length.out = 200)
    x <- c(0, cos(theta))
    y <- c(0, sin(theta))
    polygon(x, y, col = "skyblue", border = NA)
    text(0, 0, paste0(completed, "/", total_courses), cex = 2)
  })
  
  #checklist dynamically filled contents
  output$course_checklist <- renderUI({
    df <- selected()
    
    checkboxGroupInput(
      inputId = "completed_courses",
      label = "Check off completed courses:",
      choices = df$name,
      selected = NULL
    )
  })
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)