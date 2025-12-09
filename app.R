library(shiny)
library(shinythemes)
library(highcharter)
library(RColorBrewer)  

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
    
                navbarPage(
                  "UCLA Division of Physical Sciences Major Statistics by Division",
                  
                  tabPanel("Course Statistics",
                           fluidRow( #the container housing 3 columns for the primary app
                             
                             column(
                               width = 3,
                               h4("How to Use This Dashboard"),
                               helpText("1. Select a department to load the majors offered."),
                               helpText("2. Choose a major to see course requirements."),
                               helpText("3. View the bar chart in the middle to explore course distribution."),
                               helpText("4. Track your progress using the checklist and progress circle."),
                               helpText("5. View lower-division prerequisites for a selected major by switching to the Prereq Pathways tab!"),
                               br(),
                               
                               
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
                               )
                          
                               
                               ),
                             
                             
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
                               ), 
                               
                               #missing courses UI
                               div(
                                 style = "height: 180px; overflow-y: scroll; border: 1px solid #dcdcdc; border-radius: 6px; padding: 10px; background-color: #fdfdfd;",
                                 h4("Unmet Requirements"),
                                 uiOutput("missing_courses")
                               )
                             )

                          
                           )
                
                           ), 
                  
                  tabPanel("Prereq Pathways",
                           fluidRow(
                             column(
                               width = 12,
                               h3("Prerequisite Pathway Visualization"),
                               highchartOutput("pathway_sankey", height = "350px")
                             )
                           )
                  )
                  
                  
                  
                  ),
            
                ) 


server <- function(input, output, session) {
  

  courses <- read.csv("big_total.csv") #editable link to big_total.csv is on the README

  dept_majors <- list(
    "Atmospheric and Oceanic" = unique(courses$major[courses$tab == "Atmospheric and Oceanic"]),
    "Chemistry/Biochemistry" =  unique(courses$major[courses$tab == "Chemistry/Biochemistry"]),
    "Mathematics" =  unique(courses$major[courses$tab == "Mathematics"]),
    "Physics and Astronomy" =  unique(courses$major[courses$tab == "Physics and Astronomy"]),
    "Environmental Science" =  unique(courses$major[courses$tab == "Environmental Science"]),
    "Earth, Planetary, and Space Sciences" =  unique(courses$major[courses$tab == "Earth, Planetary, and Space Sciences"])
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
      col = brewer.pal(12, "Set3"),
      border = "white",
      main = paste("Courseload grouped by", input$grouping),
      xlab = paste(input$grouping),
      ylab = "Number of Courses"
    )
  
    #code for outputting summary statistics
    output$summary_stats <- renderPrint({
      
      df <- selected()
      
      group <- switch(
        input$grouping,
        "division"   = df$division,
        "department" = df$department,
        "value"      = df$value
      )
      
      counts <- table(group)
      
      summary_list <- list(
        "Selected Grouping" = input$grouping,
        "Number of Categories" = length(counts),
        "Largest Category" = names(which.max(counts)),
        "Largest Category Count" = max(counts),
        "Smallest Category" = names(which.min(counts)),
        "Smallest Category Count" = min(counts)
      )
      
      print(summary_list)
    })
   
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
    polygon(x, y, col = brewer.pal(12, "Set3"), border = NA)
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

  #missing courses output
  output$missing_courses <- renderText({
    df <- selected()
    completed <- input$completed_courses
    missing <- setdiff(df$name, completed)
    
    if(length(missing) == 0){
      "All courses completed!"
    } else {
      paste("-", missing, collapse = "\n")
    }
  })
  
  # PRE-REQ diagram for the next tab

  output$pathway_sankey <- renderHighchart({
    df <- selected()
    
    # rows that contain prereqs
    has_prereqs <- !is.na(df$prereqs) & df$prereqs != ""
    
    if (sum(has_prereqs) == 0) {
      return(
        highchart() %>%
          hc_title(text = "No prerequisites listed for this major.")
      )
    }
    
    # Build prereq → course connections
    sankey_list <- list()
    for (i in which(has_prereqs)) {
      prereq_courses <- trimws(strsplit(df$prereqs[i], ",")[[1]])
      
      for (p in prereq_courses) {
        sankey_list[[length(sankey_list) + 1]] <- data.frame(
          from = p,
          to = df$name[i],
          weight = 1,
          stringsAsFactors = FALSE
        )
      }
    }
    
    sankey_data <- do.call(rbind, sankey_list)
    sankey_data <- unique(sankey_data)
    
    hchart(sankey_data, "sankey",
           hcaes(from = from, to = to, weight = weight)) %>%
      hc_title(text = paste("Prerequisite Pathways for", input$major_select)) %>%
      hc_colors(brewer.pal(12, "Set3")) %>%
      hc_plotOptions(
        sankey = list(
          dataLabels = list(enabled = TRUE, style = list(fontSize = "11px"))
        )
      )
  })
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)