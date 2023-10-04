library(ggplot2)
library(dplyr)
library(ggforce)
library(lubridate)
library(shiny)

#--------Setting-Working-Directory---------------------------------------------------------------------------

#HomePC
workingdirectory = 'E:\\E01_College\\002_CurrentClasses\\03_MBA-694_TellingStoriesWithData\\Telling-Stories-Part-2-SubaruForesters-Dashboard\\data'


#Laptop
workingdirectory = 'C:\\Users\\Bangt\\01_College\\02_CurrentClasses\\Telling Stories\\TellingStoriesWithData\\Module2\\data'

setwd(workingdirectory)

#-------------Datasets-----------------

subi_forester = read.csv(paste(workingdirectory,"\\subaruForesters_Carbitrage.csv",sep=""))
names(subi_forester)


#-------------Filtering-----------------


# subi_forester = carbit_list %>%
#   filter(make == "subaru" & model == "forester" & odometer < 200000 & price < 40000) %>%
#   select(url, location, time_posted, make, model, year, odometer, title, paint, 
#          cylinders, fuel, type, transmission, condition, price, num_images, latitude, longitude)



#Renaming columns




#-------------Shiny-----------------





ui <- fluidPage(
  titlePanel("Interactive GGplot2 Example"),
  sidebarLayout(
    sidebarPanel(
      # Add Shiny widgets here (e.g., sliderInput, checkboxInput, selectInput)
      sliderInput("slider", "Select a value:", min = 0, max = 40000, value = c(0,40000)),
      # Dropdown menu for x-axis variable
      selectInput("xvar", "Select X-axis Variable:", choices = names(subi_forester)[c(7,15)], selected = names(subi_forester)[15]),
      
      # Dropdown menu for y-axis variable
      selectInput("yvar", "Select Y-axis Variable:", choices = names(subi_forester)[c(7,15)], selected = names(subi_forester)[7])
    ),
    mainPanel(
      plotOutput("plot")  # This is where the ggplot2 plot will be displayed
    )
  )
)





server <- function(input, output) {
  # Create a reactive object that depends on the slider input
  reactive_data <- reactive({
    data <- subi_forester[, c(input$xvar, input$yvar)]
    filtered_data <- data[data[, input$xvar] <= input$slider[2] & data[, input$xvar] >= input$slider[1], ]
    return(filtered_data)
  })
  
  # Create the ggplot2 plot based on the reactive data
  output$plot <- renderPlot({
    ggplot(reactive_data(), aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)























#  Nightmare----------

#  UI 
ui <- fluidPage(
  titlePanel("title panel"),
  
  
  
  tabsetPanel(
    tabPanel("Tab 1",
             h2("This is the content of Tab 1"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "y",
                   label = "Y-axis:",
                   choices = c("price"),
                   selected = "price"
                 ),
                 # Select variable for x-axis
                 selectInput(
                   inputId = "x",
                   label = "X-axis:",
                   choices = c("odometer"),
                   selected = "odometer"
                 ),
                 sliderInput("slider1", h3("Sliders"),
                             min = 0, max = 100, value = 50),
                 sliderInput("odometer_slider", "Select Odometer Value:",
                             min = 0, max = 200000, value = c(0, 200000), step =  1000)),
               
               mainPanel(
                 h1("First level title"),
                 plotOutput(outputId = "scatterplot")
               )
             )
    ),
    
    tabPanel("Tab 2",
             h2("This is the content of Tab 2"),
             # Add content for Tab 2 here
    ),
    
    tabPanel("Tab 3",
             h2("This is the content of Tab 3"),
             # Add content for Tab 3 here
    )
    
  )
)

server <- function(input, output) {
  
  
  reactive_data <- reactive({
    subset <- subi_forester %>%
      filter(odometer >= input$odometer_slider[1] & odometer <= input$odometer_slider[2])
    return(subset)
  })
  
  output$subi_forester_reactive <- renderDataTable({
    subi_forester_reactive()
  })
  
  
  output$scatterplot <- renderPlot({
    ggplot(data = subi_forester_reactive, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)


















