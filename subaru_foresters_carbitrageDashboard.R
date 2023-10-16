#If you haven't installed required packages (Highlight all and press Ctrl+Shift+C ):
# install.packages("ggplot2")
# install.packages("ggforce")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("shinythemes")

#---------Libraries---------------

library(ggplot2)
library(ggforce)
library(shiny)
library(leaflet)
library(shinythemes)




#--------Setting-Working-Directory---------------------------------------------------------------------------

#HomePC
#workingdirectory = 'E:\\E01_College\\002_CurrentClasses\\03_MBA-694_TellingStoriesWithData\\Telling-Stories-Part-2-SubaruForesters-Dashboard\\data'


#Laptop
#workingdirectory = 'C:\\Users\\Bangt\\01_College\\02_CurrentClasses\\Telling Stories\\TellingStoriesWithData\\Module2\\data'

workingdirectory = "data"
setwd(workingdirectory)

#-------------Datasets-----------------

#subi_forester = read.csv(paste(workingdirectory,"\\subaruForesters_Carbitrage.csv",sep=""))

#with distances
#subi_forester = read.csv(paste(workingdirectory,"\\subiforester2.csv",sep=""))

#Script
subi_forester = read.csv("subiforester3.csv")
#color_palette <- c("green" = "green", "silver" = "#adadad", "black" ="black",  "white"="white",  "white" = "blue" ,  "red" = "red"  ,  "grey"= "grey" ,    "brown"=  "brown" , "custom"= "pink", "yellow"=  "yellow","orange"= "orange","purple"="purple")
#-------------Shiny-----------------
      

#----------User-Interface--------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Carbitrage Subaru Forester Finder", "font-size: 50px; font-weight: bold;"),
  tags$style(HTML(
    ".custom-label-popup {
        font-size: 15px; /* Adjust the font size as needed for default state */
    }
    .custom-label-popup:hover {
        font-size: 16px; /* Adjust the font size as needed for hover state */
    }"
  )),
  tags$head(tags$style(".rightAlign{float:right;}")),
  headerPanel(
    img(src = "subaru_logo.png", height = "66px", width = "99px", class = 'rightAlign'), 
  ),
  theme = shinytheme("cosmo"),
  tabsetPanel(
    
    tabPanel("Scatter Plot",
             
             
             
             sidebarLayout(
               sidebarPanel(
                 #ALL FILTERS
                 #Sliders for Numeric Data
                 width = 3,
                 style = "overflow-y:scroll; max-height: 100%; position:relative;",
                 sliderInput("price_slider", "Price Range:", min = 0, max = 40000, value = c(0, 40000), step = 1000, ticks = FALSE),
                 sliderInput("miles_slider", "Distance Away from UMT (In Miles):", min = 0, max = 2800, value = c(0, 2800),step = 100, ticks = FALSE),
                 sliderInput("odometer_slider", "Select an Odometer Range:", min = 0, max = 200000, value = c(0, 200000),step = 5000, ticks = FALSE),
                 
                 #Selection for X and Y Axis
                 selectInput("xvar", "Select X-axis Variable:", choices = names(subi_forester)[c(7, 15,20)], selected = names(subi_forester)[7]),
                 selectInput("yvar", "Select Y-axis Variable:", choices = names(subi_forester)[c(7, 15,20)], selected = names(subi_forester)[15]), 
                 
                 
                 #Selections for Categorical Data
                 
                 #Column 1++++++++
                 column(width = 6,
                   selectInput("Year", "Year:", 
                                      choices = c("Any", 2023:1997),
                                      selected = "Any"
                   ),
                   
                   selectInput("Title", "Title Standing:", 
                               choices = c("Any", unique(subi_forester$Title)),
                               selected = "Any"
                   ),
                   
                   selectInput("Condition", "Condition:", 
                               choices = c("Any","new","like new", "excellent", "good", "fair","salvage"),
                               selected = "Any"
                   ),
                   
                   checkboxGroupInput("xvariableStats", "X-Axis:",
                                      choices = c("Mean", "Median")),
                   
                   actionButton("resetButton", "Reset Inputs"),
                   
                   
                 ),
                 
                 #Column 2++++++++++++++
                 column(width = 6,
                   selectInput("Cylinder", "Cylinder #:", 
                               choices = c("Any","8 cylinders","6 cylinders", "4 cylinders", "other"),
                               selected = "Any"
                   ),
                   
                   
                   selectInput("Transmission", "Transmission Type:", 
                               choices = c("Any",unique(subi_forester$Transmission)),
                               selected = "Any"
                   ),
                   
                   selectInput("Paint", "Paint Color:", 
                               choices = c("Any",unique(subi_forester$Paint)),
                               selected = "Any"
                   ),
                   
                   checkboxGroupInput("yvariableStats", "Y-Axis:",
                                      choices = c("Mean", "Median")),
                  ),
                   #Check Boxes for Mean and Medians for X and Y Axis 
                   
                 
                 
                 
                 
               ),
               
               
               
               
               mainPanel(
                   fluidRow(
                     column(12,
                            h3("Find Your Subaru Forester"),
                            p("Navigate the Filters to the Left and Locate Your Car Within Table Below"),
                            hr()  # Optional horizontal line for visual separation
                     )
                   ),
                 plotOutput("plot", width = "100%", height = "700px"),
                 
                 tableOutput("table") 
               )
             )
    ),
    
    
    #SECOND TAB ---------------------------------------------------------------------------------
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 
                 #ALL FILTERS
                 #Sliders for Numeric Data
                 width = 3,
                 style = "overflow-y:scroll; max-height: 100%; position:relative;",
                 sliderInput("price_slider_map", "Price Range:", min = 0, max = 40000, value = c(0, 40000), step = 1000, ticks = FALSE),
                 sliderInput("miles_slider_map", "Distance Away from UMT (In Miles):", min = 0, max = 2800, value = c(0, 2800),step = 100, ticks = FALSE),
                 sliderInput("odometer_slider_map", "Select an Odometer Range:", min = 0, max = 200000, value = c(0, 200000),step = 5000, ticks = FALSE),
                 
                 
                 #Selections for Categorical Data
                 
                 #Column 1++++++++
                 column(width = 6,
                        selectInput("Year_map", "Year:", 
                                    choices = c("Any", 2023:1997),
                                    selected = "Any"
                        ),
                        
                        selectInput("Title_map", "Title Standing:", 
                                    choices = c("Any", unique(subi_forester$Title)),
                                    selected = "Any"
                        ),
                        
                        selectInput("Condition_map", "Condition:", 
                                    choices = c("Any","new","like new", "excellent", "good", "fair","salvage"),
                                    selected = "Any"
                        ),
                        
                        
                        actionButton("resetButton2", "Reset Inputs"),
                        
                        
                 ),
                 
                 #Column 2++++++++++++++
                 column(width = 6,
                        selectInput("Cylinder_map", "Cylinder #:", 
                                    choices = c("Any","8 cylinders","6 cylinders", "4 cylinders", "other"),
                                    selected = "Any"
                        ),
                        
                        
                        selectInput("Transmission_map", "Transmission Type:", 
                                    choices = c("Any",unique(subi_forester$Transmission)),
                                    selected = "Any"
                        ),
                        
                        selectInput("Paint_map", "Paint Color:", 
                                    choices = c("Any",unique(subi_forester$Paint)),
                                    selected = "Any"
                        ),
                        
                 ),
                 #Check Boxes for Mean and Medians for X and Y Axis 
                 
                 
                 imageOutput("subaru_Logo"),
                 
                 
                 
                 
               ),
      
                 mainPanel(
                   fluidRow(
                     column(12,
                            h3("Subaru Forester Map"),
                            p("Click on the blue circle, representing Subaru Foresters, for their URL."),
                            hr()  # Optional horizontal line for visual separation
                     )
                   ),
                 leafletOutput("map", width = "100%", height = "700px"),
                 uiOutput("marker_info")
                 )
               
      )
    )
  ),
)


#=========================================================================================================================
#----------Server---------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  
  observeEvent(input$resetButton, {
    # Reset other input values to their defaults
    updateSliderInput(session, "price_slider", value = c(0, 40000))#PRICE
    updateSliderInput(session, "miles_slider", value = c(1, 2800))#MILES
    updateSliderInput(session, "odometer_slider", value = c(0, 200000))#ODOMETER
    updateSelectInput(session, "xvar", selected = names(subi_forester)[7])#X-AXIS
    updateSelectInput(session, "yvar", selected = names(subi_forester)[15])#Y-AXIS
    updateSelectInput(session, "Year", selected = "Any")#YEAR
    updateSelectInput(session, "Title", selected = "Any")#TITLE
    updateSelectInput(session, "Condition", selected = "Any")#CONDITION
    updateSelectInput(session, "Cylinder", selected = "Any")#CYLINDER
    updateSelectInput(session, "Transmission", selected = "Any")#TRANSMISSION
    updateSelectInput(session, "Paint", selected = "Any")#PAINTCOLOR
    #CHECKBOXES
    updateCheckboxGroupInput(session, "xvariableStats", selected = character(0))
    updateCheckboxGroupInput(session, "yvariableStats", selected = character(0))
  })
  
  observeEvent(input$resetButton2, {
    # Reset other input values to their defaults
    updateSliderInput(session, "price_slider_map", value = c(0, 40000))#PRICE
    updateSliderInput(session, "miles_slider_map", value = c(0, 2800))#MILES
    updateSliderInput(session, "odometer_slider_map", value = c(0, 200000))#ODOMETER
    updateSelectInput(session, "Year_map", selected = "Any")#YEAR
    updateSelectInput(session, "Title_map", selected = "Any")#TITLE
    updateSelectInput(session, "Condition_map", selected = "Any")#CONDITION
    updateSelectInput(session, "Cylinder_map", selected = "Any")#CYLINDER
    updateSelectInput(session, "Transmission_map", selected = "Any")#TRANSMISSION
    updateSelectInput(session, "Paint_map", selected = "Any")#PAINTCOLOR
  })
  
  # Create a reactive object that depends on the slider input
  reactive_data <- reactive({
    data <- subi_forester[, c("URL","Price","Odometer", "Miles_Away","Condition","Year","Title","Cylinders","Transmission","Paint","longitude","latitude" )]
    filtered_data <- data[
      data$Odometer <= input$odometer_slider[2] & data$Odometer >= input$odometer_slider[1] &
      data$Miles_Away <= input$miles_slider[2] & data$Miles_Away >= input$miles_slider[1] &
      data$Price <= input$price_slider[2] & data$Price >= input$price_slider[1], ]
    
    filtered_data <- filtered_data[complete.cases(filtered_data), ]
    
    #Filtering Year
    if (input$Year != "Any") {
      filtered_data <- filtered_data[filtered_data$Year == as.numeric(input$Year), ]
    }
    
    #Filtering Title
    if (input$Title != "Any") {
      filtered_data <- filtered_data[filtered_data$Title == input$Title, ]
    }
    
    
    #Filtering Condition
    if (input$Condition != "Any") {
      filtered_data <- filtered_data[filtered_data$Condition == input$Condition, ]
    }
    
    #Filtering Cylinder
    if (input$Cylinder != "Any") {
      filtered_data <- filtered_data[filtered_data$Cylinders == input$Cylinder, ]
    }
    
    #Filtering Transmission
    if (input$Transmission != "Any") {
      filtered_data <- filtered_data[filtered_data$Transmission == input$Transmission, ]
    }
    
    #Filtering Paint
    if (input$Paint != "Any") {
      filtered_data <- filtered_data[filtered_data$Paint == input$Paint, ]
    }
    
    return(filtered_data)
  })
  
  #
  reactive_data_map <- reactive({
    data <- subi_forester[, c("URL","Price","Odometer", "Miles_Away","Condition","Year","Title","Cylinders","Transmission","Paint","longitude","latitude" )]
    filtered_data <- data[
      data$Odometer <= input$odometer_slider_map[2] & data$Odometer >= input$odometer_slider_map[1] &
        data$Miles_Away <= input$miles_slider_map[2] & data$Miles_Away >= input$miles_slider_map[1] &
        data$Price <= input$price_slider_map[2] & data$Price >= input$price_slider_map[1], ]
    
    filtered_data <- filtered_data[complete.cases(filtered_data), ]
    
    # Filtering Year
    if (input$Year_map != "Any") {
      filtered_data <- filtered_data[filtered_data$Year == as.numeric(input$Year_map), ]
    }
    
    # Filtering Title
    if (input$Title_map != "Any") {
      filtered_data <- filtered_data[filtered_data$Title == input$Title_map, ]
    }
    
    # Filtering Condition
    if (input$Condition_map != "Any") {
      filtered_data <- filtered_data[filtered_data$Condition == input$Condition_map, ]
    }
    
    # Filtering Cylinder
    if (input$Cylinder_map != "Any") {
      filtered_data <- filtered_data[filtered_data$Cylinders == input$Cylinder_map, ]
    }
    
    # Filtering Transmission
    if (input$Transmission_map != "Any") {
      filtered_data <- filtered_data[filtered_data$Transmission == input$Transmission_map, ]
    }
    
    # Filtering Paint
    if (input$Paint_map != "Any") {
      filtered_data <- filtered_data[filtered_data$Paint == input$Paint_map, ]
    }
    
    return(filtered_data)
  })
  
  # Create the ggplot2 plot based on the reactive data
  output$plot <- renderPlot({
    
    p = ggplot(reactive_data(), aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(size = 2) + 
      theme_minimal()  + 
      theme(plot.title = element_text(lineheight=1, face="bold", size = 15),
            text = element_text(size = 24),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            plot.margin = margin(0, 10, 0, 0.2, "cm"),
            axis.title = element_text(size = 25)) 
    
    
    #X-Axis
    if ("Mean" %in% input$xvariableStats) {
      p <- p + geom_vline(aes(xintercept = mean(reactive_data()[[input$xvar]])), color = "#4c6ffc", linetype = "solid", size = 1.8)
    }
    
    if ("Median" %in% input$xvariableStats) {
      p <- p + geom_vline(aes(xintercept = median(reactive_data()[[input$xvar]])), color = "#ffc53d", linetype = "solid", size = 1.8)
    }
    
    # Y-Axis
    if ("Mean" %in% input$yvariableStats) {
      p <- p + geom_hline(aes(yintercept = mean(reactive_data()[[input$yvar]])), color = "#123759", linetype = "solid", size = 1.8)
    }
    
    if ("Median" %in% input$yvariableStats) {
      p <- p + geom_hline(aes(yintercept = median(reactive_data()[[input$yvar]])), color = "#3b0100", linetype = "solid", size = 1.8)
    }

   
    
    p
  })
  
  # Create a table to display the data points
  output$table <- renderTable({
    reactive_data()
  })
  
  marker_info <- reactiveVal()
  
  
  observe({
    filtered_data <- reactive_data_map()
    
    # Filter the data based on selected Year, Title, Condition, etc.
    if (input$Year != "Any") {
      filtered_data <- filtered_data[filtered_data$Year == as.numeric(input$Year), ]
    }
    
    if (input$Title != "Any") {
      filtered_data <- filtered_data[filtered_data$Title == input$Title, ]
    }
    
    if (input$Condition != "Any") {
      filtered_data <- filtered_data[filtered_data$Condition == input$Condition, ]
    }
    
    # Update the map with the filtered data
    mapProxy %>%
      clearMarkers() %>%  # Clear existing markers
      addCircleMarkers(data = reactive_data_map(), ~longitude, ~latitude,
                       label = ~paste("Price: $", Price, ", Odometer: ", Odometer, ", Year: ", Year, sep = ""),
                       popup = ~URL,
                       labelOptions = labelOptions(className = "custom-label-popup"),
                       popupOptions = popupOptions(className = "custom-label-popup")
      )
    
      
    
    
  
})
  
  mapProxy = leafletProxy("map")
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = reactive_data_map(),
                 ~longitude,
                 ~latitude,
                 label = ~paste("Price: $", Price,
                                ", Odometer: ", Odometer,
                                ", Year: ", Year,
                                ", Condition: ", Condition,
                                ", Transmission: ", Transmission,
                                ", Cylinders: ", Cylinders,
                                ", Paint: ", Paint),
                 popup = ~URL,
                 labelOptions = labelOptions(textsize = "12px" , className = "custom-label-popup:hover"),
                 popupOptions = popupOptions(className = "custom-label-popup")
                 ) %>%
      setView(lng = -99.8043, lat = 40.6001, zoom = 4)
  })
  
  
  
}


shinyApp(ui = ui, server = server)

