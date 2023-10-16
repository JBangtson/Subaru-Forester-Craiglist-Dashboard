library(ggplot2)
library(dplyr)
library(ggforce)
library(shiny)
library(ggmap)
library(googleway)

library(geodist)

#--------Setting-Working-Directory---------------------------------------------------------------------------

#HomePC
workingdirectory = 'E:\\E01_College\\002_CurrentClasses\\03_MBA-694_TellingStoriesWithData\\Telling-Stories-Part-2-SubaruForesters-Dashboard\\data'


#Laptop
workingdirectory = 'C:\\Users\\Bangt\\01_College\\02_CurrentClasses\\Telling Stories\\TellingStoriesWithData\\Module2\\data'

setwd(workingdirectory)

#-------------Datasets-----------------

subi_forester = read.csv(paste(workingdirectory,"\\subaruForesters_Carbitrage.csv",sep=""))

#with distances
subi_forester = read.csv(paste(workingdirectory,"\\subiforester2.csv",sep=""))



#-------------Filtering-----------------


# subi_forester = carbit_list %>%
#   filter(make == "subaru" & model == "forester" & odometer < 200000 & price < 40000) %>%
#   select(url, location, time_posted, make, model, year, odometer, title, paint, 
#          cylinders, fuel, type, transmission, condition, price, num_images, latitude, longitude)

subi_forester = subi_forester %>%
  filter(latitude > 0 & latitude < 180 & longitude < 0 & longitude > -180) %>%
  select(url, location, time_posted, make, model, year, odometer, title, paint, 
                   cylinders, fuel, type, transmission, condition, price, num_images, latitude, longitude)

#Google Routes API - Creating Distance Column------------

apiKey <- "AIzaSyC_Aj4oQDeNKC4M-1fG2tuSIKJkFOFl6Us"
register_google(key = apiKey)

set_key(apiKey)

destination1 <- c(45.70750,-112.22912)

#Setting to University of Montana, although this can be optimized to give the user the choice.
missoula <- c(46.862538,-113.987860)


# Get directions using google_directions
route <- google_directions(origin = missoula, destination = c(subi_forester$latitude[2], subi_forester$longitude[2]), mode = "driving", key = apiKey)$routes$legs[[1]]$distance$value

route = route$routes$legs[[1]]$distance$value

route2 = route[[1]]
route2 = route2$distance$value

#Dont want to delete, but isn't working
subi_forester <- subi_forester %>%
  rowwise() %>%
  mutate(
    distance = google_directions(origin = missoula, destination = c(latitude, longitude), mode = "driving", key = apiKey)$routes$legs[[1]]$distance$value

  )

distances <- vector("numeric", length = nrow(subi_forester))

# Calculate distances using Google Directions API and store them in the vector
for (i in 1:nrow(subi_forester)) {
  origin <- missoula
  destination <- data.frame(lon = subi_forester$longitude[i], lat = subi_forester$latitude[i])
  
  tryCatch(
    {
      route <- google_directions(origin = c(46.862538,-113.987860), destination = c(subi_forester$latitude[i], subi_forester$longitude[i]), mode = "driving", key = apiKey)
      distances[i] <- route$routes$legs[[1]]$distance$value
    },
    error = function(e) {
      # Handle errors, for example, by setting a special value or NA for distances
      distances[i] <- NA
    }
  )
}

subi_forester2 <- subi_forester %>%
  mutate(distance = distances)

write.csv(subi_forester2, file = "subiforester2.csv", row.names = FALSE)

#-------------Shiny-----------------





ui <- fluidPage(
  titlePanel("Interactive GGplot2 Example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Select a value:", min = 0, max = 40000, value = c(0, 40000)),
      selectInput("xvar", "Select X-axis Variable:", choices = names(subi_forester)[c(7, 15)], selected = names(subi_forester)[15]),
      selectInput("yvar", "Select Y-axis Variable:", choices = names(subi_forester)[c(7, 15)], selected = names(subi_forester)[7])
    ),
    mainPanel(
      plotOutput("plot"), # This is where the ggplot2 plot will be displayed
    
      tableOutput("table") 
    )
  )
)





server <- function(input, output) {
  # Create a reactive object that depends on the slider input
  reactive_data <- reactive({
    data <- subi_forester[, c(input$xvar, input$yvar, "url")]
    filtered_data <- data[data[, input$xvar] <= input$slider[2] & data[, input$xvar] >= input$slider[1], ]
    return(filtered_data)
  })
  
  # Create the ggplot2 plot based on the reactive data
  output$plot <- renderPlot({
    ggplot(reactive_data(), aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point()
  })
  
  # Create a table to display the data points
  output$table <- renderTable({
    reactive_data()
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


















