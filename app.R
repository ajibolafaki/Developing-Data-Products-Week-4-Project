#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(tidyverse)
library(leaflet)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Mtcars Data"),
   
   # Sidebar with a dropdown to select vehicle attribute 
   sidebarLayout(
      sidebarPanel(
         selectInput("attribute",
                     label = "Select Vehicle Attribute:",
                     choices = colnames(mtcars)),
         
         selectInput("model",
                     label = "Select Vehicle Model:",
                     choices = rownames(mtcars))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("vehiclePlot"),
         leafletOutput("originMap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$vehiclePlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
   vplot<-   barplot(mtcars[ ,input$attribute],
              main = input$attribute,
              xlab = ""
               )
   text(vplot, par("usr")[3], labels = rownames(mtcars), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)
     
     
   })
   
   m <- leaflet() %>% 
          addTiles()  
   
   output$originMap <- renderLeaflet({
     
    if (word(input$model,1) %in% c("Duster","Hornet","Cadillac","Lincoln","Chrysler",
                                   "Dodge","AMC","Camaro","Pontiac","Ford")){
       m %>% flyTo(lng=-95.712891, lat=37.09024 , zoom=4.2) %>% 
        addMarkers(lng = c(-83.057222,-83.045833,-83.045833,-83.213333,-83.234167,-83.234167,-83.234167,-84.561667,
                           -83.045833,-83.213333), 
                   lat = c(42.397778,42.331389,42.331389,42.314444,42.6875,42.6875,42.6875,42.722778,42.331389,42.314444), 
                   popup =  c(
                     "<a href = 'https://en.wikipedia.org/wiki/Plymouth_Duster'>Duster360 (see: Chrysler) Hamtrack, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Hudson_Hornet'>Hornet, Detroit, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Cadillac'>Cadillac (General Motors), Detroit, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Lincoln_Motor_Company'>Lincoln (see: Ford), Dearborn, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Chrysler'>Chrysler, Auburn Hills, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Dodge'>Dodge (see: Chrysler), Auburn Hills, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/American_Motors_Corporation'>AMC (see: Chrysler), Auburn Hills, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Chevrolet_Camaro'>Camaro, (General Motors) Lansing, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Pontiac'>Pontiac (General Motors) Detroit, Michigan</a>",
                     "<a href = 'https://en.wikipedia.org/wiki/Ford_Motor_Company'>Ford Dearborn, Michigan</a>"))
     } else if (word(input$model,1) %in% c("Mazda","Datsun","Honda","Toyota")){
       m %>% flyTo(lng=138.252924, lat=36.204824 , zoom=4.5) %>% 
         addMarkers(lng = c(132.5574,139.683333,139.7516,137.156333), 
                    lat = c(34.3532,35.683333,35.658069,35.082444), 
                    popup =  c("<a href = 'https://en.wikipedia.org/wiki/Mazda'>Mazda, Aki, Hiroshima, Japan</a>",
                               "<a href = 'https://en.wikipedia.org/wiki/Datsun'>Datsun, Tokyo, Japan</a>",
                               "<a href = 'https://en.wikipedia.org/wiki/Honda'>Honda, Minato, Japan</a>",
                               "<a href = 'https://en.wikipedia.org/wiki/Toyota'>Toyota, Aichi, Japan</a>"))
     } else if (word(input$model,1) %in% c("Merc","Porsche")){
       m %>% flyTo(lng=10.451526, lat=51.165691 , zoom=4.5) %>% 
         addMarkers(lng = c(9.183333,9.183333), 
                    lat = c(48.783333,48.783333), 
                    popup =  c("<a href = 'https://en.wikipedia.org/wiki/Mercedes-Benz'>Mercedes, Stuttgart, Germany</a>",
                               "<a href = 'https://en.wikipedia.org/wiki/Porsche'>Porsche, Stuttgart, Germany</a>"))
     } else if (word(input$model,1) %in% c("Fiat","Ferrari","Maserati")){
       m %>% flyTo(lng=12.56738, lat=41.87194 , zoom=4.5) %>% 
         addMarkers(lng = c(7.676111,10.866667,10.925556), 
                    lat = c(45.079167,44.533333,44.646944), 
                    popup =  c("<a href = 'https://en.wikipedia.org/wiki/Fiat_Automobiles'>Fiat, Turin, Piedmont, Italy</a>",
                               "<a href = 'https://en.wikipedia.org/wiki/Ferrari'>Ferrari, Maranello, Italy</a>",
                               "<a href = 'https://en.wikipedia.org/wiki/Maserati'>Maserati, Modena, Italy</a>"))
     } else if (word(input$model,1) %in% c("Lotus")){
       m %>% flyTo(lng=-3.435973, lat=55.378051 , zoom=4.5) %>% 
         addMarkers(lng = 1.2, lat = 52.55, 
                    popup =  "<a href = 'https://en.wikipedia.org/wiki/Lotus_Cars'>Lotus, Hethel, Norfolk, United Kingdom</a>")
     } else if (word(input$model,1) %in% c("Volvo")){
       m %>% flyTo(lng=18.643501, lat=60.128161 , zoom=4.5) %>% 
         addMarkers(lng = 11.966667, lat = 57.7, 
                    popup =  "<a href = 'https://en.wikipedia.org/wiki/Volvo'>Volvo, Gothenburg, Sweden</a>")
     } else if (word(input$model,1) %in% c("Valiant")){
       m %>% flyTo(lng=133.775136, lat=-25.274398 , zoom=4.5) %>% 
         addMarkers(lng = 138.560833, lat = -34.925278, 
                    popup =  "<a href = 'https://en.wikipedia.org/wiki/Chrysler_Valiant'>Valiant (see: Chrysler, AUS)</a>")
     } else {m}
       
 
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)





