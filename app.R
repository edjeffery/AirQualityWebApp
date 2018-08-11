
# Libraries
library(shiny)
library(shinythemes)
library(RMySQL)
library(anytime)
library(ggplot2)
library(ggmap)
library(lubridate)

# Set up MySQL database connection
mydb = dbConnect(MySQL(),
                 user='ej301',
                 password='Aesoh8oWPu1xohMu',
                 dbname='ej301',
                 host='mysql5host.bath.ac.uk')

# Query database
results = dbSendQuery(mydb, 'SELECT * FROM route_air_quality_final;')
data = fetch(results, n=-1)

dbClearResult(results)
dbDisconnect(mydb)

# Add additional columns
data$day <- weekdays(as.Date(anydate(data$time))) # Day of week column
data$am <- (hour(anytime(data$time)) < 12) # AM or PM column (AM = TRUE)
data$date <- anydate(data$time)
data$period <- ifelse(data$am == TRUE, paste(data$day, "am", sep = "_"), paste(data$day, "pm", sep = "_"))
#rawData$timediff <- diff(rawData$time)
data$weekend <- ifelse(data$day == "Saturday" | data$day == "Sunday", "weekend", "week")
data$week_period <- ifelse(data$am == TRUE, paste(data$weekend, "am", sep = "_"), paste(data$weekend, "pm", sep = "_"))
data$outlier_pm <- (data$pm_raw > 350)
data$outlier_mq131 <- (data$mq131 > 5 | data$mq131 < 1)
data$outlier_mq135 <- (data$mq135 > 5 | data$mq135 < 0.1)


#data = read.csv("data/air-quality-dataset-2018-07-06.csv")  # read csv file 
map_data <- data[, c("longitude", "latitude", "mq131")]

maxDate = max(data$date)
dates = unique(data$date)

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
   
   # Application title
   #titlePanel(title=div(img(src="pi-logo.png"), "Air quality monitoring in Bath, UK")),
   
   titlePanel(
     fluidRow(
       column(9, div(img(src="pi-logo.png"), "Air quality monitoring in Bath, UK")), 
       column(3, img(height = 88, width = 215, src = "uob-logo.png"))
     )
   ),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 3,
        
        selectizeInput(inputId = "date",
                    label = h4("Select date:"),
                    choices = dates,
                    selected = max(data$date)),
        
        radioButtons(inputId = "AMorPMradio",
                     label = h4("Morning or Evening:"),
                     choices = list("AM","PM"),
                     selected = "AM"),
        
        radioButtons(inputId = "pollutants_radio",
                     label = h4("Choose pollutant:"),
                     choices = c("Ozone (MQ131)" = "mq131",
                                 "Harmful gases (MQ135)" = "mq135",
                                 "Particulates (PM10)" = "pm_raw"),
                     selected = "mq131"),
        
        h4("Outliers:"),
        
        checkboxInput(inputId = "outliers",
                      label = "Remove outliers?",
                      value = FALSE),
        
        br(),
        h4("Information"),
        
        p("The dataset can be downloaded from the 'Table' tab.")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Graphs",
                   h3('Time series'),
                   plotOutput("distPlot", 
                              click = "plot1_click",
                              brush = brushOpts(
                                id = "plot1_brush"
                              )),
                   tableOutput("stats")
                   # column(width = 6,
                   #        h4("Points near click"),
                   #        verbatimTextOutput("click_info")
                   # ),
                   # column(width = 6,
                   #        h4("Brushed points"),
                   #        verbatimTextOutput("brush_info")
                   # )
          ),
          tabPanel(title = "Velocity & Altitude",
                   h3('Effect of bicycle velocity on air quality reading'),
                   plotOutput("speedPlot"),
                   h3('Effect of altitude on air quality reading'),
                   plotOutput("altitudePlot")
          ),
          tabPanel(title = "Table",
                   br(),
                   downloadButton('downloadData', 'Download data'),
                   br(), br(),
                   DT::dataTableOutput("dataTable"),
                   br(),
                   h3("Data summary"),
                   verbatimTextOutput("summary")
          ),
          tabPanel(title = "Map",
                   h3("Map of measurement coordinates in Bath"),
                   br(),
                   plotOutput("mapPlot"),
                   br(),
                   h3("Coloured hexagonal map"),
                   plotOutput("hexmapPlot")
          ),
          tabPanel(title = "Project info.",
                   h3("Project information"),
                   br(),
                   p("This project involved creating an air quality monitoring device using a Raspberry Pi and a variety of sensors.
                     We collected data over a period of 3 weeks.")
          )
        )
      )
   )
)

server <- function(input, output) {
  
  selectedPollutantData <- reactive({
    
    req(input$date)
    req(input$pollutants_radio)
    req(input$AMorPMradio)
    
    # Filter for selected date
    filtered_data <- data[data$date == input$date,]
    
    # Filter for morning or afternoon collection run
    if (input$AMorPMradio == "AM") {
      filtered_data <- filtered_data[filtered_data$am == TRUE,]
    } else if (input$AMorPMradio == "PM") {
      filtered_data <- filtered_data[filtered_data$am == FALSE,]
    }
    # Filter for outliers
    if (input$outliers == TRUE) {
      filtered_data <- filtered_data[filtered_data$outlier_pm == FALSE &
                                       filtered_data$outlier_mq131 == FALSE &
                                       filtered_data$outlier_mq135 == FALSE,]
    }
    # Filter for selected pollutant
    if (input$pollutants_radio == "mq131") {
      filtered_data <- filtered_data[, c("time", "mq131", "longitude", "latitude", "horizontal_speed", "altitude")]
    } else if (input$pollutants_radio == "mq135") {
      filtered_data <- filtered_data[, c("time", "mq135", "longitude", "latitude", "horizontal_speed", "altitude")]
    } else if (input$pollutants_radio == "pm_raw") {
      filtered_data <- filtered_data[, c("time", "pm_raw", "longitude", "latitude", "horizontal_speed", "altitude")]
    }
  })
  
  output$distPlot <- renderPlot({
    filteredData <- selectedPollutantData()
    pollutant <- input$pollutants_radio
    x <- anytime(filteredData$time)
    y <- filteredData[, c(pollutant)]
    d <- data.frame(x, y)
    
    ggplot(data = d, aes(x = x, y = y)) +
      geom_point() +
      xlab("Time") +
      ylab("Measurement") +
      theme(text = element_text(size=15),
            axis.title.y = element_text(vjust=3))
  })
  
  output$speedPlot <- renderPlot({
    filteredData <- selectedPollutantData()
    pollutant <- input$pollutants_radio
    x <- filteredData$horizontal_speed
    y <- filteredData[, c(pollutant)]
    d <- data.frame(x, y)
    ggplot(data = d, aes(x = x, y = y)) +
      geom_point() +
      xlab("Altitude") +
      ylab("Measurement") +
      geom_smooth(method='lm')
  })
  
  output$altitudePlot <- renderPlot({
    filteredData <- selectedPollutantData()
    pollutant <- input$pollutants_radio
    x <- filteredData$altitude
    y <- filteredData[, c(pollutant)]
    d <- data.frame(x, y)
    ggplot(data = d, aes(x = x, y = y)) +
      geom_point() +
      xlab("Altitude") +
      ylab("Measurement") +
      geom_smooth(method='lm')
  })
  
  output$mapPlot <- renderPlot({
    qmplot(longitude, latitude, data = map_data, maptype = "toner-lite", color = I("red"))
  })
  
  output$hexmapPlot <- renderPlot({
    d <- ggplot(data, aes(longitude, latitude, z = mq131))
    d + stat_summary_hex() #+ scale_fill_gradientn(limits=c(1.5,3), breaks=seq(1.3, 3, by=0.5), colours=rainbow(4))
  })
  
  output$dataTable <- DT::renderDataTable({
    data
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("air-quality-dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(data, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(data, input$plot1_brush)
  })
  
  output$summary <- renderPrint({
    summary(data[, c("pm_raw", "mq131", "mq135", "altitude", "horizontal_speed", "temp", "humidity")])
  })
  
  output$stats <- renderTable({
    filteredData <- selectedPollutantData()
    pollutant <- input$pollutants_radio
    p_data <- filteredData[, c(pollutant)]
    labels = c("Mean", "Std. dev.", "Median", "MAD", "IQR", "Max", "Min")
    mean = mean(p_data)
    sd = sd(p_data)
    median = median(p_data)
    mad = mad(p_data)
    iqr = IQR(p_data)
    max = max(p_data)
    min = min(p_data)
    values = c(mean, sd, median, mad, iqr, max, min)
    df <- data.frame(labels, values)
    colnames(df) <- c("Statistic", "Value")
    t(df)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

