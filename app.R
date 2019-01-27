# Mirko Mantovani - 01/27/2019

# libraries
library(shiny)
library(devtools)
library(ggplot2)
library(shinydashboard)
library(scales) # needed for percent function
library(shinythemes) # themes for bootstrapPage, fluidPage, navbarPage, or fixedPage
library(dashboardthemes)
library(ggthemes) 
library(shinyalert)
library(leaflet)
library(rgdal)
library(geojson)
library(geojsonio)

# importing datasets
temp = list.files(pattern="*.csv")
datasets = lapply(temp, read.csv)
dataset <- do.call(rbind, datasets)

sites <- read.table(file = "sites/aqs_sites.csv", sep=",",header = TRUE)

xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")



########################################### PREPROCESSING #########################################

years<-c(1980:2018)
states<-unique(dataset$State)
t<-subset(dataset, State == 'Illinois')
counties<-unique(t$County)

# All counties with state
all_counties <- c()
for(s in states){
  coun <- subset(dataset, State == s)
  counti<-unique(coun$County)
  counti <- paste(counti,"-",s)
  all_counties <- c(all_counties,counti)
}


############################################### UI ################################################

ui <- dashboardPage(
  #skin = "black",
  dashboardHeader(
    title = "Visual Analytics - Just Breathe",
    titleWidth = 300),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   
                   sidebarMenu(
                     useShinyalert(),
                     menuItem("Year details for County", tabName = "pie"),
                     menuItem("County trends", tabName = "time"),
                     menuItem("Compare Counties", tabName = "compare")
                   ),
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .irs-from .irs-to {background: #60ECEC;color:white;}")),
                   tags$style(
                     HTML("label{
                          color: rgb(255, 255, 255);
                          }
                          
                          .selectize-input input {
                          background: rgb(79, 232, 211);
                          }
                          .irs-grid-text {
                          bottom: 5px;
                          color: #ffffff;
                          }
                          
                          .skin-blue .main-header .navbar {
                          background: rgb(26, 72, 86);
                          box-shadow: 2px 2px 2px #e2e2e2;
                          }
                          
                          .skin-blue .main-header .navbar .sidebar-toggle {
                          background: rgb(30, 83, 99);
                          color: rgb(208, 208, 208);
                          }
                          
                          .form-control, .selectize-input, .selectize-control.single .selectize-input {
                          background: rgb(59, 236, 220);
                          color: rgb(0,0,0);
                          border-color: rgb(200,200,200);
                          border-radius: 5px;
                          height: 34px;
                          min-height: 34px;
                          padding: 6px 12px;
                          }
                          .selectize-input, .selectize-control.single .selectize-input.input-active {
                          background: rgb(59, 236, 220);
                          cursor: text;
                          display: inline-block;
                          }
                          
                          .selectize-dropdown, .selectize-dropdown.form-control {
                          background: rgb(0,170,183);
                          border-radius: 4px;
                          }
                          "
                          
                     )
                   ),
                   #selectInput("Year", "Select Year", years, selected = 2018),
                   
                   selectInput(inputId = "State", "Select State", states, selected = 'Illinois'),
                   tags$style("#County {background-color:blue;}"),
                   selectInput("County", "Select County", counties, selected = 'Adams'),
                   sliderInput(inputId = "Year", 
                               label = "Select Year", 
                               value = 2018, min = 1980, max = 2018)
                   ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabItems(
      tabItem("pie",
              fluidRow(
                column(6, box(title = "AQI levels", width = NULL,status = "primary",
                              plotOutput("aqi_pie", height = "30vh"),
                              plotOutput("aqi_bar", height = "25vh"),
                              div(DT::dataTableOutput("aqi_table"), style = "font-size:80%")
                )),
                column(6, box(title = "Pollutants",status = "primary", width = NULL, 
                              tabsetPanel(
                                tabPanel("Percentage of days",
                                         fluidRow(column(4,plotOutput("co_pie", height = "24vh")),column(4,plotOutput("no2_pie", height = "24vh")),column(4,plotOutput("ozone_pie", height = "24vh"))),
                                         fluidRow(column(4,plotOutput("so2_pie", height = "24vh")),column(4,plotOutput("pm25_pie", height = "24vh")),column(4,plotOutput("pm10_pie", height = "24vh")))
                                ),
                                tabPanel("Bar chart", plotOutput("pollutants_bar", height = "48vh"))
                              ),
                              div(DT::dataTableOutput("pollutants_table"), style = "font-size:80%")
                              
                )
                )
              )
      ),
      
      # SECOND MENU TAB
      tabItem("time",
              fluidRow(
                # Input county with search
                column(2,box(title = "County Selection",status = "success", width = NULL,
                             column(12, fluidRow(selectizeInput("CountySearch", label = h5("Search County"), sort(all_counties), selected = NULL, multiple = FALSE,
                                                                options = NULL)),
                                    fluidRow(h1("internetInfo")),
                                    fluidRow(h1(textOutput("sel_state"))),
                                    fluidRow(h1(textOutput("sel_county")))))),
                # 2 tabs, (line plots and table, map)
                column(10,
                       tabsetPanel(
                         tabPanel("AQI Time Series",
                                  plotOutput("aqi_time", height = "70vh")
                         ),
                         tabPanel("Pollutants Percentage Time Series",
                                  tabsetPanel(
                                    tabPanel("Line Plot",
                                             plotOutput("pollutants_time", height = "60vh")
                                    ),
                                    tabPanel("Table",
                                             div(DT::dataTableOutput("pollutants_time_table"), style = "font-size:90%")
                                    )
                                  )
                         ),
                         tabPanel("Map",
                                  leafletOutput("map_county")
                         )
                       )
                )
              )
      ),
      
      # THIRD MENU TAB
      tabItem("compare",
              h1("WIP")
      )
      
      
      # Finish tabs
    )
  )
                   )




############################################# SERVER ##############################################

server <- function(input, output, session) {
  
  m_palette <-  scale_fill_manual(name = "",
                                  values = c('0 to 9' = '#08306b','10 to 19' = '#103a76', '20 to 29' = '#08519c',  '30 to 39' = '#2171b5',
                                             '40 to 49'= '#4292c6', '50 to 59' = '#6baed6','60 to 69' = '#9ecae1', '70 to 79' = '#c6dbef',
                                             '80+' = '#deebf7'))
  
  # computing subset of data based on user selection of year, state, county
  current <- reactive({
    # print("reactive")
    subset(dataset, County == input$County & State == input$State & Year == input$Year)
    
  })
  
  observeEvent(priority = 10,input$State,{
    # print("observeEvent")
    selected_state_data <- subset(dataset, State == input$State)
    counties_in_state <- unique(selected_state_data$County)
    
    updateSelectInput(session, inputId = "County", choices = counties_in_state)
    county <- input$County
    
  })
  
  selected_state <- reactive({
    # if(grepl(input$CountySearch,"-")){
    strsplit(input$CountySearch," - ")[[1]][2]
    # }
  })
  
  selected_county <- reactive({
    strsplit(input$CountySearch," - ")[[1]][1]
  })
  
  # observeEvent(input$CountySearch,{
  #   
  #   
  # })
  
  diocane<- reactive({data.frame(current()$Good.Days,current()$Moderate.Days)})
  
  # pie chart of aqi
  output$aqi_pie <- renderPlot({
    print(input$County)
    print("render Plot 1")
    # if(length(current()$State)==1){
    c<-subset(dataset, County == input$County & State == isolate(input$State) & Year == input$Year)
    if(length(c$State) == 1){
      print("good")
      
      df <- data.frame(
        
        group = c("Percentage of Good Days", "Percentage of Moderate Days", "Percentage of Unhealthy for Sensitive Groups Days", "Percentage of Very Unhealthy Days", "Percentage of Hazardous Days"),
        value = c(isolate(current())$Good.Days/isolate(current())$Days.with.AQI*100, isolate(current())$Moderate.Days/isolate(current())$Days.with.AQI*100, 
                  isolate(current())$Unhealthy.for.Sensitive.Groups.Days/isolate(current())$Days.with.AQI*100,
                  isolate(current())$Very.Unhealthy.Days/isolate(current())$Days.with.AQI*100,
                  isolate(current())$Hazardous.Days/isolate(current())$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Greys","AQI Level") +
        theme(
          #plot.background = element_rect(fill = "grey"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        ) 
      
      pie
    }
    # Signaling missing data
    else {
      print("error")
      shinyalert("Oops!", "No data for this County in this Year", type = "error")
    }
  })
  
  # bar chart of aqi
  output$aqi_bar <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        
        group = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Very Unhealthy", "Hazardous"),
        value = c(current()$Good.Days, current()$Moderate.Days, 
                  current()$Unhealthy.for.Sensitive.Groups.Days,
                  current()$Very.Unhealthy.Days,
                  current()$Hazardous.Days)
      )
      
      # bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) + geom_bar(stat="identity") + scale_fill_brewer(palette="Blues") 
      
      bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) + scale_fill_brewer(palette="Greys") +
        geom_bar(stat="identity") + coord_flip() + 
        theme(
          text = element_text(size=12),
          legend.position="none"
        )+ 
        xlab("AQI level") + ylab("Days count")
      bar
    }
  })
  
  # table of aqi
  output$aqi_table <- DT::renderDataTable(current()[, c('Good.Days', 'Moderate.Days',"Unhealthy.for.Sensitive.Groups.Days", "Very.Unhealthy.Days", "Hazardous.Days")],
                                          rownames = FALSE,
                                          colnames = c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Very Unhealthy','Hazardous'), 
                                          options = list(searching = FALSE,paging = FALSE,
                                                         dom = 't'
                                          ))
  # co_pie", height = "25vh")),column(6,plotOutput("no2_pie", height = "25vh"))),
  #                   fluidRow(column(6,plotOutput("ozone_pie", height = "25vh")),column(6,plotOutput("so2_pie", height = "25vh"))),
  # fluidRow(column(6,plotOutput("pm25_pie", height = "25vh")),column(6,plotOutput("pm10_pie"
  
  # pie chart of CO
  output$co_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without CO", "Days CO"),
        value = c((current()$Days.with.AQI-current()$Days.CO)/current()$Days.with.AQI*100, current()$Days.CO/current()$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Purples","CO",direction = -1) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        )
      pie
    }
  })
  
  # pie chart of NO2
  output$no2_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without NO2", "Days NO2"),
        value = c((current()$Days.with.AQI-current()$Days.NO2)/current()$Days.with.AQI*100, current()$Days.NO2/current()$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="PuBu","NO2",direction = -1) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        )
      pie
    }
  })
  
  # pie chart of Ozone
  output$ozone_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without Ozone", "Days Ozone"),
        value = c((current()$Days.with.AQI-current()$Days.Ozone)/current()$Days.with.AQI*100, current()$Days.Ozone/current()$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues","Ozone",direction = -1) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        )
      pie
    }
  })
  
  
  
  # pie chart of SO2
  output$so2_pie <- renderPlot({
    
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without SO2", "Days SO2"),
        value = c((current()$Days.with.AQI-current()$Days.SO2)/current()$Days.with.AQI*100, current()$Days.SO2/current()$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="BuGn","SO2",direction = -1) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        )
      pie
    }
  })
  
  # pie chart of PM2.5
  output$pm25_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without PM2.5", "Days PM2.5"),
        value = c((current()$Days.with.AQI-current()$Days.PM2.5)/current()$Days.with.AQI*100, current()$Days.PM2.5/current()$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="GnBu","PM2.5",direction = -1) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        )
      pie
    }
  })
  
  # pie chart of PM10
  output$pm10_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without PM10", "Days PM10"),
        value = c((current()$Days.with.AQI-current()$Days.PM10)/current()$Days.with.AQI*100, current()$Days.PM10/current()$Days.with.AQI*100)
      )
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Greens","PM10",direction = -1) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank()
        )
      pie
    }
  })
  
  # table of pollutants
  output$pollutants_table <- DT::renderDataTable(current()[, c('Days.CO', 'Days.NO2',"Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10")],
                                                 rownames = FALSE,
                                                 colnames = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'), 
                                                 options = list(searching = FALSE,paging = FALSE,
                                                                dom = 't'
                                                 ))
  
  # bar chart of pollutants
  output$pollutants_bar <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        
        group = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'),
        value = c(current()$Days.CO, current()$Days.NO2, 
                  current()$Days.Ozone,
                  current()$Days.SO2,
                  current()$Days.PM2.5,
                  current()$Days.PM10)
      )
      
      bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) +
        geom_bar(stat="identity") + coord_flip() + 
        theme(
          text = element_text(size=12),
          legend.position="none"
        )+ 
        xlab("Detected Pollutant") + ylab("Days count") +
        scale_fill_manual(values=c("#C3B5DB", "#ABB6D4", "#83BDDF","#A2DFA8", "#98D5B3", "#93D8CD"))
      bar
    }
  })
  
  output$sel_state <- renderText({ 
    selected_state()
  })
  
  output$sel_county <- renderText({ 
    selected_county()
  })
  
  # Time series of AQI statistics
  output$aqi_time <- renderPlot({
    df<-subset(dataset, State == selected_state() & County == selected_county())
    ggplot(data = df, aes(x = Year)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_line(aes(y = Max.AQI, color = "Max"), size = 1, group = 1) + 
      geom_point(aes(y = Max.AQI, color = "Max"), size = 3) +
      geom_line(aes(y = X90th.Percentile.AQI, color = "90th Percentile"), size = 1, group = 3) +
      geom_point(aes(y = X90th.Percentile.AQI, color = "90th Percentile"), size = 3) +
      geom_line(aes(y = Median.AQI, color = "Median"), size = 1, group = 2) +
      geom_point(aes(y = Median.AQI, color = "Median"), size = 3) +
      labs(x = "Year", y = "Air Quality Index") +
      scale_x_continuous(breaks = round(seq(min(df$Year), max(df$Year), by = 1),1)) +
      # scale_color_manual(name = "Statistics",
      #                    values = c("Max" = "firebrick1", 
      #                               "90th Percentile" = "firebrick4",
      #                               "Median" = "steelblue1")) +
      scale_color_discrete(breaks=c("Max","90th Percentile","Median"))
  })
  
  # Time series of Pollutants Percentage
  output$pollutants_time <- renderPlot({
    s_county<-subset(dataset, State == selected_state() & County == selected_county())
    s_county[,14:19]<- s_county[14:19]/s_county$Days.with.AQI*100
    # df <- data.frame(
    #   
    #   group = c("CO", "NO2", "Ozone", "SO2", "PM2.5","PM10"),
    #   value = c(s_county$Days.CO/s_county$Days.with.AQI*100, s_county$Days.NO2/s_county$Days.with.AQI*100, 
    #             s_county$Days.Ozone/s_county$Days.with.AQI*100,
    #             s_county$Days.SO2/s_county$Days.with.AQI*100,
    #             s_county$Days.PM2.5/s_county$Days.with.AQI*100,
    #             s_county$Days.PM10/s_county$Days.with.AQI*100)
    # )
    ggplot(data = s_county, aes(x = Year)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_line(aes(y = Days.CO, color = "CO"), size = 1, group = 1) + 
      geom_point(aes(y = Days.CO, color = "CO"), size = 3) +
      geom_line(aes(y = Days.NO2, color = "NO2"), size = 1, group = 2) +
      geom_point(aes(y = Days.NO2, color = "NO2"), size = 3) +
      geom_line(aes(y = Days.Ozone, color = "Ozone"), size = 1, group = 3) +
      geom_point(aes(y = Days.Ozone, color = "Ozone"), size = 3) +
      geom_line(aes(y = Days.SO2, color = "SO2"), size = 1, group = 4) +
      geom_point(aes(y = Days.SO2, color = "SO2"), size = 3) +
      geom_line(aes(y = Days.PM2.5, color = "PM2.5"), size = 1, group = 5) +
      geom_point(aes(y = Days.PM2.5, color = "PM2.5"), size = 3) +
      geom_line(aes(y = Days.PM10, color = "PM10"), size = 1, group = 6) +
      geom_point(aes(y = Days.PM10, color = "PM10"), size = 3) +
      labs(x = "Year", y = "Percentage of Pollutant") +
      scale_x_continuous(breaks = round(seq(min(s_county$Year), max(s_county$Year), by = 1),1)) +
      scale_y_continuous(breaks = round(seq(min(s_county[14:19]), max(s_county[14:19]), by = 10),1)) +
      scale_color_manual(name = "Statistics",
                         values = c("CO" = "#8a63cc",
                                    "NO2" = "#514fc6",
                                    "Ozone" = "#409ace",
                                    "SO2" = "#55e864",
                                    "PM2.5" = "#3cad69",
                                    "PM10" = "#53edd4"))
    # scale_fill_manual(values=c("#9B77D8", "#758fd6", "#68aed6","#6ed378", "#6ad197", "#66d6c4"))
    
    # scale_color_discrete(breaks=c("Max","90th Percentile","Median"))
  })
  
  # table of pollutants
  output$pollutants_time_table <- DT::renderDataTable(subset(dataset, State == selected_state() & County == selected_county())[, c('Year','Days.CO', 'Days.NO2',"Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10")],
                                                      rownames = FALSE,
                                                      colnames = c('Year','CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'), 
                                                      options = list(searching = TRUE,paging = TRUE,lengthMenu = c(5, 10, 40), pageLength = 7
                                                                     # dom = 't'
                                                      ))
  
  # County on Leaflet Map
  output$map_county <- renderLeaflet({
    
    latit <- 32
    longit <- -86
    
    # Extracting long and lat of selected county from sites
    site<-subset(sites, State.Name == selected_state() & County.Name == selected_county())
    
    latit <- site$Latitude
    latit <- latit[latit!=0] # Eliminating 0 values
    latit <- latit[!is.na(latit)] # Eliminating NAs
    computed_lat <- mean(latit)
    longit <- site$Longitude
    longit <- longit[longit!=0] # Eliminating 0 values
    longit <- longit[!is.na(longit)] # Eliminating NAs
    computed_lng <- mean(longit)
    
    # xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")
    
    # nyc <- xy[xy$STATE == 36, ]
    
    leaflet(xy) %>%
      addTiles() %>%
      addPolygons(color = "#962121", weight = 0.8, smoothFactor = 0.2,
                  opacity = 1.0, fillOpacity = 0.1,
                  # fillColor = ~colorQuantile("YlOrRd"),
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE)) %>%
      setView(lng = computed_lng, lat = computed_lat, zoom = 6) %>%
      addMarkers(lng = computed_lng, lat = computed_lat, label = "Selected County")
    
    # states <- readOGR("shp/cb_2013_us_state_20m.shp",
    #                   layer = "cb_2013_us_state_20m", GDAL1_integer64_policy = TRUE)
    # neStates <- subset(states, states$STUSPS %in% c(
    #   "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
    # ))
    # leaflet(neStates) %>%
    #   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
    #               opacity = 1.0, fillOpacity = 0.5,
    #               fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
    #               highlightOptions = highlightOptions(color = "white", weight = 2,
    #                                                   bringToFront = TRUE))
    # map <- leaflet(data = us.map, height = session$width)
    # map <- addTiles(map)
    # map <- setView(map, lng = -86.47289099999999, lat = 32.437458, zoom = 15)
    # map <- addMarkers(map, lng = PumpLocations$x, lat = PumpLocations$y, label = "Pump")
    # map <- addMarkers(map, lng = DeathLocations$x, lat = DeathLocations$y, label = as.character(DeathLocations$count), clusterOptions = markerClusterOptions())
    # map
    
  }
  
  )
  
  # End of server
}

shinyApp(ui = ui, server = server)