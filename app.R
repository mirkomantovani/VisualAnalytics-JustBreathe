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

# getting the datasets

temp = list.files(pattern="*.csv")
datasets = lapply(temp, read.csv)
dataset <- do.call(rbind, datasets)

# preprocessing


# variables

years<-c(1980:2018)
states<-unique(dataset$State)
t<-subset(dataset, State == 'Illinois')
counties<-unique(t$County)

ui <- dashboardPage(
  #skin = "black",
  dashboardHeader(
    title = "Visual Analytics - Just Breathe",
    titleWidth = 300),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   
                   sidebarMenu(
                     useShinyalert(),
                     menuItem("Pie Charts AQI and pollutants", tabName = "pie"),
                     menuItem("Statistics Line Graphs", tabName = "line"),
                     menuItem("County Map", tabName = "map")
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
                        div(dataTableOutput("aqi_table", height = "15vh"), style = "font-size:80%")
          )),
          column(6, box(title = "Pollutants",status = "primary", width = NULL, 
                        tabsetPanel(
                          tabPanel("Percentage of days",
                            fluidRow(column(4,plotOutput("co_pie", height = "23vh")),column(4,plotOutput("no2_pie", height = "23vh")),column(4,plotOutput("ozone_pie", height = "23vh"))),
                            fluidRow(column(4,plotOutput("so2_pie", height = "23vh")),column(4,plotOutput("pm25_pie", height = "23vh")),column(4,plotOutput("pm10_pie", height = "23vh")))
                            ),
                          tabPanel("Bar chart", plotOutput("pollutants_bar", height = "46vh"))
                          ),
                            div(dataTableOutput("pollutants_table", height = "15vh"), style = "font-size:80%")
                        
                        )
                 )
        )
      ),
      
      tabItem("line",
              h1("WIP")
              ),
      
      tabItem("map",
              h1("WIP")
      )
      
      
      # Finish tabs
    )
  )
)
    

server <- function(input, output, session) {
  
  m_palette <-  scale_fill_manual(name = "",
                                  values = c('0 to 9' = '#08306b','10 to 19' = '#103a76', '20 to 29' = '#08519c',  '30 to 39' = '#2171b5',
                                             '40 to 49'= '#4292c6', '50 to 59' = '#6baed6','60 to 69' = '#9ecae1', '70 to 79' = '#c6dbef',
                                             '80+' = '#deebf7'))

  # computing subset of data based on user selection of year, state, county
  current <- reactive({
    print("reactive")
    subset(dataset, County == input$County & State == input$State & Year == input$Year)
    
    })
  
  
  observeEvent(priority = 10,input$State,{
    print("observeEvent")
    selected_state_data <- subset(dataset, State == input$State)
    counties_in_state <- unique(selected_state_data$County)
    
    updateSelectInput(session, inputId = "County", choices = counties_in_state)
    county <- input$County
    
  })
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
  
  
# End of server
}

shinyApp(ui = ui, server = server)