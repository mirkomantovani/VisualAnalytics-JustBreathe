# libraries

library(shiny)
library(ggplot2)
library(shinydashboard)
library(scales) # needed for percent function

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
  dashboardHeader(title = "Visual Analytics - Just Breathe"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   selectInput("Year", "Select Year", years, selected = 2018),
                   selectInput("State", "Select State", states, selected = 'Illinois'),
                   selectInput("County", "Select County", counties, selected = 'Cook')
  ),
  dashboardBody(
    fluidRow(
      column(6, box(title = "Percentage of days AQI levels", status = "primary", width = NULL,
                    plotOutput("aqi_pie", height = "30vh"),
                    plotOutput("aqi_bar", height = "30vh"),
                    dataTableOutput("aqi_table", height = "20vh")
      )),
      column(6, box(title = "test",status = "primary", width = NULL, 
                    fluidRow(column(6,plotOutput("co_pie", height = "25vh")),column(6,plotOutput("no2_pie", height = "25vh"))),
                    fluidRow(column(6,plotOutput("ozone_pie", height = "25vh")),column(6,plotOutput("so2_pie", height = "25vh"))),
                    fluidRow(column(6,plotOutput("pm25_pie", height = "25vh")),column(6,plotOutput("pm10_pie", height = "25vh")))
                    
                    ))
    ),
    fluidRow(
      column(6, h1("madonna troia")),
      column(6, h1("cagna puttana"))
    )
  )
)
    

server <- function(input, output) {
  
  m_palette <-  scale_fill_manual(name = "",
                                  values = c('0 to 9' = '#08306b','10 to 19' = '#103a76', '20 to 29' = '#08519c',  '30 to 39' = '#2171b5',
                                             '40 to 49'= '#4292c6', '50 to 59' = '#6baed6','60 to 69' = '#9ecae1', '70 to 79' = '#c6dbef',
                                             '80+' = '#deebf7'))

  # computing subset of data based on user selection of year, state, county
  current <- reactive({subset(dataset, County == input$County & State == input$State & Year == input$Year)})
  
  diocane<- reactive({data.frame(current()$Good.Days,current()$Moderate.Days)})
  
  # pie chart of aqi
  output$aqi_pie <- renderPlot({
    
    df <- data.frame(

      group = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Very Unhealthy", "Hazardous"),
      value = c(current()$Good.Days/current()$Days.with.AQI*100, current()$Moderate.Days/current()$Days.with.AQI*100, 
                current()$Unhealthy.for.Sensitive.Groups.Days/current()$Days.with.AQI*100,
                current()$Very.Unhealthy.Days/current()$Days.with.AQI*100,
                current()$Hazardous.Days/current()$Days.with.AQI*100)
    )
  
    pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank()
      )
      #+ geom_text(aes(x=1, y = cumsum(value) - value/2, label=percent(value/100)))
    
      #geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
        #            label = percent(value/100)), size=4)
    pie
  })
  
  # bar chart of aqi
  output$aqi_bar <- renderPlot({
    
    df <- data.frame(
      
      group = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Very Unhealthy", "Hazardous"),
      value = c(current()$Good.Days, current()$Moderate.Days, 
                current()$Unhealthy.for.Sensitive.Groups.Days,
                current()$Very.Unhealthy.Days,
                current()$Hazardous.Days)
    )
    
    # bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) + geom_bar(stat="identity") + scale_fill_brewer(palette="Blues") 
    
    bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) + scale_fill_brewer(palette="Blues") +
      geom_bar(stat="identity") + coord_flip() + 
      theme(
        text = element_text(size=12),
        legend.position="none"
        )+
    xlab("AQI level") + ylab("Days count")
    bar
  })
  
  # table of aqi
  output$aqi_table <- DT::renderDataTable(current()[, c('Good.Days', 'Moderate.Days',"Unhealthy.for.Sensitive.Groups.Days", "Very.Unhealthy.Days", "Hazardous.Days")],
                                          rownames = FALSE,
                                          colnames = c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Very Unhealthy','Hazardous'), 
                                          options = list(searching = FALSE,paging = FALSE,
                                                         dom = 't'
                                                         ))
    # DT::datatable({
    #   current()[, c('Good.Days', 'Moderate.Days')]
    #   # current()
    #   # diocane()
    #   # data.frame(c(current()$Good.Days, current()$Moderate.Days))
    #   
    # },
    # colnames = c("Good.Days" = 0, "Bad" = 1),
  #   rownames = FALSE,
  #   options = list(searching = FALSE,ordering = F, lengthChange = FALSE, paging = FALSE, dom = 't',
  #                  columnDefs = list(list(visible=FALSE, targets=c(1,2)), list(className = 'dt-right', targets = 0:2)))
  #   )
  # )
}

shinyApp(ui = ui, server = server)