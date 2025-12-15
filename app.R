library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(bslib)

#load data
daily_trend_all <- readRDS('daily_trend_all.rds')
daily_trend_by_mode <- readRDS('daily_trend_by_mode.rds')
hourly_heatmap <- readRDS('hourly_heatmap.rds')
mode_pie_on <- readRDS('mode_pie_on.rds')
summary_box <- readRDS('summary_box.rds')
sydney_map1 <- readRDS('sydney_map1.rds')


#UI
ui<- fluidPage(
  #title
  titlePanel('NSW Opal Tap On/Off Usage Dashboard'),
  br(),
  
  #filters
  div(class='filter-box',
      fluidRow(
        column(4, selectInput('snapshot', 'Date ▼', choices = sort(unique(daily_trend_by_mode$snapshot)))),
        column(4, selectInput('mode', 'Transport Mode ▼', choices = c('All', unique(daily_trend_by_mode$mode))))
      )
  ),
  
  #summary box
  fluidRow(
    column(6, h4('Summary Boxes'),),
    column(6, div(id='sumOn',  style='font-size:15px; font-weight:bold; margin-bottom:10px;',
               'Tap On: ', textOutput('totalOn', inline = TRUE)),
    ),
    column(6,
           div(id='sumOff', style='font-size:15px; font-weight:bold;',
               'Tap Off: ', textOutput('totalOff', inline = TRUE))
    ),
  ),
  
  br(),
  
  fluidRow(
    column(6, div(class='section-box', h4('Daily Tap On Trend'), plotOutput('dailyPlot', height='320px'))),
    column(6, div(class='section-box', h4('Hourly Heatmap'), plotOutput('heatmapPlot', height='320px')))
  ),
  

  br(),

  fluidRow(
    column(6,
           div(class='section-box',
               h4('Major stations by passenger volume (Map)'),
               plotOutput('mapPlot', height='300px')
           )),
    column(6,
           div(class='section-box',
               h4('Transport Mode Pie Chart'),
               plotOutput('piePlot', height='250px')
           ))
  ),
  
  br(),
  p('Data Source: Transport Open Data Hub')
)


#server
server <- function(input, output, session){
  #filter summary by snapshot
  filtered_summary <- reactive({
    summary_box %>%
      filter(snapshot == input$snapshot)
  })
  
  #filter daily_trend by snapshot
  filtered_daily <- reactive({
    if (input$mode == 'All') {
      daily_trend_all %>% filter(snapshot == input$snapshot)
    } else {
      daily_trend_by_mode %>% 
        filter(snapshot == input$snapshot, mode == input$mode)
    }
  })
  
  #filter heatmap
  filtered_heatmap <- reactive({
    df <- hourly_heatmap %>%
      filter(snapshot == input$snapshot)
    if (input$mode != 'All') {
      df <- df %>% filter(mode == input$mode)
    }
    df
  })
  
  #filter pie chart
  filtered_pie <- reactive({
    df <- mode_pie_on %>%
      filter(snapshot == input$snapshot)
    if (input$mode != 'All') {
      df <- df %>% filter(mode == input$mode)
    }
    df
  })
  
  #filter map
  filtered_map <- reactive({
    df <- sydney_map1
    df <- df %>% filter(snapshot == input$snapshot)
    if (input$mode != 'All') {
      df <- df %>% filter(mode == input$mode)
    }
    df
  })
  

  #summary box
  output$totalOn <- renderText({
    df <- filtered_summary()
    value <- df %>% filter(tap == 'on') %>% pull(total_count)
    if (length(value)==0) return('0')
    format(value, big.mark=',')
  })
  
  output$totalOff <- renderText({
    df <- filtered_summary()
    value <- df %>% filter(tap == 'off') %>% pull(total_count)
    if (length(value)==0) return('0')
    format(value, big.mark=',')
  })
  
  
  #daily trend plot
  output$dailyPlot <- renderPlot({
    df <- filtered_daily()
    ggplot(df, aes(x = weekday, y = total_count, group = 1)) +
      geom_line(color = 'darkgreen', linewidth = 1) +
      geom_point(color = 'darkgreen', size = 2) +
      labs(x = 'Day of Week', y = 'Tap On Total') +
      theme(
        strip.text = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
        )
  })
  
  #heatmap
  output$heatmapPlot <- renderPlot({
    df <- filtered_heatmap() %>% filter(tap == 'on')
    ggplot(df, aes(x = time_hour, y = weekday, fill = total_count)) +
      geom_tile() +
      scale_fill_viridis_c(option = 'cividis') +
      theme_minimal(base_size = 15) +
      labs(x = 'Hour', y = 'Day of Week', fill = 'Tap On') +
      theme(
        plot.title = element_text(size = 18, face = 'bold'),
        strip.text = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 10)
      )
  })
  
  
  #pie chart
  output$piePlot <- renderPlot({
    df <- filtered_pie()
    ggplot(df, aes(x = '', y = total_count, fill = mode)) +
      geom_col(color = 'white') +
      coord_polar(theta = 'y') +
      scale_fill_manual(values = c(
        "bus" = '#0076C0',
        "train" = '#FF8200',
        "ferry" = '#00843D',
        "light_rail" = '#A80532',
        'unknow' = '#139B8B'
      )) +
      theme_void(base_size = 14) +
      labs(fill='Mode')
  })
  
  #map
  output$mapPlot <- renderPlot({
    df <- filtered_map()
    ggplot(df, aes(x = long, y = lat, group = L1, fill = total)) +
      geom_polygon(color = NA) +
      scale_fill_viridis_c(option = 'inferno', direction = -1, trans = 'sqrt') +
      coord_quickmap()
  })
}

shinyApp(ui, server)




