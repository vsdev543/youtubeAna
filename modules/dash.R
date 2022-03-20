

m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 20
)

yt<-readRDS("data/yt.Rds")%>%
  as_tibble()%>%
  mutate(trending_date=ydm(trending_date))

dash_UI <- function(id,title) {
  ns <- NS(id)
  fluidPage(style="padding:0px 5vw; background-color: #364156; color:#fff; height:100vh; overflow-y: auto;",
      div(style="display:flex; justify-content:space-between;align-items:baseline;",
        span(title,style="font-size:2.5em;margin-top:10px; font-weight:900;",class="brand")#,
        ),
      fluidRow(column(9,
                      tabsetPanel(type = 'tabs',selected = "Charts",
                                  tabPanel(title = "Charts",icon = icon("chart-area"),
                                           br(),
                                          uiOutput(ns('main'))
                                  ),
                                  tabPanel(title = "Data",icon = icon("database"),
                                   br(),
                                   div(style='background-color:#fff; padding:10px; border-radius:10px;',
                                       DTOutput(ns('mainDt'))
                                   )
                                  )
                      )
                      ),
               column(3, 
                      h3("Controls"),hr(),
                      # dateRangeInput(inputId = ns('dRange'),label = "Date Range",start = min(yt$trending_date),end = max(yt$trending_date),width = "100%"),
                      sliderInput(inputId = ns('range'),'Date',min = min(yt$trending_date),max = max(yt$trending_date),
                                  value = max(yt$trending_date),width = "100%",animate = animationOptions(
                                    interval = 700,
                                    playButton = icon('play', "fa-2x"),
                                    pauseButton = icon('pause', "fa-2x")
                                  ),step = 7),
                      numericInput(ns("topx"),"Show top",value = 10,min = 0,step = 1,width = "100%"),
                      selectizeInput(ns("chanels"),"Channels",choices = c("All",unique(yt$channel_title)),selected = "All",multiple=T,width="100%"),
                      hr(),
                      plotlyOutput(ns('subp'),height = "570px")
               ))
    
  )
}

dash <- function(input, output, session) {
  ns<-session$ns
  
  byChanel<-reactive({
    yk<-yt
    
    if(!"All"%in%input$chanels){
      yk<-yk%>%
        filter(channel_title%in%input$chanels)
    }
    
    yx<-yk%>%
      filter(trending_date<=input$range)%>%
      group_by(channel_title)%>%
      summarise(views=sum(views),likes=sum(likes),dislikes=sum(dislikes),comments=sum(comment_count),totalTrendingDates=n())
  })
  
  output$main<-renderUI({
    div(class="grid-2",
    renderPlotly({
      byChanel()%>%
        arrange(desc(views))%>%
        head(input$topx)%>%
      plot_ly(labels = ~channel_title, values = ~views)%>%
        add_pie(hole = 0.6)%>%
        layout(title = paste("Top",input$topx,"Most Viewed channels"),  showlegend = T,margin=m,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }),
    renderPlotly({
      byChanel()%>%
        arrange(desc(views))%>%
        head(input$topx)%>%
        plot_ly(labels = ~channel_title, values = ~likes)%>%
        add_pie(hole = 0.6)%>%
        layout(title = paste("Top",input$topx,"Most Liked channels"),  showlegend = T,margin=m,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }),
    renderPlotly({
      byChanel()%>%
        arrange(desc(views))%>%
        head(input$topx)%>%
        plot_ly(labels = ~channel_title, values = ~dislikes)%>%
        add_pie(hole = 0.6)%>%
        layout(title = paste("Top",input$topx,"Most Liked channels"),  showlegend = T,margin=m,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }),
    renderPlotly({
      byChanel()%>%
        arrange(desc(views))%>%
        head(input$topx)%>%
        plot_ly(labels = ~channel_title, values = ~comments)%>%
        add_pie(hole = 0.6)%>%
        layout(title = paste("Top",input$topx,"Most Liked channels"),  showlegend = T,margin=m,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    )
  })
  
  output$subp<-renderPlotly({
    byChanel()%>%
      arrange(desc(views))%>%
      head(input$topx)%>%
      plot_ly(labels = ~channel_title, values = ~totalTrendingDates)%>%
      add_pie(hole = 0.6)%>%
      layout(title = paste("Top",input$topx,"channel by trending days"),  showlegend = T,margin=m,legend = list(orientation = "h",   
                                                                                                           xanchor = "center",  
                                                                                                           x = 0.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$speed_value <- renderUI({
    numericInput("speed","Speed Value :",value = 100)
  })
  
  output$mainDt<-renderDT({
    byChanel()
  },options = list(scrollX=TRUE))
}
