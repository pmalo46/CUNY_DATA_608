#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)


df <- read.csv("Final_HDI_AID_2.csv", header= TRUE)

df$Aid_Year <- df$Aid_Year/1000000
df <- df %>% group_by(Country) %>% arrange(Country, Year)


ui <-navbarPage("US International Aid",
    tabPanel("Aid Distribution per Year",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("year", "Year", unique(df$Year), selected='2018'),
                     helpText("Select the Year above to explore how aid was alloted to different countries during that year.")
                     ),
                 mainPanel(
                     plotlyOutput("plot1")
                 )
             )
             ),
    tabPanel('Aid vs. Human Development Index',
        sidebarLayout(
            sidebarPanel(
                selectInput('country', 'Country', unique(df$Country), selected='Peru'),
                helpText("Select a Country above to explore how much aid was alloted over time vs. how the country's Human Development Index has changed.")
            ),
            mainPanel(
                plotlyOutput('plot2'),
                #h6("Aid vs. Human Development Index")
    )
)
))

server <- function(input, output, session) {
    
    output$plot1 <- renderPlotly({
        plot_ly(data = df %>% filter(Country != 'Palau') %>% filter(Year == input$year),
                x = ~Aid_Per_Capita, 
                y =~HDI_Year, 
                type = 'scatter',
                text = ~paste("Country: ", Country, '<br>Aid (millions): $', Aid_Year),
                size = ~Aid_Year,
                color = ~Region,
                sizes = c(8, 200),
                marker = list(opacity = 0.5, sizemode = 'diameter')
                ) %>%
        layout(
            title = "Aid Distribution by Year",
            yaxis = list(title = "HDI"),
            xaxis = list(title = "Net Aid per Capita (in USD)"))
    })
    
    output$plot2 <- renderPlotly({
        plot_ly(data = df %>% filter(Country == input$country), x = ~Year, y = ~Aid_Year
                ,type = "scatter", mode = "lines", color = I("green")
                ,name = "Aid") %>%
            add_trace(x = ~Year, y = ~HDI_Year, yaxis = "y2", color = I("blue"), name = "HDI") %>%
            layout(
                yaxis = list(
                    showline = FALSE, side = "left"
                    ,title = "Aid (in millions USD)"
                    ,color = "#25c472"
                )
                ,yaxis2 = list(
                    showline = FALSE
                    ,overlaying = "y"
                    ,title = "HDI", side = "right"
                    ,color = "blue"
             )
                ,showlegend = FALSE
                ,margin = list(
                    pad = 30, b = 10, l = 10, r = 90
                    )
                ,legend = list(orientation = "h")
             )
       })
}

shinyApp(ui = ui, server = server)
