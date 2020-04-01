options(scipen = 999)
library(dplyr)
library(forecast)
library(shiny)
library(reshape2)
library(shinythemes)
library(plotly)
library(readr)

url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
df <- read_csv(url,progress = T)
df$`Province/State` <- NULL
df$Lat <- NULL
df$Long <- NULL
countries <- unique(df$`Country/Region`)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("COVID-19 Forecasting using auto.arima()"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("win",
                        "Forecast Window:",
                        min = 2,
                        max = 10,
                        value = 5,
                        step = 1),
            selectInput("country",
                        "Select a Country:",
                        choices = countries,
                        selected = "Colombia"),
            tags$hr(),
            tags$h6("Developed by: jhonnatan.torres.suarez@gmail.com"),
            tags$br(),
            tags$h6("Data Source: https://github.com/CSSEGISandData/COVID-19")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
                plotlyOutput("cs"),
                tags$hr(),
                plotlyOutput("gs")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$cs <- renderPlotly({
        
        cdf<- df %>% filter(`Country/Region`==input$country)%>% 
            group_by(`Country/Region`) %>% 
            summarise_all(.funs = sum) %>%
            melt(value.name = "Confirmed_Cases")
        
        cdf$`Country/Region` <- NULL
        cdf$variable <- as.Date(cdf$variable,format = "%m/%d/%y")
        cdf["Lo.95"]=NA
        cdf["Hi.95"]=NA
        cdf["Label"] = "Observed"
        
        future_dates = as.Date(array())
        
        for (i in 1:as.numeric(input$win)){
            future_dates[i] <- cdf$variable[dim(cdf)[1]]+i
        }
        
        preds <- cdf$Confirmed_Cases %>% auto.arima(stepwise = F) %>% forecast(h=as.numeric(input$win))
        preds <- data.frame(preds)
        
        fdf = rbind(cdf, data.frame("variable"=future_dates,"Confirmed_Cases"=preds$Point.Forecast,
                                    "Lo.95"=preds$Lo.95,"Hi.95"=preds$Hi.95,"Label"="Forecasted"))
        
        p <-ggplot(fdf,aes(variable,Confirmed_Cases,col=Label))+
            geom_line()+
            geom_point(size=2)+
            geom_line(aes(variable,fdf$Lo.95),col="grey",linetype="dashed")+
            geom_line(aes(variable,fdf$Hi.95),col="grey",linetype="dashed")+
            labs(x="Date",title="Forecast of Confirmed Cases",y=paste(paste(input$country,"Confirmed Cases")))
        
        ggplotly(p)
        
        })
    
    output$gs <- renderPlotly({
        
        cdf<- df %>% select(-`Country/Region`) %>%
            summarise_all(.funs = sum) %>%
            melt(value.name = "Confirmed_Cases")
        
        cdf$`Country/Region` <- NULL
        cdf$variable <- as.Date(cdf$variable,format = "%m/%d/%y")
        cdf["Lo.95"]=NA
        cdf["Hi.95"]=NA
        cdf["Label"] = "Observed"
        
        future_dates = as.Date(array())
        
        for (i in 1:as.numeric(input$win)){
            future_dates[i] <- cdf$variable[dim(cdf)[1]]+i
        }
        
        preds <- cdf$Confirmed_Cases %>% auto.arima(stepwise = F) %>% forecast(h=as.numeric(input$win))
        preds <- data.frame(preds)
        
        fdf = rbind(cdf, data.frame("variable"=future_dates,"Confirmed_Cases"=preds$Point.Forecast,
                                    "Lo.95"=preds$Lo.95,"Hi.95"=preds$Hi.95,"Label"="Forecasted"))
        
        p <-ggplot(fdf,aes(variable,Confirmed_Cases,col=Label))+
            geom_line()+
            geom_point(size=2)+
            geom_line(aes(variable,fdf$Lo.95),col="grey",linetype="dashed")+
            geom_line(aes(variable,fdf$Hi.95),col="grey",linetype="dashed")+
            labs(x="Date",title="Forecast of Confirmed Cases",y="Global Confirmed Cases")
        
        ggplotly(p)
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
