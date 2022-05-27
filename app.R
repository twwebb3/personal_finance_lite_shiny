#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(dplyr)
library(googlesheets4)

gs4_auth(cache = ".secrets", email= "twwebb1991@gmail.com")

url_df <- read.csv("url.csv", stringsAsFactors = F)
url = url_df$url

# df <- data.frame(date = "", expenditure = "", stringsAsFactors = F)

today <- as.Date(now()) # print this in the app to make sure it refreshes daily
mth_start <- as.Date(paste(year(today),
                           month(today),
                           "01", sep="-"))
mth_end <- as.Date(paste(year(today),
                         month(today) + 1,
                         "01", sep="-")) -1
days <- as.Date(mth_start:today, origin = "1970-01-01")

spend_cap <- 3000

spend_df <- data.frame(week = unique(week(days)), stringsAsFactors = F)
spend_df$budget <- spend_cap/length(spend_df$week)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monthly Expenditures"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("date",
                        "Date",
                        choices = days,
                        selected = today),
            
            numericInput("expenditure",
                         "Expenditure Amount",
                         0),
            
            submitButton(text = "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("summary"),
          tableOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({
    source_df <- read_sheet(url,sheet = "Sheet1")
    
    if (input$expenditure>0) {
      tempDf <- data.frame(date = input$date,
                           expenditure = input$expenditure,
                           stringsAsFactors = F)
      
      out_df <- rbind(source_df, tempDf)
      
      out_df <- filter(out_df, 
                       date!="", 
                       month(date)==month(today)) # filter out blanks and last mth
      
      out_df %>% sheet_write(url,sheet = "Sheet1")
    } else {
      out_df <- source_df
    }
    
    return(out_df)
  })
  
  output$summary <- renderTable({
    req(df())
    
    weekly_df <- df() %>% mutate(week=week(date)) %>% 
      group_by(week) %>% 
      summarise(expenditures=sum(expenditure))
    
    summary_df <- left_join(spend_df,
                         weekly_df,
                         by="week")
    summary_df$expenditures[is.na(summary_df$expenditures)] <- 0
    
    summary_df <- summary_df %>%
      mutate(cumulative_budget = cumsum(budget),
             cumulative_expenditures = cumsum(expenditures),
             buget_remaining = cumulative_budget - cumulative_expenditures)
    
    summary_df
  })

  output$distPlot <- renderTable({
    req(df())
    print(df())
  
    df()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
