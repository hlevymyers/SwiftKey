#

library(dplyr)
library(stringi)
library(stringr)
library(tidytext)
library(tidyr)
library(readr)
library(readtext)
library(shinythemes)
library(shiny)

#Load data & function modules
source("tokenize_text.R", local = TRUE)
source("lookup_word.R", local = TRUE)
quadgrams.RDS <- read_rds("quadgrams.RDS")
threegrams.RDS <- read_rds("threegrams.RDS")

#Make column name clearer
colnames(quadgrams.RDS)[4] <- "Predicted.Word"
colnames(threegrams.RDS)[3] <- "Predicted.Word"

# Define UI
ui <- fluidPage(
        theme = shinytheme("spacelab"),
        titlePanel("What is the Next Word?"),
        sidebarLayout(
                sidebarPanel(
                        textInput("txt", "Type a two or three word phrase, then submit.", "text here"),
                        actionButton("goButton", "Submit")
                ),
                mainPanel(
                        tabsetPanel(
                                tabPanel("Top Suggestions")
                        ),
                        tableOutput("top10")
                )
        )
)

#Build server function
server <- function(input, output) {
        bc <- eventReactive(input$goButton, {
                token_tbl <- tokenize_text(input$txt)
                lookup_word(token_tbl)     
        })
        output$top10 <- renderTable({
                bc()
        })
}
# Run the application 
shinyApp(ui = ui, server = server)