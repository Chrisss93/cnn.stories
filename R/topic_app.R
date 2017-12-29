library(shiny)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(rmongodb)
library(radarchart)

# mongo <- cnn.stories::mongoConnect()
# ns    <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")
mongo <- mongo.create()
ns <- "test.junk"

ui <- dashboardPage(
    dashboardHeader(title = "Hey you"),
    dashboardSidebar(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
        checkboxInput("searchCase", "Case-sensitive", value = FALSE),
        dateRangeInput("date", "When?", Sys.Date() - 30, Sys.Date(), format = "M d, yyyy"),
        textOutput("days"),
        verbatimTextOutput("inn")
    ),
    dashboardBody(
        fluidRow(
            box(dataTableOutput("story_dt"),
                uiOutput("lt"),
                plotOutput("plot"),
                title = "Data"),
            box(title = "Topic distribution",
                chartJSRadarOutput("radar"),
                numericInput("topic_n", "How many topics?", value = 5, min = 2, max = 15)
            )
        )
    )
)

server <- function(input, output, session) {
    articles <- eventReactive(input$searchButton, {
        validate(
            need( nchar(input$searchText) > 1, "I need more than a letter"),
            need( !grepl("(?!')[[:punct:]]", input$searchText, perl = TRUE),
                  "No symbols permitted except apostrophes")
        )
        query <- list("$text" = list("$search" = input$searchText,
                                     "$caseSensitive" = input$searchCase),
                      "date"  = list("$gte" = as.POSIXct(input$date[1]),
                                     "$lte" = as.POSIXct(input$date[2])),
                      "topics" = list("$exists" = TRUE))
        out <- mongo.find.all(mongo, ns, query,
                              fields = list("_id" = 0, url = 1, title = 1, topics = 1, source = 1,
                                            score = list("$meta" = "textScore")))
        validate(need(length(out) > 0, "No articles found under these criteria."))
        names(out) <- sapply(out, "[[", "title")
        # browser()
        return(out)
    })

    output$lt <- renderUI({
        print(paste("ui says:", length(articles())))
        checkboxGroupInput("chosen_docs", "Articles", choices = c("All", names(articles())), "All")
    })

    output$story_dt <- renderDataTable({
        print(paste("dt says:", length(articles())))
        lapply(articles(), "[", c("title", "url", "source")) %>%
            bind_rows() %>%
            transmute(Article = sprintf("<a href='%s'>%s</a>", url, title), Subdomain = source) %>%
            arrange(desc(Score))
    }, escape = FALSE)

    output$plot <- renderPlot({
        print("plot says hi")
        barplot(table( sapply(articles(), "[[", "source")))
    })

    # output$radar <- renderChartJSRadar({
    #     print("radar replies: 'I'm here!'")
    #     topics <- lapply(articles(), function(a) a$topics[[as.character(input$topic_n)]] ) %>%
    #         bind_rows() %>%
    #         bind_rows( summarize_all(., mean) ) %>%
    #         t() %>%
    #         as.data.frame() %>%
    #         setNames( c(names(articles()), "All") )
    #     print(row.names(topics))
    #     chartJSRadar(topics[, "All", drop = FALSE], row.names(topics))
    # })

    output$inn <- renderPrint({
        for (nm in names(input)) {
            print(paste(nm, input[[nm]], sep = ": "))
        }
    })

    session$onSessionEnded(function() {
        mongo.destroy(mongo);
        cat("Connection terminated.")
    })
}

shinyApp(ui, server)
