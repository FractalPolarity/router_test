#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.router)

p404 <- page404(message404 = "Nie ma takiej strony")

menu <- (
  tags$ul(
    tags$li(a(class = "item", href = "/", "Page")),
    tags$li(a(class = "item", href = route_link("plot"), "Plot page")),
    tags$li(a(class = "item", href = route_link("third"), "Bad link page"))
  )
)

page <- function(title, content) {
  div(
    menu,
    titlePanel(title),
    p(content),
    textInput("number", "Numerek"),
    textOutput("text_one"),
    textOutput("text_two"),
    plotOutput("plot")
  )
}

main_page <- page("Main Page", "This is main")
plot_page <- page("Plot Page", "This is your plot")

main_callback <- function(input, output, session){
  output$text_one <- renderText(ifelse(is.na(as.numeric(input$number)), 0, as.numeric(input$number)))
}

plot_callback <- function(input, output, session){
  output$plot <- renderPlot({
       x    <- faithful[, 2]
       bins <- seq(min(x), max(x), length.out = ifelse(is.na(as.numeric(input$number)), 0, as.numeric(input$number)) + 10)
       hist(x, breaks = bins, col = 'red', border = 'yellow')
    })
}


router <- make_router(
  route("/", main_page, main_callback),
  route("plot", plot_page, plot_callback),
  page_404 = p404
)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  router_ui(),
  actionButton("btn", "Go to plot with d=2")
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  router(input, output, session)
  
  output$text_two <- renderText({print(get_query_param("d"))
    warunek <- ifelse(!is.null(get_query_param("d")), paste("parameter d =", get_query_param("d")), "")
    print(warunek)
    warunek
    # cbind("parameter d =", get_query_param("d"))
    })
  observeEvent(input$btn, {
    change_page("?c=8&d=2#plot")
  })
})

# Run the application 
shinyApp(ui, server)

