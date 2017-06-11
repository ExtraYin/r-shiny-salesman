# Inspired by Todd Schneider's fantastic work
# http://toddwschneider.com/posts/traveling-salesman-with-simulated-annealing-r-and-shiny/
# https://github.com/toddwschneider/shiny-salesman

library(shiny)

# load cities
chn_cities <- read.csv("./data/chn_cities.csv", stringsAsFactors = FALSE)

shinyUI(fluidPage(
    title = "TSP Tours of the 31 Provincial Capital Cities in China",
    tags$h2(tags$a(href="/", "China Map", target="_blank")),
    fluidRow(
        column(5, plotOutput("map", height="500px")), 
        column(3, 
               plotOutput("annealing_schedule", height="220px"), 
               plotOutput("distance_results", height="220px")
               )
    ), 
    fluidRow(
        column(4,
               tags$ol(
                   tags$li("Customize the list of cities, based on the China map"),
                   tags$li("Adjust simulated annealing parameters to taste"),
                   tags$li("Click the 'SOLVE' button!")
               )
        ),
        column(2,
               tags$button("SOLVE", id="go_button", class="btn btn-info btn-large action-button shiny-bound-input")
        ),
        column(2,
               tags$button("PAUSE", id="pause_button", class="btn btn-info btn-large action-button shiny-bound-input")
        )
    ),
    hr(),
    fluidRow(
        column(4,
               p("Type below to select individual cities, or", actionButton("set_random_cities", "Select Randomly", icon=icon("refresh"))),
               selectizeInput("cities", NA, chn_cities$city, multiple=TRUE, width="100%",
                              options = list(maxItems=50, maxOptions=100, placeholder="Start typing to select some cities...",
                                             selectOnTab=TRUE, openOnFocus=FALSE, hideSelected=TRUE))
        ),
        
        column(4,
               h4("Simulated Annealing Parameters"),
               inputPanel(
                   numericInput("initial_temperature", "Initial Temperature", 1e8, min=0, max=10000000),
                   numericInput("alpha", "Annealing Rate", 0.99998, min=0, max=1), 
                   numericInput("total_iterations", "Number of Iterations to Run", 500000, min=0, max=1000000), 
                   numericInput("plot_every_iterations", "Draw Map Every N Iterations", 10000, min=1, max=1000000)
               ),
               class="numeric-inputs"
        )
    )
))




