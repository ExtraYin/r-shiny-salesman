# Inspired by Todd Schneider's fantastic work
# http://toddwschneider.com/posts/traveling-salesman-with-simulated-annealing-r-and-shiny/
# https://github.com/toddwschneider/shiny-salesman

library(shiny)
source("utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    vals = reactiveValues()
    
    one_time_initialization = observe({
        isolate({
            cty = generate_capital_cities()
            updateSelectizeInput(session, "cities", selected=sort(cty$city))
            vals$cities = cty
            vals$distance_matrix = calculate_distance_matrix(cty)
        })
    }, priority=1000)
    
    set_cities_randomly = observe({
        if (input$set_random_cities == 0) return()
        run_annealing_process$suspend()
        isolate({
            cty = generate_random_cities()
            updateSelectizeInput(session, "cities", selected=sort(cty$city))
            vals$cities = cty
            vals$tour = c()
        })
    }, priority=100)
    
    set_cities_from_selected = observe({
        if (input$go_button == 0) return()
        run_annealing_process$suspend()
        
        isolate({
            cty = subset(chn_cities, city %in% input$cities)
            vals$cities = cty
        })
    }, priority=50)
    
    set_dist_matrix = observe({
        if (input$go_button == 0 & input$set_random_cities == 0) return()
        
        isolate({
            if (nrow(vals$cities) < 2) return()
            dist_mat = calculate_distance_matrix(vals$cities)
            vals$distance_matrix = dist_mat
        })
    }, priority=40)
    
    setup_to_run_annealing_process = observe({
        input$go_button
        
        isolate({
            vals$tour = sample(vals$cities$city)
            vals$tour_distance = calculate_tour_distance(vals$tour, vals$distance_matrix)
            vals$best_tour = c()
            vals$best_distance = Inf
            
            vals$initial_temperature = input$initial_temperature
            vals$alpha = input$alpha
            
            vals$total_iterations = input$total_iterations
            vals$plot_every_iterations = input$plot_every_iterations
            
            vals$number_of_loops = ceiling(vals$total_iterations / vals$plot_every_iterations)
            vals$distances = rep(NA, vals$number_of_loops)
            
            vals$iter = 0
        })
        
        run_annealing_process$resume()
    }, priority=20)
    
    run_annealing_process = observe({
        if (input$go_button == 0) return()
        
        if (nrow(isolate(vals$cities)) < 2) return()
        
        isolate({
            intermediate_results = run_intermediate_annealing_process(
                cities = vals$cities,
                dist_matrix = vals$distance_matrix,
                tour = vals$tour,
                tour_distance = vals$tour_distance,
                best_tour = vals$best_tour,
                best_distance = vals$best_distance,
                initial_temperature = vals$initial_temperature, 
                alpha = vals$alpha, 
                starting_iteration = vals$iter,
                number_of_iterations = vals$plot_every_iterations
            )
            
            vals$tour = intermediate_results$tour
            vals$tour_distance = intermediate_results$tour_distance
            vals$best_tour = intermediate_results$best_tour
            vals$best_distance = intermediate_results$best_distance
            
            vals$iter = vals$iter + vals$plot_every_iterations
            
            vals$distances[ceiling(vals$iter / vals$plot_every_iterations)] = intermediate_results$tour_distance
        })
        
        if (isolate(vals$iter) < isolate(vals$total_iterations)) {
            invalidateLater(0, session)
        } else {
            isolate({
                vals$tour = vals$best_tour
                vals$tour_distance = vals$best_distance
            })
        }
    }, priority=10)
    
    pause_annealing_process = observe({
        if (input$pause_button == 0) return()
        if (input$pause_button %% 2 == 1){
            run_annealing_process$suspend()
        } else {
            run_annealing_process$resume()
        }
        
    }, priority=150)
    
    output$map <- renderPlot({
        plot_tour(vals$cities, vals$tour)
        
        if (length(vals$tour) > 1) {
            pretty_dist = prettyNum(vals$tour_distance, big.mark=",", digits=0, scientific=FALSE)
            pretty_iter = prettyNum(vals$iter, big.mark=",", digits=0, scientific=FALSE)
            pretty_temp = prettyNum(current_temperature(vals$iter, vals$initial_temperature, vals$alpha), big.mark=",", digits=0, scientific=FALSE)
            
            plot_title = paste0("Distance: ", pretty_dist, " meters\n",
                                "Iterations: ", pretty_iter, "\n", 
                                "Temperature: ", pretty_temp)
            title(plot_title)
        }
    }, height=550)
    
    output$annealing_schedule = renderPlot({
        xvals = seq(from=0, to=vals$total_iterations, length.out=100)
        yvals = current_temperature(xvals, vals$initial_temperature, vals$alpha)
        plot(xvals, yvals, type='l', xlab="iterations", ylab="temperature", main="Annealing Schedule")
        points(vals$iter, current_temperature(vals$iter, vals$initial_temperature, vals$alpha), pch=19, col='red')
    }, height=260)
    
    output$distance_results = renderPlot({
        if (all(is.na(vals$distances))) {
            plot(1, type="n", xlim=c(0, vals$total_iterations), ylim=c(0, 1e7), xlab="iterations", ylab="current tour distance", 
                 main="Evolution of Current Tour Distance")
            return()
        }
        xvals = vals$plot_every_iterations * (1:vals$number_of_loops)
        plot(xvals, vals$distances, type='o', pch=19, cex=0.7, 
             ylim=c(0, max(vals$distances, na.rm=TRUE)), xlab="iterations", ylab="current tour distance",
             main="Evolution of Current Tour Distance")
    }, height=260)
    
    session$onSessionEnded(function() {
        run_annealing_process$suspend()
        set_cities_randomly$suspend()
    })
})