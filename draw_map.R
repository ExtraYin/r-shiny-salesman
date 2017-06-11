# Inspired by Todd Schneider's fantastic work
# http://toddwschneider.com/posts/traveling-salesman-with-simulated-annealing-r-and-shiny/
# https://github.com/toddwschneider/shiny-salesman

require(plotrix)  # put labels on map
require(geosphere)  # calculate geographic distance between 2 lists of lat/lon coordinates

# load rds file
chn <- readRDS("./data/chn_min.rds")

# load cities
chn_cities <- read.csv("./data/chn_cities.csv", stringsAsFactors = FALSE)

generate_capital_cities <- function(){
    name_capital_cities <- c("Hefei", "Fuzhou", "Lanzhou", "Guangzhou", "Guiyang",    # 22 Province capitals
                             "Haikou", "Shijianzhuang", "Harbin", "Zhengzhou", "Wuhan", 
                             "Changsha", "Nanjing", "Nanchang", "Changchun", "Shenyeng", 
                             "Xining", "Xian", "Jinan", "Taiyuan", "Chengdu", 
                             "Kunming", "Hangzhou", 
                             "Nanning", "Hohhot", "Yinchuan", "Lhasa", "Urumqi",    # 5 regions capitals
                             "Beijing", "Chongqing", "Shanghai", "Tianjin")    # 4 Other cities
    capital_cities <- chn_cities[chn_cities$city %in% name_capital_cities, ]
    return(capital_cities)
}

generate_random_cities <- function(n = 20, min_pop = 250000) {
    candidate_names <- chn_cities[chn_cities$pop >= min_pop, "city"]
    selected_cities <- sample(candidate_names, min(n, length(candidate_names)))
    return(chn_cities[chn_cities$city %in% selected_cities, ])
}

plot_city <- function(cities, label_cities=TRUE){
    plot(chn, col='lightgrey', border='darkgrey', asp=1)
    points(x=cities$lng, y=cities$lat, pch=16, cex=0.6, col="black")
    if(label_cities){
        thigmophobe.labels(x=cities$lng, y=cities$lat, labels=cities$city, cex=0.6, offset=0.2)
    }
}

plot_tour <- function(cities, tour, label_cities=TRUE) {
    plot_city(cities, label_cities)
    if(length(tour) > 1){
        tour <- c(tour, tour[1])   # make circle
        tour_order <- unlist(sapply(tour, function(x) which(x==cities$city)))
        lines(x=cities[tour_order, "lng"], y=cities[tour_order, "lat"])
    }
}

calculate_distance_matrix <- function(cities){
    distance_matrix <- distm(cbind(cities$lng, cities$lat))
    dimnames(distance_matrix) <- list(cities$city, cities$city)
    return(distance_matrix)
}

calculate_tour_distance <- function(tour, dist_matrix) {
    sum(dist_matrix[embed(c(tour, tour[1]), 2)])
}

current_temperature <- function(iter, initial_temperature=1e8, alpha=0.99998) {
    initial_temperature * alpha ^ iter
}

run_intermediate_annealing_process <- function(cities, dist_matrix, 
                                               tour, tour_distance, best_tour, best_distance, 
                                               initial_temperature, alpha, 
                                               starting_iteration, number_of_iterations) {
    for(i in 1:number_of_iterations) {
        iter <- starting_iteration + i
        candidate_tour <- tour
        swap <- sample(nrow(cities), 2)
        candidate_tour[swap] <- candidate_tour[rev(swap)]
        candidate_dist <- calculate_tour_distance(candidate_tour, dist_matrix)

        ratio <- min(1, exp((tour_distance - candidate_dist) / current_temperature(iter, initial_temperature, alpha)))
        if (ratio >= runif(1)) {
            tour <- candidate_tour
            tour_distance <- candidate_dist
            if (tour_distance < best_distance) {
                best_tour <- tour
                best_distance <- tour_distance
            }
        }
    }
    return(list(tour=tour, tour_distance=tour_distance, best_tour=best_tour, best_distance=best_distance))
}

################################################################################################################################
# 
################################################################################################################################
default_annealing_process <- function(){
    cities <- generate_capital_cities()
    distance_matrix <- calculate_distance_matrix(cities)
    tour <- sample(cities$city)
    tour_distance <- calculate_tour_distance(tour, distance_matrix)
    best_tour = c()
    best_distance = Inf
    total_iterations = 500000
    plot_every_iterations = 10000
    number_of_loops = ceiling(total_iterations / plot_every_iterations)
    distances = rep(NA, number_of_loops)
    iter = 0
    
    while(iter < total_iterations){
        intermediate_results = run_intermediate_annealing_process(
            cities = cities,
            dist_matrix = distance_matrix,
            tour = tour,
            tour_distance = tour_distance,
            best_tour = best_tour,
            best_distance = best_distance,
            initial_temperature = 1e8, 
            alpha = 0.99998, 
            starting_iteration = iter,
            number_of_iterations = plot_every_iterations
        )
        
        tour = intermediate_results$tour
        tour_distance = intermediate_results$tour_distance
        best_tour = intermediate_results$best_tour
        best_distance = intermediate_results$best_distance
        
        iter = iter + plot_every_iterations
        
        distances[ceiling(iter / plot_every_iterations)] = intermediate_results$tour_distance
        
        plot_tour(cities, tour)
        pretty_dist = prettyNum(tour_distance, big.mark=",", digits=0, scientific=FALSE)
        pretty_iter = prettyNum(iter, big.mark=",", digits=0, scientific=FALSE)
        plot_title = paste0("Distance: ", pretty_dist, " meters\n",
                            "Iterations: ", pretty_iter, "\n")
        title(plot_title)
    }
    plot(distances, type="l")
}

# default_annealing_process()