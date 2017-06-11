# 3 ways to increase the speed of plotting the country borders from shape files for R
#
# (1) We can extract the coordinates from the shape files to get the longitude and latitudes of the polygons. 
# Then we can put them into a data frame with the first column containing the longitudes and the second column 
# containing latitudes. The different shapes are separated by NAs.
# 
# (2) We can remove some polygons from our shape file. The shape file is very, very detailed, but some of the 
# shapes are tiny islands that are unimportant (for my plots, anyways). We can set a minimum polygon area 
# threshold to keep the bigger polygons.
# 
# (3) We can simplify the geometry of our shapes using the Douglas-Peuker algorithm. The edges of our polygon 
# shapes can be simplified, as they are very intricate in the original file. Fortunately, there is a package, 
# rgeos, that implements this.
# 
# https://gis.stackexchange.com/questions/62292/how-to-speed-up-the-plotting-of-polygons-in-r

# Load packages
library(rgdal)
library(raster)
library(sp)
library(rgeos)

# Convert the polygons into data frames so we can make lines
poly2df <- function(poly) {
    # Convert the polygons into data frames so we can make lines
    # Number of regions
    n_regions <- length(poly@polygons)
    
    # Get the coords into a data frame
    poly_df <- c()
    for(i in 1:n_regions) {
        # Number of polygons for first region
        n_poly <- length(poly@polygons[[i]]@Polygons)
        print(paste("There are",n_poly,"polygons"))
        # Create progress bar
        pb <- txtProgressBar(min = 0, max = n_poly, style = 3)
        for(j in 1:n_poly) {
            poly_df <- rbind(poly_df, NA, 
                             poly@polygons[[i]]@Polygons[[j]]@coords)
            # Update progress bar
            setTxtProgressBar(pb, j)
        }
        close(pb)
        print(paste("Finished region",i,"of",n_regions))
    }
    poly_df <- data.frame(poly_df)
    names(poly_df) <- c('lon','lat')
    return(poly_df)
}

# Get the main polygons, will determine by area.
getSmallPolys <- function(poly, minarea=0.05) {
    # Get the areas
    areas <- lapply(poly@polygons, 
                    function(x) sapply(x@Polygons, function(y) y@area))
    
    # Quick summary of the areas
    print(quantile(unlist(areas)))
    
    # Which are the big polygons?
    bigpolys <- lapply(areas, function(x) which(x > minarea))
    length(unlist(bigpolys))
    
    # Get only the big polygons
    for(i in 1:length(bigpolys)){
        if(length(bigpolys[[i]]) >= 1){
            poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
            poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
        }
    }
    return(poly)
}

# Load the shape files
chn <- getData('GADM', country="CHN", level=1)
chn <- gSimplify(chn, tol=0.15, topologyPreserve=TRUE)
chn <- getSmallPolys(chn)
# chn <- poly2df(chn)

saveRDS(chn, "chn_min.rds")

ptm <- proc.time()
plot(chn, col = 'lightgrey', border = 'darkgrey')
proc.time() - ptm
