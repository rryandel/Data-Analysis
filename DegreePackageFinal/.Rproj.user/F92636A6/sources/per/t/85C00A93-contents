#' This function takes a matrix or data frame that contains a set of latitudes and longitudes
#' in degree notation. It will convert the degree coordinates to decimal degrees and reassign
#' the given matrix to the newly converted values. There is also an option for direction.
#' When set to true, the function will give the user a direction
#' calculcated by the average of all points.
#' @usage DegreeConv(long_data, lat_data, Direction = FALSE)
#' @param long_data a matrix or data frame
#' @param lat_data a matrix or data frame
#' @param Direction enables users to add a mean direction if curious
#' @param value A list or data frame of converted data
#' @aliases DegreeConv
#' @aliases value
#' @value a matrix that contains converted decimal degree values
#' @return matrix thats contains converted decimal degree values
#' @export

# This part is necessary to test and run the function.
# Users should already have this in their code
# This should be deleted or edited before use
data <- read.csv("~/Desktop/School/Fall 2022/Programming/Assingment 2/gw_data.csv")
long_data <- data.frame(data$Longitude)
lat_data <- data.frame(data$Latitude)
DegreeConv <- function(long_data, lat_data, Direction=FALSE){

  #Conversion of Longitude
  for (i in 1:nrow(long_data)){
    hour <- as.numeric(substring(long_data[i, ], 1, 2))
    temp <- gsub("[^-.0-9]", "", long_data[i, ])
    min <- as.numeric(gsub(hour, "", temp))
    degree <- (hour + (min/60))
    long_data[i, ] <- degree
  }

  #Conversion of Latitude
  for (i in 1:nrow(lat_data)){
    hour <- as.numeric(substring(lat_data[i, ], 1, 2))
    temp <- gsub("[^-.0-9]", "", lat_data[i, ])
    min <- as.numeric(gsub(hour, "", temp))
    degree <- (hour + (min/60))
    lat_data[i, ] <- degree
  }


  x <- as.matrix(lat_data)
  z <- as.matrix(long_data)
  x <- mean(as.integer(x))
  z <- mean(as.integer(z))

  # Direction TF statement
  if (Direction == TRUE){

    if (x > 0){
      D <- as.character("North")
    }
    else{
      D <- as.character("South")
    }

    if (z > 0){
      R <- as.character("East")
    }
    else{
      R <- as.character("West")
    }
    cat("The average area of these coordinates is located in:", D, R)
  }

  return(cbind(long_data, lat_data))

}





