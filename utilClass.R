library(geometry)
library(units)
library(jsonlite)
library(ggplot2)
library(ggsoccer)
library(dplyr)
library(tidyr)
library(logr)

calculate_triangle_area_goal <- function(x1, y1) {
  #stolpe 1 (120,44)
  x2=120
  y2=44
  #stolpe 2 (120,36)
  x3=120
  y3=36
  
  area <- 0.5 * abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
  return(area)
}

calculate_triangle_area <- function(x1, y1, x2, y2, x3, y3) {
  area <- 0.5 * abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
  return(area)
}

is_opponent_inside_triangle <- function(o_x, o_y,shooter_x,shooter_y) {
  # Coordinates of the three vertices of the triangle
  #goalkeeper_x=117
  #goalkeeper_y=40
  x1 <- 120
  y1 <- 44
  x2 <- 120
  y2 <- 36
  x3 <- shooter_x
  y3 <- shooter_y
  
  # Calculate the area of the entire triangle
  total_area <- calculate_triangle_area(x1, y1, x2, y2, x3, y3)
  
  # Calculate the area of three sub-triangles formed by the goalkeeper and two vertices of the triangle
  area1 <- calculate_triangle_area(o_x, o_y, x2, y2, x3, y3)
  area2 <- calculate_triangle_area(x1, y1, o_x, o_y, x3, y3)
  area3 <- calculate_triangle_area(x1, y1, x2, y2, o_x, o_y)
  
  # If the sum of the areas of the sub-triangles is equal to the total area, the goalkeeper is inside the triangle
  trisum = (area1 + area2 + area3)
  diff=trisum - total_area < 0.1
  return(diff)
}

plotSingleShotTri <-  function(shot) {
  g1=c(120, 36)
  g2=c(120,44)
  x=c(shot[1],g1[1],g2[1],shot[1])
  y=c(shot[2],g1[2],g2[2],shot[2])
  resshot=data.frame(x,y)
  colnames(resshot)=c("sx","sy")
  return(resshot)
}