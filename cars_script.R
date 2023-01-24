# cars <- read.csv( "cars.csv")  # needed if you were reading in the file cars.csv
cars     # but cars is a built-in dataset - it comes with R

speed <-  cars$speed  # save speed as a vector
dist <-  cars$dist

plot( speed, dist, cex=2)  # plot x,y
speed.lm <- lm (dist ~ speed)   # save regression line

abline( speed.lm, col="red" )   # plot regression line - you now have the best fit line plotted with the points

par(mfrow=c(2,2))   # this sets the plot environment to do 4 plots, 2 by 2
plot(speed.lm)  # the default plot method for lm - crazy huh?

write.csv(cars, "cars.csv")  # save data to csv file in working directory