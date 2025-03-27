#library(stats)        # stats is part of the base package and doesn't need to be loaded, 
    # but if you need an add-on package, you would use library or require here.

dat <- read.csv ("Data/morphpre.csv")  # read in data

lm.HLSVL <- lm(dat$HandL ~ dat$SVL)   # run a linear model

summary(lm.HLSVL) # get summary statistics
# str(lm.HLSVL) # look at the linear model object
coef(lm.HLSVL)[2] # get the slope of the regression

plot(dat$HandL ~ dat$SVL, cex=2)    # make a plot with big dots (cex controls size of symbols)
abline(lm.HLSVL, col="red")              # plots the regression line, in red
  title("Microhylid Hand Length vs Body Size")  # add a title
  text(x=15, y=13, paste("slope = ", coef(lm.HLSVL)[2]))   
      # add important info to the text


####### Plot to PDF ######################
pdf(file="MicrohylidHandLvsSize.pdf") # open pdf device for printing
  plot(dat$HandL ~ dat$SVL, cex=2)  # remake plot as before
  abline(lm.HLSVL, col="red")              
    title("Microhylid Hand Length vs Body Size")  
    text(x=15, y=13, paste("slope = ", coef(lm.HLSVL)[2]))  
dev.off() # turn off pdf device so future plots go back to screen