#require( ...addonpackage... )    # anything between ... needs to be changed 
      # if none, then you don't need that line

#dat <- read.csv(..."your input file.csv"... )       # input data

print(iris)
print("AAAA") 

# Your lines of code to run analyses
# You may have output or processed data that you want to save, 
# create an object for it and write it out to a csv file at the end 

# plot graphics

write.csv(iris, file="iris-data.csv")      # output data 