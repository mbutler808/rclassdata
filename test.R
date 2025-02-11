head(iris)		# iris is a built-in dataset. Print first few lines to screen
names(iris) 	# will not print to console when sourced

plot(iris$Sepal.Length, iris$Sepal.Width)

spp <- unique(iris$Species)  # only unique values
spp <- as.character(spp)     # factor -> character
spp            # will not print to console when sourced

print('Species names')  # will print
print(spp)              # will print

cat('\n', 'Species names =', spp)  # concatenate 
							# \n is a carriage return character
summary(iris)							