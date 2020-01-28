############## iris dataset #####
head(iris)
install.packages("ggfortify")
library(ggfortify)
df = iris[-5]
autoplot(prcomp(df), data = iris, 
         colour = "Species")

# Draw eigenvalues
autoplot(prcomp(df), data = iris, 
         colour = 'Species', loadings = TRUE,
         loadings.label = TRUE)

ggbiplot(prcomp(df))





############## mtcars dataset ##########
#### From DATACAMP
# PCA works the best with numeric data, so drop 2 categorical variables
mtcars.pca = prcomp(mtcars[ , c(1:7, 10, 11)], 
                    center = TRUE, scale. = TRUE)

# Plot the PCA
ggbiplot(mtcars.pca, labels = rownames(mtcars))
## Commment: Variables hp, cyl, disp all contribute to PC1, 
## with higher values in those variables moving the samples to the right.

## After labelling each point with the rownames(sample names),
## we can now see which cars are similar to one another.

## Pass the country variable to cars
mtcars.country = c(rep("Japan", 3),
                   rep("US", 4),
                   rep("Europe", 7),
                   rep("US", 3), "Europe",
                   rep("Japan", 3),
                   rep("US", 4),
                   rep("Europe", 3), "US",
                   rep("Europe", 3))

ggbiplot(mtcars.pca, ellipse = TRUE, labels = rownames(mtcars),
         groups = mtcars.country)

### WHAT PCA TELLS US??????????
### we can see a clear separation between American and Japanese cars 
### along a principal component that is closely correlated to cyl, disp, wt, and mpg. 
### This provides us with some clues for future analyses;
### if we were to try to build a classification model to identify the origin of a car, 
### these variables might be useful.




library(ggbiplot)

##### From STATQUEST
# Plot first 2 PCs
plot(mtcars.pca$x[ , 1], mtcars.pca$x[ , 2])

# Get standard deviation to see 
# how much variation in the original data each PC accounts for.
pca.var = mtcars.pca$sdev^2
# In percentage
pca.var.per = round(pca.var / sum(pca.var) * 100, 1)
# Plot the percentage
barplot(pca.var.per, main = "Scree Plot", 
        xlab = "Principal Component", ylab = "Percent Variation")

library(ggplot2)
head(mtcars)
# Format data in the way ggplot2 likes
pca.data = data.frame(Sample = rownames(mtcars.pca$x),
                      X = mtcars.pca$x[ , 1], 
                      Y = mtcars.pca$x[ , 2])
ggplot(data = pca.data, aes(x = X, y = Y, label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("My PCA Graph")


# Use loading scores to determine which genes have the largest effects 
# on where samples are plotted in the PCA plot
mtcars.pca$rotation

# Variables that push samples to the left side of the graph have large negative values
# Variables that push samples to the right side of the graph have large positive values

# Get the name of the top 9 variables that contribute the most to PC1
loading_scores = mtcars.pca$rotation[ , 1] 
# We are interested in both sets of variables, 
# use abs() to sort based on the number's magitude
variable_scores = abs(loading_scores)
variable_scores_ranked = sort(variable_scores, decreasing = TRUE)
top_9_variables = names(variable_scores_ranked)
mtcars.pca$rotation[top_9_variables, 1]  ## show the scores(and +/- sign)
# The variables with + scores, push to the samples to the right side of the graph
# The variables with - scores, push to the samples to the left side of the graph
