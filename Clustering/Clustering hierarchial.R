
# Load the data
heart_disease <- read.csv("C:/personal files/data analytics/docs/datacamp project/kmeans/heart_disease_patients.csv")

# Print the first ten rows
# .... YOUR CODE FOR TASK 1 ....
head (heart_disease , 10)



soln_heart_disease <- read.csv("C:/personal files/data analytics/docs/datacamp project/kmeans/heart_disease_patients.csv")




# Evidence that the data should be scaled?
# .... YOUR CODE FOR TASK 2 ....
summary (heart_disease)
# Remove id
heart_disease <- heart_disease[ , !(names(heart_disease) %in% c('id'))]

# Scaling data and saving as a data frame
# .... YOUR CODE FOR TASK 2 ....
scaled =as.data.frame(scale(heart_disease))



# What do the data look like now?
summary(scaled)
# .... YOUR CODE FOR TASK 2 ....

soln_heart_disease <- soln_heart_disease[ , !(names(soln_heart_disease) %in% c("id"))]
soln_scaled <- scale(soln_heart_disease)



# Set the seed so that results are reproducible
seed_val  <- 1000
set.seed(seed_val)

# Select a number of clusters
k = 5
# .... YOUR CODE FOR TASK 3 ....

# Run the k-means algorithm
first_clust = kmeans(scaled , centers = k, nstart =100)
first_clust

# How many patients are in each cluster?

first_clust$size

# .... YOUR CODE FOR TASK 3 ....

soln_seed_val <- 10
set.seed(soln_seed_val)
soln_k <- 5
soln_first_clust <- kmeans(soln_scaled, centers = soln_k, nstart = 1)




# Set the seed
seed_val <- 38
set.seed(seed_val)

# Select a number of clusters and run the k-means algorithm
# .... YOUR CODE FOR TASK 4 ....
second_clust = kmeans(scaled , centers = k)

# How many patients are in each cluster?
second_clust$size
# .... YOUR CODE FOR TASK 4 ....

seed_val_2 <- 38
set.seed(seed_val_2)
k_2 <- 5
soln_second_clust <- kmeans(soln_scaled, centers = k_2, nstart = 1)



# Add cluster assignments to the data
heart_disease[,'first_clust'] <- first_clust$cluster
heart_disease[, 'second_clust'] <- second_clust$cluster
# Load ggplot2
# .... YOUR CODE FOR TASK 5 ....
library(ggplot2)


# Create and print the plot of age and chol for the first clustering algorithm
plot_one  <- ggplot(heart_disease, aes(age , chol , color=as.factor(first_clust)) ) +
  geom_point()
# .... YOUR CODE FOR TASK 5 ....
plot_one 

# Create and print the plot of age and chol for the second clustering algorithm
# .... YOUR CODE FOR TASK 5 ....
plot_two = ggplot(heart_disease , aes(age , chol , color = as.factor(second_clust)))+
  geom_point()
plot_two

soln_heart_disease["first_clust"] <- soln_first_clust$cluster
soln_heart_disease["second_clust"] <- soln_second_clust$cluster

# creating the correct graphs and getting fingerprints
soln_plot_one <- ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(first_clust))) + geom_point()
soln_plot_two <- ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(second_clust))) + geom_point()



# Execute hierarchical clustering with complete linkage
hier_clust_1 <- hclust(dist(scaled), method ='complete' )

# Print the dendrogram
# .... YOUR CODE FOR TASK 6 ....
plot(hier_clust_1)

# Get cluster assignments based on number of selected clusters
# .... YOUR CODE FOR TASK 6 ....
hc_1_assign =cutree(hier_clust_1 , k= 5)

soln_hier_clust_1 <- hclust(dist(soln_scaled), method='complete')
soln_hc_1_assign <- cutree(soln_hier_clust_1, 5)


# Execute hierarchical clustering with single linkage
# .... YOUR CODE FOR TASK 7 ....
hier_clust_2 = hclust(dist(scaled) , method= 'single')
hier_clust_2

# Print the dendrogram
# .... YOUR CODE FOR TASK 7 ....
plot(hier_clust_2)

# Get cluster assignments based on number of selected clusters
# .... YOUR CODE FOR TASK 7 ....
hc_2_assign= cutree(hier_clust_2 , k = 5)
head(hc_2_assign)

soln_hier_clust_2 <- hclust(dist(soln_scaled), method = "single")
soln_hc_2_assign <- cutree(soln_hier_clust_2, 5)


# Add assignment of chosen hierarchical linkage
# .... YOUR CODE FOR TASK 8 ....
heart_disease [,'hc_clust'] = hc_1_assign
head(heart_disease)

# Remove the sex, first_clust, and second_clust variables
# .... YOUR CODE FOR TASK 8 ....
hd_simple = heart_disease[,!(names(heart_disease)%in%
                               c('sex' , 'first_clust' , 'second_clust'))]


# Get the mean and standard deviation summary statistics
clust_summary <- do.call(data.frame,
                         aggregate(. ~ hc_clust, data = hd_simple ,
                                   function(x) c(avg = mean(x), sd = sd(x))))
clust_summary

soln_heart_disease["hc_clust"] <- soln_hc_1_assign

soln_hd_simple <- soln_heart_disease[, !(names(soln_heart_disease) %in% c("sex", "first_clust", "second_clust"))]

soln_clust_summary <- do.call(data.frame, aggregate(. ~hc_clust, data = soln_hd_simple, function(x) c(avg = mean(x), sd = sd(x))))




# Plot age and cholf
# .... YOUR CODE FOR TASK 9 ....
plot_one=ggplot(hd_simple , aes ( age , chol , color = as.factor(hc_clust)))+
  geom_point()

# Plot oldpeak and trestbps
# .... YOUR CODE FOR TASK 9 ....
plot_two=ggplot(hd_simple , aes(oldpeak , trestbps , color= as.factor(hc_clust)))+
  geom_point()


soln_plot_one <- ggplot(soln_heart_disease, aes(x = age, y = chol, 
                                                color = as.factor(hc_clust))) + 
  geom_point()
soln_plot_two <- ggplot(soln_heart_disease, aes(x=oldpeak, y=trestbps, 
                                                color=as.factor(hc_clust))) + 
  geom_point()



# Add TRUE if the algorithm shows promise, add FALSE if it does not
explore_kmeans <- FALSE
explore_hierarch_complete <- TRUE
explore_hierarch_single <- FALSE

soln_1 <- FALSE
soln_2 <- TRUE
soln_3 <- FALSE



