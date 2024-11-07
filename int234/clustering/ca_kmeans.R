# K-Means Clustering 

# Importing the dataset 
# Assuming you have the dataset ready as shown 
d
# Check the structure of the dataset
print(head(data))

# Using the elbow method to find the optimal number of clusters 
set.seed(6) 
wcss = vector() 
for (i in 1:10) {
  wcss[i] = sum(kmeans(data,i)$withinss) 
}

# Plotting the elbow method
plot(1:10, 
     wcss, 
     type = 'b', 
     main = 'The Elbow Method', 
     xlab = 'Number of clusters', 
     ylab = 'WCSS')

# Fitting K-Means to the dataset 
set.seed(25) 
kmeans = kmeans(x = data, centers = 4)  # Adjust the centers based on the elbow plot 

y_kmeans = kmeans$cluster 
print(y_kmeans)  # Display cluster assignments

# Visualising the clusters 
library(cluster) 
clusplot(data, 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 2, 
         plotchar = FALSE, 
         span = TRUE, 
         main = 'Clusters of Students', 
         xlab = 'CGPA', 
         ylab = 'Marks Obtained')

