# K-Means Clustering 



# Importing the dataset1 
#getwd() 
#dataset1 = read.csv('Mall_Customers.csv') 
dataset1 = read.csv(file.choose()) 
dataset1 = dataset1[4:5]
dataset1 <- na.omit(dataset1)

# Using the elbow method to find the optimal number of clusters 
set.seed(6) 

wcss = vector() 
for (i in 1:10) wcss[i] = sum(kmeans(dataset1, i)$withinss) 
plot(1:10, 
     wcss, 
     type = 'b', 
     main = paste('The Elbow Method'), 
     xlab = 'Number of clusters', 
     ylab = 'WCSS')




# Fitting K-Means to the dataset1 
set.seed(29) 
kmeans = kmeans(x = dataset1, centers = 6) 

y_kmeans = kmeans$cluster 
y_kmeans

# Visualising the clusters 
library(cluster) 
clusplot(dataset1, 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 2, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste('Clusters of customers'), 
         xlab = 'Annual Income', 
         ylab = 'Spending Score')

