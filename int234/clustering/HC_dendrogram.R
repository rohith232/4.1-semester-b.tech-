# Hierarchical Clustering

# Importing the dataset
#dataset = read.csv('Mall_Customers.csv')
dataset=read.csv(file.choose())
dataset = dataset[4:5]

# Using the dendrogram to find the optimal number of clusters 
dendrogram = hclust(d = dist(dataset, method = 'euclidean'),  
                    method = 'ward.D') 
plot(dendrogram, 
     main = paste('Dendrogram'), 
     xlab = 'Customers', 
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset 
hc = hclust(d = dist(dataset, method = 'euclidean'),  
            method = 'ward.D') 
y_hc = cutree(hc, 2) 
y_hc 

# Visualising the clusters 
library(cluster) 
clusplot(dataset, 
         y_hc, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels= 2, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste('Clusters of customers'), 
         xlab = 'Annual Income', 
         ylab = 'Spending Score')
