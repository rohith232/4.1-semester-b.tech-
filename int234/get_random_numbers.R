# consider the vector 
data=c(23,45,21,34,5,6,7,8,86,40,3) 

# get 4 random elements 
print(sample(data,4,replace=TRUE)) #replace=true mean it allows duplicate values

# get 1 random element 
print(sample(data,1,replace=TRUE)) 

# get 6 random elements 
print(sample(data,6,replace=TRUE)) 


# get 4 random elements 
print(sample(data,4,replace=FALSE)) #replace=false mean it does not allows duplicate values

# get 1 random element 
print(sample(data,1,replace=FALSE)) 

# get 6 random elements 
print(sample(data,6,replace=FALSE))


data1=c(2,2,2,2,2,2)  #in this case 

#replace=true mean it gives duplicate values
#replace=false mean it gives same elements from other index


# get 4 random elements  
print(sample(data1,4,replace=TRUE))  
#print(sample(data1,4,replace=FALSE))  

data1=c(2)  

# get 4 random elements  
print(sample(data1,4,replace=TRUE))  
#print(sample(data1,4,replace=FALSE))
