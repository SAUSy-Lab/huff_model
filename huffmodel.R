#Huff Model - 2018-06-18
#based off of: https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf

#geometric mean function
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

lt = function(var,vec){
  #takes a variable [var] and a vector [vec] to computer the linear transform of 
  #log(variable/gm_mean(variable across dataset))
  log(var/gm_mean(vec))
}

#######################
#TOY PROBLEM/FAKE DATA
#######################

#Attractiveness data
#j x h dataframe, j = number of observations, h = number of attractiveness dimensions

attract_data = matrix(c(120, 50, 5, 112,8,184,1000,1440,470), nrow = 3, ncol = 3) #rows are j, cols are dimensions 

# > attract_data
#       [,1]  [,2]   [,3]
# [1,]   120   112   1000
# [2,]    50     8   1440
# [3,]     5   184    470

#######################

#Distance data
#cost matrix between all i (demand points) and j (stores)
d_ij = matrix(c(1, 10, 4, 13, 14, 12, 31, 15, 14, 14, 1, 8), nrow = 4, ncol = 3)

# > d_ij
# i  j->[,1] [,2] [,3]
# [1,]    1   14   14
# [2,]   10   12   14
# [3,]    4   31    1
# [4,]   13   15    8

#######################

#Market share (proportion) data
#what is the prob a person in zone i shops at store j? data are in i x j matrix

prop_ij = matrix(c(.7,.4,.3,.1,.15,.3,.05,.1,.15,.3,.65,.8),nrow = 4,ncol=3)

# > prop_ij
#  i  j->[,1] [,2] [,3]
# [1,]  0.70  0.15  0.15
# [2,]  0.40  0.30  0.30
# [3,]  0.30  0.05  0.65
# [4,]  0.10  0.10  0.80

#######################

#create empty dataframe for use in regression model used to predict parameters
number_cols = (3+length(attract_data[1,])) #number of columns needed for param est dataframe
par.df = data.frame(matrix(ncol = number_cols, nrow = 0)) #generate parameter estimate dataframe
x = c("id","proportion", "attract1", "attract2", "attract3", "distance") #hardcoded labels...
colnames(par.df) = x

for (i in 1:length(d_ij[,1])){
  for(j in 1:length(d_ij[1,])){
    tempdf = data.frame(id=i, 
                        proportion=lt(prop_ij[i,j],prop_ij[i,]),
                        attract1=lt(attract_data[j,1],attract_data[,1]),
                        attract2=lt(attract_data[j,2],attract_data[,2]),
                        attract3=lt(attract_data[j,3],attract_data[,3]),
                        distance=lt(d_ij[i,j],d_ij[i,]))
    par.df = rbind(par.df, tempdf)
    }
}

#######################
#use 'regression through the origin', per huff documentation ... why not regular ols?
#drop attract3 ... singularity for some reason ... not correlated to other vars.
param_est = lm(par.df$proportion~par.df$distance)
#param_est = lm(par.df$proportion~par.df$attract1+par.df$attract2+par.df$attract3 + par.df$distance)  
print(summary(param_est))

######################
#compile predicted proportions...
fitmatrix = matrix(param_est$fitted.values,nrow=4,ncol=3, byrow = T)
predicted_proportions = exp(fitmatrix)/rowSums(exp(fitmatrix))
x = c("store1", "store2", "store3")
y = c("zone1","zone2","zone3","zone4")
predicted_proportions.df = data.frame(predicted_proportions)
colnames(predicted_proportions.df) = x
rownames(predicted_proportions.df) = y
print(predicted_proportions.df)
print("original proportion data:")
print(prop_ij)

