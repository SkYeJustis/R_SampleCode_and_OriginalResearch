# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 

# Reference: http://rpubs.com/sinhrks/plot_pca
# http://stackoverflow.com/questions/13837873/princomp-results
#   Principal Component Regression
# http://stats.stackexchange.com/questions/57467/how-to-reconstruct-original-data-using-a-small-number-of-principal-components
data = read.csv(file="2016_jawbone_edited_num.csv", header=T)
summary(data)


data = subset(data, select = c(-s_count)) #Rmv a constant variable that prevents PCR analysis

summary(data)

rem.data <- data[,1:27]
rem = data[, 28]
  


rem.pca <- princomp(rem.data, cor=TRUE)

pcascores = rem.pca$scores



summary(rem.pca) # print variance accounted for 
loadings(rem.pca) # pc loadings 
plot(rem.pca,type="lines") # scree plot 
rem.pca$scores # the principal components
biplot(rem.pca)

rem.pca$scale
rem.pca$center

rem.pca$scores


#Transform dummy variable to discrete variable
rem = as.data.frame(rem)
rem$rem[rem == 1] <- "moreThan30mins"
rem$rem[rem == 0] <- "lessThan30mins"

library(reshape2)
library(ggplot2)
library(ggfortify)

autoplot(rem.pca, data = data, colour = 'Num.s_rem',
         loadings = TRUE, loadings.label = TRUE,
         loadings.label.size = 3)

##########
# prcomp #
##########


pca2.rem = prcomp(data, scale = TRUE)

x = pca2.rem$rotation

pca2.rem$x

plot(pca2.rem)
summary(pca2.rem)

biplot(pca2.rem, choices=1:2)
biplot(pca2.rem, choices=3:4)
biplot(pca2.rem, choices=5:6)



std_dev <- pca2.rem$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")


