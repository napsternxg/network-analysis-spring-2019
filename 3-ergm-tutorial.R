## Based on https://statnet.github.io/Workshops/ergm_tutorial.html

library(statnet)
library(sna)
sessionInfo()

data(package='ergm') # tells us the datasets in our packages

data(florentine) # loads flomarriage and flobusiness data
flomarriage # Look at the flomarriage network properties (uses `network`)

par(mfrow=c(1,2)) # Setup a 2 panel plot
plot(flomarriage, main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage)) # Plot the network
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth

plot(flomarriage, vertex.cex=wealth/25, main="Florentine marriage by wealth", cex.main=0.8) # Plot the network with vertex size proportional to wealth


summary(flomarriage ~ edges) # Look at the $g(y)$ statistic for this model


flomodel.01 <- ergm(flomarriage ~ edges) # Estimate the model 
summary(flomodel.01) # Look at the fitted model object


# What this means is that the probability of a network edge is
exp(flomodel.01$coef)/(1+exp(flomodel.01$coef))

# This is same as the network density.

gden(flomarriage)