###########################################################
###########################################################
##3 network objects: importing, exploring, and manipulating network data

##3.1 Importing Relational Data
#Be sure to be in the directory where you stored the data for the workshop. If you've not set the working directory, you must do so now. See Section 1.7 for how to do this.
#You can use setwd() to change it, or platform specific shortcuts
getwd() # Check what directory you're in
list.files() # Check what's in the working directory

#Read an adjacency matrix (R stores it as a data frame by default)
relations <- read.csv("relationalData.csv",header=FALSE,stringsAsFactors=FALSE)
relations
#Here's a case where matrix format is preferred
relations <- as.matrix(relations) # convert to matrix format

#Read in some vertex attribute data (okay to leave it as a data frame)
nodeInfo <- read.csv("vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)
nodeInfo

#Since our relational data has no row/column names, let's set them now
rownames(relations) <- nodeInfo$name
colnames(relations) <- nodeInfo$name
relations

#Why did we set stringsAsFactors=FALSE?
relations_wFactors<-read.csv("vertexAttributes.csv",header=T,stringsAsFactors=TRUE)
relations_wFactors[1,2]<-"Derek"

numFactor<-as.factor(c(1,3,5))
numFactor+1
as.numeric(numFactor)

#For more information. . .
?list.files
?read.csv
?as.matrix
?rownames

###########################################################
##3.2 Creating network objects, working with edgelists
library(network)

nrelations<-network(relations,directed=FALSE) # Create a network object based on relations
nrelations # Get a quick description of the data
nempty <- network.initialize(5) # Create an empty graph with 5 vertices
nempty # Compare with nrelations

edgelist<-read.csv("edgeList.csv",header=T,stringsAsFactors=F)
head(edgelist)

nedge<-network(edgelist,matrix.type="edgelist")
nedge
nedge[,]
plot(nedge,displaylabels=T)

nedge<-network(edgelist,matrix.type="edgelist",ignore.eval=F,names.eval="weight")
nedge
nedge[,]
as.matrix(nedge,"weight")

plot(nedge,displaylabels=T,edge.lwd=as.sociomatrix(nedge,"weight")*5)

#For more information. . .
?network 
?as.network.matrix

###########################################################
##3.3 Description and Visualization

summary(nrelations) # Get an overall summary
network.dyadcount(nrelations) # How many dyads in nflo?
network.edgecount(nrelations) # How many edges are present?
network.size(nrelations) # How large is the network?
as.sociomatrix(nrelations) # Show it as a sociomatrix
nrelations[,] # Another way to do it

plot(nrelations,displaylabels=T) # Plot with names
plot(nrelations,displaylabels=T,mode="circle") # A less useful layout...

library(sna) # Load the sna library
gplot(nrelations) # Requires sna
gplot(relations) # gplot Will work with a matrix object too

#For more information. . .
?summary.network
?network.dyadcount
?network.edgecount
?as.sociomatrix
?as.matrix.network
?is.directed

###########################################################
##3.4 Working With Edges
#Adding Edges
g <- network.initialize(5) # Create an empty graph
g[1,2] <- 1 # Add an edge from 1 to 2
g[2,] <- 1 # Add edges from 2 to everyone else
g # Examine the result
m <- matrix(0, nrow=5, ncol=5) # Create an adjacency matrix
m[3,4:5] <- 1 # Add entries from 3 to 4 and 5
g[m>0] <- 1 # Add more entries
g

#Delete edges
g[3,5] <- 0 # Remove the edge from 3 to 5
g # Its gone!
g[,] <- 0 # Remove all edges
g # Now, an empty graph

#Testing adjacency
nrelations[4,5] # Emma to Sarah?
nrelations[4,] # Entire Emma row
nrelations[1:4,5:8] # Subsets are possible
nrelations[-9,-9] # Leaving Gil out of the picture

#Setting edge values
m<-nrelations[,] # copy over the relational structure
m[upper.tri(m)>0]<-rep(1:3,times=3) # give different edge values
m<-symmetrize(m,rule="upper")
m

nrelations %e% "strength" <- m # Add the valued ties back in

#Retrieving edge values
list.edge.attributes(nrelations) # See whats available
nrelations %e% "strength" # Use the %e% operator
as.sociomatrix(nrelations,attrname="strength") # Can also do it this way
nrelations[,] # Original tie structure is preserved

#For more information. . .
?network.extraction
?add.edge
?delete.edges
?delete.vertices
?get.edges
?upper.tri

###########################################################
##3.5 Network and Vertex Attributes 
#Add some attributes
nrelations %v% "id" <- nodeInfo$id # Add in our vertex attributes
nrelations %v% "age" <- nodeInfo$age
nrelations %v% "sex" <- nodeInfo$sex
nrelations %v% "handed" <- nodeInfo$handed
nrelations %v% "lastDocVisit" <- nodeInfo$lastDocVisit

#Listing attributes
list.vertex.attributes(nrelations) # List all vertex attributes
list.network.attributes(nrelations) # List all network attributes

#Retrieving attributes
nrelations %v% "age" # Retrieve vertex ages
nrelations %v% "id" # Retrieve vertex ids

#For more information. . .
?attribute.methods

###########################################################
###########################################################
##4 Classical Network Analysis with the SNA package
##4.1 Getting started
library(sna) # Load the sna library
library(help="sna") # See a list of help pages for sna package
load("IntroToSNAinR.Rdata") # Load supplemental workshop data

###########################################################
##4.2 Network data in SNA
#The sna package can handle network data in many forms. For instance, the function gden() calculates network density; we can use it on a network object, an adjacency matrix, a list of such matrices, etc.

data(flo)
flo # Adjacency form
gden(flo)
nflo<-network(flo,directed=FALSE) # Network form
gden(nflo)

#The sna package also supports a special kind of matrix called an \sna edgelist." These are three-column matrices, each row of which represents an edge (via its sender, recipient, and value, respectively). These sna edgelists" have special attributes that indicate their size, vertex names (if any), and bipartite status (if applicable).
eflo<-as.edgelist.sna(flo) # Coerce flo to an sna edgelist
eflo
attr(eflo,"n") # How many vertices are there?
attr(eflo,"vnames") # Are there vertex names?
as.sociomatrix.sna(eflo) # Can transform back w/ as.sociomatrix.sna 

#For more information. . .
?as.edgelist.sna
?as.sociomatrix.sna
?attr
?sna

###########################################################
##4.3 Network visualization with gplot()
#Plotting the data we imported earlier
gplot(nrelations)
gplot(nrelations,displaylabels=TRUE) # This time with labels
#Let's color the nodes in sex-stereotypic colors
nodeColors<-ifelse(nodeInfo$sex=="F","hotpink","dodgerblue")
gplot(relations,gmode="graph",displaylabels=TRUE,vertex.col=nodeColors)

#Using data we just loaded in, plot the contiguity among nations in 1993 (from the Correlates of War (CoW)1 project)
gplot(contig_1993) # The default visualization
gplot(contig_1993, usearrows=FALSE) # Turn off arrows manually
gplot(contig_1993, gmode="graph") # Can also tell gplot the data is undirected

#We can add labels to the vertices
gplot(contig_1993, gmode="graph",displaylabels=TRUE,label.cex=0.5,label.col="blue")

#Other ways to specify the labeling
gplot(contig_1993,gmode="graph",label=network.vertex.names(contig_1993),label.cex=0.5,label.col="blue")

#Here's an example of directed data|militarized interstate disputes (MIDs) for 1993
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE)

#All those isolates can get in the way|we can suppress them using displayisolates
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE)

#The default layout algorithm is that of Frutchterman-Reingold (1991), can use others
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE,mode="circle") # The infamous circle

#or perhaps
gplot(mids_1993,label.cex=0.5,label.col="blue",displaylabels=TRUE,displayisolates=FALSE,mode="mds") # MDS of position similarity

#When a layout is generated, the results can be saved for later reuse:
coords <- gplot(contig_1993,gmode="graph",label=colnames(contig_1993[,]),label.cex=0.5,label.col="blue") # Capture the magic of the moment
coords # Show the vertex coordinates

#Saved (or a priori) layouts can be used via the coord argument
gplot(mids_1993,gmode="graph",label=colnames(contig_1993[,]),label.cex=0.5,label.col="blue",coord=coords)

#When the default settings are insuficient, interactive mode allows for tweaking
coords <- gplot(contig_1993, interactive=TRUE) # Modify and save
gplot(contig_1993,coord=coords,displaylabels=TRUE,gmode="graph",label.cex=0.5,label.col="blue") # Should reproduce the modified layout

#For more information. . .
?gplot ?gplot.layout

###########################################################
##4.4 Basic centrality indices: degree, betweenness, and closeness
#We begin with the simplest case: degree centrality

#Component information can be obtained in various ways
components(mids_1993) # Strong component count
components(mids_1993, connected="weak") # Weak component count
cd <- component.dist(mids_1993, connected="weak") # Get weak components
cd

#Component sizes
plot(1:length(cd$cdist),cd$cdist,xlab="Size",ylab="Frequency")

#Who's in the largest component?
cl <- component.largest(mids_1993, connected="weak")
cl

#Plot the largest weak component
gplot(mids_1993[cl,cl], 
      boxed.lab=FALSE, 
      label.cex=0.5,
      label.col=4,
      label=network.vertex.names(mids_1993)[cl])

#Likewise, many routines exist for handling isolates
is.isolate(mids_1993, 3) # Is the third vertex (BHM) an isolate?
is.isolate(mids_1993, which(mids_1993%v%"vertex.names"=="BHM")) # The peaceful islands?
is.isolate(mids_1993, which(mids_1993%v%"vertex.names"=="USA")) # Perhaps less so....
isol <- isolates(mids_1993) # Get the entire list of isolates
isol
network.vertex.names(mids_1993)[isol] # Which countries are isolates?

#Another way to remove isolates from sociograms
gplot(mids_1993[-isol,-isol], 
      label.cex=0.5,
      label.col=4,
      label=network.vertex.names(mids_1993)[-isol])

#Geodesic distances
contig.dist<-geodist(contig_1993)

#look at the resulting object
attributes(contig.dist)
contig.dist$gdist
contig.dist$counts


###########################################################
##4.2 Basic centrality indices: degree, betweenness, and closeness
#We begin with the simplest case: degree centrality
degree(mids_1993) # Default: total degree
ideg <- degree(mids_1993, cmode="indegree") # Indegree for MIDs
odeg <- degree(mids_1993, cmode="outdegree") # Outdegree for MIDs
all(degree(mids_1993) == ideg+odeg) # In + out = total?

#Once centrality scores are computed, we can handle them using standard R methods:
plot(ideg, 
     odeg, 
     type="n", 
     xlab="Incoming MIDs", 
     ylab="Outgoing MIDs") # Plot ideg by odeg

abline(0, 1, lty=3)

text(jitter(ideg), 
     jitter(odeg), 
     network.vertex.names(contig_1993), 
     cex=0.75, 
     col=2)

#Plot simple histograms of the degree distribution:
par(mfrow=c(2,2)) # Set up a 2x2 display

hist(ideg, 
     xlab="Indegree",
     main="Indegree Distribution", 
     prob=TRUE)

hist(odeg, 
     xlab="Outdegree", 
     main="Outdegree Distribution", 
     prob=TRUE)

hist(ideg+odeg, 
     xlab="Total Degree", 
     main="Total Degree Distribution", 
     prob=TRUE)

par(mfrow=c(1,1)) # Restore display

#Centrality scores can also be used with other sna routines, e.g., gplot()
gplot(mids_1993, 
      vertex.cex=(ideg+odeg)^0.5/2, 
      vertex.sides=50,
      label.cex=0.4,
      vertex.col=rgb(odeg/max(odeg),0,ideg/max(ideg)),
      displaylabels=TRUE)

#Betweenness and closeness are also popular measures
bet <- betweenness(contig_1993, 
                   gmode="graph") # Geographic betweenness

bet

gplot(contig_1993, 
      vertex.cex=sqrt(bet)/25, 
      gmode="graph") # Use w/gplot

clo <- closeness(contig_1993) # Geographic closeness
clo # A large world after all?

#Can use sna routines to explore alternatives to the common measures. . .
contig_1993_gdist<-geodist(contig_1993)$gdist # matrix of geodesic distances

contig_1993_gdist
1/sum(contig_1993_gdist[1,2:186]) # calculate closeness for country 1

which(contig_1993_gdist[,1]=="Inf") # disconnected matrix

sum(1/contig_1993_gdist[1,2:186]) # alternate closeness for country 1

closeness2 <- function(x){ # Create an alternate closeness function!
  geo <- 1/geodist(x)$gdist # Get the matrix of 1/geodesic distance
  diag(geo) <- 0 # Define self-ties as 0
  apply(geo, 1, sum) # Return sum(1/geodist) for each vertex
}

clo2 <- closeness2(contig_1993) # Use our new function on contiguity data
clo2

hist(clo2, xlab="Alt. Closeness", prob=TRUE) # Much better behaved!
cor(clo2, bet) # Correlate with betweenness
plot(clo2, bet) # Plot the bivariate relationship
all(clo2/185==closeness(contig_1993,cmode="suminvundir")) # Actually, we support this in sna!

#For more information. . .
?betweenness
?bonpow
?closeness
?degree
?evcent
?graphcent
?infocent
?prestige
?stresscent

###########################################################
##4.3 From centrality to centralization
centralization(mids_1993, degree, cmode="indegree") # Do MIDs concentrate?
centralization(contig_1993, evcent) # Eigenvector centralization

#For more information. . .
?centralization
###########################################################
##4.4 Elementary graph-level indices
gden(mids_1993) # Density
grecip(mids_1993) # Dyadic reciprocity
grecip(mids_1993, measure="edgewise") # Edgewise reciprocity
gtrans(mids_1993) # Transitivity

#For more information. . .
?gden 
?grecip 
?gtrans

###########################################################
##4.5 Subgraph census routines, isolates, clusters, and geodesic distance

dyad.census(mids_1993) # M,A,N counts
dyad.census(contig_1993) # No As in undirected graphs
triad.census(mids_1993) # Directed triad census
triad.census(contig_1993, mode="graph") # Undirected triad census

###########################################################
##4.6 Brokerage

#look at the trade data
class(trade)
dim(trade)

gplot(trade[1,,])

tradeat
table(tradeat[,1])

gplot(trade[1,,],vertex.col=rainbow(5)[tradeat[,1]+1]) #color by pop growth
tradeBro<-brokerage(trade[1,,],tradeat[,1])
summary(tradeBro)

############################################################
#4.7 Matrix Algebra
davis

gplot(davis,label=c(rownames(davis),colnames(davis)),vertex.col=c(rep("red",18),rep("blue",14)))

nDavis<-network(davis,bipartite=T)
nDavis ##what does bipartite number indicate?

nDavis[,]  ##doesn't return the original bipartite adjacency matrix

##That nasty repeating vector thing -- beware when assigning vertex attributes for 2-mode networks
nDavis%v%"num"<-1:18
nDavis%v%"num"

##Back to matrix multiplication
t(davis) ##take the transpose
davis%*%t(davis)  ##here's a case where the function demands matrix format
davis<-as.matrix(davis)

women_coattendence<-davis%*%t(davis)  ##womenXwomen projection
diag(women_coattendence)  ## what does the diagonal mean now?

event_coattendence<-t(davis)%*%davis  ##eventXevent projection

gplot(women_coattendence,edge.lwd=women_coattendence)
gplot(event_coattendence,edge.lwd=event_coattendence)


#Geography and MDS
city
cityNames<-rownames(city)

city<-as.matrix(city)
city<-symmetrize(city,rule="upper")

XYCoords<-cmdscale(city)  ##this will 'throw' an error because of NAs
which(is.na(city),arr.ind=T) ##where are the NAs?
diag(city) ##another way to find them

diag(city)<-0 ##why is the diagonal zero?

XYCoords<-cmdscale(city)
plot(XYCoords,type="n")
text(XYCoords,label=cityNames)

#Correspondence analysis -- think two-mode MDS
library(MASS)
plot(corresp(davis,nf=2))




