* Install R:
	- I suggest installing Microsoft R Open from: https://mran.microsoft.com/documents/rro/installation
	- However, you can also install the default R from: https://cran.cnr.berkeley.edu/
* Make sure you install R in a path which does not have spaces in its name. E.g. `F:\ROpen` as opposed to `F:\R Tools\ROpen`	
* Install R Studio from: https://www.rstudio.com/products/rstudio/download/#download
* Open R Studio and type the following lines in R Console: 
```r
install.packages("network")
install.packages("numDeriv")
install.packages('statnet')
install.packages('ndtv')
install.packages('htmlwidgets')
install.packages('latticeExtra')
install.packages("sna")
install.packages("tsna")

library(network)
library(numDeriv)
library(statnet)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)
library(sna)
library(tsna)

```


Add `, dependencies = T` to each command. E.g. 

```r
install.packages("network", dependencies = T)
```


If any error occurs let us know. 