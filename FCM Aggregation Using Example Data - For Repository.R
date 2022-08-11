
## Set working directory (should have the example data file in that same folder)
setwd("~/FCM Code")

## Set the packages, if you have not already downloaded them, do so like the example below:
#install.packages("igraph")

library(rJava)
library(openxlsx)
library(xlsx)
library(igraph)
library(tnet)

#### PREPARE THE DATA ####

## Import the raw data - replace with your file name
## We basically just need a bit of information before we properly import the data
rawdata <- loadWorkbook("Example Random FCM Maps.xlsx")

## Extract the sheet names - can be numeric or characters, I use this to track participant IDs
sheetnames <- as.vector(getSheetNames("Example Random FCM Maps.xlsx")) #this function uses the package openxlsx

## Set the number of sheets (number of participants/maps)
nsheets <- as.numeric(length(sheetnames))

#### The import function converts the raw data into a matrix in R. X is a vector of the sheetnames and Y is the data file (xlsx of FCMs)
## I generated the example data to have zeros (no edge) in the adjacency matrices but if you're importing directly from Mental Modeler it will have blank cells. 
##  That's totally fine and the import function will manage that!

importfxn <- function(x, y){
  sheet <- read.xlsx(y,sheetName = x)
  dims <- dim(sheet)
  mat <- as.matrix(sheet[1:dims[1],2:dims[2]], rownames.force=TRUE)
  rownames(mat) <- as.list(sheet[,1])
  colnames(mat) <- as.list(sheet[,1])
  mat[is.na(mat)] = 0
  
  print(mat)
}

### Now we use the import function to read the xlsx and create a list of matrix data
matrix_data <- lapply(X=sheetnames, FUN=importfxn, y="Example Random FCM Maps.xlsx")
names(matrix_data) <- sheetnames


#### DESCRIPTIVE DATA ####

## To determine the descriptive data for each individual map, we'll use a pretty long function and apply it to all of them
descriptivedatafxn_singlegraph <- function(matrixdata){
  
  #read in the graph
  testgraph <- graph_from_adjacency_matrix(matrixdata, mode ="directed", weighted=T )
  
  #want one without the negative connections for centrality
  abstestgraph <- testgraph
  E(abstestgraph)$weight <- abs(E(testgraph)$weight)
  
  # Concepts
  
  nconcepts <- as.numeric(length(V(testgraph)))
  
  conceptnames <- names(V(testgraph))
  
  
  # Edges
  
  nedges <- as.numeric(length(E(testgraph)))
  
  edges <- as_edgelist(testgraph)
  weights <- E(testgraph)$weight
  edgedata <- cbind(edges, weights)
  colnames(edgedata)[1]  <- "from"
  colnames(edgedata)[2]  <- "to"
  
  edgetype <- numeric(nedges)
  for (i in 1:nedges){
    if (weights[i]>0){
      edgetype[i] <- "positive"
    }
    else {
      edgetype[i] <- "negative"
    }
  }
  
  testgraph <- set_edge_attr(testgraph, name = "sign", value=edgetype)
  
  
  # Type of Concepts
  nodetype <- numeric(nconcepts)
  
  for (i in 1:nconcepts){
    if ((any(edgedata[,1] == conceptnames[i])) == FALSE)
    {nodetype[i] <- "Receiver"}
    
    if ((any(edgedata[,1] == conceptnames[i])) == TRUE & (any(edgedata[,2] == conceptnames[i])) == TRUE)
    {nodetype[i] <- "Ordinary"}
    
    if ((any(edgedata[,1] == conceptnames[i])) == TRUE & (any(edgedata[,2] == conceptnames[i])) == FALSE)
    {nodetype[i] <- "Driver"}
  }
  
  
  summarytypes <- cbind(sum(nodetype=="Receiver"), sum(nodetype=="Driver"), sum(nodetype=="Ordinary"))
  colnames(summarytypes) <- c("Receiver", "Driver", "Ordinary")
  
  testgraph <- set.vertex.attribute(testgraph, "type", value=nodetype)
  
  
  # Centrality of Concepts
  
  degreecent <- degree(abstestgraph, mode = "all")
  indegreecent <- degree(abstestgraph, mode = "in")
  outdegreecent <- degree(abstestgraph, mode = "out")
  
  normdegreecent <- degree(abstestgraph, mode = "all", normalized = T)
  normindegreecent <- degree(abstestgraph, mode = "in", normalized = T)
  normoutdegreecent <- degree(abstestgraph, mode = "out", normalized = T)
  
  degcentdist <- degree.distribution(abstestgraph, mode="all")
  outdegdist <- degree.distribution(abstestgraph, mode="out")
  indegdist <- degree.distribution(abstestgraph, mode="in")
  
  absmatrix <- abs(matrixdata)
  tnet_mat <- as.tnet(absmatrix, type="weighted one-mode tnet")
  degree_out <- degree_w(tnet_mat, type= "out")
  degree_in <- degree_w(tnet_mat, type= "in")
  
  absweightedcent <- numeric(length=as.numeric(length(degree_out[,"output"])))
  
  for (i in 1:as.numeric(length(degree_out[,"output"]))){
    absweightedcent[i] <- sum(degree_out[i,"output"], degree_in[i,"output"])
  }
  
  closenesscent <- closeness(abstestgraph, mode = "all", weights = abs(E(testgraph)$weight))
  betweennesscent <- betweenness(abstestgraph, weights = abs(E(testgraph)$weight))
  
  attributelist <- c("absolute weighted degree centrality" , "degree centrality", "indegree centrality", "outdegree centrality", "normalized degree centrality", "normalized indegree centrality", "normalized outdegree centrality", "closeness centrality", "betweenness centrality")
  centlist <- cbind(absweightedcent, degreecent, indegreecent, outdegreecent, normdegreecent, normindegreecent, normoutdegreecent, closenesscent, betweennesscent)
  
  for (i in 1:as.numeric(length(attributelist))){
    testgraph <- set_vertex_attr(graph=testgraph, name = attributelist[i], value= centlist[,i])
  }
  
  # Density
  density <- edge_density(testgraph)
  
  # C/N
  CN <- nedges/nconcepts
  
  # Complexity
  complexity <- 0
  
  if (summarytypes[1,1] != 0 & summarytypes[1,2] != 0) {
    complexity <-  as.numeric(summarytypes[1,1]/summarytypes[1,2])
  } else {complexity <-  0}
  
  
  
  # putting it all together
  graphattributelist <- c("Number of Components", "Number of Connections", "Summary of Component Types", "Degree Centrality Distribution", "Outdegree Centrality Distribution", "Indegree Centrality Distribution", "Density", "Connections per Component", "Complexity", "Edge List" )
  graphlist <- list(nconcepts, nedges, summarytypes, degcentdist, outdegdist, indegdist, density, CN, complexity, edgedata)
  
  for (i in 1:as.numeric(length(graphattributelist))){
    testgraph <- set_graph_attr(graph=testgraph, name = graphattributelist[i], value= graphlist[[i]])
  }
  
  
  print(testgraph)
  
}


### Run the function
descripdatalist_individuals <- lapply(X=matrix_data, FUN = descriptivedatafxn_singlegraph)
# If there are warnings about closeness centrality that is okay, the closeness centrality metric may just not being a great fit for your data
names(descripdatalist_individuals) <- sheetnames

#### Individual Map Descriptive Data ####

individualmapdata_fxn <- function(descripdatalist){
  sheetnames <- names(descripdatalist)
  
densitylist <- vector(mode="numeric", length=as.numeric(length(sheetnames))) # Density
numcomplist <- vector(mode="numeric", length=as.numeric(length(sheetnames))) # Number of Components
numconnlist <- vector(mode="numeric", length=as.numeric(length(sheetnames))) # Number of Connections
CNlist <- vector(mode="numeric", length=as.numeric(length(sheetnames))) # Connections per Component
complexitylist <- vector(mode="numeric", length=as.numeric(length(sheetnames))) # Complexity

for (i in 1:as.numeric(length(sheetnames))){
  densitylist[i] <- get.graph.attribute(descripdatalist[[i]], name = "Density")
  numcomplist[i] <- get.graph.attribute(descripdatalist[[i]], name = "Number of Components")
  numconnlist[i] <- get.graph.attribute(descripdatalist[[i]], name = "Number of Connections")
  CNlist[i] <- get.graph.attribute(descripdatalist[[i]], name = "Connections per Component")
  complexitylist[i] <- get.graph.attribute(descripdatalist[[i]], name = "Complexity")
}

## Export Individual Descriptive Data
descriptivedata_types <- list(sheetnames, densitylist, numcomplist, numconnlist, CNlist, complexitylist)
individualmapdata <- matrix(data=NA, nrow=as.numeric(length(sheetnames)), ncol=6)

for (i in 1:6){
  individualmapdata[,i] <- descriptivedata_types[[i]]
}

colnames(individualmapdata) <- c("ID", "Density", "Number of Components", "Number of Connections", "CN", "Complexity")
print(individualmapdata)
}

individualmapdata_individuals <- individualmapdata_fxn(descripdatalist_individuals)
### Write the File ###
write.xlsx(individualmapdata_individuals, file="Example Data - Individual Map Data.xlsx", row.names = F) 




#### Group Descriptive Data ####
### First set the groups by creating vectors of the map IDs/names <- make sure its the name! We'll use this as character, not numeric data
## For this example, I'll create three random groups
one <- c(1,3,4,6,9,11,12,15,24,25)
two <- c(2,7,10,16,17,20,22,26,28,30)
three <- c(5,8,13,14,18,19,21,23,27,29)

## ^^This info could also be in an excel file that you read in.

groupnames_expertise <- c("Group One", "Group Two", "Group Three")
grouplist_expertise <- list(one, two, three)
names(grouplist_expertise) <- groupnames_expertise

detach("package:xlsx", unload = TRUE)

groupmapdata_fxn <- function(individualmapdata, groupnames, grouplist){

## Then we'll create a workbook for each of the descriptive data types
descrip_types <- c("Density", "Number of Components", "Number of Connections", "CN", "Complexity")
datacompwb <- createWorkbook()

for (i in 1:as.numeric(length(descrip_types))){
  addWorksheet(wb=datacompwb, sheetName=descrip_types[i])
} 

### for each group and each metric we'll calculate the number of participants in each group, and the minimum value, maximum value, mean, and standard deviation for the given metric
datacomp_colnames <- c("count", "min", "max", "mean", "std") # set the names of each measure
datacomp_rownames <- groupnames # names of each group
datacomp_groups <- grouplist #vector of each group of participants
names(datacomp_groups) <- datacomp_rownames

changed.data <- datacomp_groups # make a list to be altered

## Pull the metric data for each participant within the group
for (j in 1:as.numeric(length(datacomp_groups))){
  changed.data[[j]] <-  individualmapdata[individualmapdata[,1] %in% datacomp_groups[[j]] == TRUE,]
}

## Do the calculations for min, max, mean, and standard deviation and fill the workbook
for (i in 1:as.numeric(length(descrip_types))){
  datacomp <- matrix(nrow=as.numeric(length(datacomp_rownames)), ncol = as.numeric(length(datacomp_colnames))) 
  colnames(datacomp) <- datacomp_colnames
  row.names(datacomp) <- datacomp_rownames
  
  
  for (r in 1:as.numeric(length(datacomp_groups))){
    if (as.numeric(length(datacomp_groups[[r]])) == 1) {
      current <- t(as.matrix(changed.data[[r]]))
      hold <- as.numeric(current[,descrip_types[i]])
    }
    else {
      hold <- as.numeric(changed.data[[r]][,descrip_types[i]])
    }
    datacomp[r,] <- c(as.numeric(length(hold)), min(hold), max(hold), mean(hold), sd(hold))
  }
  
  writeData(wb=datacompwb, sheet = descrip_types[i], x = datacomp, colNames = T, rowNames = T)
}
print(datacompwb)
}

groupdata_expertise <- groupmapdata_fxn(individualmapdata_individuals, groupnames_expertise, grouplist_expertise)

saveWorkbook(groupdata_expertise, "Example Data - Descriptive Data by Expertise Group.xlsx") 



#### COMPONENTS ####
## There are several pieces of information about components that could be useful

## List of components within each map
conceptlist <- lapply(X=1:as.numeric(length(sheetnames)), FUN = function(x) {
  get.vertex.attribute(descripdatalist_individuals[[x]], name = "name")
})
names(conceptlist) <- sheetnames

# Determining the unique concepts
allconcepts <- unlist(conceptlist)
uniqueconcepts <- unique(allconcepts)

### Make an excel sheet of all the unique concepts within the maps
write.xlsx(uniqueconcepts, "Unique Concepts.xlsx")

### Compare the frequency of concepts between groups

components_fxn <- function(descripdatalist, groupnames, grouplist){
  datacomp_groups <- grouplist
  sheetnames <- names(descripdatalist)
  
frequency_types <- c("Net", "Proportion of Component", "Proportion of Group")
componentwb <- createWorkbook()

for (i in 1:as.numeric(length(frequency_types))){
  addWorksheet(wb=componentwb, sheetName=frequency_types[i])
} 


## Net Count - just the number of participants in each group that mentioned each concept
netconcepts <- matrix(data=NA, nrow=as.numeric(length(uniqueconcepts)), ncol=(2+as.numeric(length(datacomp_groups))))
netconcepts[,1] <- uniqueconcepts
colnames(netconcepts) <- c("Component", "Count", groupnames)


for (i in 1:as.numeric(length(uniqueconcepts))){
  netconcepts[i,2] <-  sum(allconcepts  == netconcepts[i,1])
}

for (j in 1:as.numeric(length(datacomp_groups))){
  pull <- which(as.numeric(names(conceptlist)) %in% datacomp_groups[[j]])
  newlist <- vector(mode = "list", length = as.numeric(length(pull)))
  for (i in 1:as.numeric(length(pull))){
    newlist[[i]] <- conceptlist[[pull[i]]]
  }
  concepts <- unlist(newlist)
  for (r in 1:as.numeric(length(uniqueconcepts)))
    netconcepts[r,j+2] <- sum(concepts == netconcepts[r,1]) 
}

writeData(wb=componentwb, sheet = frequency_types[1], x = netconcepts, colNames = T, rowNames = F)

## Proportion of Component - how each component is represented across groups, so the rows will sum to 1
fullconcepts <- matrix(data=NA, nrow=as.numeric(length(uniqueconcepts)), ncol=(2+as.numeric(length(datacomp_groups))))
fullconcepts[,1] <- uniqueconcepts
colnames(fullconcepts) <- c("Component", "Count", groupnames)

for (i in 1:as.numeric(length(uniqueconcepts))){
  fullconcepts[i,2] <-  sum(allconcepts  == fullconcepts[i,1])
}

for (j in 1:as.numeric(length(datacomp_groups))){
  pull <- which(as.numeric(names(conceptlist)) %in% datacomp_groups[[j]])
  newlist <- vector(mode = "list", length = as.numeric(length(pull)))
  for (i in 1:as.numeric(length(pull))){
    newlist[[i]] <- conceptlist[[pull[i]]]
  }
  concepts <- unlist(newlist)
  for (r in 1:as.numeric(length(uniqueconcepts)))
    fullconcepts[r,j+2] <- (sum(concepts == fullconcepts[r,1]))/as.numeric(fullconcepts[r,2]) 
}

writeData(wb=componentwb, sheet = frequency_types[2], x = fullconcepts, colNames = T, rowNames = F)

## Proportion of Group - distribution of concepts within the group, so columns will sum to 1
fullconcepts_proportion <- matrix(data=NA, nrow=as.numeric(length(uniqueconcepts)), ncol=(2+as.numeric(length(datacomp_groups))))
fullconcepts_proportion[,1] <- uniqueconcepts
colnames(fullconcepts_proportion) <- c("Component", "Count", groupnames)

for (i in 1:as.numeric(length(uniqueconcepts))){
  fullconcepts_proportion[i,2] <-  sum(allconcepts  == fullconcepts_proportion[i,1])
}

for (j in 1:as.numeric(length(datacomp_groups))){
  pull <- which(as.numeric(names(conceptlist)) %in% datacomp_groups[[j]])
  newlist <- vector(mode = "list", length = as.numeric(length(pull)))
  for (i in 1:as.numeric(length(pull))){
    newlist[[i]] <- conceptlist[[pull[i]]]
  }
  concepts <- unlist(newlist)
  for (r in 1:as.numeric(length(uniqueconcepts)))
    fullconcepts_proportion[r,j+2] <- (sum(concepts == fullconcepts_proportion[r,1]))/as.numeric(length(datacomp_groups[[j]])) 
}

writeData(wb=componentwb, sheet = frequency_types[3], x = fullconcepts_proportion, colNames = T, rowNames = F)
print(componentwb)
}


components_expertise <- components_fxn(descripdatalist_individuals, groupnames_expertise, grouplist_expertise)
saveWorkbook(components_expertise, "Example Data - Concepts and Frequency by Expertise Group.xlsx")


####  NODE CENTRALITY ####

# input data should be your list of all the data from the descriptive data function (graph objects)
# inputconcepts should be a vector/list of all the final concepts there are

nodedataextractionfxn <- function(inputdata, inputconcepts, datatype){
  rownum <- as.numeric(length(inputconcepts))
  colnum <- as.numeric(length(inputdata))
  
  indegreedata <- matrix(data=NA, nrow=(rownum), ncol=(colnum+2))
  indegreedata[1:(rownum),1] <- inputconcepts
  colnames(indegreedata) <- c("Concepts", names(inputdata), "Mean")
  
  
  for (j in 1:colnum){
    same <- match(get.vertex.attribute(inputdata[[j]], name = "name"), indegreedata[,1])
    numvals <- as.numeric(length(same))
    
    for (i in 1:numvals){
      indegreedata[same[i], (j+1)] <- get.vertex.attribute(inputdata[[j]], name = datatype)[i]
      
    }
  }
  
  for (r in 1:rownum){
    indegreedata[r, (colnum+2)] <- mean(as.numeric(indegreedata[r,2:(colnum+1)]), na.rm = TRUE)
  }
  
  print(indegreedata)
  
}

nodecentrality_fxn <- function(descripdatalist, groupnames, grouplist){
  datacomp_groups <- grouplist
  datacomp_rownames <- groupnames
  sheetnames <- names(descripdatalist)

## Now we'll use that funciton for all the different kinds of centrality
absweightedcollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts, "absolute weighted degree centrality")
indegreecollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts, "indegree centrality")
outdegreecollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "outdegree centrality")
degreecentcollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "degree centrality")

normindegreecollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "normalized indegree centrality")
normoutdegreecollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "normalized outdegree centrality")
normdegreecentcollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "normalized degree centrality")

closenesscentcollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "closeness centrality")
betweennesscentcollected <- nodedataextractionfxn(descripdatalist, uniqueconcepts,  "betweenness centrality")

### Exporting centrality data

centwb <- createWorkbook()

centralityattributelist <- c("abs weighted degree centrality", "degree centrality", "indegree centrality", "outdegree centrality", "normalized degree centrality", "normalized indegree centrality", "normalized outdegree centrality", "closeness centrality", "betweenness centrality")

addWorksheet(centwb, sheetName = "Summary")

for (i in 1:as.numeric(length(centralityattributelist))){
  addWorksheet(wb=centwb, sheetName=centralityattributelist[i])
}

allcentdata <- list(absweightedcollected, degreecentcollected, indegreecollected, outdegreecollected, normdegreecentcollected, normindegreecollected, normoutdegreecollected, closenesscentcollected, betweennesscentcollected)
names(allcentdata) <- centralityattributelist

for (i in 1:as.numeric(length(centralityattributelist))){
  writeData(wb=centwb, sheet = centralityattributelist[i], x= allcentdata[[i]])
}

## Now we'll make a summary page that compares by group
testcolnames <- unlist(lapply(X=centralityattributelist, FUN=paste, sep= " - ", datacomp_rownames))
summarycentdata <- matrix(ncol=as.numeric(length(testcolnames)), nrow = as.numeric(length(uniqueconcepts)))
row.names(summarycentdata) <- uniqueconcepts
colnames(summarycentdata) <- testcolnames


for (j in 1:as.numeric(length(centralityattributelist))) {
  for (r in 1:as.numeric(length(datacomp_groups))) {
    
    if (as.numeric(length(datacomp_groups[[r]] == 1))) {
      test <- (as.matrix(allcentdata[[j]][, which(colnames(allcentdata[[j]]) %in% datacomp_groups[[r]] == T)]))
    }
    else {
      test <- allcentdata[[j]][, which(colnames(allcentdata[[j]]) %in% datacomp_groups[[r]] == T)]
    }
    num.test <- apply(test, 2, as.numeric)
    means <- rowMeans(num.test, na.rm=T)
    summarycentdata[, (j-1)*as.numeric(length(datacomp_rownames))+r] <- means
  }
}
writeData(wb=centwb, sheet = "Summary", x= summarycentdata, colNames = T, rowNames = T)
print(centwb)
}

centrality_expertise <- nodecentrality_fxn(descripdatalist_individuals, groupnames_expertise, grouplist_expertise)
## Write File ##
saveWorkbook(centrality_expertise, "Example Data - Node Centrality Data By Expertise Group.xlsx")    



#### NODE TYPE ####

# Comparing whether nodes are Ordinary, Receivers, or Drivers

nodeTYPEextractionfxn <- function(inputdata, inputconcepts){
  rownum <- as.numeric(length(inputconcepts))
  colnum <- as.numeric(length(inputdata))
  
  indegreedata <- matrix(data=NA, nrow=(rownum), ncol=(colnum+4))
  indegreedata[1:(rownum),1] <- inputconcepts
  colnames(indegreedata) <- c("Concepts", names(inputdata), "Ordinary", "Receiver", "Driver")
  
  
  for (j in 1:colnum){
    same <- match(get.vertex.attribute(inputdata[[j]], name = "name"), indegreedata[,1])
    numvals <- as.numeric(length(same))
    
    for (i in 1:numvals){
      indegreedata[same[i], (j+1)] <- get.vertex.attribute(inputdata[[j]], name = "type")[i]
      
    }
  }
  
  indegreedata[is.na(indegreedata)] <- "NA"
  
  for (r in 1:rownum){
    indegreedata[r, (colnum+2)] <- (sum(indegreedata[r, 1:colnum+1] == "Ordinary"))
    indegreedata[r, (colnum+3)] <- (sum(indegreedata[r, 1:colnum+1] == "Receiver"))
    indegreedata[r, (colnum+4)] <- (sum(indegreedata[r, 1:colnum+1] == "Driver"))
  }
  
  print(indegreedata)
  
}

nodetype_fxn <- function(descripdatalist, groupnames, grouplist){
  datacomp_groups <- grouplist
  datacomp_rownames <- groupnames
  sheetnames <- names(descripdatalist)
  
  
  typewb <- createWorkbook()
  addWorksheet(typewb, sheetName = "Node Types")
  addWorksheet(typewb, sheetName = "Avg Percent by Group")
  addWorksheet(typewb, sheetName = "Group Summary")


  nodetypescollected <- nodeTYPEextractionfxn(descripdatalist, uniqueconcepts)

  nodetypescollected_percent <- nodetypescollected


  typesums <- vector(mode="numeric", length=as.numeric(length(uniqueconcepts)))
  ncols_nodetypes <- as.numeric(ncol(nodetypescollected_percent))
  for (i in 1:as.numeric(length(uniqueconcepts))){
    typesums[i] <- sum(as.numeric(nodetypescollected[i,(ncols_nodetypes-2):ncols_nodetypes]))
  
    nodetypescollected_percent[i,ncols_nodetypes-2] <- (as.numeric(nodetypescollected[i,ncols_nodetypes-2]))/typesums[i]
    nodetypescollected_percent[i,ncols_nodetypes-1] <- (as.numeric(nodetypescollected[i,ncols_nodetypes-1]))/typesums[i]
    nodetypescollected_percent[i,ncols_nodetypes] <- (as.numeric(nodetypescollected[i,ncols_nodetypes]))/typesums[i]
  }

  finalnodedata <- cbind(nodetypescollected, nodetypescollected_percent[,(ncol(nodetypescollected_percent) - 2):ncol(nodetypescollected_percent)])

  writeData(wb=typewb, sheet = "Node Types", x= finalnodedata, colNames = T, rowNames = F)

  ## Average percent by group
  typecolnames <- unlist(lapply(X=c("Ordinary", "Receiver", "Driver"), FUN=paste, sep= " - ", datacomp_rownames))
  percentmat <- matrix(nrow=as.numeric(length(uniqueconcepts)), ncol = as.numeric(length(typecolnames)))
  colnames(percentmat) <- typecolnames
  rownames(percentmat) <- uniqueconcepts


  for (r in 1:as.numeric(length(datacomp_groups))) {
    if (as.numeric(length(datacomp_groups[[r]] == 1))) {
      test <- (as.matrix(nodetypescollected[, which(colnames(nodetypescollected) %in% datacomp_groups[[r]] == T)]))
    }
    else {
      test <- nodetypescollected[, which(colnames(nodetypescollected) %in% datacomp_groups[[r]] == T)]
    }
  
    sumord <- vector(mode = "numeric", length=as.numeric(length(uniqueconcepts)))
    sumrec <- vector(mode = "numeric", length=as.numeric(length(uniqueconcepts)))
    sumdriv <- vector(mode = "numeric", length=as.numeric(length(uniqueconcepts)))
  
    for (i in 1:as.numeric(length(uniqueconcepts))){
      sumord[i] <- (sum(test[i,] == "Ordinary"))/as.numeric(length(datacomp_groups[[r]]))
      sumrec[i] <- (sum(test[i,] == "Receiver"))/as.numeric(length(datacomp_groups[[r]]))
      sumdriv[i] <- (sum(test[i,] == "Driver"))/as.numeric(length(datacomp_groups[[r]]))
    }
  
    for (j in 1:as.numeric(length(uniqueconcepts))) {
      percentmat[j,1+(r-1)*3] <- mean(sumord[j])
      percentmat[j,2+(r-1)*3] <- mean(sumrec[j])
      percentmat[j,3+(r-1)*3] <- mean(sumdriv[j])
    
    }
  }


  writeData(wb=typewb, sheet = "Avg Percent by Group", x= percentmat, colNames = T, rowNames = T)

  ## Summary by Group
  summarymat <- matrix(nrow=as.numeric(length(datacomp_groups)), ncol = 6)
  colnames(summarymat) <- c("Ordinary - Norm", "Receiver - Norm", "Driver - Norm", "Ordinary - Percent", "Receiver - Percent", "Driver - Percent")
  rownames(summarymat) <- datacomp_rownames

  for (r in 1:as.numeric(length(datacomp_groups))) {
    if (as.numeric(length(datacomp_groups[[r]] == 1))) {
      test <- (as.matrix(nodetypescollected[, which(colnames(nodetypescollected) %in% datacomp_groups[[r]] == T)]))
    }
    else {
      test <- nodetypescollected[, which(colnames(nodetypescollected) %in% datacomp_groups[[r]] == T)]
      test <- nodetypescollected[, which(colnames(nodetypescollected) %in% datacomp_groups[[3]] == T)]
    }
  
    sumord <- vector(mode = "numeric", length=as.numeric(length(uniqueconcepts)))
    sumrec <- vector(mode = "numeric", length=as.numeric(length(uniqueconcepts)))
    sumdriv <- vector(mode = "numeric", length=as.numeric(length(uniqueconcepts)))
  
    for (i in 1:as.numeric(length(uniqueconcepts))){
      sumord[i] <- (sum(test[i,] == "Ordinary"))
      sumrec[i] <- (sum(test[i,] == "Receiver"))
      sumdriv[i] <- (sum(test[i,] == "Driver"))
    }
  
    total <- sum(sumord, sumrec, sumdriv)  
    summarymat[r,1] <- sum(sumord)/as.numeric(length(datacomp_groups[[r]]))
    summarymat[r,2] <- sum(sumrec)/as.numeric(length(datacomp_groups[[r]]))
    summarymat[r,3] <- sum(sumdriv)/as.numeric(length(datacomp_groups[[r]]))
    summarymat[r,4] <- sum(sumord)/total
    summarymat[r,5] <- sum(sumrec)/total
    summarymat[r,6] <- sum(sumdriv)/total
  
  }

  writeData(wb=typewb, sheet = "Group Summary", x= summarymat, colNames = T, rowNames = T)
  print(typewb)
}

nodetype_expertise <- nodetype_fxn(descripdatalist_individuals, groupnames_expertise, grouplist_expertise)
saveWorkbook(nodetype_expertise, "Example Data - Node Type Data By Expertise Group.xlsx")



#### EDGES ####

edges_fxn <- function(descripdatalist){
  sheetnames <- names(descripdatalist)
  # List of Edges          
  edgelists <- list()
  for (i in 1:as.numeric(length(sheetnames))){
    edgelists[[i]] <- get.graph.attribute(descripdatalist[[i]], name = "Edge List")
  }
  names(edgelists) <- sheetnames
  
  #Export edge data
  alledges <- do.call(rbind, Map(data.frame, edgelists))
  edgerownames <- as.numeric(rownames(alledges))
  ID <- trunc(edgerownames, digits=0)
  alledgedata <- cbind(ID, alledges)
  print(alledgedata)
}

edgedata_individualmaps <- edges_fxn(descripdatalist_individuals)

## You might not need this information, I used "Text Edges" for transcript coding
edgewb_fxn <- function(edgedata){
edgewb <- createWorkbook()
addWorksheet(edgewb, sheetName = "All Edges with IDs") 
addWorksheet(edgewb, sheetName = "Edge List")
addWorksheet(edgewb, sheetName = "All Text Edges")
addWorksheet(edgewb, sheetName = "Text Edges with Weights")

alledges <- edgedata[,-1]
writeData(wb=edgewb, sheet = "All Edges with IDs", x= edgedata, colNames = T, rowNames = T)
writeData(wb=edgewb, sheet = "Edge List", x= alledges, colNames = T, rowNames = T)

textonlyedges <- paste(alledges[,1], "TO", alledges[,2])
textonlyedges_weights <- cbind(textonlyedges, edgedata[,4])

writeData(wb=edgewb, sheet = "All Text Edges", x= textonlyedges, colNames = T, rowNames = T)
writeData(wb=edgewb, sheet = "Text Edges with Weights", x= textonlyedges_weights, colNames = T, rowNames = T)
print(edgewb)
}

edgewb_individualmaps <- edgewb_fxn(edgedata_individualmaps)
## Write File ##
saveWorkbook(edgewb_individualmaps, "Example Data - Edge Data for Individual Maps.xlsx")







#### AGGREGATION INTO GROUPS ####

## To aggregate groups we want to use the means of the connections

### There are a few base functions we'll use in a larger analysis function
sheetgroupingfxn <- function(y, r){
  groupmats <- lapply(X=y, function(x){r[r$ID == x,]})
  names(groupmats) <- y
  print(groupmats)
}

groupconndatafxn_wozero <- function(groupedgedata){
  nsheets <- as.numeric(length(groupedgedata))
  
  allfromedges <- unlist(lapply(X=1:nsheets, function(x){groupedgedata[[x]]$from})) 
  alltoedges <- unlist(lapply(X=1:nsheets, function(x){groupedgedata[[x]]$to}))  
  allweights <- unlist(lapply(X=1:nsheets, function(x){groupedgedata[[x]]$weights}))  
  
  allconns <-  paste(allfromedges, "TO", alltoedges)
  conns_weights <- cbind(allconns, allweights)
  uniqueconns <- unique(allconns)
  
  conndata <- matrix(ncol = 6,  nrow = as.numeric(length(uniqueconns)))
  conndata[,1] <- uniqueconns
  colnames(conndata) <- c("Edge", "Min", "Max", "Mean", "StDev", "Count")
  
  for (i in 1:as.numeric(length(uniqueconns))) {
    hold <- as.numeric(conns_weights[(conns_weights[,1] %in% uniqueconns[i] == TRUE), 2])
    conndata[i,2:6] <- c(max(hold), min(hold), mean(hold), sd(hold), as.numeric(length(hold)))
  }
  print(conndata)
}

conndatafxn_agg_nozeros <- function(edgedata, grouplist){
  nedges <- as.numeric(length(edgedata$from))
  fromedges <- unlist(lapply(X=1:nedges, function(x){edgedata$from})) 
  toedges <- unlist(lapply(X=1:nedges, function(x){edgedata$to}))  
  weights <- unlist(lapply(X=1:nedges, function(x){edgedata$weights}))  
  
  conns <- paste(fromedges, "TO", toedges)
  finaluniqueconns <- unique(conns)
  conndata <- matrix(nrow=as.numeric(length(finaluniqueconns)), ncol=as.numeric(length(grouplist))+2)
  colnames(conndata) <- c("from", "to", names(grouplist))
  
  connedgelist <- strsplit(finaluniqueconns, " TO ")
  for (i in 1:as.numeric(length(connedgelist))){
    conndata[i,1] <- connedgelist[[i]][1]
    conndata[i,2] <- connedgelist[[i]][2]
  }
  
  for (r in 1:as.numeric(length(grouplist))){
    groupedgedata <- lapply(X=grouplist[[r]], function(x){edgedata[edgedata$ID == x,]})
    names(groupedgedata) <- grouplist[[r]]
    
    nsheets <- as.numeric(length(grouplist[[r]]))
    
    allfromedges <- unlist(lapply(X=1:nsheets, function(x){groupedgedata[[x]]$from})) 
    alltoedges <- unlist(lapply(X=1:nsheets, function(x){groupedgedata[[x]]$to}))  
    allweights <- unlist(lapply(X=1:nsheets, function(x){groupedgedata[[x]]$weights}))  
    
    allconns <-  paste(allfromedges, "TO", alltoedges)
    
    conns_weights <- cbind(allconns, allweights)
    
    for (t in 1:as.numeric(length(finaluniqueconns))) {
      hold <- as.numeric(conns_weights[(conns_weights[,1] %in% finaluniqueconns[t] == TRUE), 2])
      conndata[t,2+r] <- mean(as.numeric(hold))
    }
  }
  print(conndata)
}

### Combined into one function
analysis.megafxn <- function(matrix_data, grouplist, edgedata){
  ngroups <- as.numeric(length(grouplist))
  expertisegroups <- names(grouplist)
  alledgedata <- edgedata
  
  #### AGGREGATION INTO GROUPS ####
  group_edges <- vector("list", ngroups)
  for (i in 1:ngroups){
    group_edges[[i]] <- sheetgroupingfxn(grouplist[[i]], alledgedata)
  }
  
  ## Then use that edge data to calculate some information
  #### Determines max, min, mean, standard deviation, and number of mentions
  #### ONLY includes edges that exist, no zeros for no edges
  group_conndata_list <- vector("list", ngroups)
  for (i in 1:ngroups){
    group_conndata_list[[i]] <- groupconndatafxn_wozero(group_edges[[i]])
  }
  
  
  ## Now a comparison across groups
  conndata_groups <- conndatafxn_agg_nozeros(alledgedata, grouplist)
  conndata_groups[which(conndata_groups == "NaN")] <- 0
  medians <- vector(mode = "numeric", length = nrow(conndata_groups))
  for (i in 1:nrow(conndata_groups)) {
    medians[i] <- median(as.numeric(conndata_groups[i,3:(2+as.numeric(length(grouplist)))]))
  }
  conndata_groups_withagg <- cbind(conndata_groups, medians)
  
  
  ## First make edgelists of just the means per group & median of metamodel
  edgelists_groups <- vector("list", length=(as.numeric(length(grouplist))+1))
  names(edgelists_groups) <- c(expertisegroups, "Metamodel")
  
  for (i in 1:as.numeric(length(edgelists_groups))){
    edgelists_groups[[i]] <- cbind(conndata_groups_withagg[,1], conndata_groups_withagg[,2], conndata_groups_withagg[,2+i])
    edgelists_groups[[i]] <- edgelists_groups[[i]][edgelists_groups[[i]][,3] !=0,]
    colnames(edgelists_groups[[i]]) <- c("from", "to", "weights")
  }
  
  ## Now we can make graphs to mess with in R and adjacency matrices for MentalModeler
  
  # Graph
  graphs_groups <- vector("list", length=(as.numeric(length(grouplist))+1))
  names(graphs_groups) <- c(names(grouplist), "Metamodel")
  
  for (i in 1:as.numeric(length(graphs_groups))){
    graphs_groups[[i]] <- graph_from_edgelist(edgelists_groups[[i]][,1:2], directed=T)
    E(graphs_groups[[i]])$weight <- as.numeric(edgelists_groups[[i]][,3])
  }
  
  print(graphs_groups)
}


## Run the function - the output will be aggregated group graphs and a metamodel graph as list objects
expertisegroups <- analysis.megafxn(matrix_data, grouplist_expertise, edgedata_individualmaps)



#### PCA ####
library(factoextra)
library(cluster)

### Read in the categorization
themes <- read.xlsx("Example Random Categorization.xlsx")

### Fxns we need
nodedataextractionfxn <- function(inputdata, inputconcepts, datatype){
  rownum <- as.numeric(length(inputconcepts))
  colnum <- as.numeric(length(inputdata))
  
  indegreedata <- matrix(data=NA, nrow=(rownum), ncol=(colnum+2))
  indegreedata[1:(rownum),1] <- inputconcepts
  colnames(indegreedata) <- c("Concepts", names(inputdata), "Mean")
  
  
  for (j in 1:colnum){
    same <- match(get.vertex.attribute(inputdata[[j]], name = "name"), indegreedata[,1])
    numvals <- as.numeric(length(same))
    
    for (i in 1:numvals){
      indegreedata[same[i], (j+1)] <- get.vertex.attribute(inputdata[[j]], name = datatype)[i]
      
    }
  }
  
  for (r in 1:rownum){
    indegreedata[r, (colnum+2)] <- mean(as.numeric(indegreedata[r,2:(colnum+1)]), na.rm = TRUE)
  }
  
  print(indegreedata)
  
}


#### Two Big PCA Functions ####
pca.dataprep <- function(descripdatalist, themes.data){
  uniquethemes <- unique(themes.data$Category)
  cent_all_themes <- matrix(ncol=nsheets, nrow=as.numeric(length(uniquethemes)))
  row.names(cent_all_themes) <- uniquethemes
  colnames(cent_all_themes) <- sheetnames
  
  
  ### Get a list of unique concepts
  conceptlist <- lapply(X=1:as.numeric(length(sheetnames)), FUN = function(x) {
    get.vertex.attribute(descripdatalist[[x]], name = "name")
  })
  names(conceptlist) <- sheetnames
  allconcepts <- unlist(conceptlist)
  uniqueconcepts <- unique(allconcepts)
  
  ##### Centrality Calculation
  absweightedcollected_rounded <- nodedataextractionfxn(descripdatalist, uniqueconcepts, "absolute weighted degree centrality")
  
  
  ### Calc
  for (j in 1:nsheets){
    sumcent <- sum(as.numeric(absweightedcollected_rounded[,1+j]), na.rm=T) 
    
    for (i in 1:as.numeric(length(uniquethemes))) {
      hold <- themes.data[which(themes.data$Category == uniquethemes[i]),1]
      vals <- as.numeric(absweightedcollected_rounded[which(absweightedcollected_rounded[,1] %in% hold == T),1+j]) 
      add <- sum(vals, na.rm = T)
      cent_all_themes[i,j] <- add/sumcent 
    }
  }
  
  ## Transpose: t(x)
  transposed.data <- t(cent_all_themes)
  raw.data <- transposed.data[,1:as.numeric(ncol(transposed.data))]
  row.names(raw.data) <- sheetnames
  
  print(raw.data)
}

pca.groups <- function(raw.data, num.clust, alledgedata, sheetnames){
  raw.pca <- prcomp(raw.data, scale. = T)
  rank.pca <- as.numeric(length(which(get_eig(raw.pca)$eigenvalue >= 1)))
  pca.dim <-  prcomp(raw.data, scale=T, rank. = rank.pca)
  pca.dim.ind <- get_pca_ind(pca.dim)
  ward.pca <- agnes(pca.dim.ind$coord, method="ward")
  pltree(ward.pca, cex = 0.6, hang = -1, main = "Dendrogram")
  
  clust.raw <- cutree(ward.pca, k = num.clust)
  groups.raw <- cbind(sheetnames, clust.raw)
  pca.grouplist <- vector("list", num.clust)
  
  for (i in 1:num.clust){
    pca.grouplist[[i]] <- groups.raw[which(groups.raw[,2] == i),1]
    names(pca.grouplist)[i] <- i
  }
  
  print(pca.grouplist)
  
  conndata.pca <- conndatafxn_agg_nozeros(alledgedata, pca.grouplist)
  conndata.pca.zeros <- conndata.pca
  conndata.pca.zeros[which(conndata.pca.zeros == "NaN")] <- 0
  
  medians.pca <- vector(mode = "numeric", length = nrow(conndata.pca.zeros))
  
  for (i in 1:nrow(conndata.pca.zeros)) {
    medians.pca[i] <- median(as.numeric(conndata.pca.zeros[i,3:(2+as.numeric(length(pca.grouplist)))]))
  }
  
  conndata.pca.withagg <- cbind(conndata.pca.zeros, medians.pca)
  
  #edge lists
  edgelists_groups <- vector("list", length=num.clust+1)
  names(edgelists_groups) <- c(1:num.clust, "Metamodel")
  
  for (i in 1:as.numeric(length(edgelists_groups))){
    edgelists_groups[[i]] <- cbind(conndata.pca.withagg[,1], conndata.pca.withagg[,2], conndata.pca.withagg[,2+i])
    edgelists_groups[[i]] <- edgelists_groups[[i]][edgelists_groups[[i]][,3] !=0,]
    colnames(edgelists_groups[[i]]) <- c("from", "to", "weights")
  }
  
  ## Now we can make graphs to mess with in R and adjacency matrices for MentalModelers
  
  # Graph
  graphs_groups <- vector("list", length=(num.clust+1))
  names(graphs_groups) <- c(1:num.clust, "Metamodel")
  
  
  for (i in 1:as.numeric(length(graphs_groups))){
    graphs_groups[[i]] <- graph_from_edgelist(edgelists_groups[[i]][,1:2], directed=T)
    E(graphs_groups[[i]])$weight <- as.numeric(edgelists_groups[[i]][,3])
  }
  
  print(graphs_groups)
}

## Run it
pca.data <- pca.dataprep(descripdatalist_individuals, themes)
pca.clustered <- pca.groups(pca.data, 4, edgedata_individualmaps, sheetnames) ## Use your best guess for the number of clusters the first time you run this, then check out the dendrogram to really decide


#### Loadings, if you need them
forloadings.pca <- prcomp(pca.data, scale. = T)
rank.pca <- as.numeric(length(which(get_eig(forloadings.pca)$eigenvalue >= 1)))
pca.dim <-  prcomp(pca.data, scale=T, rank. = rank.pca)
loadings <- pca.dim$rotation

write.csv(loadings, "Example Data - PCA Loadings.csv")


#### CONVERSION TO ADJACENCY MATRICIES for Mental Modeler ####
make.adjmats.fxn <- function(graphnames, graphs_groups){
  adjmat_groups <- vector("list", length=(as.numeric(length(graphnames))))
  names(adjmat_groups) <- graphnames
  adjmat_names <- graphnames
  
  for (i in 1:as.numeric(length(adjmat_groups))){
    adjmat_groups[[i]] <- get.adjacency(graphs_groups[[i]], sparse = FALSE, attr="weight")
    write.csv(adjmat_groups[[i]], file=paste0(adjmat_names[i], "_Adjmat", ".csv"))
  }
}

expertise_graphnames <- c(paste("Expertise", groupnames_expertise, sep = " - "), "Expertise - Metamodel")
make.adjmats.fxn(expertise_graphnames, expertisegroups)

pca_graphnames <- paste("PCA", names(pca.clustered), sep = " - ")
make.adjmats.fxn(pca_graphnames, pca.clustered)


#### PCA Group Data ####

## We can pull how the maps were grouped during the PCA using print out of the pca.clustered function, or pull the pieces out to hold it as vectors
pca.one <- c(1,2,7,8,13,17,26,30)
pca.two <- c(3,10,11,14,15,19,20,22,23,25)
pca.three <- c(4,6,9,16,18,28)
pca.four <- c(5,12,21,24,27,29)

### Then any of the processes done for the expertise data can be re-run, I'll do the descriptive data just as an example

## We'll create a workbook for each of the descriptive data types
groupnames_PCA <- c("PCA One", "PCA Two", "PCA Three", "PCA Four") # names of each group
grouplist_PCA <- list(pca.one, pca.two, pca.three, pca.four) #vector of each group of participants
names(grouplist_PCA ) <- groupnames_PCA

groupdata_PCA <- groupmapdata_fxn(individualmapdata_individuals, groupnames_PCA, grouplist_PCA)

saveWorkbook(groupdata_PCA, "Example Data - Descriptive Data by PCA Group.xlsx") 


#### Comparing Methods ####
names(expertisegroups) <- expertise_graphnames 
names(pca.clustered) <- pca_graphnames
allgraphs <- append(expertisegroups, pca.clustered)

## To determine the descriptive data for each group map + metamodel, we'll adapt the descriptive data function and apply it to all of them
descriptivedatafxn_GRAPHS <- function(graphdata){
  
  #read in the graph
  testgraph <- graphdata
  
  #want one without the negative connections for centrality
  abstestgraph <- testgraph
  E(abstestgraph)$weight <- abs(E(testgraph)$weight)
  
  # Concepts
  
  nconcepts <- as.numeric(length(V(testgraph)))
  
  conceptnames <- names(V(testgraph))
  
  
  # Edges
  
  nedges <- as.numeric(length(E(testgraph)))
  
  edges <- as_edgelist(testgraph)
  weights <- E(testgraph)$weight
  edgedata <- cbind(edges, weights)
  colnames(edgedata)[1]  <- "from"
  colnames(edgedata)[2]  <- "to"
  
  edgetype <- numeric(nedges)
  for (i in 1:nedges){
    if (weights[i]>0){
      edgetype[i] <- "positive"
    }
    else {
      edgetype[i] <- "negative"
    }
  }
  
  testgraph <- set_edge_attr(testgraph, name = "sign", value=edgetype)
  
  
  # Type of Concepts
  nodetype <- numeric(nconcepts)
  
  for (i in 1:nconcepts){
    if ((any(edgedata[,1] == conceptnames[i])) == FALSE)
    {nodetype[i] <- "Receiver"}
    
    if ((any(edgedata[,1] == conceptnames[i])) == TRUE & (any(edgedata[,2] == conceptnames[i])) == TRUE)
    {nodetype[i] <- "Ordinary"}
    
    if ((any(edgedata[,1] == conceptnames[i])) == TRUE & (any(edgedata[,2] == conceptnames[i])) == FALSE)
    {nodetype[i] <- "Driver"}
  }
  
  
  summarytypes <- cbind(sum(nodetype=="Receiver"), sum(nodetype=="Driver"), sum(nodetype=="Ordinary"))
  colnames(summarytypes) <- c("Receiver", "Driver", "Ordinary")
  
  testgraph <- set.vertex.attribute(testgraph, "type", value=nodetype)
  
  
  # Centrality of Concepts
  
  degreecent <- degree(abstestgraph, mode = "all")
  indegreecent <- degree(abstestgraph, mode = "in")
  outdegreecent <- degree(abstestgraph, mode = "out")
  
  normdegreecent <- degree(abstestgraph, mode = "all", normalized = T)
  normindegreecent <- degree(abstestgraph, mode = "in", normalized = T)
  normoutdegreecent <- degree(abstestgraph, mode = "out", normalized = T)
  
  degcentdist <- degree.distribution(abstestgraph, mode="all")
  outdegdist <- degree.distribution(abstestgraph, mode="out")
  indegdist <- degree.distribution(abstestgraph, mode="in")
  
  absmatrix <- abs(as_adj(graphdata, sparse=F, attr = "weight"))
  tnet_mat <- as.tnet(absmatrix, type="weighted one-mode tnet")
  degree_out <- degree_w(tnet_mat, type= "out")
  degree_in <- degree_w(tnet_mat, type= "in")
  
  absweightedcent <- numeric(length=as.numeric(length(degree_out[,"output"])))
  
  for (i in 1:as.numeric(length(degree_out[,"output"]))){
    absweightedcent[i] <- sum(degree_out[i,"output"], degree_in[i,"output"])
  }
  
  
  
  # for (i in 1:nconcepts){
  
  #  }
  
  closenesscent <- closeness(abstestgraph, mode = "all", weights = abs(E(testgraph)$weight))
  betweennesscent <- betweenness(abstestgraph, weights = abs(E(testgraph)$weight))
  
  attributelist <- c("absolute weighted degree centrality" , "degree centrality", "indegree centrality", "outdegree centrality", "normalized degree centrality", "normalized indegree centrality", "normalized outdegree centrality", "closeness centrality", "betweenness centrality")
  centlist <- cbind(absweightedcent, degreecent, indegreecent, outdegreecent, normdegreecent, normindegreecent, normoutdegreecent, closenesscent, betweennesscent)
  
  for (i in 1:as.numeric(length(attributelist))){
    testgraph <- set_vertex_attr(graph=testgraph, name = attributelist[i], value= centlist[,i])
  }
  
  # Density
  density <- edge_density(testgraph)
  
  # C/N
  CN <- nedges/nconcepts
  
  # Complexity
  complexity <- 0
  
  if (summarytypes[1,1] != 0 & summarytypes[1,2] != 0) {
    complexity <-  as.numeric(summarytypes[1,1]/summarytypes[1,2])
  } else {complexity <-  0}
  
  
  
  # putting it all together
  graphattributelist <- c("Number of Components", "Number of Connections", "Summary of Component Types", "Degree Centrality Distribution", "Outdegree Centrality Distribution", "Indegree Centrality Distribution", "Density", "Connections per Component", "Complexity", "Edge List" )
  graphlist <- list(nconcepts, nedges, summarytypes, degcentdist, outdegdist, indegdist, density, CN, complexity, edgedata)
  
  for (i in 1:as.numeric(length(graphattributelist))){
    testgraph <- set_graph_attr(graph=testgraph, name = graphattributelist[i], value= graphlist[[i]])
  }
  
  
  print(testgraph)
  
}


### Run the function
descripdata_allgraphs <- lapply(X=allgraphs, FUN = descriptivedatafxn_GRAPHS)
names(descripdata_allgraphs) <- c(expertise_graphnames, pca_graphnames)

allgraphs_mapsdata <- individualmapdata_fxn(descripdata_allgraphs)
write.xlsx(allgraphs_mapsdata, "Example Data - Map Data for Group Maps + Metamodels.xlsx") 

## You can also use some of the other functions and just have the groups be like all of them individually or by aggregation technique.





#### MICRO-MOTIFS ####
motif_count_fxn <- function(graph) {
  motifs <- vector(mode = "list", length=8)
  names(motifs) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops", "# of Nodes", "# of Edges")
  
  template.bidirectionality <- make_graph(c(1,2,2,1), directed =T)
  iso <- subgraph_isomorphisms(template.bidirectionality , graph)
  
  if (as.numeric(length(iso)) >= 1) {
    possbidir <- vector(mode = "list", length = as.numeric(length(iso)))
    for (i in 1:(as.numeric(length(iso)))){
      possbidir[[i]] <- sort(as_ids(iso[[i]]))
    }
    motifs[[1]] <- as.numeric(length(unique(possbidir)))} else {motifs[[1]]<- 0}
  
  triads <- triad.census(graph)
  motifs[[2]] <- triads[[5]]
  motifs[[3]] <- triads[[4]]
  motifs[[4]] <- triads[[6]]
  motifs[[5]] <- triads[[9]]
  motifs[[6]] <- triads[[10]]
  motifs[[7]] <- length(V(graph))
  motifs[[8]] <- length(E(graph))
  
  print(motifs)
}

individualgraphs <- descripdatalist_individuals

individual_motifs <- lapply(individualgraphs, motif_count_fxn)

expertise_motifs <- lapply(expertisegroups, motif_count_fxn)
PCA_motifs <- lapply(pca.clustered, motif_count_fxn)

### Now we need to export this info
motif_wb <- createWorkbook()
mofit_wb_sheetnames <- c("Individual Maps", "Expertise", "PCA")
lapply(mofit_wb_sheetnames, FUN = addWorksheet, wb=motif_wb)

motiflist_to_matrix <- function(motiflist){
  nrows <- as.numeric(length(motiflist))
  motifmatrix <- matrix(nrow = nrows, ncol = 8)
  colnames(motifmatrix) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops", "# of Nodes", "# of Edges")
  row.names(motifmatrix) <- names(motiflist)
  for (i in 1:nrows){
    motifmatrix[i,] <- unlist(motiflist[[i]])
  }
  print(motifmatrix)
}

writeData(motif_wb, sheet = "Individual Maps", x=motiflist_to_matrix(individual_motifs), rowNames = T, colNames = T)
writeData(motif_wb, sheet = "Expertise", x=motiflist_to_matrix(expertise_motifs), rowNames = T, colNames = T)
writeData(motif_wb, sheet = "PCA", x=motiflist_to_matrix(PCA_motifs), rowNames = T, colNames = T)

saveWorkbook(motif_wb, "Example Data - Motif Calculation for Individuals and Groups.xlsx")



#### Comparison with random connected graphs of the same size ####

rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

motiflist_to_matrix_hist <- function(motiflist){
  nrows <- as.numeric(length(motiflist))
  motifmatrix <- matrix(nrow = nrows, ncol = 6)
  colnames(motifmatrix) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops")
  row.names(motifmatrix) <- names(motiflist)
  for (i in 1:nrows){
    motifmatrix[i,] <- unlist(motiflist[[i]])
  }
  print(motifmatrix)
}


#### Calculate the probability that the number of micromotifs in a random graph is LESS than the actual graph
hist_fxn <- function(motif_list_single, ntests){
  nnodes <- as.numeric(motif_list_single$`# of Nodes`)
  nedges <- as.numeric(motif_list_single$`# of Edges`)
  
  prob_motifs <- vector("list", length = 6)
  names(prob_motifs) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops")
  
  counts_empty <- vector("numeric", length = ntests)
  motifs <- list(counts_empty,counts_empty,counts_empty,counts_empty,counts_empty,counts_empty)
  names(motifs) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops")
  
  for (t in 1:ntests){
    r <- nnodes
    
    while (r > 0){
      out_vect <- rand_vect(nnodes, nedges)
      in_vect <- rand_vect(nnodes, nedges)
      
      for (i in 1:nnodes){
        if((out_vect[i] == 0) & (in_vect[i] == 0)){
          print(i)
        }else r <- r-1} 
    }
    
    g <-  sample_degseq(out.deg = out_vect, in.deg = in_vect, method = "simple.no.multiple")
    counts <- motif_count_fxn(g)
    
    for (q in 1:6){ # 6 is the number of motifs
      motifs[[q]][t] <- counts[[q]]
    }
    r <- nnodes
  }
  for (v in 1:6){
    prob_motifs[[v]] <- length(which(motifs[[v]] < motif_list_single[[v]]))/1000 
  }
  print(prob_motifs)
  
}

expertise_hist <- lapply(expertise_motifs, FUN = hist_fxn, ntests=1000)
PCA_hist <- lapply(PCA_motifs, FUN = hist_fxn, ntests=1000)


#### INDIVIDUAL HIST ####

hist_fxn_problem <- function(motif_list_single, ntests){
  nnodes <- as.numeric(motif_list_single$`# of Nodes`)
  nedges <- as.numeric(motif_list_single$`# of Edges`)
  
  counts_empty <- vector("numeric", length = ntests)
  motifs <- list(counts_empty,counts_empty,counts_empty,counts_empty,counts_empty,counts_empty)
  names(motifs) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops")
  
  for (t in 1:ntests){
    r <- nnodes
    
    while (r > 0){
      r <- nnodes
      out_vect <- rand_vect(nnodes, nedges)
      in_vect <- rand_vect(nnodes, nedges)
      
      for (i in 1:nnodes){
        if((out_vect[i] == 0) & (in_vect[i] == 0)){
          print(i)
        }else r <- r-1} 
    }
    
    g <-  sample_degseq(out.deg = out_vect, in.deg = in_vect, method = "simple.no.multiple")
    counts <- motif_count_fxn(g)
    
    for (q in 1:6){ # 6 is the number of motifs
      motifs[[q]][t] <- counts[[q]]
    }
    r <- nnodes
  }
  print(motifs)
}

individual_hist <- vector("list", nsheets)
names(individual_hist) <- sheetnames

## Start with 1:nsheets
for (i in 1:nsheets){
  individual_hist[[i]] <- hist_fxn(individual_motifs[[i]], 1000)
}

## If you get the error "No simple directed graph can realize the given degree sequence"
### FIRST change the 1 to the value where it stopped (check the individual_hist list to see which doesn't have an entry) and re-run
### IF when you re-run it, you still get the same error FOR THAT ENTRY, follow the below process for that specific map.


which(sheetnames == NAME) ## If your IDs/sheet names are numeric, change this to whichever one doesn't have the list of final probabilities, to get the problem.number
## This process is a bit of a nightmare if you have super sparse or super dense maps

## start at 1:1000 and basically update where it got to and rerun it.
q <- 1
individual_hist_problem <- vector("list", 1000)
names(individual_hist_problem) <- c(1:1000)
problem.number <- 1 ## which list item is the issue?

## START HERE FOR RE-RUNS
for (i in q:1000){
  individual_hist_problem[[i]] <- hist_fxn_problem(individual_motifs[[problem.number]], 1)
}


r <- 1
while(r>0){
  if (class(individual_hist_problem[[r]]) == "list") {r <- r+1
  } else {
    q <- r
    r <- 0
    print(q)
  }}

## END HERE FOR RE-RUNS
### Your final r value should be 1001, that's when you know to stop! You should get the error "subscript out of bounds"

#### Now put it in your main matrix
indiv_hist_problem_matrix <- motiflist_to_matrix_hist(individual_hist_problem)
prob_motifs_problem <- vector("list", length = 6)
names(prob_motifs_problem) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops")

for (v in 1:6){
  prob_motifs_problem[[v]] <- length(which(indiv_hist_problem_matrix[,v] < individual_motifs[[problem.number]][[v]]))/1000 # or ==
}


individual_hist[[problem.number]] <- prob_motifs_problem
## then change the main loop are re-run to problem number + 1




### Write the data
motif_wb_hist <- createWorkbook()
mofit_wb_hist_sheetnames <- c("Individual Maps", "Expertise", "PCA")
lapply(mofit_wb_hist_sheetnames, FUN = addWorksheet, wb=motif_wb_hist)

motiflist_to_matrix_hist <- function(motiflist){
  nrows <- as.numeric(length(motiflist))
  motifmatrix <- matrix(nrow = nrows, ncol = 6)
  colnames(motifmatrix) <- c("Bidirectionality", "Multiple Causes", "Multiple Effects", "Indirect Effect", "Moderated Effect", "Feedback Loops")
  row.names(motifmatrix) <- names(motiflist)
  for (i in 1:nrows){
    motifmatrix[i,] <- unlist(motiflist[[i]])
  }
  print(motifmatrix)
}

writeData(motif_wb_hist, sheet = "Individual Maps", x=motiflist_to_matrix_hist(individual_hist), rowNames=T, colNames=T)
writeData(motif_wb_hist, sheet = "Expertise", x=motiflist_to_matrix_hist(expertise_hist), rowNames = T, colNames = T)
writeData(motif_wb_hist, sheet = "PCA", x=motiflist_to_matrix_hist(PCA_hist), rowNames = T, colNames = T)

saveWorkbook(motif_wb_hist, "Example Data - Motif Probability Calculation.xlsx")
