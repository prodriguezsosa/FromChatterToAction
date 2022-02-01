######################################################################################################################################
#
# TITLE: From Chatter to Action: How Social Networks Inform and Motivate in Rural Uganda - Replication Code to run analysis
# AUTHORS: Jennifer M. Larson, Janet I. Lewis, Pedro L. Rodriguez
# DATE (BEGUN): 04-11-2017
# DATE (LAST MODIFIED): 05-04-2021
#
#######################################################################################################################################

# sessionInfo()
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 10.16

# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] corrplot_0.84      magrittr_2.0.1     linkcomm_1.0-14    data.table_1.14.0  nlme_3.1-152       gdata_2.18.0       dplyr_1.0.5        dtplyr_1.1.0      
# [9] car_3.0-10         carData_3.0-4      RColorBrewer_1.1-2 xtable_1.8-4       igraph_1.2.6       stargazer_5.2.2    mfx_1.2-2          betareg_3.1-4     
# [17] MASS_7.3-53.1      lmtest_0.9-38      zoo_1.8-9          sandwich_3.0-0     plyr_1.8.6         reshape2_1.4.4     ggplot2_3.3.3      gplots_3.1.1      

# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.6            lattice_0.20-41       gtools_3.8.2          assertthat_0.2.1      digest_0.6.27         utf8_1.2.1            cellranger_1.1.0     
# [8] R6_2.5.0              dynamicTreeCut_1.63-1 stats4_4.0.5          evaluate_0.14         pillar_1.5.1          rlang_0.4.10          readxl_1.3.1         
# [15] curl_4.3              rmarkdown_2.7         stringr_1.4.0         foreign_0.8-81        munsell_0.5.0         compiler_4.0.5        xfun_0.22            
# [22] pkgconfig_2.0.3       htmltools_0.5.1.1     nnet_7.3-15           tidyselect_1.1.0      tibble_3.1.0          rio_0.5.26            fansi_0.4.2          
# [29] crayon_1.4.1          withr_2.4.1           bitops_1.0-7          gtable_0.3.0          lifecycle_1.0.0       DBI_1.1.1             scales_1.1.1         
# [36] zip_2.1.1             KernSmooth_2.23-18    stringi_1.5.3         flexmix_2.3-17        ellipsis_0.3.1        generics_0.1.0        vctrs_0.3.7          
# [43] openxlsx_4.2.3        Formula_1.2-4         tools_4.0.5           forcats_0.5.1         glue_1.4.2            purrr_0.3.4           hms_1.0.0            
# [50] abind_1.4-5           yaml_2.2.1            colorspace_2.0-0      caTools_1.18.2        knitr_1.31            haven_2.3.1           modeltools_0.2-23   

# define path
in_path <- "~/Dropbox/GitHub/repositories/FromChatterToAction/data/" # set this path to where you saved the file abalang.csv

#################################################################################################################
#
#
#                                           GENERATE MAIN DATASET
#
#
#################################################################################################################

# Libraries
library(igraph)
library(gdata)
library(stargazer)
library(nlme)
library(plyr)
library(car)
library(data.table)
library(reshape2)
library(linkcomm)
library(magrittr)

#################################################################################################################
# FUNCTIONS
#################################################################################################################

# Edgelist Builder
ConstructEdgelist <- function(df, layers, directed){
  # Converts a wide dataset containing edges to long dataset form
  # Provides option to build undirected network
  # The function is designed for the Abalang dataset format although it can easily be adjusted to other formats
  #
  # Args:
  # df: dataset in wide format
  # layers: layers to be included (vector)
  # directed: FALSE if we want undirected edgelist (deletes one instance of all bi-directional edges)
  #
  # Returns:
  # dataset of edges in long format
  #
  edges <- c("Ego", layers)                                                                                               # define layers of interest
  edges <- df[edges]                                                                                                      # keep only layers of interest & ego
  edge.list <- reshape(edges, varying = layers, v.names = "Alter", timevar = "Layer", times = layers, direction = "long") # reshape from wide to long
  row.names(edge.list) <- NULL                                                                                            # clean row names
  edge.list <- edge.list[,-4]                                                                                             # remove id variable
  edge.list$Layer <- gsub("[0-9]*$", "", edge.list$Layer)                                                                 # delete numbers at end of layer label
  edge.list <- subset(edge.list, !(is.na(Alter)))                                                                         # remove missing links due to reshaping
  edge.list <- edge.list[,c(1, 3, 2)]                                                                                     # correct column order
  edge.list <- subset(edge.list, Ego!=Alter)                                                                              # delete loops (Ego = Alter)
  edge.list <- ddply(edge.list, .(Layer), unique)                                                                         # delete duplicate edges by layer
  if(directed == FALSE){                                                                                                  # apply this part of function if directed == FALSE
    layers <- unique(gsub("[0-9]*$", "", layers))                                                                         # relabel layers list (w/o numbers)
    el <- list()                                                                                                          # empty list to store edges
    for (i in 1:length(layers)){                         
      temp <- subset(edge.list, edge.list$Layer == layers[[i]])                                                           # subset edgelist by layer
      duplicate.edges <- data.frame("index" = edge.duplicates(temp, verbose = TRUE)$inds)                                 # identify duplicate edges and loops
      temp$ID <- seq(1, nrow(temp), 1)                                                                                    # create index variable with which to subset
      temp <- temp[! temp$ID %in% duplicate.edges$index,]                                                                 # delete one instance of any bi-directional edge
      el[[i]] <- temp[,-4]                                                                                                # store edges
    }
    return(ldply(el, rbind))                                                                                              # build edges dataframe
  } 
  return(edge.list)
}

#################################################################################################################
# LOAD DATA
#################################################################################################################

# Set working directory
data <- read.csv(paste0(in_path, "abalang_deidentified.csv"), stringsAsFactors = FALSE)
# Construct variable identifying Abalang residents (broadest definition)
data$abalang1 <- ifelse(data$CoarseVlg == "ABALANG" | data$CoarseVlg == "AKUM" | data$CoarseVlg == "ALOET" | data$CoarseVlg == "CENTRAL" | data$CoarseVlg == "UNKNOWN" | data$CoarseVlg == "AKAIKAI" | data$CoarseVlg == "GWERI" | data$CoarseVlg == "OGOLOID", 1, 0)
# Construct variable identifying Abalang residents (intermediate definition)
data$abalang2 <- ifelse(data$CoarseVlg == "ABALANG" | data$CoarseVlg == "AKUM" | data$CoarseVlg == "ALOET" | data$CoarseVlg == "CENTRAL" | data$CoarseVlg == "UNKNOWN", 1, 0)
# Construct variable identifying Abalang residents (narrowest definition)
data$abalang3 <- ifelse(data$CoarseVlg == "ABALANG", 1, 0)
# Drop observations with missing Ego
data <- subset(data, !is.na(Ego))   
# Drop observations with duplicated Ego (keep just one)
data <- data[!duplicated(data$Ego), ]   
# Keep only Abalang observations (broadest definition, change when performing robustness checks)
data <- subset(data, abalang1==1)       
# Assume that if Ego attended she must have heard about the event (only one such case)
#which(is.na(data$Attend) & data$Hear == "N")
data$Attend[is.na(data$Attend) & data$Hear=='N'] <- 'N'

#################################################################################################################
# CLEAN DATA 1:
# only those sampled
#################################################################################################################

#########################################################
# NODE ATTRIBUTES
#########################################################

# Specify the variables of interest (don't forget Egos)
attributes <- c("Ego", "Hear", "Attend", "EventDay", "IsSeed", "IsSeedHH", "IsSeedRing", "Gender", "Age", "MinsTrav", "Educ", "WallMat", "JobType", "Married", "NoteRumor", "Relig", "EventCode")
# Keep only these varibles
nodedf <- data[attributes]
# Hear, attend and note rumor
nodedf$Hear <- as.numeric(ifelse(nodedf$Hear == "Y", 1, ifelse(!is.na(nodedf$Hear), 0, NA)))
nodedf$Attend <- as.numeric(ifelse(nodedf$Attend == "Y", 1, ifelse(!is.na(nodedf$Attend), 0, NA)))
nodedf$NoteRumor <- as.numeric(ifelse(nodedf$NoteRumor == "Y", 1, ifelse(!is.na(nodedf$NoteRumor), 0, NA)))
# Gender
nodedf$Gender <- as.numeric(ifelse(nodedf$Gender == "F", 1, ifelse(!is.na(nodedf$Gender), 0, NA)))
# Marriage
nodedf$Married <- as.numeric(ifelse(nodedf$Married == "Y", 1, ifelse(!is.na(nodedf$Married), 0, NA)))
# Religion (note: there are no reported muslims)
#nodedf$Relig[nodedf$Relig==5] <- NA
nodedf$Other <- ifelse(nodedf$Relig==5, 1, ifelse(!is.na(nodedf$Relig), 0, NA))
nodedf$Catholic <- ifelse(nodedf$Relig==1, 1, ifelse(!is.na(nodedf$Relig), 0, NA))
nodedf$Anglican<- ifelse(nodedf$Relig==2, 1, ifelse(!is.na(nodedf$Relig), 0, NA))
#nodedf$Muslim <- ifelse(nodedf$Relig==3, 1, ifelse(!is.na(nodedf$Relig), 0, NA))
nodedf$Pentacostal <- ifelse(nodedf$Relig==4, 1, ifelse(!is.na(nodedf$Relig), 0, NA))
# Employment
nodedf$Retired <- ifelse(nodedf$JobType == 1, 1, ifelse(!is.na(nodedf$JobType), 0, NA)) 
nodedf$Unemp <- ifelse(nodedf$JobType == 2, 1, ifelse(!is.na(nodedf$JobType), 0, NA)) 
nodedf$PartTime <- ifelse(nodedf$JobType == 3, 1, ifelse(!is.na(nodedf$JobType), 0, NA))
nodedf$FullTime <- ifelse(nodedf$JobType == 4, 1, ifelse(!is.na(nodedf$JobType), 0, NA))
# Housing (1 = cement or burned bricks, 0 = mud)
nodedf$WallMat <- ifelse(nodedf$WallMat >=5, 1, ifelse(!is.na(nodedf$WallMat), 0, NA)) 
nodedf$IsSeedRing <- as.numeric(nodedf$IsSeedRing)

#########################################################
# EDGES
#########################################################

# Specify layers of interest
layers.all <- c("Time1", "Time2", "Time3", "Time4", "Time5",               
                "Phone1", "Phone2", "Phone3", "Phone4", "Phone5",
                "Pol1", "Pol2", "Pol3", "Pol4", "Pol5",
                "Relig1", "Relig2", "Relig3", "Relig4", "Relig5",
                "Meal1", "Meal2", "Meal3", "Meal4", "Meal5",
                "Visit1", "Visit2", "Visit3", "Visit4", "Visit5",
                "Secret1", "Secret2", "Secret3", "Secret4", "Secret5",
                "To1", "To2", "To3", "To4", "To5",
                "From1", "From2", "From3", "From4", "From5")

# Use costum built function to construct undirected edgelist
edgelistall <- ConstructEdgelist(df = data, layers = layers.all, directed = FALSE)

#########################################################
# USE TO AND FROM ANSWERS TO COMPLETE EACH NETWORK
#########################################################

# Assumption: if A lists B in the "from" network then B must have told A. If A lists B in the "to" network, then B must have heard from A
# Keep "to" edgelist
elfromab <- subset(edgelistall, edgelistall$Layer=="To")
# Switch ego and alter labels
colnames(elfromab) <- c("Alter", "Ego", "Layer")
# Change the value of the layer variable from "To" to "From"
elfromab$Layer <- "From"
# Reorder columns
elfromab <- elfromab[,c(2, 1, 3)]
# Repeat same procedure with "From" layer
eltoab <- subset(edgelistall, edgelistall$Layer=="From")
colnames(eltoab) <- c("Alter", "Ego", "Layer")
eltoab$Layer <- "To"
eltoab <- eltoab[,c(2, 1, 3)]
# Append to overall edgelist
edgelistall <- rbind(edgelistall, eltoab, elfromab)   
# Delete duplicates created by append above (duplicates result from directed To-From edges)
edgelistall <- edgelistall[!duplicated(edgelistall[c("Ego", "Alter", "Layer")]),]
# Spring cleaning
rm(attributes, eltoab, elfromab)

#############################################################################################################
# CLEAN DATA 2:
# Add as egos any name that was mentioned as a link
# Even for non-sampled egos we can get such attributes as: attended, heard, told and number of people told
# This is useful both for the clustering analysis as well as regressions including neighborhood attributes
#############################################################################################################

# Use edgelist to create list of entire population (i.e all that we either mentioned and/or sampled)
# This just grabs all alters and appends them to the ego column
abalangego <- edgelistall["Ego"]
abalangalter <- edgelistall["Alter"]
colnames(abalangalter) <- c("Ego")
abalangpop <- rbind(abalangego, abalangalter)
abalangpop <- unique(abalangpop)

# Merge with attribute dataframe from above using Ego as the merging variable
# This creates a node attribute dataset of the entire population (sampled and/or mentioned) in which only those sampled will have non-missing attributes
# Create new variable to indicate whether the Ego was sampled or not (i.e. was only mentioned)
nodedf$Sampled <- 1
nodedfall <- join(abalangpop, nodedf, by = "Ego", type = "left")
nodedfall$Sampled[is.na(nodedfall$Sampled)] <- 0

# Complete hear attribute
# Assumption: if A was listed in either a "To" or "From" network then A must have heard about the event
# Keep only "To" and "From networks
hearcheck <- subset(edgelistall, Layer == "To" | Layer == "From")
# Create hearcheck variable to be merged to main dataframe
hearcheck$HearCheck <- 1
hearcheck <- hearcheck[c("Ego", "HearCheck")]
hearcheck <- unique(hearcheck)
# Merge with node-attribute framework previously created
nodedfall <- join(nodedfall, hearcheck, by = "Ego", type = "left")
# Change hear value to 1 if hearcheck value is equal to 1 (i.e. if Ego appeared in a to and/or from network)
nodedfall$Hear[nodedfall$HearCheck == 1] <- 1
# Delete hearcheck variable
nodedfall <- subset(nodedfall, select=-c(HearCheck))

# Add told attribute
# Dichotomous variable indicating whether ego told somone or not about the event
# Assumption: if ego is not mentioned in either the To or From network then told = 0
told <- subset(edgelistall, Layer == "To")
told$Told <- 1
told <- told[c("Ego", "Told")]
told <- unique(told)
nodedfall <- join(nodedfall, told, by = "Ego", type = "left")
nodedfall$Told[which(is.na(nodedfall$Told))] <- 0 

# Add told attribute as count
# Number of people ego told about event
told <- subset(edgelistall, Layer == "To")
told$Told <- 1
told <- told[c("Ego", "Told")]
told.sum <- aggregate(told$Told, by = list(told$Ego), sum)
colnames(told.sum) <- c("Ego", "ToldCount")
nodedfall <- join(nodedfall, told.sum, by = "Ego", type = "left")
nodedfall$ToldCount[which(is.na(nodedfall$ToldCount))] <- 0 

# Add from attribute as count
# Number of people ego heard from
from <- subset(edgelistall, Layer == "From")
from$From <- 1
from <- from[c("Ego", "From")]
from.sum <- aggregate(from$From, by = list(from$Ego), sum)
colnames(from.sum) <- c("Ego", "FromCount")
nodedfall <- join(nodedfall, from.sum, by = "Ego", type = "left")

# Complete attend 
# Assumption: anyone not sampled in the event did not attend the event (i.e. all that attended were sampled)
nodedfall$Attend <- ifelse(nodedfall$EventCode==1| nodedfall$EventCode==4| nodedfall$EventCode==5, 1, 0)
nodedfall$Attend[is.na(nodedfall$Attend)] <- 0

# Compute degree (including only the 7 social networks)
# create network graph using all edges
layers.social <- list("Time", "Phone", "Pol", "Relig", "Meal", "Visit", "Secret")
graph.edgelistall <- graph.data.frame(edgelistall[edgelistall$Layer %in% layers.social,], directed=FALSE, vertices=NULL)
# compute degree
degree.temp <- data.frame("Degree" = degree(graph.edgelistall))
degree.temp$Ego <- rownames(degree.temp)
rownames(degree.temp) <- NULL
nodedfall <- merge(nodedfall, degree.temp, by = "Ego", all.x = TRUE)
# If degree is missing assume it's 0
#nodedfall$Degree[is.na(nodedfall$Degree)] <- 0

# Compute eigenvector centrality
eigen.temp <- data.frame("Eigen" = eigen_centrality(graph.edgelistall)$vector)
eigen.temp$Ego <- rownames(eigen.temp)
rownames(eigen.temp) <- NULL
nodedfall <- merge(nodedfall, eigen.temp, by="Ego", all.x = TRUE)

# Compute betweenness
betweenness.temp <- data.frame("Betweennes" = betweenness(graph.edgelistall, normalized = TRUE))
betweenness.temp$Ego <- rownames(betweenness.temp)
rownames(betweenness.temp) <- NULL
nodedfall <- merge(nodedfall, betweenness.temp, by="Ego", all.x = TRUE)

# Compute shortest path to seed
# make list of seed names
seed <- as.list(nodedfall$Ego[which(nodedfall$IsSeed == 1)])
# compute distance matrix
distMatrix <- shortest.paths(graph.edgelistall, v=V(graph.edgelistall), to=V(graph.edgelistall))
# make list of ego names
ego <- rownames(distMatrix)
# extract relevant distance from distance matrix (i.e. minimum distance to a seed for each ego)
dist <- vector()
dist_other <- vector("numeric", length(ego))
temp <- vector()
for (i in 1:length(ego)){
  for (j in 1:length(seed)){
    temp[j] <- distMatrix[ego[[i]],seed[[j]]]
  }
  dist[i] <- min(temp)
  if(ego[[i]] %in% seed){dist_other[i] <- temp[order(temp)][2]}
}
ego <- as.character(unlist(ego))
# create distance dataframe
dist <- data.frame("Ego" = ego, "DistSeed" = dist, "DistOtherSeed" = dist)
dist$DistOtherSeed[dist$DistOtherSeed== 0] <- dist_other[dist_other!=0]
# merge with nodes dataframe
nodedfall <- merge(nodedfall, dist, by = "Ego", all.x = TRUE)
# connected to seed dummy
nodedfall$SeedConnect <- ifelse(!is.infinite(nodedfall$DistSeed) & !is.na(nodedfall$DistSeed), 1, ifelse(is.infinite(nodedfall$DistSeed), 0, NA))
# directly connected to seed dummy (part of seed ring)
nodedfall$SeedRing <- ifelse(nodedfall$DistSeed == 1, 1, ifelse(!is.na(nodedfall$DistSeed), 0, NA))

###########################################################
# Distance to each of the seven seeds individually
###########################################################

seedcols <- c()
for (i in 1:length(seed)){
  newrow <- which(colnames(distMatrix) == seed[[i]])
  seedcols <- c(seedcols, newrow)
}
## Uncomment to check:
#seed
#colnames(distMatrix)[seedcols]
## same.

distSeedMatrix <- distMatrix[,seedcols]
distseedsdf <- data.frame(distSeedMatrix)
colnames(distseedsdf) <- c("DistSeed1", "DistSeed2", "DistSeed3", "DistSeed4", "DistSeed5", "DistSeed6", "DistSeed7")
distseedsdf$Ego <- rownames(distseedsdf)
nodedfall <- merge(nodedfall, distseedsdf, by = "Ego", all.x=T)

##############################################################################################################
# Add variable for neighborhood size 
# (different from degree, which counts in- and out-links separately, even if to same person)
##############################################################################################################
neighbsize <- c()
for(i in 1:nrow(distMatrix)){
  neighbsize[i] <- sum(distMatrix[i,]==1)
}

nstemp <- data.frame("Ego" = rownames(distMatrix), "Neighborhood"=neighbsize)	
nodedfall <- merge(nodedfall, nstemp, by="Ego", all.x=T)

# And 2-neighborhood size
neighb2 <- c()
for(i in 1:nrow(distMatrix)){
  neighb2[i] <- sum(distMatrix[i,]==1 | distMatrix[i,] == 2)
}

nb2temp <- data.frame("Ego" = rownames(distMatrix), "Neighb2"=neighb2)	
nodedfall <- merge(nodedfall, nb2temp, by="Ego", all.x=T)

# And 3-neighborhood size
neighb3 <- c()
for(i in 1:nrow(distMatrix)){
  neighb3[i] <- sum(distMatrix[i,]==1 | distMatrix[i,] == 2 | distMatrix[i,]==3)
}

nb3temp <- data.frame("Ego" = rownames(distMatrix), "Neighb3"=neighb3)	
nodedfall <- merge(nodedfall, nb3temp, by="Ego", all.x=T)

# And 4-neighborhood size
neighb4 <- c()
for(i in 1:nrow(distMatrix)){
  neighb4[i] <- sum(distMatrix[i,]==1 | distMatrix[i,] == 2 | distMatrix[i,]==3 | distMatrix[i,]==4)
}

nb4temp <- data.frame("Ego" = rownames(distMatrix), "Neighb4"=neighb4)	
nodedfall <- merge(nodedfall, nb4temp, by="Ego", all.x=T)

# And 5-neighborhood size
neighb5 <- c()
for(i in 1:nrow(distMatrix)){
  neighb5[i] <- sum(distMatrix[i,]==1 | distMatrix[i,] == 2 | distMatrix[i,]==3 | distMatrix[i,]==4 | distMatrix[i,]==5)
}

nb5temp <- data.frame("Ego" = rownames(distMatrix), "Neighb5"=neighb5)	
nodedfall <- merge(nodedfall, nb5temp, by="Ego", all.x=T)

# And 6-neighborhood size
neighb6 <- c()
for(i in 1:nrow(distMatrix)){
  neighb6[i] <- sum(distMatrix[i,]==1 | distMatrix[i,] == 2 | distMatrix[i,]==3 | distMatrix[i,]==4 | distMatrix[i,]==5 | distMatrix[i,]==6)
}

nb6temp <- data.frame("Ego" = rownames(distMatrix), "Neighb6"=neighb6)	
nodedfall <- merge(nodedfall, nb6temp, by="Ego", all.x=T)

########################################################
# Compute average distance to others in the network:
########################################################

avgdist.temp <- data.frame("AvgDist" = apply(distMatrix, 1, mean))
avgdist.temp$Ego <- rownames(avgdist.temp)
rownames(avgdist.temp) <- NULL
nodedfall <- merge(nodedfall, avgdist.temp, by = "Ego", all.x = TRUE)

########################################################
# Compute shortest path to 1st day attender (leader)
########################################################

# make list of seed names
leader <- as.list(nodedfall$Ego[which(nodedfall$EventDay == 1)])
# extract relevant distance from distance matrix (i.e. minimum distance to a seed for each ego)
dist <- vector()
dist_other <- vector("numeric", length(ego))
temp <- vector()
for (i in 1:length(ego)){
  for (j in 1:length(leader)){
    temp[j] <- distMatrix[ego[[i]],leader[[j]]]
  }
  dist[i] <- min(temp)
  if(ego[[i]] %in% leader){dist_other[i] <- temp[order(temp)][2]}
}
ego <- as.character(unlist(ego))
# create distance dataframe
dist <- data.frame("Ego" = ego, "DistLeader" = dist, "DistOtherLeader" = dist)
dist$DistOtherLeader[dist$DistOtherLeader== 0] <- dist_other[dist_other!=0]
# merge with nodes dataframe
nodedfall <- merge(nodedfall, dist, by = "Ego", all.x = TRUE)
# connected to leader dummy
nodedfall$LeaderConnect <- ifelse(!is.infinite(nodedfall$DistLeader) & !is.na(nodedfall$DistLeader), 1, ifelse(is.infinite(nodedfall$DistLeader), 0, NA))
# directly connected to leader dummy (part of leader ring)
nodedfall$LeaderRing <- ifelse(nodedfall$DistLeader == 1, 1, ifelse(!is.na(nodedfall$DistLeader), 0, NA))

# Spring cleaning
keep(data, edgelistall, nodedfall, layers.all, ConstructEdgelist, layers.social, sure = TRUE)

#############################################################################################################
# CLEAN DATA 3:
# Add neighbors and neighbor attributes to each ego
# Each row represents an ego-alter combination and will contain both ego and alter attributes
# Equivalent to an edgelist with ego and alter attributes
# This is useful when creating neighborhood measures to include in the regression analysis
#############################################################################################################

# Take overall edgelist created above (we leave in To/From and will adjust for duplicates below as they are symmetric)
ego.neighbors <- edgelistall
alter.neighbors <- edgelistall[c(2, 1, 3)]
colnames(alter.neighbors) <- c("Ego", "Alter", "Layer")
# Note: To, From and Father are directed networks by definition
alter.neighbors$Layer[ego.neighbors$Layer == "From"] <- "To"
alter.neighbors$Layer[ego.neighbors$Layer == "To"] <- "From"
#alter.neighbors$Layer[ego.neighbors$Layer == "Father"] <- "Offspring"
neighbors <- rbind(ego.neighbors, alter.neighbors)
# Delete duplicates (this will delete duplicates created by To and From)
neighbors <- neighbors[!duplicated(neighbors[c("Ego", "Alter", "Layer")]),]
# Merge with node-attributes dataframe using Ego
nodedfallneighbors <- join(nodedfall, neighbors, by = "Ego", type = "left")
# Sort by ego and alter names
nodedfallneighbors <- nodedfallneighbors[order(nodedfallneighbors$Ego, nodedfallneighbors$Alter),]   
# Reorder columns
nodedfallneighbors <- nodedfallneighbors[,c(1, ncol(nodedfallneighbors)-1, ncol(nodedfallneighbors), 2:(ncol(nodedfallneighbors)-2))]
# Build alter attributes dataframe simply by relabeling the node-attributes dataframe above
alternodedfall <- nodedfall
# Relabel columns of the node-attribute dataframe to distinguish from ego attributes
columnlabels <- paste(colnames(nodedfall), "Alter", sep = "")
columnlabels[1] <- "Alter"
colnames(alternodedfall) <- columnlabels
# Merge
nodedfallneighbors <- join(nodedfallneighbors, alternodedfall, by = "Alter", type = "left")
# Spring cleaning
# Note: nodedfallneighbors contains nodedfall, as such we can drop nodedfall
keep(data, nodedfall, nodedfallneighbors, edgelistall, layers.all, layers.social, ConstructEdgelist, sure = TRUE)

#############################################################################################################
# Compute degree and distance to seed by layer for ego and alter
#############################################################################################################

# Degree
nodedfallneighbors$DegreeLayer <- NA
nodedfallneighbors$DegreeLayerAlter <- NA

# Compute degree
DegreeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  degree.temp <- data.frame("DegreeTemp" = degree(graph.el))
  degree.temp$Ego <- rownames(degree.temp)
  rownames(degree.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, degree.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$DegreeLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$DegreeTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "DegreeTemp")]
  colnames(degree.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, degree.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$DegreeLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$DegreeTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "DegreeTemp")]
  return(nodedfallneighbors)
}

# Relabel layers list (w/o numbers)
layers.all <- unique(gsub("[0-9]*$", "", layers.all))     
# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- DegreeByLayer(layers.all[[i]])
}

# Compute neighborhood size

NeighbSizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighbsize.temp <- data.frame("NeighborhoodTemp" = (ego_size(graph.el, order=1, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighbsize.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighbsize.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighbsize.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$NeighborhoodLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$NeighborhoodTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "NeighborhoodTemp")]
  colnames(neighbsize.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighbsize.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$NeighborhoodLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$NeighborhoodTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "NeighborhoodTemp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- NeighbSizeByLayer(layers.all[[i]])
}

## Compute 2-neighborhood size by layer

Neighb2SizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighb2size.temp <- data.frame("Neighb2Temp" = (ego_size(graph.el, order=2, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighb2size.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighb2size.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighb2size.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$Neighb2Layer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb2Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb2Temp")]
  colnames(neighb2size.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighb2size.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$Neighb2LayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb2Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb2Temp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- Neighb2SizeByLayer(layers.all[[i]])
}

## Compute 3-neighborhood size by layer

Neighb3SizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighb3size.temp <- data.frame("Neighb3Temp" = (ego_size(graph.el, order=3, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighb3size.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighb3size.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighb3size.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$Neighb3Layer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb3Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb3Temp")]
  colnames(neighb3size.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighb3size.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$Neighb3LayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb3Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb3Temp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- Neighb3SizeByLayer(layers.all[[i]])
}

## Compute 4-neighborhood size by layer

Neighb4SizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighb4size.temp <- data.frame("Neighb4Temp" = (ego_size(graph.el, order=4, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighb4size.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighb4size.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighb4size.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$Neighb4Layer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb4Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb4Temp")]
  colnames(neighb4size.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighb4size.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$Neighb4LayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb4Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb4Temp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- Neighb4SizeByLayer(layers.all[[i]])
}

## Compute 5-neighborhood size by layer

Neighb5SizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighb5size.temp <- data.frame("Neighb5Temp" = (ego_size(graph.el, order=5, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighb5size.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighb5size.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighb5size.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$Neighb5Layer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb5Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb5Temp")]
  colnames(neighb5size.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighb5size.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$Neighb5LayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb5Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb5Temp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- Neighb5SizeByLayer(layers.all[[i]])
}

## Compute 6-neighborhood size by layer

Neighb6SizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighb6size.temp <- data.frame("Neighb6Temp" = (ego_size(graph.el, order=6, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighb6size.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighb6size.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighb6size.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$Neighb6Layer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb6Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb6Temp")]
  colnames(neighb6size.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighb6size.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$Neighb6LayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb6Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb6Temp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- Neighb6SizeByLayer(layers.all[[i]])
}

## Compute 7-neighborhood size by layer

Neighb7SizeByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  neighb7size.temp <- data.frame("Neighb7Temp" = (ego_size(graph.el, order=7, nodes = V(graph.el)) - 1 ) ) #ego_size counts the ego too, hence -1
  neighb7size.temp$Ego <- V(graph.el)$name #ego_size does not preserve rownames but does preserve order of the vertices
  rownames(neighb7size.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, neighb7size.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$Neighb7Layer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb7Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb7Temp")]
  colnames(neighb7size.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, neighb7size.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$Neighb7LayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Neighb7Temp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "Neighb7Temp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- Neighb7SizeByLayer(layers.all[[i]])
}

# Compute average distance WITHIN COMPONENTS (manual inverse of closeness centrality, treats inf as NAs)

AvgDistByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  distMatrix <- shortest.paths(graph.el, v=V(graph.el), to=V(graph.el))
  is.na(distMatrix) <- sapply(distMatrix, is.infinite) # Change infinite paths to NAs-- ensures mean is only over nodes in same component
  avgdist.temp <- data.frame("AvgDistTemp" = apply(distMatrix, 1, mean, na.rm=T))
  avgdist.temp$Ego <- rownames(avgdist.temp)
  rownames(avgdist.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, avgdist.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$AvgDistLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$AvgDistTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "AvgDistTemp")]
  colnames(avgdist.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, avgdist.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$AvgDistLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$AvgDistTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "AvgDistTemp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- AvgDistByLayer(layers.all[[i]])
}

# Compute eigenvector centrality by layer
nodedfallneighbors$EigenLayer <- NA
nodedfallneighbors$EigenLayerAlter <- NA

EigenVectorByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  eigen.temp <- data.frame(eigen_centrality(graph.el)[1])
  eigen.temp$Ego <- rownames(eigen.temp)
  rownames(eigen.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, eigen.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$EigenLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$vector[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "vector")]
  colnames(eigen.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, eigen.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$EigenLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$vector[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "vector")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- EigenVectorByLayer(layers.all[[i]])
}

# Compute betweenness by layer
nodedfallneighbors$BetweennessLayer <- NA
nodedfallneighbors$BetweennessLayerAlter <- NA

BetweennessLayerByLayer <- function(layer){
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer == layer,], directed=FALSE, vertices=NULL)
  betweenness.temp <- data.frame("BetweennesTemp" = betweenness(graph.el, normalized = TRUE))
  betweenness.temp$Ego <- rownames(betweenness.temp)
  rownames(betweenness.temp) <- NULL
  nodedfallneighbors <- merge(nodedfallneighbors, betweenness.temp, by = "Ego", all.x = TRUE)
  nodedfallneighbors$BetweennessLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$BetweennesTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "BetweennesTemp")]
  colnames(betweenness.temp)[2] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, betweenness.temp, by = "Alter", all.x = TRUE)
  nodedfallneighbors$BetweennessLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$BetweennesTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "BetweennesTemp")]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- BetweennessLayerByLayer(layers.all[[i]])
}

# DISTANCE TO SEED

nodedfallneighbors$DistSeedLayer <- as.numeric(NA)
nodedfallneighbors$SeedConnectLayer <- as.numeric(NA)
nodedfallneighbors$SeedRingLayer <- as.numeric(NA)
nodedfallneighbors$DistSeedLayerAlter <- as.numeric(NA)
nodedfallneighbors$SeedConnectLayerAlter <- as.numeric(NA)
nodedfallneighbors$SeedRingLayerAlter <- as.numeric(NA)

# Compute shortest path
ShortestPathByLayer <- function(layer){
  # Compute shortest path to seed
  # make list of seed names
  seed <- unique(as.list(nodedfallneighbors$Ego[which(nodedfallneighbors$IsSeed == 1 & nodedfallneighbors$Layer==layer)]))
  # compute distance matrix
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer==layer,], directed=FALSE, vertices=NULL)
  distMatrix <- shortest.paths(graph.el, v=V(graph.el), to=V(graph.el))
  # make list of ego names
  ego <- rownames(distMatrix)
  # extract relevant distance from distance matrix (i.e. minimum distance to a seed for each ego)
  dist <- vector()
  temp <- vector()
  for (i in 1:length(ego)){
    for (j in 1:length(seed)){
      temp[j] <- distMatrix[ego[[i]],seed[[j]]]
    }
    dist[i] <- min(temp)
  }
  ego <- unlist(ego)
  dist <- data.frame("Ego" = ego, "Dist" = dist)
  dist$Connect <- ifelse(dist$Dist!="Inf", 1, 0)
  dist$Ring <- ifelse(dist$Dist==1, 1, 0)
  nodedfallneighbors <- merge(nodedfallneighbors, dist, by = "Ego", all.x = TRUE)
  nodedfallneighbors$DistSeedLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Dist[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$DistSeedLayer[is.infinite(nodedfallneighbors$DistSeedLayer)] <- NA
  nodedfallneighbors$SeedConnectLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Connect[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$SeedRingLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Ring[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-((ncol(nodedfallneighbors)-2):ncol(nodedfallneighbors))]
  colnames(dist)[1] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, dist, by = "Alter", all.x = TRUE)
  nodedfallneighbors$DistSeedLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Dist[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$DistSeedLayerAlter[is.infinite(nodedfallneighbors$DistSeedLayerAlter)] <- NA
  nodedfallneighbors$SeedConnectLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Connect[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$SeedRingLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Ring[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-((ncol(nodedfallneighbors)-2):ncol(nodedfallneighbors))]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- ShortestPathByLayer(layers.all[[i]])
}

# Sort data
nodedfallneighbors <- nodedfallneighbors[order(nodedfallneighbors$Ego, nodedfallneighbors$Alter),]
nodedfallneighbors <- nodedfallneighbors[,c(2, 1, 3:ncol(nodedfallneighbors))]

# DISTANCE TO LEADER (1st day attender)

nodedfallneighbors$DistLeaderLayer <- as.numeric(NA)
nodedfallneighbors$LeaderConnectLayer <- as.numeric(NA)
nodedfallneighbors$LeaderRingLayer <- as.numeric(NA)
nodedfallneighbors$DistLeaderLayerAlter <- as.numeric(NA)
nodedfallneighbors$LeaderConnectLayerAlter <- as.numeric(NA)
nodedfallneighbors$LeaderRingLayerAlter <- as.numeric(NA)

# Compute shortest path
ShortestPathByLayer <- function(layer){
  # Compute shortest path to seed
  # make list of seed names
  leader <- unique(as.list(nodedfallneighbors$Ego[which(nodedfallneighbors$EventDay == 1 & nodedfallneighbors$Layer==layer)]))
  # compute distance matrix
  graph.el <- graph.data.frame(edgelistall[edgelistall$Layer==layer,], directed=FALSE, vertices=NULL)
  distMatrix <- shortest.paths(graph.el, v=V(graph.el), to=V(graph.el))
  # make list of ego names
  ego <- rownames(distMatrix)
  # extract relevant distance from distance matrix (i.e. minimum distance to a seed for each ego)
  dist <- vector()
  temp <- vector()
  for (i in 1:length(ego)){
    for (j in 1:length(leader)){
      temp[j] <- distMatrix[ego[[i]],leader[[j]]]
    }
    dist[i] <- min(temp)
  }
  ego <- unlist(ego)
  dist <- data.frame("Ego" = ego, "Dist" = dist)
  dist$Connect <- ifelse(dist$Dist!="Inf", 1, 0)
  dist$Ring <- ifelse(dist$Dist==1, 1, 0)
  nodedfallneighbors <- merge(nodedfallneighbors, dist, by = "Ego", all.x = TRUE)
  nodedfallneighbors$DistLeaderLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Dist[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$DistLeaderLayer[is.infinite(nodedfallneighbors$DistLeaderLayer)] <- NA
  nodedfallneighbors$LeaderConnectLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Connect[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$LeaderRingLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Ring[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-((ncol(nodedfallneighbors)-2):ncol(nodedfallneighbors))]
  colnames(dist)[1] <- "Alter"
  nodedfallneighbors <- merge(nodedfallneighbors, dist, by = "Alter", all.x = TRUE)
  nodedfallneighbors$DistLeaderLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Dist[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$DistLeaderLayerAlter[is.infinite(nodedfallneighbors$DistLeaderLayerAlter)] <- NA
  nodedfallneighbors$LeaderConnectLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Connect[nodedfallneighbors$Layer == layer]
  nodedfallneighbors$LeaderRingLayerAlter[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$Ring[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-((ncol(nodedfallneighbors)-2):ncol(nodedfallneighbors))]
  return(nodedfallneighbors)
}

# Apply function
for(i in 1:length(layers.all)){
  nodedfallneighbors <- ShortestPathByLayer(layers.all[[i]])
}

# Tie strength
TieStrength <- ddply(nodedfallneighbors[nodedfallneighbors$Layer %in% layers.social,],.(Ego,Alter), nrow)
colnames(TieStrength)[3] <- "TieStrength"
nodedfallneighbors <- merge(nodedfallneighbors, TieStrength, by=c("Ego","Alter"), all.x = TRUE)

# Reciprocal
layers.social <- c("Time1", "Time2", "Time3", "Time4", "Time5",               
                   "Phone1", "Phone2", "Phone3", "Phone4", "Phone5",
                   "Pol1", "Pol2", "Pol3", "Pol4", "Pol5",
                   "Relig1", "Relig2", "Relig3", "Relig4", "Relig5",
                   "Meal1", "Meal2", "Meal3", "Meal4", "Meal5",
                   "Visit1", "Visit2", "Visit3", "Visit4", "Visit5",
                   "Secret1", "Secret2", "Secret3", "Secret4", "Secret5")
edgelistallsocial <- ConstructEdgelist(df = data, layers = layers.social, directed = TRUE)
edgelistallsocial <- edgelistallsocial[,1:2]
edgelistallsocial$Reciprocal <- NA
for(i in 1:nrow(edgelistallsocial)){
  edgelistallsocial$Reciprocal[i] <- ifelse(nrow(match_df(edgelistallsocial[,(1:2)], data.frame("Ego" = edgelistallsocial$Alter[i], "Alter" = edgelistallsocial$Ego[i])))>0, 1, 0)
  if (i%%100 == 0) print(i)
}
ego.neighbors <- edgelistallsocial
alter.neighbors <- edgelistallsocial[c(2, 1, 3)]
colnames(alter.neighbors) <- c("Ego", "Alter", "Reciprocal")
neighbors <- rbind(ego.neighbors, alter.neighbors)
neighbors <- unique(neighbors)
nodedfallneighbors <- merge(nodedfallneighbors, neighbors, by=c("Ego","Alter"), all.x = TRUE)

# Reciprocity by social layer
nodedfallneighbors$ReciprocalLayer <- NA
layers.social <- c("Time", "Phone", "Pol", "Relig", "Meal", "Visit", "Secret")     
ReciprocityByLayer <- function(layer){
  layertemp <- c(paste(layer, 1:5, sep = ""))
  edgelisttemp <- ConstructEdgelist(df = data, layers = layertemp, directed = TRUE)
  edgelisttemp <- edgelisttemp
  for(i in 1:nrow(edgelisttemp)){
    edgelisttemp$ReciprocalLayerTemp[i] <- ifelse(nrow(match_df(edgelisttemp[,(1:2)], data.frame("Ego" = edgelisttemp$Alter[i], "Alter" = edgelisttemp$Ego[i])))>0, 1, 0)
    if (i%%100 == 0) print(i)
  }
  ego.neighbors <- edgelisttemp
  alter.neighbors <- edgelisttemp[c(2, 1, 3, 4)]
  colnames(alter.neighbors) <- c("Ego", "Alter", "Layer", "ReciprocalLayerTemp")
  neighbors <- rbind(ego.neighbors, alter.neighbors)
  neighbors <- unique(neighbors)
  nodedfallneighbors <- merge(nodedfallneighbors, neighbors, by=c("Ego","Alter", "Layer"), all.x = TRUE)
  nodedfallneighbors$ReciprocalLayer[nodedfallneighbors$Layer == layer] <- nodedfallneighbors$ReciprocalLayerTemp[nodedfallneighbors$Layer == layer]
  nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "ReciprocalLayerTemp")]
}

# Apply function
for(i in 1:length(layers.social)){
  nodedfallneighbors <- ReciprocityByLayer(layers.social[[i]])
}

# Reciprocity by from layer
layers.from <- c(paste("From", 1:5, sep = ""))
layers.to <- c(paste("To", 1:5, sep = ""))
edgelistfrom <- ConstructEdgelist(df = data, layers = layers.from, directed = TRUE)
edgelistto <- ConstructEdgelist(df = data, layers = layers.to, directed = TRUE)
for(i in 1:nrow(edgelistfrom)){
  edgelistfrom$ReciprocalLayerTemp[i] <- ifelse(nrow(match_df(edgelistto[,(1:2)], data.frame("Ego" = edgelistfrom$Alter[i], "Alter" = edgelistfrom$Ego[i])))>0, 1, 0)
}
nodedfallneighbors <- merge(nodedfallneighbors, edgelistfrom, by=c("Ego","Alter", "Layer"), all.x = TRUE)
nodedfallneighbors$ReciprocalLayer[nodedfallneighbors$Layer == "From"] <- nodedfallneighbors$ReciprocalLayerTemp[nodedfallneighbors$Layer == "From"]
nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "ReciprocalLayerTemp")]

# Reciprocity by to layer
for(i in 1:nrow(edgelistto)){
  edgelistto$ReciprocalLayerTemp[i] <- ifelse(nrow(match_df(edgelistfrom[,(1:2)], data.frame("Ego" = edgelistto$Alter[i], "Alter" = edgelistto$Ego[i])))>0, 1, 0)
}
nodedfallneighbors <- merge(nodedfallneighbors, edgelistto, by=c("Ego","Alter", "Layer"), all.x = TRUE)
nodedfallneighbors$ReciprocalLayer[nodedfallneighbors$Layer == "To"] <- nodedfallneighbors$ReciprocalLayerTemp[nodedfallneighbors$Layer == "To"]
nodedfallneighbors <- nodedfallneighbors[,-which(colnames(nodedfallneighbors) == "ReciprocalLayerTemp")]

# Spring cleaning
keep(data, nodedfall, edgelistall, nodedfallneighbors, sure = TRUE)

#################################################################################################################
#
#
#                                         COLLAPSE DATA FOR ANALYSIS
#
#
#################################################################################################################

#################################################################################################################
# FUNCTIONS
#################################################################################################################

collapsedata <- function(layers, sampled = FALSE, heard = FALSE, bylink=FALSE){
  # keep layers of interest
  temp <- nodedfallneighbors[nodedfallneighbors$Layer %in% layers,]
  # collapse dataset to obtain average values by Ego
  # if bylink=FALSE, average over unique pairs of (ego,alter)s.  If bylink=TRUE, a link between A and B gets weighted in the means as many times as it appears (i.e. as many layers as it appears in).
  if(bylink == FALSE){ #first find all unique links, then average over egos.  Counts all unique ego-alter pairs once in the averages.
    temp.samp.heard <- aggregate(temp[,-(1:3)], by=list("Ego" = temp$Ego, "Alter" = temp$Alter), FUN = mean, na.rm = TRUE)
    temp.samp.heard <- aggregate(temp.samp.heard[,-(1:2)], by=list("Ego" = temp.samp.heard$Ego), FUN = mean, na.rm = TRUE)
  }	
  if(bylink == TRUE){
    temp.samp.heard <- aggregate(temp[,-(1:3)], by=list("Ego" = temp$Ego), FUN = mean, na.rm = TRUE)
  }
  # apply desired filter
  if(sampled == TRUE & heard == FALSE) return({temp.samp.heard <- temp.samp.heard[temp.samp.heard$Sampled==1,]}) # pulls everyone we sampled
  if(sampled == TRUE & heard == TRUE) return({temp.samp.heard <- temp.samp.heard[temp.samp.heard$Sampled==1 & temp.samp.heard$Hear==1,]}) # pulls everyone we sampled who heard
  if(sampled == FALSE & heard == TRUE) return({temp.samp.heard <- temp.samp.heard[temp.samp.heard$Hear==1,]}) # pulls everyone who heard
  if(sampled == FALSE & heard == FALSE) return({temp.samp.heard <- temp.samp.heard}) # keeps all
}

#################################################################################################################
# APPLY FUNCTION
#################################################################################################################
# include only those that were sampled
SocialSamp <- collapsedata(layers = c("Time", "Relig", "Pol", "Visit", "Secret", "Phone", "Meal"), sampled = TRUE, heard = FALSE, bylink = FALSE)
# include only those that were sampled and who heard
SocialSampHeard <- collapsedata(layers = c("Time", "Relig", "Pol", "Visit", "Secret", "Phone", "Meal"), sampled = TRUE, heard = TRUE, bylink = FALSE)
# include only those that were sampled and who heard (by sub-network)
TimeSampHeard <- collapsedata(layers=c("Time"), sampled=TRUE, heard=TRUE)
ReligSampHeard <- collapsedata(layers=c("Relig"), sampled= TRUE, heard= TRUE)
PolSampHeard <- collapsedata(layers=c("Pol"), sampled= TRUE, heard= TRUE)
VisitSampHeard <- collapsedata(layers=c("Visit"), sampled= TRUE, heard= TRUE)
SecretSampHeard <- collapsedata(layers=c("Secret"), sampled= TRUE, heard= TRUE)
PhoneSampHeard <- collapsedata(layers=c("Phone"), sampled= TRUE, heard= TRUE)
MealSampHeard <- collapsedata(layers=c("Meal"), sampled= TRUE, heard= TRUE)
