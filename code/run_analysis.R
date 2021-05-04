######################################################################################################################################
#
# TITLE: From Chatter to Action: How Social Networks Inform and Motivate in Rural Uganda - Replication Code to run analysis
# AUTHORS: Jennifer M. Larson, Janet I. Lewis, Pedro L. Rodriguez
# DATE (BEGUN): 04-11-2017
# DATE (LAST MODIFIED): 05-04-2021
# BEFORE RUNNING THIS CODE: run process_data.R
#
#######################################################################################################################################

# libraries
library(gplots)
library(ggplot2)
library(reshape2)
library(grid)
library(plyr)
library(mfx)
library(stargazer)
library(igraph)
library(xtable)
library(RColorBrewer)
library(car)
library(dtplyr)
library(dplyr)

# path to store figures
#figure_path <- '~/Dropbox/GitHub/repositories/Abalang/figures/'

#################################################################################################################
#
#
#                                   FIGURE 1 & FIGURE 1 (SI)
#
#
#################################################################################################################

#################################################################################################################
# FIGURE 1
#################################################################################################################

#################################

## Make heatmap of overlap in each of the 7 constituents of the social network
## (Figure 1)
## Modified to take the edgelist as an argument.  

## First make heatmap of overlap in each of the 7 constituents of the social network using every recorded link
## (Figure 1)

SpanSingleWithPreProcessing <- function(x, z, elwithlayer=edgelistall){
  # Computes the % of links of z contained in x without the need to pre-process the data (as is the case with ComputeSpanSingle)
  # We term this span (open for discussion)
  # Span provides a measure of association between networks
  # Span rate = 1 if the network x spans the entire network z (i.e. all links in z can be found in x)
  # Note: ComputeSpanSingle(x, z) != ComputeSpanSingle(x, z)
  # Warning: to run this function the adjacency matrix must have the same dimensions
  # To guarantee this construct each adjacency matrix using the "aggregate" population
  # Aggregate population = the union of the egos and alters in each network 
  # This function does the necessary pre-processing to meet this warning
  #
  # Args:
  # x: adjacency matrix (in matrix form)
  # z: adjacency matrix (in matrix form)
  #
  # Returns:
  # span.rate = number of links in z contained in x divided by the total links in z
  # Specify layers
  if(x == "Aggregate"){x <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")}
  if(z == "Aggregate"){z <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")}
  layers <- c(x, z)
  # Construct edgelist
  edgelist <- elwithlayer[elwithlayer$Layer %in% layers,]
  # Construct population using costum function
  nodes <- ConstructPopulation(el = edgelist, nodes = nodedfall)
  # Construct adjacency matrices using costum function
  adj.z <- AdjacencyMatrix(nodes = nodes, edgelist = edgelist, layer = z)
  adj.x <- AdjacencyMatrix(nodes = nodes, edgelist = edgelist, layer = x)
  adj.matrix.span <- adj.x + adj.z                                                # create union of networks x and z by adding adjacency matrices
  common.links <- sum(rowSums(adj.matrix.span == 2, na.rm=TRUE))                  # counts the number of entries equal 2 (implying overlap) in the union of adjacecny matrices
  total.links <- sum(rowSums(adj.z == 1, na.rm=TRUE))                             # counts the number of links in z
  span.rate <- common.links/total.links                                           # compute span rate
  return(span.rate)
  print(span.rate)
}

# Attributes Dataframe Builder

ConstructPopulation <- function(el, nodes){
  # Constructs an sub-attributes dataframe from an edgelist and a larger attributes dataframe
  # Egos: includes all Egos and Alters in a given edgelist
  #
  # Args:
  # el: edgelist
  # nodes = attribuets dataframe from which to extract information
  #
  # Returns:
  # dataframe of the population of a given edgelist with their respective attributes
  ego <- el["Ego"]                  
  alter <- el["Alter"]
  colnames(alter) <- c("Ego")
  population <- rbind(ego, alter)
  population <- unique(population)
  # Join with attributes
  nodedf <- join(population, nodes, by = "Ego", type = "left")
  return(nodedf)
}

# Adjacency matrix builder

AdjacencyMatrix <- function(nodes, edgelist, layer){
  # Converts an edgelist into an adjacency matrix
  #
  # Args:
  # nodes: the list of nodes
  # edgelist: the list of links between nodes
  # layers: layers to be included (different networks)
  #
  # Returns:
  # an adjacency matrix
  edges <- subset(edgelist, edgelist$Layer %in% layer)                  # keeps the edges of interest (defined by the layer of interest)
  edges <- edges[,-3]                                                   # delete the layer variable
  graph <- graph.data.frame(edges, directed="FALSE", vertices = nodes)  # creates a network graph of the edges
  adjacency <- get.adjacency(graph)                                     # obtain adjacency matrix
  adjacency <- as.matrix(adjacency)                                     # convert to matrix form for easy manipulation
  adjacency[adjacency>=1] <- 1                                          # replace double links (assumption: undirected networks)
  return(adjacency)
}

# Compute span matrix (only the 7 social networks)
# specify relevant layers
#layers <- c("From", "Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit", "Aggregate")
#layers <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit", "Aggregate")
layers <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")
# create list of layers to label matrix col/row names
layers <- list(layers, layers)
# create empty matrix
#span.matrix <- matrix(data = NA, nrow = 9, ncol = 9, dimnames = layers)
#span.matrix <- matrix(data = NA, nrow = 8, ncol = 8, dimnames = layers)
span.matrix <- matrix(data = NA, nrow = 7, ncol = 7, dimnames = layers)
# fill empty matrix with span values (rows = x, and cols = z for the span function)
for(j in 1:ncol(span.matrix)){
  for(i in 1:nrow(span.matrix)){
    span.matrix[i,j] = SpanSingleWithPreProcessing(rownames(span.matrix)[i], colnames(span.matrix)[j], edgelistall) #added edgelist
  }
}
# create heatmap

## Toggle to autmatically save heatmap as png.
#pdf(file=paste0(figure_path, "heatmap_overlap.pdf"))
#ggplot(melt(span.matrix), aes(Var2, Var1)) + 
ggplot(reshape2::melt(span.matrix), aes(Var2, Var1)) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradientn(colours = c("aliceblue", "steelblue1", "steelblue2", "steelblue3", "steelblue4"), values = c(0, 0.1, 0.2, 0.3, 1), guide = "colorbar") +
  geom_text(aes(label = round(reshape2::melt(span.matrix)$value, 3)), size = 4) +
  scale_x_discrete(labels=c("Meal", "Phone", "Politics", "Religion", "Secret", "Time", "Visit"), expand = c(0, 0)) +
  scale_y_discrete(labels=c("Meal", "Phone", "Politics", "Religion", "Secret", "Time", "Visit"), expand = c(0, 0)) +
  xlab(label = "Overlapped Layer") +
  ylab(label = "Overlapping Layer") +
  theme(legend.position="none", 
        axis.line = element_blank(), 
        axis.text = element_text(colour="snow4"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=16, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size=16, margin = margin(t = 0, r = 15, b = 0, l = 15)))
#dev.off()

#################################################################################################################
# FIGURE 1 (SI)
#################################################################################################################

## Now restrict the edgelist to just those egos that were sampled

restrictedel <- function(el, candidates){
  keepind <- c()
  for(i in 1:nrow(el)){
    if(el$Ego[i] %in% candidates & el$Alter[i] %in% candidates){
      keepind <- c(keepind, i)
    }
  }
  out <- el[keepind,]
  return(out)
}	

sampledegos <- SocialSamp$Ego

closedel <- restrictedel(edgelistall, sampledegos)


# Compute span matrix (only the 7 social networks) for the closed networks among just those sampled
# specify relevant layers
layers <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")
# create list of layers to label matrix col/row names
layers <- list(layers, layers)
# create empty matrix
#span.matrix <- matrix(data = NA, nrow = 9, ncol = 9, dimnames = layers)
#span.matrix <- matrix(data = NA, nrow = 8, ncol = 8, dimnames = layers)
span.matrix <- matrix(data = NA, nrow = 7, ncol = 7, dimnames = layers)
# fill empty matrix with span values (rows = x, and cols = z for the span function)
for(j in 1:ncol(span.matrix)){
  for(i in 1:nrow(span.matrix)){
    span.matrix[i,j] = SpanSingleWithPreProcessing(rownames(span.matrix)[i], colnames(span.matrix)[j], closedel) #added edgelist
  }
}
# create heatmap for the closed networks

## Toggle to automatically save image as png
#png("heatmap_overlap_closednw.png")   
ggplot(reshape2::melt(span.matrix), aes(Var2, Var1)) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradientn(colours = c("aliceblue", "steelblue1", "steelblue2", "steelblue3", "steelblue4"), values = c(0, 0.1, 0.2, 0.3, 1), guide = "colorbar") +
  geom_text(aes(label = round(reshape2::melt(span.matrix)$value, 3)), size = 4) +
  scale_x_discrete(labels=c("Meal", "Phone", "Politics", "Religion", "Secret", "Time", "Visit"), expand = c(0, 0)) +
  scale_y_discrete(labels=c("Meal", "Phone", "Politics", "Religion", "Secret", "Time", "Visit"), expand = c(0, 0)) +
  xlab(label = "Overlapped Layer") +
  ylab(label = "Overlapping Layer") +
  theme(legend.position="none", 
        axis.line = element_blank(), 
        axis.text = element_text(colour="snow4"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=16, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size=16, margin = margin(t = 0, r = 15, b = 0, l = 15)))
#dev.off()

#################################################################################################################
#
#
#                                                 TABLE 1, FIGURE 2, TABLE 10 - 12 (SI)
#
#
#################################################################################################################

#################################################################################################################
# TABLE 1
#################################################################################################################

seeds <- which(SocialSamp$IsSeed == 1)
seeds_attenders <- which(SocialSamp$IsSeed == 1 & SocialSamp$Attend == 1)
seeds_non_attenders <- which(SocialSamp$IsSeed == 1 & SocialSamp$Attend == 0)
earlyattenders <- which(SocialSamp$EventDay == 1)
earlyattenders_seeds <- which(SocialSamp$EventDay == 1 & SocialSamp$IsSeed == 1)
earlyattenders_non_seeds <- which(SocialSamp$EventDay == 1 & SocialSamp$IsSeed != 1)
day2attenders <- which(SocialSamp$EventDay == 2)
day3attenders <- which(SocialSamp$EventDay == 3)
attendersnotearly <- which(SocialSamp$EventDay==2 | SocialSamp$EventDay==3)
allattenders <- which(SocialSamp$Attend == 1)
hearers <- which(SocialSamp$Hear == 1)
nonattenders <- which(SocialSamp$Attend == 0)
nohearnoattend <- which(SocialSamp$Attend == 0 & SocialSamp$Hear == 0)
hearnoattend <- which(SocialSamp$Attend == 0 & SocialSamp$Hear == 1)
nohear <- which(SocialSamp$Hear == 0)


varlist <- c("Gender", "Age", "Anglican", "Catholic", "Pentacostal", "WallMat", "Married", "Educ", "Unemp", "PartTime", "FullTime", "Retired", "Neighborhood", "NeighborhoodAlter",  "DistSeed", "DistOtherSeed", "DistLeader", "DistOtherLeader", "AvgDist", "HearAlter", "AttendAlter", "Eigen", "Hear", "Attend")

groups <- cbind(
  "all" = apply(as.matrix(SocialSamp[, varlist]), 2, mean, na.rm=T),
  "seeds" = apply(as.matrix(SocialSamp[seeds, varlist]), 2, mean, na.rm=T), 
  "seeds_attenders" = apply(as.matrix(SocialSamp[seeds_attenders, varlist]), 2, mean, na.rm=T), 
  "seeds_non_attenders" =apply(as.matrix(SocialSamp[seeds_non_attenders, varlist]), 2, mean, na.rm=T), 
  "leaders" = apply(as.matrix(SocialSamp[earlyattenders, varlist]), 2, mean, na.rm=T),
  "leaders_seeds" = apply(as.matrix(SocialSamp[earlyattenders_seeds, varlist]), 2, mean, na.rm=T),
  "leaders_non_seeds" = apply(as.matrix(SocialSamp[earlyattenders_non_seeds, varlist]), 2, mean, na.rm=T),
  "day2" = apply(as.matrix(SocialSamp[day2attenders, varlist]), 2, mean, na.rm=T),
  "day3" = apply(as.matrix(SocialSamp[day3attenders, varlist]), 2, mean, na.rm=T),
  "allattenders" = apply(as.matrix(SocialSamp[allattenders, varlist]), 2, mean, na.rm=T),
  "attendersnotearly" = apply(as.matrix(SocialSamp[attendersnotearly, varlist]), 2, mean, na.rm=T),
  "hearers" = apply(as.matrix(SocialSamp[hearers, varlist]), 2, mean, na.rm=T),
  "noattend" = apply(as.matrix(SocialSamp[nonattenders, varlist]), 2, mean, na.rm=T),
  "nohearnoattend"= apply(as.matrix(SocialSamp[nohearnoattend, varlist]), 2, mean, na.rm=T),
  "hearnoattend" = apply(as.matrix(SocialSamp[hearnoattend, varlist]), 2, mean, na.rm=T),
  "nohear" = apply(as.matrix(SocialSamp[nohear, varlist]), 2, mean, na.rm=T)
)

rownames(groups) <- varlist

varstokeep <- c("Gender", "Age", "Married", "Catholic", "WallMat", "Educ", "Unemp", "PartTime", "FullTime", "Retired", "Neighborhood", "NeighborhoodAlter", "HearAlter", "AttendAlter", "DistSeed", "DistLeader", "DistOtherLeader", "AvgDist", "Eigen")

tab1 <- round(groups[varstokeep,c("nohearnoattend", "hearnoattend", "allattenders")], digits = 2)
colnames(tab1) <- c("Base", "Hearers", "Attenders")
tab1 <- rbind(tab1, "Number of People" = round(c(length(nohearnoattend), length(hearnoattend), length(allattenders)), digits = 0))

xtable(tab1)

## Now do t-Tests to compare with base.

## Hear but didn't attend v. didn't hear or attend:
pvalshearnoattend <- c()
for(i in 1:length(varstokeep)){
  thetest <- t.test(SocialSamp[nohearnoattend,varstokeep[i]], SocialSamp[hearnoattend,varstokeep[i]])
  pvalshearnoattend[i] <- thetest$p.value
}
names(pvalshearnoattend) <- varstokeep
round(pvalshearnoattend, 2)

## Attend v. didn't hear or attend:
pvalsattend <- c()
for(i in 1:length(varstokeep)){
  thetest <- t.test(SocialSamp[nohearnoattend,varstokeep[i]], SocialSamp[allattenders,varstokeep[i]])
  pvalsattend[i] <- thetest$p.value
}
names(pvalsattend) <- varstokeep
round(pvalsattend, 2)

## Attend v. heard but didn't attend:
pvalshearvattend <- c()
for(i in 1:length(varstokeep)){
  thetest <- t.test(SocialSamp[hearnoattend,varstokeep[i]], SocialSamp[allattenders,varstokeep[i]])
  pvalshearvattend[i] <- thetest$p.value
}
names(pvalshearvattend) <- varstokeep
round(pvalshearvattend, 2)

#################################################################################################################
# FIGURE 2
#################################################################################################################

## Group comparisons
group_comparisons <- SocialSamp %>% mutate(group = case_when(Attend == 1 ~ 'allattenders',
                                                             Attend == 0 & Hear == 0 ~ 'nohearnoattend',
                                                             Attend == 0 & Hear == 1 ~ 'hearnoattend')) %>% filter(!is.na(group))

group_comparisons_mean <- group_comparisons[, c(varstokeep, 'group')] %>% group_by(group) %>% summarise_all(list(mean), na.rm = TRUE) %>% melt(.,id = 'group')
group_comparisons_sd <- group_comparisons[, c(varstokeep, 'group')] %>% group_by(group) %>% summarise_all(list(sd), na.rm = TRUE) %>% melt(.,id = 'group')
group_comparisons_counts <- group_comparisons[, c(varstokeep, 'group')] %>% group_by(group) %>% summarise(count = n(), .groups = 'drop_last')
plot_tibble <- cbind(group_comparisons_mean[,c('group', 'variable')], 'mean' = group_comparisons_mean$value, 'sd' = group_comparisons_sd$value)
plot_tibble <- left_join(plot_tibble, group_comparisons_counts, by = 'group') %>% mutate(std.error = sd/sqrt(count)) %>% select(-c(sd, count))
plot_tibble <- plot_tibble %>% mutate(group = factor(group, levels = c('nohearnoattend', 'hearnoattend', 'allattenders')))

# proportions
#pdf(file=paste0(figure_path, "nw_proportions.pdf"))
ggplot(plot_tibble[plot_tibble$variable %in% c('HearAlter', 'AttendAlter'),], aes(x = variable, y = mean, fill = group)) + 
  geom_bar(position = position_dodge(), stat="identity", color = 'white') +
  geom_errorbar(aes(ymin = mean - 1.96*std.error, ymax =  mean + 1.96*std.error),
                size=.8,   
                width=.2,
                position=position_dodge(.9),
                color = 'black') +
  scale_fill_manual(values = c("#d9d9d9", "#969696", "#525252"), labels = c('Base', 'Hearers', 'Attenders')) +
  #scale_x_discrete(name ="", labels=c("PropPeersHeard","PropPeersAttend")) +
  scale_x_discrete(name ="", labels=c("Heard but \n did not attend","Heard and \n attended")) +
  ylim(0,1) +
  ylab('Average proportion of peers (that)') +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16, vjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_blank(),
        legend.text=element_text(size=20),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))
#dev.off()

# distance metrics
#pdf(file=paste0(figure_path, "nw_distance.pdf"))
ggplot(plot_tibble[plot_tibble$variable %in% c('DistSeed', 'DistLeader', 'AvgDist'),], aes(x = variable, y = mean, fill = group)) + 
  geom_bar(position = position_dodge(), stat="identity", color = 'white') +
  geom_errorbar(aes(ymin = mean - 1.96*std.error, ymax =  mean + 1.96*std.error),
                size=.8,   
                width=.2,
                position=position_dodge(.9),
                color = 'black') +
  scale_fill_manual(values = c("#d9d9d9", "#969696", "#525252"), labels = c('Base', 'Hearers', 'Attenders')) +
  #scale_x_discrete(name ="", labels=c("DistSeed","DistEarlyAttend", 'AvgDist')) +
  scale_x_discrete(name ="", labels=c("Seeds","Early attenders", 'All peers')) +
  ylab('Average minimum distance (to)') +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16, vjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_blank(),
        legend.text=element_text(size=16),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))
#dev.off()

# neighborhood metrics
#pdf(file=paste0(figure_path, "neighborhood.pdf"))
ggplot(plot_tibble[plot_tibble$variable %in% c("Neighborhood", "NeighborhoodAlter"),], aes(x = variable, y = mean, fill = group)) + 
  geom_bar(position = position_dodge(), stat="identity", color = 'white') +
  geom_errorbar(aes(ymin = mean - 1.96*std.error, ymax =  mean + 1.96*std.error),
                size=.8,   
                width=.2,
                position=position_dodge(.9),
                color = 'black') +
  scale_fill_manual(values = c("#d9d9d9", "#969696", "#525252"), labels = c('Base', 'Hearers', 'Attenders')) +
  #scale_x_discrete(name ="", labels=c("NumPeers","AvgNumPeersPeers")) +
  scale_x_discrete(name ="", labels=c("Peers","Peers of peers")) +
  ylim(0,20) +
  ylab('Average number (of)') +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16, vjust = 0.5, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_blank(),
        legend.text=element_text(size=16),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))
#dev.off()

# eigenvector centrality
#pdf(file=paste0(figure_path, "eigencentrality.pdf"))
ggplot(plot_tibble[plot_tibble$variable %in% "Eigen",], aes(x = variable, y = mean, fill = group)) + 
  geom_bar(position = position_dodge(), stat="identity", color = 'white') +
  geom_errorbar(aes(ymin = mean - 1.96*std.error, ymax =  mean + 1.96*std.error),
                size=.8,   
                width=.2,
                position=position_dodge(.9),
                color = 'black') +
  scale_fill_manual(values = c("#d9d9d9", "#969696", "#525252"), labels = c('Base', 'Hearers', 'Attenders')) +
  scale_x_discrete(name ="", labels=c("")) +
  ylab('Eigenvector centrality') +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_blank(),
        legend.text=element_text(size=16),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))
#dev.off()

#################################################################################################################
# TABLE 10 (SI)
#################################################################################################################

## Early vs later attendance
pvalshearvattend <- c()
for(i in 1:length(varstokeep)){
  thetest <- t.test(SocialSamp[earlyattenders,varstokeep[i]], SocialSamp[attendersnotearly,varstokeep[i]])
  pvalshearvattend[i] <- thetest$p.value
}
names(pvalshearvattend) <- varstokeep
round(pvalshearvattend, 2)

tab2 <- round(groups[varstokeep, c("leaders", "attendersnotearly")], digits = 2)
colnames(tab2) <- c("Early Attenders", "Later Attenders")
tab2 <- rbind(tab2, "Number of People" = c(length(earlyattenders), length(attendersnotearly)))

xtable(tab2)

#################################################################################################################
# TABLE 11 (SI)
#################################################################################################################

## Early vs later attendance split by seed
pvalshearvattend <- c()
for(i in 1:length(varstokeep)){
  thetest <- t.test(SocialSamp[earlyattenders_seeds,varstokeep[i]], SocialSamp[earlyattenders_non_seeds,varstokeep[i]])
  pvalshearvattend[i] <- thetest$p.value
}
names(pvalshearvattend) <- varstokeep
round(pvalshearvattend, 2)

tab3 <- round(groups[varstokeep, c("leaders_seeds", "leaders_non_seeds")], digits = 2)
colnames(tab2) <- c("Early Attenders (seeds)", "Early Attenders (non-seeds)")
tab3 <- rbind(tab3, "Number of People" = c(length(earlyattenders_seeds), length(earlyattenders_non_seeds)))

xtable(tab3)

#################################################################################################################
# TABLE 12 (SI)
#################################################################################################################

## Seeds that attended vs seeds that did not attend

varstokeep <- c("Gender", "Age", "Married", "Catholic", "WallMat", "Educ", "Unemp", "PartTime", "FullTime", "Retired", "Neighborhood", "NeighborhoodAlter", "HearAlter", "AttendAlter", "DistSeed", "DistOtherSeed", "DistLeader", "AvgDist", "Eigen")

tab4 <- round(groups[varstokeep, c("seeds_attenders", "seeds_non_attenders")], digits = 2)
colnames(tab4) <- c("Attending Seeds", "Non-Attending Seeds")
tab4 <- rbind(tab4, "Number of People" = c(length(seeds_attenders), length(seeds_non_attenders)))

xtable(tab4)

varstokeep <- setdiff(varstokeep, "Married")

pvalshearvattend <- c()
for(i in 1:length(varstokeep)){
  thetest <- t.test(SocialSamp[seeds_attenders,varstokeep[i]], SocialSamp[seeds_non_attenders,varstokeep[i]])
  pvalshearvattend[i] <- thetest$p.value
}
names(pvalshearvattend) <- varstokeep
round(pvalshearvattend, 2)

#################################################################################################################
#
#
#                                   FIGURE 3: PIE CHART
#
#
#################################################################################################################

# Overlap matrix builder

OverlapRate <- function(x, z){
  # Computes the % of links of z contained in x without the need to pre-process the data
  # We term this overlap
  # Overlap provides a measure of association between networks
  # Overlap rate = 1 if the network x spans the entire network z (i.e. all links in z can be found in x)
  # Note: OverlapMatrix(x, z) != OverlapMatrix(x, z)
  # Warning: to run this function the adjacency matrix must have the same dimensions
  # To guarantee this construct each adjacency matrix using the "aggregate" population
  # Aggregate population = the union of the egos and alters in each network 
  # This function does the necessary pre-processing to meet this warning
  # Makes use of the following functions: AdjacencyMatrix, ConstructPopulation
  #
  # Args:
  # x: adjacency matrix (in matrix form)
  # z: adjacency matrix (in matrix form)
  #
  # Returns:
  # overlap.rate = number of links in z contained in x divided by the total links in z
  #
  if(x == "Aggregate"){x <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")}                    # specify layers in x
  if(z == "Aggregate"){z <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")}                    # specify layers in z
  layers <- c(x, z)
  edgelist <- edgelistall[edgelistall$Layer %in% layers,]                                                     # construct edge list
  nodes <- ConstructPopulation(el = edgelist, nodes = nodedfall)                                              # construct population using costum function
  
  adj.z <- AdjacencyMatrix(nodes = nodes, edgelist = edgelist, layer = z)                                     # construct adjacency matrix of z using costum function
  adj.x <- AdjacencyMatrix(nodes = nodes, edgelist = edgelist, layer = x)                                     # construct adjacency matrix of x using costum function
  adj.matrix.span <- adj.x + adj.z                                                                            # create union of networks x and z by adding adjacency matrices
  common.links <- sum(rowSums(adj.matrix.span == 2, na.rm=TRUE))                                              # counts the number of entries equal 2 (implying overlap) in the union of adjacecny matrices
  total.links <- sum(rowSums(adj.z == 1, na.rm=TRUE))                                                         # counts the number of links in z
  span.rate <- common.links/total.links                                                                       # compute span rate
  return(span.rate)
  print(span.rate)
}

# define layers of interest
layers <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit", "From")
# create list of layers to label matrix col/row names
layers <- list(layers, layers)
# create empty matrix
overlap.matrix <- matrix(data = NA, nrow = 8, ncol = 8, dimnames = layers)
# fill empty matrix with span values (rows = x, and cols = z for the span function)
for(j in 1:ncol(overlap.matrix)){
  for(i in 1:nrow(overlap.matrix)){
    overlap.matrix[i,j] = OverlapRate(rownames(overlap.matrix)[i], colnames(overlap.matrix)[j])
  }
}

#pdf(file=paste0(figure_path, "overlap_piechart_ratio.pdf"))
slices <- c(overlap.matrix[8,1], overlap.matrix[8,2], overlap.matrix[8,3], overlap.matrix[8,4], overlap.matrix[8,5], overlap.matrix[8,6], overlap.matrix[8,7]) 
lbls <- c("Meal", "Phone", "Politics", "Religion", "Secret", "Time", "Visit")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)))
#dev.off()

#################################################################################################################
#
#
#                                        TABLE 2: simple diffusion
#
#
#################################################################################################################

# create list of models to run
model.list <- list("m1" = c("HearAlter"), "m2" = c("HearAlter", "Neighborhood"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste(x, collapse= " + "))), data = SocialSamp, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Hear About the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between proportion of peers who heard and hearing about the event",
                                covariate.labels = c("Prop. Peers Who Heard", "Neighborhood"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values are the marginal effects for the average observation. Network statistics calculated for undirected aggregate social network.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "hear", no.space = TRUE, out = "regsimplehear.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "regsimplehear.tex")

#################################################################################################################
#
#
#                                            FOOTNOTE 14
#
#
#################################################################################################################

## sample from the SocialSamp used in the hear regressions to collect 268 observations to be sure the difference in significance isn't driven by power issues.

nsims <- 10000
ncases <- 268
estvec1 <- c()
estvec2 <- c()
pvalvec1 <- c()
pvalvec2 <- c()
sampsize1 <- c()
sampsize2 <- c()

hearregdata <- na.omit(SocialSamp[,c("Hear", "HearAlter", "Neighborhood")])
for(i in 1:nsims){
  newdraw <- sample(1:nrow(hearregdata), ncases, replace=FALSE)
  restricteddata <- hearregdata[newdraw,]
  out1 <- logitmfx(Hear ~ HearAlter, data = restricteddata, atmean=TRUE, robust=TRUE)
  out2 <- logitmfx(Hear ~ HearAlter + Neighborhood, data = restricteddata, atmean=TRUE, robust=TRUE)
  estvec1[i] <- out1$mfxest[1]                                                                         # grab estimate
  pvalvec1[i] <- out1$mfxest[4]                                                                        # grab p-value
  estvec2[i] <- out2$mfxest[1]
  pvalvec2[i] <- out2$mfxest[4]
  sampsize1[i] <- out1$fit$df.null + 1
  sampsize2[i] <- out2$fit$df.null + 1
  if(i %% 100 == 0) print(i)
}

sum(pvalvec1 <= .05)/ nsims  ## All of them
sum(pvalvec2 <= .05)/ nsims  #All of them
sum(sampsize1 == ncases) #All of them-- worked.
sum(sampsize2 == ncases) #All of them-- worked.

min(estvec1) #.47
max(estvec1) #.98
min(estvec2) #.44
max(estvec2) #.95

## The difference in significance is not due to the difference in sample size.  If our sample for the hearing regressions were only as large as the sample for the attendance regressions -- 268-- the marginal effect of prop. peers who heard would range from .47 to .98 in the simple regresion and .44 to .95 in the multivariate regression, and would be significant at the .05 level every time.  

#################################################################################################################
#
#
#                      TABLE 3: simple diffusion, attending conditional on hearing
#
#
#################################################################################################################

model.list <- list("m1" = c("AttendAlter"), "m2" = c("AttendAlter", "Neighborhood"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between proportion of peers who attended and attending the event",
                                covariate.labels = c("Prop. Peers Who Attended", "Neighborhood"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values are the marginal effects for the average observation. Data include all respondents who heard about the event. Network statistics calculated for undirected aggregate social network.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "regsimpleattend", no.space = TRUE, out = "regsimpleattend.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "regsimpleattend.tex")

#################################################################################################################
#
#
#                                   FIGURE 4: non-parametric estimation
#
#
#################################################################################################################

#################################################################################################################
# FUNCTIONS
#################################################################################################################

# Proportion of links between attenders by subnetwork
numwithin <- function(el, membervec){ #edgelist, vector of string names of those in set
  linkcount <- 0
  for(j in 1:nrow(el)){
    if (  (sum(el[j,1] == membervec) >= 1) &  (sum(el[j,2] == membervec) >=1 ) ){
      linkcount <- linkcount + 1
    }
  }
  return(linkcount)
}

# Draws random samples of nodes and computes the number of links between them
nwsims <- function(data, layer, membcands, membersize, nsims){
  el.temp <- subset(data, Layer %in% layer)
  numvec <- c()
  for(i in 1:nsims){
    draw <- sample(membcands, size = membersize, replace = FALSE)
    numvec[i] <- numwithin(el.temp, draw)
  }
  return(numvec)
}	

#################################################################################################################
# SIMULATIONS
#################################################################################################################

set.seed(07272016)
membcands <- unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled==1])
nsims <- 1000

# Number in each network that report hearing
layer.social <- c("Time", "Phone", "Pol", "Relig", "Meal", "Visit", "Secret")
propaction <- data.frame("Layer" = "Agg. Social", "HearCount" = NA, "HearProp" = NA, "AttendCount" = NA, "AttendProp" = NA)
# hear
hearers <- na.omit(unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled==1 & nodedfallneighbors$Hear==1]))
propaction$HearCount <- numwithin(el = edgelistall[edgelistall$Layer %in% layer.social,], membervec = hearers)
hearsims_data <- data.frame("Layer" = "Agg. Social", "Links" = nwsims(data = edgelistall, layer = layer.social, membcands = membcands, membersize = length(hearers), nsims = nsims))
propaction$HearProp <- sum(hearsims_data$Links < propaction$HearCount)/nsims 
# attend
abattenders <- unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled==1 & nodedfallneighbors$Attend==1])
propaction$AttendCount <- numwithin(el = edgelistall[edgelistall$Layer %in% layer.social,], membervec = abattenders)
sims_data <- data.frame("Layer" = "Agg. Social", "Links" = nwsims(data = edgelistall, layer = layer.social, membcands = membcands, membersize = length(abattenders), nsims = nsims))
propaction$AttendProp <- sum(sims_data$Links < propaction$AttendCount)/nsims 

#################################################################################################################
# DENSITY PLOTS
#################################################################################################################

# Hear
#pdf(file=paste0(figure_path, "aggsocial_density_hear.pdf"))
ggplot(hearsims_data, aes(Links)) +
  geom_density(size = 1) +
  scale_x_continuous(limits=c(min(hearsims_data$Links)-15, max(hearsims_data$Links)+15)) +
  xlab("Number of ties") +
  ylab("Density") +
  #ggtitle("Agg. Social") +
  geom_vline(xintercept = propaction$HearCount, colour = ifelse((propaction$HearProp>=0.95), "blue", "red"), linetype = "dashed", size = 1) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        plot.margin=unit(c(1,1,0,0),"cm"))
#dev.off()

# Attend
#pdf(file=paste0(figure_path, "aggsocial_density_attend.pdf"))
ggplot(sims_data, aes(Links)) +
  geom_density(size = 1) +
  scale_x_continuous(limits=c(min(sims_data$Links)-15, max(sims_data$Links)+15)) +
  xlab("Number of ties") +
  ylab("Density") +
  #ggtitle("Agg. Social") +
  geom_vline(xintercept = propaction$AttendCount, colour = ifelse((propaction$AttendProp>=0.95), "blue", "red"), linetype = "dashed", size = 1) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        plot.margin=unit(c(1,1,0,0),"cm"))
#dev.off()


#################################################################################################################
#
#
#                                                  FIGURE 5:
#
#
#################################################################################################################

hearprops <- c()
attendprops <- c()
d0 <- which(SocialSamp$DistLeader == 0)
hearprops[1] <- sum(SocialSamp$Hear[d0], na.rm=T)/length(d0)
attendprops[1] <- sum(SocialSamp$Attend[d0], na.rm=T)/length(d0)

d1 <- which(SocialSamp$DistLeader == 1)
hearprops[2] <- sum(SocialSamp$Hear[d1], na.rm=T)/length(d1)
attendprops[2] <- sum(SocialSamp$Attend[d1], na.rm=T)/length(d1)

d2 <- which(SocialSamp$DistLeader == 2)
hearprops[3] <- sum(SocialSamp$Hear[d2], na.rm=T)/length(d2)
attendprops[3] <- sum(SocialSamp$Attend[d2], na.rm=T)/length(d2)

d3 <- which(SocialSamp$DistLeader == 3)
hearprops[4] <- sum(SocialSamp$Hear[d3], na.rm=T)/length(d3)
attendprops[4] <- sum(SocialSamp$Attend[d3], na.rm=T)/length(d3)

d4 <- which(SocialSamp$DistLeader == 4)
hearprops[5] <- sum(SocialSamp$Hear[d4], na.rm=T)/length(d4)
attendprops[5] <- sum(SocialSamp$Attend[d4], na.rm=T)/length(d4)

par(mfrow=c(1,2))
barplot(table(SocialSamp$DistLeader), xlab="Minimum Distance to an Early Attender", ylab = "Count")
barplot(rbind(hearprops, attendprops), beside = TRUE, legend.text=c("Hear", "Attend"), xlab="Minimum Distance to an Early Attender", ylab="Proportion of People by Distance", names.arg=c(0,1,2,3,4))		


#################################################################################################################
#
#
#                                                  FIGURE 6:
#
#
#################################################################################################################

hearprops <- c()
attendprops <- c()
d0 <- which(SocialSamp$DistSeed == 0)
hearprops[1] <- sum(SocialSamp$Hear[d0], na.rm=T)/length(d0)
attendprops[1] <- sum(SocialSamp$Attend[d0], na.rm=T)/length(d0)

d1 <- which(SocialSamp$DistSeed == 1)
hearprops[2] <- sum(SocialSamp$Hear[d1], na.rm=T)/length(d1)
attendprops[2] <- sum(SocialSamp$Attend[d1], na.rm=T)/length(d1)

d2 <- which(SocialSamp$DistSeed == 2)
hearprops[3] <- sum(SocialSamp$Hear[d2], na.rm=T)/length(d2)
attendprops[3] <- sum(SocialSamp$Attend[d2], na.rm=T)/length(d2)

d3 <- which(SocialSamp$DistSeed == 3)
hearprops[4] <- sum(SocialSamp$Hear[d3], na.rm=T)/length(d3)
attendprops[4] <- sum(SocialSamp$Attend[d3], na.rm=T)/length(d3)

d4 <- which(SocialSamp$DistSeed == 4)
hearprops[5] <- sum(SocialSamp$Hear[d4], na.rm=T)/length(d4)
attendprops[5] <- sum(SocialSamp$Attend[d4], na.rm=T)/length(d4)

par(mfrow=c(1,2))
barplot(table(SocialSamp$DistSeed), xlab="Minimum Distance to a Seed", ylab = "Count")
barplot(rbind(hearprops, attendprops), beside = TRUE, legend.text=c("Hear", "Attend"), xlab="Minimum Distance to a Seed", ylab="Proportion of People by Distance", names.arg=c(0,1,2,3,4))		
par(mfrow=c(1,1))
#################################################################################################################
#
#
#                                                   TABLE 4
#
#
#################################################################################################################

# Disaggregating network types
# create list of models to run
data.list <- list(TimeSampHeard, PhoneSampHeard, PolSampHeard, ReligSampHeard, MealSampHeard, VisitSampHeard, SecretSampHeard)
names(data.list) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7")
# run models and compute adjusted R-squared
results <- lapply(data.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste("AttendAlter", collapse= " + "))), data = x, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit,coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1,results$m7$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1,results$m7$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = FALSE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits = 3), round(rsqr$m7, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between attending the event and neighbors in each network type who attended",
                                covariate.labels = c("PropPeersAttend"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values represent the marginal effects for the average observation. Network statistics calculated for undirected network comprised of a single tie type.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                column.labels = c("Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret"),
                                label = "attend_disag", no.space = TRUE, out = "attend_disag.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "attend_disag.tex")

#################################################################################################################
#
#
#                                                   TABLE 5
#
#
#################################################################################################################

# create list of models to run
model.list <- list("m1" = c("HearAlter", "Neighborhood", "Eigen",  "DistLeader", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("HearAlter", "Neighborhood",  "Eigen", "DistLeader","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                      "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste(x, collapse= " + "))), data = SocialSamp, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Heard about the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between peers who heard and hearing, conditional on other network attributes and ego and peer demographic characteristics",
                                covariate.labels = c("PropPeersHear", "NumPeers", "Eigen", "DistEarlyAttend", "Female", "Age", "Catholic", "Educ", "Married", "WallMat", "FemalePeers", "AgePeers", "CatholicPeers", "EducPeers", "MarriedPeers", "WallMatPeers"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values represent the marginal effects for the average observation. Network statistics calculated for undirected aggregate social network.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "hear_demognw6", no.space = TRUE, out = "hear_demognw6.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "hear_demognw6.tex")

#################################################################################################################
#
#
#                                                   TABLE 6
#
#
#################################################################################################################

# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood", "Eigen",  "DistLeader", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("AttendAlter", "Neighborhood",  "Eigen", "DistLeader","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                        "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between peers who attended and attendance, conditional on other network attributes and ego and peer demographic characteristics",
                                covariate.labels = c("PropPeersAttend", "NumPeers", "Eigen", "DistEarlyAttend", "Female", "Age", "Catholic", "Educ", "Married", "WallMat", "FemalePeers", "AgePeers", "CatholicPeers", "EducPeers", "MarriedPeers", "WallMatPeers"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values represent the marginal effects for the average observation. Network statistics calculated for undirected aggregate social network. Data include all respondents who heard about the event.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_demognw", no.space = TRUE, out = "attend_demognw.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_demognw.tex")

#################################################################################################################
#
#
#                                                   TABLE 7
#
#
#################################################################################################################

## Attending, centrality
# create list of models to run
model.list <- list("m1" = c("Neighborhood"), "m2" = c("NeighborhoodAlter"), "m3" = c("AvgDist"), "m4" = c("Eigen"), "m5" = c("Neighborhood", "AvgDist"), "m6" = c("Neighborhood", "Eigen"), "m7" = c("Neighborhood", "NeighborhoodAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1, results$m7$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1, results$m7$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits=3), round(rsqr$m7, digits=3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between network centrality and attending the event",
                                covariate.labels = c("NumPeers", "NumPeersPeers", "AvgDist", "Eigen"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values represent the marginal effects for the average observation. Network statistics calculated for undirected aggregate social network. Data include all respondents who heard about the event.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "central", no.space = TRUE, out = "central.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "central.tex")


#################################################################################################################
#
#
#                                                 TABLE 8:
#
#
#################################################################################################################

varlist <- c("Gender", "Age", "Catholic", "WallMat", "Married", "Educ", "Unemp", "PartTime", "FullTime", "Retired", "Neighborhood", "NeighborhoodAlter",  "DistSeed", "DistLeader", "AvgDist", "HearAlter", "AttendAlter", "Eigen", "Hear", "Attend")


bottomeigencut <- sort(SocialSamp$Eigen)[ceiling(.061*length(SocialSamp$Eigen))] #20th
topeigencut <- sort(SocialSamp$Eigen, decreasing=TRUE)[ceiling(.061*length(SocialSamp$Eigen))] #328th

bottomeigens <- which(SocialSamp$Eigen < bottomeigencut) #20 people
topeigens <- which(SocialSamp$Eigen > topeigencut) #20 people


eigencomp <- cbind(
  "bottomeigens"  = apply(as.matrix(SocialSamp[bottomeigens, varlist]), 2, mean, na.rm=T),
  "topeigens" = apply(as.matrix(SocialSamp[topeigens, varlist]), 2, mean, na.rm=T)
)

round(eigencomp, digits = 2)

# bottomeigens topeigens
# Gender                    0.75      0.30
# Age                      42.58     39.89
# Anglican                  0.05      0.05
# Catholic                  0.95      0.32
# Pentacostal               0.00      0.58
# WallMat                   0.11      0.10
# Married                   0.79      0.89
# Educ                      3.11      3.70
# Unemp                     0.00      0.10
# PartTime                  0.50      0.25
# FullTime                  0.11      0.10
# Retired                   0.39      0.55
# Neighborhood              7.20     22.70
# NeighborhoodAlter         6.71     13.05
# DistSeed                  2.75      1.35
# DistLeader                2.75      1.80
# AvgDist                   4.49      3.48
# HearAlter                 0.90      0.90
# AttendAlter               0.18      0.17
# Eigen                     0.004      0.349
# Hear                      0.80      0.90
# Attend                    0.55      0.35

## So the most central are more male, a bit younger, more pentacostal and more educated.  And, of course as per the definition of eigenvector centrality, they have larger neighborhoods, their neighbors have larger neighborhoods, and they're closer to everyone.  The most central were more likely to hear, but less likely to attend.   This also suggests the importance of conditioning on gender, religion, age and education.  

# And dividing hearing by attending (since all who attended heard and only those who heard were eligible to attend), bottom eigens: .3889 of hearers attended.  Top eigens: .6875 of hearers attended.

t.test((SocialSamp$Attend[bottomeigens]/SocialSamp$Hear[bottomeigens]), (SocialSamp$Attend[topeigens]/SocialSamp$Hear[topeigens]), alternative = "greater")  ## p.value .043 for one-sided t.test for the null that the true difference in means is not greater than zero.  Welch Two sample t-test.

pvalseigens <- c()
for(i in 1:length(varlist)){
  thetest <- t.test(SocialSamp[bottomeigens,varlist[i]], SocialSamp[topeigens,varlist[i]])
  pvalseigens[i] <- thetest$p.value
}
names(pvalseigens) <- varlist
round(pvalseigens, 4)


xtable(round(eigencomp, digits = 2))


#################################################################################################################
#
#
#                                           SUPPLEMENTARY INFORMATION
#
#
#################################################################################################################

#################################################################################################################
# TABLE 1 (SI)
#################################################################################################################


######################
## Make table of basic demographic information
## (Table 1)

## NOTE THAT IN abalang_layer_characterization.r, THESE ARE AVERAGED OVER LINKS, NOT EGOS.
#varint <- c("Gender", "Age", "Educ", "Married", "Catholic", "Anglican", "Pentacostal", "Other", "FullTime", "PartTime", "Unemp", "Retired", "WallMat", "Hear", "Attend", "MinsTrav")
varint <- c("Gender", "Age", "Educ", "Married", "Catholic", "FullTime", "PartTime", "Unemp", "Retired", "WallMat", "Hear", "Attend", "MinsTrav")
stargazer(SocialSamp[SocialSamp$Sampled==1, varint], summary=TRUE, digits = 2, title = "Descriptive Statistics", 
          covariate.labels = c("Gender (1 = female)", "Age", "Years of Schooling", "Married", "Catholic", "Employed Full Time", 
                               "Employed Part Time", "Unemployed", "Retired", "Housing Material (1 = cement, 0 = mud)", "Hear", "Attend", "Minutes Travelled"),
          nobs = FALSE, no.space = TRUE, align = TRUE, table.placement = "h", label = "demog", out = "demog.tex")



#################################################################################################################
# TABLE 3 (SI)
#################################################################################################################

## Make datasets for analyses:
TimeSamp <- collapsedata(layers=c("Time"), sampled=TRUE, heard=FALSE)
ReligSamp <- collapsedata(layers=c("Relig"), sampled= TRUE, heard=FALSE)
PolSamp <- collapsedata(layers=c("Pol"), sampled= TRUE, heard=FALSE)
VisitSamp <- collapsedata(layers=c("Visit"), sampled= TRUE, heard=FALSE)
SecretSamp <- collapsedata(layers=c("Secret"), sampled= TRUE, heard=FALSE)
PhoneSamp <- collapsedata(layers=c("Phone"), sampled= TRUE, heard=FALSE)
MealSamp <- collapsedata(layers=c("Meal"), sampled= TRUE, heard=FALSE)

## Attributes of the seven networks

layers <- c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit")
ind <- which(edgelistall$Layer %in% layers)
elsoc <- edgelistall[ind,c("Ego", "Alter")]
nwsoc <- graph_from_data_frame(elsoc, directed = FALSE)

SocTies <- edgelistall[edgelistall$Layer %in% layers, c("Ego", "Alter")]
TimeTies <- edgelistall[edgelistall$Layer == "Time", c("Ego", "Alter")]
PhoneTies <- edgelistall[edgelistall$Layer == "Phone", c("Ego", "Alter")]
PolTies <- edgelistall[edgelistall$Layer == "Pol", c("Ego", "Alter")]
MealTies <- edgelistall[edgelistall$Layer == "Meal", c("Ego", "Alter")]
VisitTies <- edgelistall[edgelistall$Layer == "Visit", c("Ego", "Alter")]
ReligTies <- edgelistall[edgelistall$Layer == "Relig", c("Ego", "Alter")]
SecretTies <- edgelistall[edgelistall$Layer == "Secret", c("Ego", "Alter")]

nwtime <- graph_from_data_frame(TimeTies, directed = FALSE)
nwphone <-  graph_from_data_frame(PhoneTies, directed = FALSE)
nwpol <-  graph_from_data_frame(PolTies, directed = FALSE)
nwmeal <- graph_from_data_frame(MealTies, directed = FALSE)
nwvisit <- graph_from_data_frame(VisitTies, directed = FALSE)
nwrelig <- graph_from_data_frame(ReligTies, directed = FALSE)
nwsecret <- graph_from_data_frame(SecretTies, directed = FALSE)

nwsocC<- induced_subgraph(nwsoc, v = SocialSamp$Ego)

nwtimeC <- induced_subgraph(nwtime, v = V(nwtime)$name[V(nwtime)$name %in% SocialSamp$Ego])
nwphoneC <- induced_subgraph(nwphone, v = V(nwphone)$name[V(nwphone)$name %in% SocialSamp$Ego])
nwpolC <- induced_subgraph(nwpol, v = V(nwpol)$name[V(nwpol)$name %in% SocialSamp$Ego])
nwmealC <- induced_subgraph(nwmeal, v = V(nwmeal)$name[V(nwmeal)$name %in% SocialSamp$Ego])
nwvisitC <- induced_subgraph(nwvisit, v = V(nwvisit)$name[V(nwvisit)$name %in% SocialSamp$Ego])
nwreligC <- induced_subgraph(nwrelig, v = V(nwrelig)$name[V(nwrelig)$name %in% SocialSamp$Ego])
nwsecretC <- induced_subgraph(nwsecret, v = V(nwsecret)$name[V(nwsecret)$name %in% SocialSamp$Ego])

comparenws <- rbind(
  "Nodes" = c(
    vcount(nwtime),
    vcount(nwphone),
    vcount(nwpol),
    vcount(nwrelig),
    vcount(nwmeal),
    vcount(nwvisit),
    vcount(nwsecret),
    vcount(nwsoc)
  ),
  "Edges" = c(
    ecount(nwtime),
    ecount(nwphone),
    ecount(nwpol),
    ecount(nwrelig),
    ecount(nwmeal),
    ecount(nwvisit),
    ecount(nwsecret),
    ecount(nwsoc)
  ),
  "SampledNodes" = c(
    vcount(nwtimeC),
    vcount(nwphoneC),
    vcount(nwpolC),
    vcount(nwreligC),
    vcount(nwmealC),
    vcount(nwvisitC),
    vcount(nwsecretC),
    vcount(nwsocC)
  ),
  "EdgesamongSampledNodes" = c(
    ecount(nwtimeC),
    ecount(nwphoneC),
    ecount(nwpolC),
    ecount(nwreligC),
    ecount(nwmealC),
    ecount(nwvisitC),
    ecount(nwsecretC),
    ecount(nwsocC)
  ),
  "AverageNeighborhoodSize"=c(
    mean(ego_size(nwtime, order = 1)-1),
    mean(ego_size(nwphone, order = 1)-1),
    mean(ego_size(nwpol, order = 1)-1),
    mean(ego_size(nwrelig, order = 1)-1),
    mean(ego_size(nwmeal, order = 1)-1),
    mean(ego_size(nwvisit, order = 1)-1),
    mean(ego_size(nwsecret, order = 1)-1),
    mean(ego_size(nwsoc, order = 1)-1)
  ),
  "AverageNeighborhoodSizeforSampled"=c(
    mean(TimeSamp$NeighborhoodLayer),
    mean(PhoneSamp$NeighborhoodLayer),
    mean(PolSamp$NeighborhoodLayer),
    mean(ReligSamp$NeighborhoodLayer),
    mean(MealSamp$NeighborhoodLayer),
    mean(VisitSamp$NeighborhoodLayer),
    mean(SecretSamp$NeighborhoodLayer),
    mean(SocialSamp$Neighborhood)
  ),
  "AverageEigenvectorCentrality"=c(
    mean(eigen_centrality(nwtime)$vector),
    mean(eigen_centrality(nwphone)$vector),
    mean(eigen_centrality(nwpol)$vector),
    mean(eigen_centrality(nwrelig)$vector),
    mean(eigen_centrality(nwmeal)$vector),
    mean(eigen_centrality(nwvisit)$vector),
    mean(eigen_centrality(nwsecret)$vector),
    mean(eigen_centrality(nwsoc)$vector)
  ),
  "AverageEigenCentforSampled"=c(
    mean(TimeSamp$EigenLayer),
    mean(PhoneSamp$EigenLayer),
    mean(PolSamp$EigenLayer),
    mean(ReligSamp$EigenLayer),
    mean(MealSamp$EigenLayer),
    mean(VisitSamp$EigenLayer),
    mean(SecretSamp$EigenLayer),
    mean(SocialSamp$Eigen)
  ),
  "AverageTransitivity" = c(
    mean(transitivity(nwtime, "local"), na.rm=T),
    mean(transitivity(nwphone, "local"), na.rm=T),
    mean(transitivity(nwpol, "local"), na.rm=T),
    mean(transitivity(nwrelig, "local"), na.rm=T),
    mean(transitivity(nwmeal, "local"), na.rm=T),
    mean(transitivity(nwvisit, "local"), na.rm=T),
    mean(transitivity(nwsecret, "local"), na.rm=T),
    mean(transitivity(nwsoc, "local"), na.rm=T)
  ),
  "Diameter" = c(
    diameter(nwtime),
    diameter(nwphone),
    diameter(nwpol),
    diameter(nwrelig),
    diameter(nwmeal),
    diameter(nwvisit),
    diameter(nwsecret),
    diameter(nwsoc)
  ),
  "DiameteramongSampled" = c(
    diameter(nwtimeC),
    diameter(nwphoneC),
    diameter(nwpolC),
    diameter(nwreligC),
    diameter(nwmealC),
    diameter(nwvisitC),
    diameter(nwsecretC),
    diameter(nwsocC)
  ),
  "NumberofComponentsSamp" = c(
    components(nwtimeC)$no,
    components(nwphoneC)$no,
    components(nwpolC)$no,
    components(nwreligC)$no,
    components(nwmealC)$no,
    components(nwvisitC)$no,
    components(nwsecretC)$no,
    components(nwsocC)$no
  ),
  "SizeofLgestComponentSamp" = c(
    components(nwtimeC)$csize[1],
    components(nwphoneC)$csize[1],
    components(nwpolC)$csize[1],
    components(nwreligC)$csize[1],
    components(nwmealC)$csize[1],
    components(nwvisitC)$csize[1],
    components(nwsecretC)$csize[1],
    components(nwsocC)$csize[1]
  ),
  "PropSampledConnectedtoSeed" = c(
    mean(TimeSamp$SeedConnectLayer),
    mean(PhoneSamp$SeedConnectLayer),
    mean(PolSamp$SeedConnectLayer),
    mean(ReligSamp$SeedConnectLayer),
    mean(MealSamp$SeedConnectLayer),
    mean(VisitSamp$SeedConnectLayer),
    mean(SecretSamp$SeedConnectLayer),
    mean(SocialSamp$SeedConnect)
  ),
  "AvgDist" = c(
    mean(TimeSamp$AvgDistLayer),
    mean(PhoneSamp$AvgDistLayer),
    mean(PolSamp$AvgDistLayer),
    mean(ReligSamp$AvgDistLayer),
    mean(MealSamp$AvgDistLayer),
    mean(VisitSamp$AvgDistLayer),
    mean(SecretSamp$AvgDistLayer),
    mean(SocialSamp$AvgDist)
  ),
  "AvgDisttoSeed" = c(
    mean(TimeSamp$DistSeedLayer, na.rm=T),
    mean(PhoneSamp$DistSeedLayer, na.rm=T),
    mean(PolSamp$DistSeedLayer, na.rm=T),
    mean(ReligSamp$DistSeedLayer, na.rm=T),
    mean(MealSamp$DistSeedLayer, na.rm=T),
    mean(VisitSamp$DistSeedLayer, na.rm=T),
    mean(SecretSamp$DistSeedLayer, na.rm=T),
    mean(SocialSamp$DistSeed, na.rm=T)
  )
)
colnames(comparenws) <- c("Time", "Phone", "Pol", "Relig", "Meal", "Visit", "Secret", "Agg. Social")

round(comparenws, digits =2)

SDs <- rbind(
  "SDNeighborhoodSize"=c(
    sd(ego_size(nwtime, order = 1)-1),
    sd(ego_size(nwphone, order = 1)-1),
    sd(ego_size(nwpol, order = 1)-1),
    sd(ego_size(nwrelig, order = 1)-1),
    sd(ego_size(nwmeal, order = 1)-1),
    sd(ego_size(nwvisit, order = 1)-1),
    sd(ego_size(nwsecret, order = 1)-1),
    sd(ego_size(nwsoc, order = 1)-1)
  ),
  "SDNeighborhoodSizeforSampled"=c(
    sd(TimeSamp$NeighborhoodLayer),
    sd(PhoneSamp$NeighborhoodLayer),
    sd(PolSamp$NeighborhoodLayer),
    sd(ReligSamp$NeighborhoodLayer),
    sd(MealSamp$NeighborhoodLayer),
    sd(VisitSamp$NeighborhoodLayer),
    sd(SecretSamp$NeighborhoodLayer),
    sd(SocialSamp$Neighborhood)
  ),
  "SDEigenvectorCentrality"=c(
    sd(eigen_centrality(nwtime)$vector),
    sd(eigen_centrality(nwphone)$vector),
    sd(eigen_centrality(nwpol)$vector),
    sd(eigen_centrality(nwrelig)$vector),
    sd(eigen_centrality(nwmeal)$vector),
    sd(eigen_centrality(nwvisit)$vector),
    sd(eigen_centrality(nwsecret)$vector),
    sd(eigen_centrality(nwsoc)$vector)
  ),
  "SDEigenCentforSampled"=c(
    sd(TimeSamp$EigenLayer),
    sd(PhoneSamp$EigenLayer),
    sd(PolSamp$EigenLayer),
    sd(ReligSamp$EigenLayer),
    sd(MealSamp$EigenLayer),
    sd(VisitSamp$EigenLayer),
    sd(SecretSamp$EigenLayer),
    sd(SocialSamp$Eigen)
  ),
  "SDTransitivity" = c(
    sd(transitivity(nwtime, "local"), na.rm=T),
    sd(transitivity(nwphone, "local"), na.rm=T),
    sd(transitivity(nwpol, "local"), na.rm=T),
    sd(transitivity(nwrelig, "local"), na.rm=T),
    sd(transitivity(nwmeal, "local"), na.rm=T),
    sd(transitivity(nwvisit, "local"), na.rm=T),
    sd(transitivity(nwsecret, "local"), na.rm=T),
    sd(transitivity(nwsoc, "local"), na.rm=T)
  ),
  "SDDist" = c(
    sd(TimeSamp$AvgDistLayer),
    sd(PhoneSamp$AvgDistLayer),
    sd(PolSamp$AvgDistLayer),
    sd(ReligSamp$AvgDistLayer),
    sd(MealSamp$AvgDistLayer),
    sd(VisitSamp$AvgDistLayer),
    sd(SecretSamp$AvgDistLayer),
    sd(SocialSamp$AvgDist)
  ),
  "SDDisttoSeed" = c(
    sd(TimeSamp$DistSeedLayer, na.rm=T),
    sd(PhoneSamp$DistSeedLayer, na.rm=T),
    sd(PolSamp$DistSeedLayer, na.rm=T),
    sd(ReligSamp$DistSeedLayer, na.rm=T),
    sd(MealSamp$DistSeedLayer, na.rm=T),
    sd(VisitSamp$DistSeedLayer, na.rm=T),
    sd(SecretSamp$DistSeedLayer, na.rm=T),
    sd(SocialSamp$DistSeed, na.rm=T)
  )
)


summary.social <- rbind(
  round(comparenws["Nodes",],0),
  round(comparenws["Edges",],0),
  round(comparenws["SampledNodes",],0),
  round(comparenws["EdgesamongSampledNodes",],0),
  round(comparenws["AverageNeighborhoodSize",],2),
  paste0("(", round(SDs["SDNeighborhoodSize",],2), ")", sep = ""),
  round(comparenws["AverageNeighborhoodSizeforSampled",],2),
  paste0("(", round(SDs["SDNeighborhoodSizeforSampled",],2), ")", sep = ""),
  round(comparenws["AverageEigenvectorCentrality",],2),
  paste0("(", round(SDs["SDEigenvectorCentrality",],2), ")", sep = ""),
  round(comparenws["AverageEigenCentforSampled",],2),
  paste0("(", round(SDs["SDEigenCentforSampled",],2), ")", sep = ""),
  round(comparenws["AverageTransitivity",],2),
  paste0("(", round(SDs["SDTransitivity",],2), ")", sep = ""),
  round(comparenws["AvgDist",],2),
  paste0("(", round(SDs["SDDist",],2), ")", sep = ""),
  round(comparenws["AvgDisttoSeed",],2),
  paste0("(", round(SDs["SDDisttoSeed",],2), ")", sep = ""),
  round(comparenws["Diameter",],0),
  round(comparenws["NumberofComponentsSamp",],2),
  round(comparenws["SizeofLgestComponentSamp",],2),
  round(comparenws["PropSampledConnectedtoSeed",],2)
)

summary.social <- as.data.frame(summary.social)

# Variable names
Variable <- c("Nodes", "Links", "Sampled Nodes", "Links among Sampled", "Avg. Neighb. Size", "", 
              "Avg. Neighb. Size for Samp.", "", "Avg. Eigen Cent.", "", "Avg. Eigen Cent. for Samp.", "", "Avg. Transitivity", "", "Avg. Dist.", "", "Avg. Dist. to Seed", "", "Diameter", "Number of Comp. among Samp.", "Size of Lgest Comp. among Samp", "Prop. Sampled with Path to Seed")

summary.social <- cbind(Variable, summary.social)

names(summary.social) <- c("", "Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secrets", "Agg. Social")                           


# Stargazer (manually add substitute l for p{\linewidth} to wrap table note.)
out <- capture.output(stargazer(summary.social, summary = FALSE, digits = 2, rownames = FALSE, table.placement = "h",
                                title = "Descriptive Statistics of Abalang Networks", label = "subnetworks_table",
                                notes = "Note: standard deviations in parentheses.", out = "subnetworks_table.tex"))


out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
# add additional code to adjust labels
out <- sub("AggSocial", "Agg. Social", out)
out <- sub("ccccccccc", "lcccccccc", out)
out <- sub("Variable", "", out)
#writeLines(out, "subnetworks_table.tex")

#################################################################################################################
# TABLE 4 (SI)
#################################################################################################################

#Hear again, this time without distseeds
# create list of models to run
model.list <- list("m1" = c("HearAlter", "Gender", "Age", "Catholic","Educ", "Married", "WallMat"), 
                   "m2" = c("HearAlter", "Gender", "Age", "Catholic","Educ", "Married", "WallMat", 
                            "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste(x, collapse= " + "))), data = SocialSamp, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Hear About the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Peer Effects on the Probability of Hearing About the Event for Agg. Social Network (conditional on other network characteristics and Ego/Alter demographics)",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "hear_justdemog", no.space = TRUE, out = "hear_justdemog.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "hear_justdemog.tex")

#################################################################################################################
# TABLE 5 (SI)
#################################################################################################################

#Attend, demographic controls
# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), 
                   "m2" = c("AttendAlter", "Gender", "Age", "Catholic","Educ", "Married", "WallMat", 
                            "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between peers attending and attendance, conditional on demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_justdemog", no.space = TRUE, out = "attend_justdemog.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_justdemog.tex")

#################################################################################################################
# TABLE 6 (SI)
#################################################################################################################

## Simple diffusion test 2, using the existence of a tie to a hearer:

SocialSamp$TieHeard <- ifelse(SocialSamp$HearAlter > 0, 1, 0)

## Regress hearing on hear alter and neighborhood 
# create list of models to run
model.list <- list("m1" = c("TieHeard"), "m2" = c("TieHeard", "Neighborhood"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste(x, collapse= " + "))), data = SocialSamp, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Hear About the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between having a peer who heard and hearing about the event",
                                covariate.labels = c("Tie to Peer Who Heard", "Neighborhood"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "regsimplehear_atie", no.space = TRUE, out = "regsimplehear_atie.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "regsimplehear_atie.tex")

#################################################################################################################
# TABLE 7 (SI)
#################################################################################################################

SocialSampHeard$TieAttended <- ifelse(SocialSampHeard$AttendAlter > 0, 1, 0)

## Simple diffusion test 2, using the existence of a tie to an attender:
model.list <- list("m1" = c("TieAttended"), "m2" = c("TieAttended", "Neighborhood"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between having a peer who attended and attending the event, conditional on hearing about it",
                                covariate.labels = c("Tie to Peer Who Attended", "Neighborhood"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "regsimpleattend_atie", no.space = TRUE, out = "regsimpleattend_atie.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "regsimpleattend_atie.tex")

#################################################################################################################
# TABLE 8 (SI)
#################################################################################################################

# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood"), "m2" = c("AttendAlter", "DistSeed"), "m3" = c("AttendAlter", "DistLeader"), "m4" = c("AttendAlter", "AvgDist"), "m5" = c("AttendAlter", "Eigen"), "m6" = c("AttendAlter",  "DistLeader", "AvgDist"), "m7" = c("AttendAlter", "DistSeed", "AvgDist"), "m8" = c("AttendAlter", "Neighborhood", "Eigen", "DistSeed", "DistLeader", "AvgDist"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit, results$m8$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1, results$m7$mfxest[,1]), c(1, results$m8$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1, results$m7$mfxest[,2]), c(1, results$m8$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits=3), round(rsqr$m7, digits=3), round(rsqr$m8, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Attendance Conditional on Network Attributes",
                                covariate.labels = c("PropPeersAttended", "NumPeers", "DistSeed", "DistEarlyAttend", "AvgDist", "Eigen"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Reported values are the marginal effects for the average observation. Network statistics calculated for undirected aggregate social network. Data include all respondents who heard about the event.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "reg1avgdistattend", no.space = TRUE, out = "reg1avgdistattend.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "reg1avgdistattend.tex")

#################################################################################################################
# TABLE 9 (SI)
#################################################################################################################

################################
## Let's calculate the sampling distribution of coefficients and p-values from regressions that are the same except DistLeader is swapped out for DistRnd, the distance from 11 randomly-chosen day 2 or 3 attenders.

## PLACEBO TEST 1: randomly select 11 attenders who were not early attenders (day 2 or day 3 attenders).  Calculate everyone's distance to them and rerun four specifications.  Collect the marginal effects and p values.  

## Start with distmat calculated on the social network above

ind <- which(edgelistall$Layer %in% c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit"))
elsoc <- edgelistall[ind,c("Ego", "Alter")]
nwsoc <- graph_from_data_frame(elsoc, directed = FALSE)
distmat <- shortest.paths(nwsoc, v=V(nwsoc), to=V(nwsoc))

set.seed <- 13579

lateattenders <- which(SocialSampHeard$EventDay == 2 | SocialSampHeard$EventDay == 3)

nsims <- 5000
estvec1 <- c()
estvec2 <- c()
estvec3 <- c()
estvec4 <- c()
pvalvec1 <- c()
pvalvec2 <- c()
pvalvec3 <-c()
pvalvec4 <- c()


for(i in 1:nsims){
  draw <- sample(lateattenders, 11, replace = FALSE)
  namesdraw <- SocialSampHeard$Ego[draw]
  disttodraw <- distmat[,namesdraw]
  newdatatemp <- data.frame("Ego" = rownames(disttodraw), "DistDraw" = apply(disttodraw, 1, min, na.rm=T))
  datatouse <- merge(SocialSampHeard, newdatatemp, by = "Ego", all.x=TRUE)
  
  out1 <- logitmfx(Attend ~ DistDraw + AvgDist, data = datatouse, atmean=TRUE, robust=TRUE)
  out2 <- logitmfx(Attend ~ DistDraw + AvgDist + AttendAlter, data = datatouse, atmean=TRUE, robust=TRUE)
  out3 <- logitmfx(Attend ~ DistDraw + AttendAlter, data = datatouse, atmean=TRUE, robust = TRUE)
  out4 <- logitmfx(Attend ~ DistDraw + AttendAlter + Neighborhood + DistSeed + AvgDist + Eigen + Gender + Age + Catholic + Educ + Married + WallMat, data = datatouse, atmean = TRUE, robust = TRUE)
  
  estvec1[i] <- out1$mfxest[1]
  pvalvec1[i] <- out1$mfxest[4]
  
  estvec2[i] <- out2$mfxest[1]
  pvalvec2[i] <- out2$mfxest[4]
  
  estvec3[i] <- out3$mfxest[1]
  pvalvec3[i] <- out3$mfxest[4]
  
  estvec4[i] <- out4$mfxest[1]
  pvalvec4[i] <- out4$mfxest[4]
  
  if(i %% 100 == 0) print(i)
  
}

## PLACEBO TEST 2: randomly select 11 people who were not early attenders, regardless of whether they attended or not.  Calculate everyone's distance to them and rerun four specifications.  Collect the marginal effects and p values. 

earlyattenders <- which(SocialSampHeard$EventDay == 1)
notearlyattenders <- 1:nrow(SocialSampHeard)
notearlyattenders <- notearlyattenders[-earlyattenders]
notearlyattenders <- notearlyattenders[SocialSampHeard$Ego[notearlyattenders] %in% rownames(distmat)]

nsims <- 5000
estvec1b <- c()
estvec2b <- c()
estvec3b <- c()
estvec4b <- c()
pvalvec1b <- c()
pvalvec2b <- c()
pvalvec3b <-c()
pvalvec4b <- c()


for(i in 1:nsims){
  draw <- sample(notearlyattenders, 11, replace = FALSE)
  namesdraw <- SocialSampHeard$Ego[draw]
  disttodraw <- distmat[,namesdraw]
  newdatatemp <- data.frame("Ego" = rownames(disttodraw), "DistDraw" = apply(disttodraw, 1, min, na.rm=T))
  datatouse <- merge(SocialSampHeard, newdatatemp, by = "Ego", all.x=TRUE)
  
  out1 <- logitmfx(Attend ~ DistDraw + AvgDist, data = datatouse, atmean=TRUE, robust=TRUE)
  out2 <- logitmfx(Attend ~ DistDraw + AvgDist + AttendAlter, data = datatouse, atmean=TRUE, robust=TRUE)
  out3 <- logitmfx(Attend ~ DistDraw + AttendAlter, data = datatouse, atmean=TRUE, robust = TRUE)
  out4 <- logitmfx(Attend ~ DistDraw + AttendAlter + Neighborhood + DistSeed + AvgDist + Eigen + Gender + Age + Catholic + Educ + Married + WallMat, data = datatouse, atmean = TRUE, robust = TRUE)
  
  estvec1b[i] <- out1$mfxest[1]
  pvalvec1b[i] <- out1$mfxest[4]
  
  estvec2b[i] <- out2$mfxest[1]
  pvalvec2b[i] <- out2$mfxest[4]
  
  estvec3b[i] <- out3$mfxest[1]
  pvalvec3b[i] <- out3$mfxest[4]
  
  estvec4b[i] <- out4$mfxest[1]
  pvalvec4b[i] <- out4$mfxest[4]
  
  if(i %% 100 == 0) print(i)
  
}

## PLACEBO TEST 3: randomly select 11 people who were not attenders.  Calculate everyone's distance to them and rerun four specifications.  Collect the marginal effects and p values. 

nonattenders <- which(SocialSampHeard$Attend == 0)

nsims <- 5000
estvec1c <- c()
estvec2c <- c()
estvec3c <- c()
estvec4c <- c()
pvalvec1c <- c()
pvalvec2c <- c()
pvalvec3c <-c()
pvalvec4c <- c()


for(i in 1:nsims){
  draw <- sample(nonattenders, 11, replace = FALSE)
  namesdraw <- SocialSampHeard$Ego[draw]
  disttodraw <- distmat[,namesdraw]
  newdatatemp <- data.frame("Ego" = rownames(disttodraw), "DistDraw" = apply(disttodraw, 1, min, na.rm=T))
  datatouse <- merge(SocialSampHeard, newdatatemp, by = "Ego", all.x=TRUE)
  
  out1 <- logitmfx(Attend ~ DistDraw + AvgDist, data = datatouse, atmean=TRUE, robust=TRUE)
  out2 <- logitmfx(Attend ~ DistDraw + AvgDist + AttendAlter, data = datatouse, atmean=TRUE, robust=TRUE)
  out3 <- logitmfx(Attend ~ DistDraw + AttendAlter, data = datatouse, atmean=TRUE, robust = TRUE)
  out4 <- logitmfx(Attend ~ DistDraw + AttendAlter + Neighborhood + DistSeed + AvgDist + Eigen + Gender + Age + Catholic + Educ + Married + WallMat, data = datatouse, atmean = TRUE, robust = TRUE)
  
  estvec1c[i] <- out1$mfxest[1]
  pvalvec1c[i] <- out1$mfxest[4]
  
  estvec2c[i] <- out2$mfxest[1]
  pvalvec2c[i] <- out2$mfxest[4]
  
  estvec3c[i] <- out3$mfxest[1]
  pvalvec3c[i] <- out3$mfxest[4]
  
  estvec4c[i] <- out4$mfxest[1]
  pvalvec4c[i] <- out4$mfxest[4]
  
  if(i %% 100 == 0) print(i)
  
}

## For the simulations of the four specifications where the 11 were selected from the other attenders:

mean(pvalvec1) #.11
mean(estvec1) #-.13
mean(pvalvec2) #.046
mean(estvec2) #-.13
mean(pvalvec3) #.24
mean(estvec3) #-.10
mean(pvalvec4) #.04
mean(estvec4) #-.13


## This is a placebo test.  The reported results are more believable if these placebos do NOT produce significant results.  

sum(pvalvec1 <= .05)/ nsims  ## None.  Our significant result here is meaningful.
sum(pvalvec2 <= .05)/ nsims  ## .88  Our significant result here is less meaningful.  
sum(pvalvec3 <= .05)/ nsims ## None.
sum(pvalvec4 <= .05)/ nsims ## .73

sum(pvalvec1 <= .01)/ nsims  ## None.  
sum(pvalvec2 <= .01)/ nsims  ## None.  
sum(pvalvec3 <= .01)/ nsims ## None.
sum(pvalvec4 <= .01)/ nsims ## .04  So 4% of the time we'd get a result significant at the .01 level by using distance to later attenders.

min(estvec1) #-.32
max(estvec1) #.06
min(estvec2) #-.33
max(estvec2) #.07
min(estvec3) #-.27
max(estvec3) #.07
min(estvec4) #-.37
max(estvec4) #.04

## For the simulations of the four specifications where the 11 were selected from ANY non-early attendender, including non-attenders.

mean(pvalvec1b) #.10
mean(estvec1b) #.02
mean(pvalvec2b) #.046
mean(estvec2b) #.02
mean(pvalvec3b) #.23
mean(estvec3b) #.03
mean(pvalvec4b) #.05
mean(estvec4b) #.02

sum(pvalvec1b <= .05)/ nsims  ## None.  Our significant result here is meaningful.  None of the simulated values are significant at the .05 level.  
sum(pvalvec2b <= .05)/ nsims  ## .80  Our significant result here is less meaningful.  80% of simulated results are also significant at the .05 level.  
sum(pvalvec3b <= .05)/ nsims ## None.
sum(pvalvec4b <= .05)/ nsims ## .83

sum(pvalvec1b <= .01)/ nsims  ## None.  
sum(pvalvec2b <= .01)/ nsims  ## None.  
sum(pvalvec3b <= .01)/ nsims ## None.
sum(pvalvec4b <= .01)/ nsims ## .03  So 3% of the time we'd get a result significant at the .01 level by using distance to later attenders.

min(estvec1b) #-.23
max(estvec1b) #.24
min(estvec2b) #-.23
max(estvec2b) #.25
min(estvec3b) #-.18
max(estvec3b) #.22
min(estvec4b) #-.23
max(estvec4b) #.28

## And for the simulations where the 11 were selected specifically from a non-attender

mean(pvalvec1c) #.10
mean(estvec1c) #.16
mean(pvalvec2c) #.046
mean(estvec2c) #.16
mean(pvalvec3c) #.23
mean(estvec3c) #.15
mean(pvalvec4c) #.04
mean(estvec4c) #.16

sum(pvalvec1c <= .05)/ nsims  ## None.  Our significant result here is meaningful.  None of the simulated values are significant at the .05 level.  
sum(pvalvec2c <= .05)/ nsims  ## .80  Our significant result here is less meaningful.  80% of simulated results are also significant at the .05 level.  
sum(pvalvec3c <= .05)/ nsims ## None.
sum(pvalvec4c <= .05)/ nsims ## .72

sum(pvalvec1c <= .01)/ nsims  ## None.  
sum(pvalvec2c <= .01)/ nsims  ## None.  
sum(pvalvec3c <= .01)/ nsims ## None.
sum(pvalvec4c <= .01)/ nsims ## .09  So 9% of the time we'd get a result significant at the .01 level by using distance to later attenders.

min(estvec1c) #.005
max(estvec1c) #.37
min(estvec2c) #.008
max(estvec2c) #.38
min(estvec3c) #.02
max(estvec3c) #.34
min(estvec4c) #-.06
max(estvec4c) #.35

## So using as placebos 11 attenders who were not early attenders, we still get negative coefficients, but they're not significant as often.  Using as placebos 11 people who were not early attenders but may have attended later or not, we get coefficients that are really close to zero, bouncing between negative and positive.  Instead using 11 people who did not attend, we get coefficients that are positive.  No results are as precise as consistently as our finding.  Shows two things.  1: in general, relationship between connections to early attenders v. attenders v. anyone v. non-attenders is as expected.  2: our finding that distance to early attenders is very precise is unusual-- we would not find the same level of precision or a coeffficient of the same magnitude if we were using 11 other people as the targets.  

#################################################################################################################
# TABLE 13 (SI)
#################################################################################################################
# CHI-SQUARED TESTS WITH CLAN
library(MASS)

layer_list <- list("Time", "Phone", "Pol", "Relig", "Meal", "Visit", "Secret", "From", "To")

perform_chi_test <- function(layer){
  layerClan <- data %>% select()
  
  layerClan <- rbind(data.frame("layer" = data[,paste0(layer, 1)], "clan" = as.character(data[,paste0(layer, 1, "Clan")]),  stringsAsFactors = F),
                     data.frame("layer" = data[,paste0(layer, 2)], "clan" = as.character(data[,paste0(layer, 2, "Clan")]),  stringsAsFactors = F),
                     data.frame("layer" = data[,paste0(layer, 3)], "clan" = as.character(data[,paste0(layer, 3, "Clan")]),  stringsAsFactors = F),
                     data.frame("layer" = data[,paste0(layer, 4)], "clan" = as.character(data[,paste0(layer, 4, "Clan")]),  stringsAsFactors = F),
                     data.frame("layer" = data[,paste0(layer, 5)], "clan" = as.character(data[,paste0(layer, 5, "Clan")])),  stringsAsFactors = F)
  
  layerClan <- layerClan[!is.na(layerClan$layer) & (layerClan$clan %in% c("Y", "N")),]
  tbl <- table(layerClan$clan)
  test_stat <- chisq.test(tbl)
  return(data.frame("no"= tbl[1], "yes" = tbl[2], "p_value" = test_stat$p.value))
}

chi_results <- lapply(layer_list, function(x) perform_chi_test(layer = x)) %>% setNames(layer_list)
chi_results <- do.call("rbind", chi_results)

#################################################################################################################
# TABLE 14 (SI)
#################################################################################################################

#############################
## Now disaggregate the network into its seven consituents 

# create list of models to run
data.list <- list(TimeSamp, PhoneSamp, PolSamp, ReligSamp, MealSamp, VisitSamp, SecretSamp)
names(data.list) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7")
# run models and compute adjusted R-squared
results <- lapply(data.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste("HearAlter", collapse= " + "))), data = x, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit,  coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1,results$m7$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1,results$m7$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Hear About the Event)", dep.var.labels.include = FALSE, model.numbers = FALSE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits = 3), round(rsqr$m7, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between hearing about the event and neighbors in each network type who heard",
                                covariate.labels = c("Prop. Peers Who Heard"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Each network is the undirected network comprised of a single tie type. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                column.labels = c("Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret"),
                                label = "hear_disag", no.space = TRUE, out = "hear_disag.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "hear_disag.tex")

#################################################################################################################
# TABLE 15 (SI)
#################################################################################################################

# create list of models to run
data.list <- list(TimeSampHeard, PhoneSampHeard, PolSampHeard, ReligSampHeard, MealSampHeard, VisitSampHeard, SecretSampHeard)
names(data.list) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7")
# run models and compute adjusted R-squared
results <- lapply(data.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(c("AttendAlter", "DistLeaderLayer", "AvgDistLayer"), collapse= " + "))), data = x, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit,  coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1,results$m7$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1,results$m7$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = FALSE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits = 3), round(rsqr$m7, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between attending the event and neighbors in each network type who attended",
                                covariate.labels = c("Prop. Peers Who Attended"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                column.labels = c("Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret"),
                                label = "attend_disag_nw3", no.space = TRUE, out = "attend_disag_nw3.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "attend_disag_nw3.tex")

#################################################################################################################
# TABLE 16 (SI)
#################################################################################################################

# Proportion of links between attenders by subnetwork
numwithin <- function(el, membervec){ #edgelist, vector of string names of those in set
  linkcount <- 0
  for(j in 1:nrow(el)){
    if (  (sum(el[j,1] == membervec) >= 1) &  (sum(el[j,2] == membervec) >=1 ) ){
      linkcount <- linkcount + 1
    }
  }
  return(linkcount)
}

# Number in each network that report hearing
hearers <- na.omit(unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled==1 & nodedfallneighbors$Hear==1]))
layer.social <- c("Time", "Phone", "Pol", "Relig", "Meal", "Visit", "Secret")
propaction <- data.frame("Layer" = c("Agg. Social", "Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret"), "HearCount" = NA, "HearProp" = NA, "AttendCount" = NA, "AttendProp" = NA)
propaction$HearCount[propaction$Layer == "Agg. Social"] <- numwithin(el = edgelistall[edgelistall$Layer %in% layer.social,], membervec = hearers)
propaction$HearCount[propaction$Layer == "Time"] <- numwithin(el = edgelistall[edgelistall$Layer == "Time",], membervec = hearers)
propaction$HearCount[propaction$Layer == "Phone"] <- numwithin(el = edgelistall[edgelistall$Layer == "Phone",], membervec = hearers)
propaction$HearCount[propaction$Layer == "Politics"] <- numwithin(el = edgelistall[edgelistall$Layer == "Pol",], membervec = hearers)
propaction$HearCount[propaction$Layer == "Religion"] <- numwithin(el = edgelistall[edgelistall$Layer == "Relig",], membervec = hearers)
propaction$HearCount[propaction$Layer == "Meal"] <- numwithin(el = edgelistall[edgelistall$Layer == "Meal",], membervec = hearers)
propaction$HearCount[propaction$Layer == "Visit"] <- numwithin(el = edgelistall[edgelistall$Layer == "Visit",], membervec = hearers)
propaction$HearCount[propaction$Layer == "Secret"] <- numwithin(el = edgelistall[edgelistall$Layer == "Secret",], membervec = hearers)

# Number in each network that report attending
abattenders <- unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled==1 & nodedfallneighbors$Attend==1])
propaction$AttendCount[propaction$Layer == "Agg. Social"] <- numwithin(el = edgelistall[edgelistall$Layer %in% layer.social,], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Time"] <- numwithin(el = edgelistall[edgelistall$Layer == "Time",], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Phone"] <- numwithin(el = edgelistall[edgelistall$Layer == "Phone",], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Politics"] <- numwithin(el = edgelistall[edgelistall$Layer == "Pol",], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Religion"] <- numwithin(el = edgelistall[edgelistall$Layer == "Relig",], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Meal"] <- numwithin(el = edgelistall[edgelistall$Layer == "Meal",], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Visit"] <- numwithin(el = edgelistall[edgelistall$Layer == "Visit",], membervec = abattenders)
propaction$AttendCount[propaction$Layer == "Secret"] <- numwithin(el = edgelistall[edgelistall$Layer == "Secret",], membervec = abattenders)

# Draws random samples of nodes and computes the number of links between them
nwsims <- function(data, layer, membcands, membersize, nsims){
  el.temp <- subset(data, Layer %in% layer)
  numvec <- c()
  for(i in 1:nsims){
    draw <- sample(membcands, size = membersize, replace = FALSE)
    numvec[i] <- numwithin(el.temp, draw)
  }
  return(numvec)
}	

# Run simulations
set.seed(07272016)
membcands <- unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled==1])
nsims <- 1000
# Hear
hearoverallsims <- data.frame("Layer" = "Agg. Social", "Links" = nwsims(data = edgelistall, layer = layer.social, membcands = membcands, membersize = length(hearers), nsims = nsims))
heartimesims <- data.frame("Layer" = "Time", "Links" = nwsims(data = edgelistall, layer = "Time", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearphonesims <- data.frame("Layer" = "Phone", "Links" = nwsims(data = edgelistall, layer = "Phone", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearpolsims <- data.frame("Layer" = "Politics", "Links" = nwsims(data = edgelistall, layer = "Pol", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearreligsims <- data.frame("Layer" = "Religion", "Links" = nwsims(data = edgelistall, layer = "Relig", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearmealsims <- data.frame("Layer" = "Meal", "Links" = nwsims(data = edgelistall, layer = "Meal", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearvisitsims <- data.frame("Layer" = "Visit", "Links" = nwsims(data = edgelistall, layer = "Visit", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearsecretsims <- data.frame("Layer" = "Secret", "Links" = nwsims(data = edgelistall, layer = "Secret", membcands = membcands, membersize = length(hearers), nsims = nsims))
hearsims_data <- rbind(hearoverallsims, heartimesims, hearphonesims, hearpolsims, hearreligsims, hearmealsims, hearvisitsims, hearsecretsims)

# Attend
overallsims <- data.frame("Layer" = "Agg. Social", "Links" = nwsims(data = edgelistall, layer = layer.social, membcands = membcands, membersize = length(abattenders), nsims = nsims))
timesims <- data.frame("Layer" = "Time", "Links" = nwsims(data = edgelistall, layer = "Time", membcands = membcands, membersize = length(abattenders), nsims = nsims))
phonesims <- data.frame("Layer" = "Phone", "Links" = nwsims(data = edgelistall, layer = "Phone", membcands = membcands, membersize = length(abattenders), nsims = nsims))
polsims <- data.frame("Layer" = "Politics", "Links" = nwsims(data = edgelistall, layer = "Pol", membcands = membcands, membersize = length(abattenders), nsims = nsims))
religsims <- data.frame("Layer" = "Religion", "Links" = nwsims(data = edgelistall, layer = "Relig", membcands = membcands, membersize = length(abattenders), nsims = nsims))
mealsims <- data.frame("Layer" = "Meal", "Links" = nwsims(data = edgelistall, layer = "Meal", membcands = membcands, membersize = length(abattenders), nsims = nsims))
visitsims <- data.frame("Layer" = "Visit", "Links" = nwsims(data = edgelistall, layer = "Visit", membcands = membcands, membersize = length(abattenders), nsims = nsims))
secretsims <- data.frame("Layer" = "Secret", "Links" = nwsims(data = edgelistall, layer = "Secret", membcands = membcands, membersize = length(abattenders), nsims = nsims))
sims_data <- rbind(overallsims, timesims, phonesims, polsims, religsims, mealsims, visitsims, secretsims)

# Table
# Hear
propaction$HearProp[propaction$Layer == "Agg. Social"] <- sum(hearoverallsims$Links < propaction$HearCount[propaction$Layer == "Agg. Social"])/nsims 
propaction$HearProp[propaction$Layer == "Time"] <- sum(heartimesims$Links < propaction$HearCount[propaction$Layer == "Time"])/nsims
propaction$HearProp[propaction$Layer == "Phone"] <- sum(hearphonesims$Links < propaction$HearCount[propaction$Layer == "Phone"])/nsims
propaction$HearProp[propaction$Layer == "Politics"] <- sum(hearpolsims$Links < propaction$HearCount[propaction$Layer == "Politics"])/nsims
propaction$HearProp[propaction$Layer == "Religion"] <- sum(hearreligsims$Links < propaction$HearCount[propaction$Layer == "Religion"])/nsims
propaction$HearProp[propaction$Layer == "Meal"] <- sum(hearmealsims$Links < propaction$HearCount[propaction$Layer == "Meal"])/nsims
propaction$HearProp[propaction$Layer == "Visit"] <- sum(hearvisitsims$Links < propaction$HearCount[propaction$Layer == "Visit"])/nsims
propaction$HearProp[propaction$Layer == "Secret"] <- sum(hearsecretsims$Links < propaction$HearCount[propaction$Layer == "Secret"])/nsims

# Attend
propaction$AttendProp[propaction$Layer == "Agg. Social"] <- sum(overallsims$Links < propaction$AttendCount[propaction$Layer == "Agg. Social"])/nsims 
propaction$AttendProp[propaction$Layer == "Time"] <- sum(timesims$Links < propaction$AttendCount[propaction$Layer == "Time"])/nsims
propaction$AttendProp[propaction$Layer == "Phone"] <- sum(phonesims$Links < propaction$AttendCount[propaction$Layer == "Phone"])/nsims
propaction$AttendProp[propaction$Layer == "Politics"] <- sum(polsims$Links < propaction$AttendCount[propaction$Layer == "Politics"])/nsims
propaction$AttendProp[propaction$Layer == "Religion"] <- sum(religsims$Links < propaction$AttendCount[propaction$Layer == "Religion"])/nsims
propaction$AttendProp[propaction$Layer == "Meal"] <- sum(mealsims$Links < propaction$AttendCount[propaction$Layer == "Meal"])/nsims
propaction$AttendProp[propaction$Layer == "Visit"] <- sum(visitsims$Links < propaction$AttendCount[propaction$Layer == "Visit"])/nsims
propaction$AttendProp[propaction$Layer == "Secret"] <- sum(secretsims$Links < propaction$AttendCount[propaction$Layer == "Secret"])/nsims
# Spring cleaning
rm(hearoverallsims, heartimesims, hearphonesims, hearpolsims, hearreligsims, hearmealsims, hearvisitsims, hearsecretsims,
   overallsims, timesims, phonesims, polsims, religsims, mealsims, visitsims, secretsims)

# Stargazer table
stargazer(propaction, summary = FALSE, digits = 2, rownames = FALSE, label = "densitytable",
          covariate.labels = c("", "Links Among Hearers", "Prop. Rnd $<$ Links", "Links Among Attenders", "Prop. Rnd $<$ Links"),
          title = "Links Among Hearers and Attenders Relative to Random Samples of Nodes",
          out = "densitytable.tex")

#################################################################################################################
# FIGURE 2 & FIGURE 3 (SI)
#################################################################################################################

# Density Plots Hear
density_plot_hear <- function(subnetwork){
  #png(paste0(tolower(subnetwork), "_density_hear.png", sep = ""))
  ggplot(hearsims_data[hearsims_data$Layer == subnetwork,], aes(Links)) +
    geom_density() +
    scale_x_continuous(limits=c(min(hearsims_data$Links[hearsims_data$Layer == subnetwork])-15, max(hearsims_data$Links[hearsims_data$Layer == subnetwork])+15)) +
    xlab("") +
    ylab("Density") +
    ggtitle(subnetwork) +
    geom_vline(xintercept = propaction$HearCount[propaction$Layer == subnetwork], 
               colour = ifelse((propaction$HearProp[propaction$Layer == subnetwork]>=0.95), "blue", "red"), linetype = "dashed") +
    theme(plot.title = element_text(hjust = 0.5))
}

# Density Plots Attend
density_plot_attend <- function(subnetwork){
  #png(paste0(tolower(subnetwork), "_density_attend.png", sep = ""))
  ggplot(sims_data[sims_data$Layer == subnetwork,], aes(Links)) +
    geom_density() +
    scale_x_continuous(limits=c(min(sims_data$Links[sims_data$Layer == subnetwork])-15, max(sims_data$Links[sims_data$Layer == subnetwork])+15)) +
    xlab("") +
    ylab("Density") +
    ggtitle(subnetwork) +
    geom_vline(xintercept = propaction$AttendCount[propaction$Layer == subnetwork], 
               colour = ifelse((propaction$AttendProp[propaction$Layer == subnetwork]>=0.95), "blue", "red"), linetype = "dashed") +
    theme(plot.title = element_text(hjust = 0.5))
}

# Run function
#subnetwork.list <- list("Agg. Social", "Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret")
#lapply(subnetwork.list, function(x) density_plot(x) %>% dev.off())
# Hear
density_plot_hear(subnetwork = "Agg. Social")
#dev.off()
density_plot_hear(subnetwork = "Time")
#dev.off()
density_plot_hear(subnetwork = "Phone")
#dev.off()
density_plot_hear(subnetwork = "Politics")
#dev.off()
density_plot_hear(subnetwork = "Religion")
#dev.off()
density_plot_hear(subnetwork = "Meal")
#dev.off()
density_plot_hear(subnetwork = "Visit")
#dev.off()
density_plot_hear(subnetwork = "Secret")
#dev.off()
# Attend
density_plot_attend(subnetwork = "Agg. Social")
#dev.off()
density_plot_attend(subnetwork = "Time")
#dev.off()
density_plot_attend(subnetwork = "Phone")
#dev.off()
density_plot_attend(subnetwork = "Politics")
#dev.off()
density_plot_attend(subnetwork = "Religion")
#dev.off()
density_plot_attend(subnetwork = "Meal")
#dev.off()
density_plot_attend(subnetwork = "Visit")
#dev.off()
density_plot_attend(subnetwork = "Secret")
#dev.off()

#################################################################################################################
# TABLE 17 (SI)
#################################################################################################################

## Make datasets for analyses:

## Calculates network statistics on network that includes every layer's links, even those duplicating a link already present between two people.  Eigenvector centrality then weighted by these links. 
ind <- which(edgelistall$Layer %in% c("Meal", "Phone", "Pol", "Relig", "Secret", "Time", "Visit"))
elsoc <- edgelistall[ind,c("Ego", "Alter")]
nwsoc <- graph_from_data_frame(elsoc, directed = FALSE)
rm(ind)

## Calculates network statistics on network that only includes one link between any two people (throws away links present in additional layers).    Eigenvector centrality then unweighted. 

elsocunique <- elsoc[duplicated(elsoc) == 0,]
nwsocunique <- graph_from_data_frame(elsocunique, directed = FALSE)


weighted <- data.frame("Ego"=V(nwsoc)$name, "WeightedEigen" = eigen_centrality(nwsoc)$vector, row.names=NULL)

unweighted <- data.frame("Ego"=V(nwsocunique)$name, "UnweightedEigen" = eigen_centrality(nwsocunique)$vector, row.names=NULL)

## Calculates network statistics on netowrk containing only links among those sampled

sampled <- unique(nodedfallneighbors$Ego[nodedfallneighbors$Sampled == TRUE])
nwsoccl <- induced_subgraph(nwsoc, v = sampled)

nwsocuniquecl <- induced_subgraph(nwsocunique, v = sampled)

weightedclosed <- data.frame("Ego"=V(nwsoccl)$name, "WeightedEigenCl" = eigen_centrality(nwsoccl)$vector, row.names=NULL)

unweightedclosed <- data.frame("Ego"=V(nwsocuniquecl)$name, "UnweightedEigenCl" = eigen_centrality(nwsocuniquecl)$vector, row.names=NULL)

## Now add these variables to nodedfallneighbors:

nodedfallneighbors <- merge(nodedfallneighbors, weighted, by = "Ego", all.x=T)
nodedfallneighbors <- merge(nodedfallneighbors, unweighted, by = "Ego", all.x=T)
nodedfallneighbors <- merge(nodedfallneighbors, weightedclosed, by = "Ego", all.x=T)
nodedfallneighbors <- merge(nodedfallneighbors, unweightedclosed, by = "Ego", all.x=T)

## collect count of number of links that a person reports that were sampled (for weighted)
sampalterlinkscount <- aggregate(nodedfallneighbors[,c("SampledAlter")], by=list("Ego" = nodedfallneighbors$Ego), FUN = sum, na.rm = TRUE)
names(sampalterlinkscount) <- c("Ego", "SampledAlterLkCount")


## collect count of number of alters that a person reports that were sampled (for unweighted)
temp <- aggregate(nodedfallneighbors[,c("SampledAlter")], by=list("Ego" = nodedfallneighbors$Ego, "Alter" = nodedfallneighbors$Alter), FUN = mean, na.rm = TRUE)
names(temp) <- c("Ego", "Alter", "SampledAlter")
sampaltercount <- aggregate(temp[,c("SampledAlter")], by=list("Ego" = temp$Ego), FUN = sum, na.rm = TRUE)
names(sampaltercount) <- c("Ego", "SampledAlterCount")

## Now add these to the main dataframe:
nodedfallneighbors <- merge(nodedfallneighbors, sampalterlinkscount, by="Ego", all.x=T)
nodedfallneighbors <- merge(nodedfallneighbors, sampaltercount, by="Ego", all.x=T)

## Now create the main dataframes for analysis:
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

Social <- collapsedata(layers = c("Time", "Relig", "Pol", "Visit", "Secret", "Phone", "Meal"), sampled = FALSE, heard = FALSE, bylink = FALSE)
SocialSamp <- collapsedata(layers = c("Time", "Relig", "Pol", "Visit", "Secret", "Phone", "Meal"), sampled = TRUE, heard = FALSE, bylink = FALSE)
SocialSampHeard <- collapsedata(layers = c("Time", "Relig", "Pol", "Visit", "Secret", "Phone", "Meal"), sampled = TRUE, heard = TRUE, bylink = FALSE)

# run regression
model.list <- list("m1" = c("Neighborhood"), "m2" = c("NeighborhoodAlter"), "m3" = c("AvgDist"), "m4" = c("Eigen"), "m5" = c("Neighborhood", "AvgDist"), "m6" = c("Neighborhood", "Eigen"), "m7" = c("Neighborhood", "NeighborhoodAlter"), "m8" = c("UnweightedEigen"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste(x, collapse= " + "))), data = SocialSamp, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit, results$m8$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1, results$m7$mfxest[,1]), c(1,results$m8$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1, results$m7$mfxest[,2]), c(1, results$m8$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Hear the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits=3), round(rsqr$m7, digits=3), round(rsqr$m8, digits=3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between network centrality and hearing about the event",
                                #covariate.labels = c("Prop. Peers Who Heard", "DistSeed", "DistLeader", "AvgDist"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "central_hear_more", no.space = TRUE, out = "central_hear_more.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "central_hear_more.tex")

#################################################################################################################
# TABLE 18 (SI)
#################################################################################################################

# create list of models to run
model.list <- list("m1" = c("HearAlter", "Neighborhood", "DistSeed", "DistLeader", "AvgDist","Eigen", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), 
                   "m2" = c("HearAlter", "Neighborhood",  "DistSeed", "DistLeader", "AvgDist","Eigen","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",                                "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Hear", " ~ ", paste(x, collapse= " + "))), data = SocialSamp, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Hear About the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship bewteen peers who heard and hearing about the event, conditional on other network attributes and ego and alter demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "hear_demognw", no.space = TRUE, out = "hear_demognw.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "hear_demognw.tex")

#################################################################################################################
# TABLE 19 (SI)
#################################################################################################################

#Attend, demographic and network controls
# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood", "NeighborhoodAlter", "DistSeed", "DistLeader", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("AttendAlter", "Neighborhood", "NeighborhoodAlter", "DistSeed", "DistLeader","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                                               "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship bewteen peers who attended and attendance, conditional on other network attributes and ego and alter demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_demognw2", no.space = TRUE, out = "attend_demognw2.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_demognw2.tex")

#################################################################################################################
# TABLE 20 (SI)
#################################################################################################################

#Attend, demographic and network controls
# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood", "DistSeed", "DistLeader", "AvgDist","Eigen", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("AttendAlter", "Neighborhood", "DistSeed", "DistLeader", "AvgDist","Eigen","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                                             "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship bewteen peers who attended and attendance, conditional on other network attributes and ego and alter demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_demognw", no.space = TRUE, out = "attend_demognw.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_demognw.tex")

#################################################################################################################
# TABLE 21 (SI)
#################################################################################################################

## 1) Raw relationship between various calculations of eigenvector centrality and attending

model.list <- list("m1" = c("WeightedEigen"), "m2" = c("WeightedEigen", "SampledAlterLkCount"), "m3" = c("UnweightedEigen"), "m4" = c("UnweightedEigen","SampledAlterCount"), "m5" = c("WeightedEigenCl"), "m6" = c("UnweightedEigenCl"), "m7" = c("WeightedEigen", "AttendAlter"), "m8" = c("UnweightedEigen", "AttendAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit, results$m8$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1, results$m7$mfxest[,1]), c(1, results$m8$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1, results$m7$mfxest[,2]), c(1, results$m8$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits=3), round(rsqr$m7, digits=3), round(rsqr$m8, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Testing robustness to four different measurements of eigenvector centrality",
                                #covariate.labels = c("Prop. Peers Who Heard", "DistSeed", "DistLeader", "AvgDist"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: Network statistics calculated for the social network-- the undirected union of all seven social network tie types. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "centrality_rawcomp", no.space = TRUE, out = "centrality_rawcomp.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "centrality_rawcomp.tex")

#################################################################################################################
# FIGURE 4 (SI)
#################################################################################################################
library(corrplot)
############
## Some correlations.  

cors2 <- cor(as.matrix(SocialSamp[,c("WeightedEigen", "UnweightedEigen", "WeightedEigenCl", "UnweightedEigenCl", "Neighborhood", "SampledAlterLkCount", "SampledAlterCount")]))

rownames(cors2) <- c("Eigen", "Trimmed Eigen", "Eigen Cl.", "Trimmed Eig. Cl.", "Neighborhood", "Links Sampled", "Neighb. Sampled")
colnames(cors2) <- c("Eigen", "Trimmed Eigen", "Eigen Cl.", "Trimmed Eig. Cl.", "Neighborhood", "Links Sampled", "Neighb. Sampled")


corrplot(cors2, type="lower", tl.col="black", cl.lim=c(0,1))
## saved as eigen_cors2.pdf

##plot with hear and attend added (there's an NA in hear, so list-wise delete for simplicity)

cors1 <- cor(as.matrix(na.omit(SocialSamp[,c("WeightedEigen", "UnweightedEigen", "WeightedEigenCl", "UnweightedEigenCl", "Neighborhood", "SampledAlterLkCount", "SampledAlterCount")])))

rownames(cors1) <- c("Eigen", "Trimmed Eigen", "Eigen Cl.", "Trimmed Eig. Cl.", "Neighborhood", "Links Sampled", "Neighb. Sampled")
colnames(cors1) <- c("Eigen", "Trimmed Eigen", "Eigen Cl.", "Trimmed Eig. Cl.", "Neighborhood", "Links Sampled", "Neighb. Sampled")

corrplot(cors1, type="lower", tl.col="black", cl.lim=c(0,1))


#################################################################################################################
# FIGURE 5 (SI)
#################################################################################################################
## Plot with just sampledaltercount, unweighted, and attendance
cors3 <- cor(SocialSamp[,c("UnweightedEigen","SampledAlterCount",  "Attend")])
rownames(cors3) <- c("Trimmed Eigen.", "Neighb. Sampled", "Attend")
colnames(cors3) <- c("Trimmed Eigen.", "Neighb. Sampled", "Attend")
corrplot(cors3, type="lower", tl.col="black", cl.lim=c(-1,1))

cors4 <- cor(SocialSamp[,c("WeightedEigen","SampledAlterLkCount",  "Attend")])
rownames(cors4) <- c("Eigen.", "Links Sampled", "Attend")
colnames(cors4) <- c("Eigen.", "Links Sampled", "Attend")
corrplot(cors4, type="lower", tl.col="black", cl.lim=c(-1,1))

#################################################################################################################
# TABLE 22 (SI)
#################################################################################################################

#Attend, demographic and network controls
# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood", "UnweightedEigen",  "DistLeader", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("AttendAlter", "Neighborhood",  "UnweightedEigen", "DistLeader","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                                  "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship bewteen peers who attended and attendance, conditional on other network attributes and ego and alter demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_demognw6_unweighted", no.space = TRUE, out = "attend_demognw6_unweighted.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_demognw6_unweighted.tex")

#################################################################################################################
# TABLE 23 (SI)
#################################################################################################################

#Attend, demographic and network controls
# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood", "WeightedEigenCl",  "DistLeader", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("AttendAlter", "Neighborhood",  "WeightedEigenCl", "DistLeader","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                                  "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship bewteen peers who attended and attendance, conditional on other network attributes and ego and alter demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_demognw6_weightedclosed", no.space = TRUE, out = "attend_demognw6_weightedclosed.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_demognw6_weightedclosed.tex")

#################################################################################################################
# TABLE 24 (SI)
#################################################################################################################

#Attend, demographic and network controls
# create list of models to run
model.list <- list("m1" = c("AttendAlter", "Neighborhood", "UnweightedEigenCl",  "DistLeader", "Gender", "Age", "Catholic", "Educ", "Married", "WallMat"), "m2" = c("AttendAlter", "Neighborhood",  "UnweightedEigenCl", "DistLeader","Gender", "Age", "Catholic", "Educ", "Married", "WallMat",  
                                                                                                                                                                    "GenderAlter", "AgeAlter", "CatholicAlter", "EducAlter", "MarriedAlter","WallMatAlter"))
# run models and compute adjusted R-squared
results <- lapply(model.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(x, collapse= " + "))), data = SocialSampHeard, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = TRUE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship bewteen peers who attended and attendance, conditional on other network attributes and ego and alter demographic characteristics",
                                # covariate.labels = c("Prop. Peers Who Heard", "Distance to Seed", "Degree", "Multinetwork Links", "Gender", "Age", "Years of Schooling", "Married", "Housing Material (1 = cement, 0 = mud)", "Distance to Seed Alter", "Degree Alter", "Gender Alter", "Age Alter", "Years of Schooling Alter", "Married Alter", "Housing Material Alter (1 = cement, 0 = mud)"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                label = "attend_demognw6_closed", no.space = TRUE, out = "attend_demognw6_closed.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
out <- sub("\\\\begin\\{tabular\\}\\{\\@\\{\\\\extracolsep\\{5pt\\}\\}lcc\\} ", "\\\\begin\\{tabular\\}\\{l C\\{5cm\\} C\\{5cm\\}}", out)
#writeLines(out, "attend_demognw6_closed.tex")

#################################################################################################################
# TABLE 25 (SI)
#################################################################################################################

# create list of models to run
data.list <- list(TimeSampHeard, PhoneSampHeard, PolSampHeard, ReligSampHeard, MealSampHeard, VisitSampHeard, SecretSampHeard)
names(data.list) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7")
# run models and compute adjusted R-squared
results <- lapply(data.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(c("EigenLayer"), collapse= " + "))), data = x, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit,  coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1,results$m7$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1,results$m7$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = FALSE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits = 3), round(rsqr$m7, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between attending the event and eigenvector centrality in each network type",
                                covariate.labels = c("Eigenvector Centrality"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                column.labels = c("Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret"),
                                label = "eigen_disag", no.space = TRUE, out = "eigen_disag.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "eigen_disag.tex")

#################################################################################################################
# TABLE 26 (SI)
#################################################################################################################

TimeSampHeard$Neighb2Only <- TimeSampHeard$Neighb2Layer - TimeSampHeard$NeighborhoodLayer
PhoneSampHeard$Neighb2Only <- PhoneSampHeard$Neighb2Layer - PhoneSampHeard$NeighborhoodLayer
PolSampHeard$Neighb2Only <- PolSampHeard$Neighb2Layer - PolSampHeard$NeighborhoodLayer
ReligSampHeard$Neighb2Only <- ReligSampHeard$Neighb2Layer - ReligSampHeard$NeighborhoodLayer
MealSampHeard$Neighb2Only <- MealSampHeard$Neighb2Layer - MealSampHeard$NeighborhoodLayer
VisitSampHeard$Neighb2Only <- VisitSampHeard$Neighb2Layer - VisitSampHeard$NeighborhoodLayer
SecretSampHeard$Neighb2Only <- SecretSampHeard$Neighb2Layer - SecretSampHeard$NeighborhoodLayer

## Attending by type, eigenvector centrality and neighborhood size
data.list <- list(TimeSampHeard, PhoneSampHeard, PolSampHeard, ReligSampHeard, MealSampHeard, VisitSampHeard, SecretSampHeard)
names(data.list) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7")
# run models and compute adjusted R-squared
results <- lapply(data.list, function(x) logitmfx(as.formula(paste("Attend", " ~ ", paste(c( "NeighborhoodLayerAlter"), collapse= " + "))), data = x, atmean = TRUE, robust = TRUE))
rsqr <- lapply(results, function(x) (1-(x$fit$deviance/x$fit$null.deviance))*x$fit$df.null/x$fit$df.residual)
# output results to tex
# Note: when only one variable, given the constant has no marginal effect, we have to "fool" stargazer into believing there's a constant, hence the 1)
out <- capture.output(stargazer(results$m1$fit, results$m2$fit, results$m3$fit, results$m4$fit, results$m5$fit, results$m6$fit, results$m7$fit,  coef = list(c(1,results$m1$mfxest[,1]), c(1,results$m2$mfxest[,1]), c(1,results$m3$mfxest[,1]), c(1,results$m4$mfxest[,1]), c(1,results$m5$mfxest[,1]), c(1,results$m6$mfxest[,1]), c(1,results$m7$mfxest[,1])), 
                                se = list(c(1,results$m1$mfxest[,2]), c(1,results$m2$mfxest[,2]), c(1,results$m3$mfxest[,2]), c(1,results$m4$mfxest[,2]), c(1,results$m5$mfxest[,2]), c(1,results$m6$mfxest[,2]), c(1,results$m7$mfxest[,2])), 
                                digits = 3, dep.var.caption  = "P(Attend the Event)", dep.var.labels.include = FALSE, model.numbers = FALSE, table.placement = "htbp",
                                add.lines = list(c("Adj. R-Squared", round(rsqr$m1, digits = 3), round(rsqr$m2, digits = 3), round(rsqr$m3, digits = 3), round(rsqr$m4, digits = 3), round(rsqr$m5, digits = 3), round(rsqr$m6, digits = 3), round(rsqr$m7, digits = 3))), 
                                omit.stat = c("ll", "aic"), 
                                title = "Relationship between attending the event and eigenvector centrality in each network type",
                                #covariate.labels = c("Eigenvector Centrality", "Degree"),
                                omit = c("Constant"),
                                notes.label = "",
                                notes = "Note: The aggregate social network is the undirected combined network of all seven social subnetworks. Reported coefficients represent the marginal effects for the average observation.",
                                notes.append = TRUE,
                                notes.align = "l",
                                align = FALSE,
                                column.labels = c("Time", "Phone", "Politics", "Religion", "Meal", "Visit", "Secret"),
                                label = "alterdegree4", no.space = TRUE, out = "alterdegree4.tex"))
# add additional code in order for the note to be wrapped (prevents it from running out of the page)
out <- sub("\\{l\\}\\{Note\\:", "\\{p\\{\\\\linewidth\\}\\}\\{Note\\:", out)
# add additional code in order to adjust tabel to fit page
out <- sub("\\\\centering", "\\\\centering \\\\begin\\{adjustbox\\}\\{width=1\\\\textwidth\\}", out)
out <- sub("\\\\end\\{tabular\\}", "\\\\end\\{tabular\\} \\\\end\\{adjustbox\\}", out)
#writeLines(out, "alterdegree4.tex")



