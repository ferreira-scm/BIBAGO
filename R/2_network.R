library(qgraph)
library(glasso)
library(bootnet)
library(mgm)
library(igraph)

source("R/1_data_exploration.R")

net.df <- Perso
id <- net.df$ID
net.df$ID <- NULL
ncol(net.df) # number of nodes

l <- labels[which(labels$Label%in%names(net.df)),]

all(l$Label==names(net.df))

# reordering
l <- l[match(names(net.df), l$Label),]
    
all(l$Label==names(net.df))# santiy check

# adding test and dimension
Test <- l$Test_name[match(names(net.df), l$Label)]
Dimension <- l$Hypothesis.dimension[match(names(net.df), l$Label)]

# changing some names here
names(net.df) <- c("BIBAGO_chew_dur", "BIBAGO_expl_wob_dur", "BIBAGO_freez_dur", "BIBAGO_inter_voc_dur", "BIBAGO_voc_freq", "BIBAGO_choco_eaten", "NOT_expl_obj_dur", "NOT_inter_voc_dur", "OFT_jump_freq", "OFT_locom_dur", "OFT_voc_freq", "OFT_expl_dur", "HAT_expl_hum_lat", "HAT_tail_wagging_freq", "NPT_back_dur", "NPT_middle_dur", "NPT_sudden_display_dur", "NPT_tail_wagging_freq","NPT_turn_back_freq", "NPT_walking_by_fence_dur")


net.df <- scale(net.df)

############ first a simple network
results.5 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.5)

results.4 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.4)

results.3 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.3)

results.2 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.2)

results.25 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.25)

results.19 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="cor",
                tuning=0.19)

results.18 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.18)


results.15 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.15)

results.12 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="cor",
                tuning=0.12)


results.1 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="cor",
                tuning=0.1)



library(igraph)

adjm <- as.matrix(results.12$graph)
net.grph <- graph_from_adjacency_matrix(adjm, mode="undirected", weighted=T)
edgew <- E(net.grph)$weight
E(net.grph)$weight <- abs(E(net.grph)$weight)

V(net.grph)$Test <- gsub("_.*", "",  names(V(net.grph)))

V(net.grph)$Dimension <- Dimension

E.color.Uni = edgew
E.color.Uni = ifelse(E.color.Uni>0, "#00539C",ifelse(E.color.Uni<0, "#EEA47F","grey"))
E(net.grph)$color = as.character(E.color.Uni)

V(net.grph)$Shape <- "0"
V(net.grph)$Shape[V(net.grph)$Test=="OFT"] <- "square"
V(net.grph)$Shape[V(net.grph)$Test=="NOT"] <- "rectangle"
V(net.grph)$Shape[V(net.grph)$Test=="BIBAGO"] <- "circle"
V(net.grph)$Shape[V(net.grph)$Test=="HAT"] <- "sphere"
V(net.grph)$Shape[V(net.grph)$Test=="NPT"] <- "circle"

#change edge width
E(net.grph)$width = abs(edgew)*20

unique(V(net.grph)$Dimension)

V(net.grph)$color <- "white"
V(net.grph)$color[which(V(net.grph)$Dimension=="Activity")] <- "#99D3CF"
V(net.grph)$color[which(V(net.grph)$Dimension=="BAS")] <- "#ffc300"
V(net.grph)$color[which(V(net.grph)$Dimension=="BIS")] <- "#F93822FF"
V(net.grph)$color[which(V(net.grph)$Dimension=="Boldness")] <- "#be99ea"
V(net.grph)$color[which(V(net.grph)$Dimension=="Exploration")] <- "#5e8d5e"
V(net.grph)$color[which(V(net.grph)$Dimension=="Sociability")] <- "#ddb7ac"


levels(as.factor(V(net.grph)$Dimension))
col <- c("#99D3CF", "#ffc300","#F93822FF","#be99ea", "#5e8d5e", "#ddb7ac") #these need to match

#V(net.grph)$name <- gsub("OFT_", "", V(net.grph)$name)
#V(net.grph)$name <- gsub("NPT_", "", V(net.grph)$name)
#V(net.grph)$name <- gsub("HAT_", "", V(net.grph)$name)
#V(net.grph)$name <- gsub("_", " ", V(net.grph)$name)

pdf("fig/Network_12.pdf",
                width =8, height = 8)
set.seed(123)
plot(net.grph,layout=layout.fruchterman.reingold,
     vertex.label.color="black",
     vertex.size=5,
     vertex.shape=V(net.grph)$Shape,
     vertex.label.cex = 0.8,
     vertex.label.dist = 0.2,
     repel=TRUE)
#     vertex.label=V(net.grph)$Test)
legend(x=-1, y=1, legend=levels(as.factor(V(net.grph)$Dimension)), col=col, bty="n",x.intersp=0.25,text.width=0.045, pch=20, pt.cex=1.5)
dev.off()

net.grph

oc <- cluster_walktrap(net.grph)

pdf("fig/Network_12_cluster.pdf",
                width =8, height = 8)
set.seed(123)
plot(oc, net.grph, layout=layout.fruchterman.reingold,
     vertex.label.color="black",
     vertex.size=5,
     vertex.shape=V(net.grph)$Shape,
     vertex.label.cex = 0.8,
     vertex.label.dist = 0.2,
     repel=TRUE)
#     vertex.label=V(net.grph)$Test)
legend(x=-1, y=1, legend=levels(as.factor(V(net.grph)$Dimension)), col=col, bty="n",x.intersp=0.25,text.width=0.045, pch=20, pt.cex=1.5)
dev.off()

print(modularity(oc))

head(centrality(results.12))

centralityPlot(results.12, include = c("Degree","Strength","Betweenness", "Closeness"))

bootnet_case_dropping <- bootnet(results.12,
                                       nBoots = 2500,
                                       type = "case",
                                       nCores = 10,
                                       statistics = c('strength',
                                                      'expectedInfluence',
                                                      'betweenness',
                                                      'closeness'))

plot(bootnet_case_dropping, 'all')

