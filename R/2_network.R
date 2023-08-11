
library(qgraph)
library(glasso)
library(bootnet)
library(mgm)
library(igraph)

source("R/1_data_exploration.R")

net.df <- perA


names(net.df)


#net.df$bias_w <- NULL
#net.df$bias_f <- NULL
#net.df$bias_t <- NULL

net.df$Batch <- NULL
net.df$Sow <- NULL

id <- net.df$Group.1
net.df$Group.1 <- NULL


############ first a simple network
results.8 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.8)

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


results.15 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.15)

results.1 <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.1)





class(results.2)

names(net.df)

library(igraph)

plot(results.5,
     label.cex=2)

plot(results.4)

plot(results.3)


results.2$graph

plot(results.2)

adjm <- as.matrix(results.2$graph)

net.grph <- graph_from_adjacency_matrix(adjm, mode="undirected", weighted=T)

edgew <- E(net.grph)$weight

E(net.grph)$weight <- abs(E(net.grph)$weight)


l <- labels[which(labels$Label%in%names(V(net.grph))),]

all(l$Label==names(V(net.grph)))

# reordering
l <- l[match(names(V(net.grph)), l$Label),]
all(l$Label==names(V(net.grph)))# santiy check

# adding test and dimension
V(net.grph)$Test <- l$Test_name[match(names(V(net.grph)), l$Label)]
V(net.grph)$Dimension <- l$Hypothesis.dimension[match(names(V(net.grph)), l$Label)]

E.color.Uni = edgew
E.color.Uni = ifelse(E.color.Uni>0, "#488f31",ifelse(E.color.Uni<0, "#de425b","grey"))
E(net.grph)$color = as.character(E.color.Uni)

V(net.grph)$Test

l[match(names(V(net.grph)), l$Label),c(3,4)]

                                        #change edge width
E(net.grph)$width = abs(edgew)*20

V(net.grph)$Dimension

names(V(net.grph))


unique(V(net.grph)$Dimension)

V(net.grph)$color <- "white"
V(net.grph)$color[which(V(net.grph)$Dimension=="Activity")] <- "#99D3CF"
V(net.grph)$color[which(V(net.grph)$Dimension=="BAS")] <- "#ffc300"
V(net.grph)$color[which(V(net.grph)$Dimension=="BAS_Sociability")] <- "#ffe766"
V(net.grph)$color[which(V(net.grph)$Dimension=="BIS")] <- "#F93822FF"
V(net.grph)$color[which(V(net.grph)$Dimension=="Boldness")] <- "#be99ea"
V(net.grph)$color[which(V(net.grph)$Dimension=="Exploration")] <- "#5e8d5e"
V(net.grph)$color[which(V(net.grph)$Dimension=="Sociability")] <- "#ddb7ac"


levels(as.factor(V(net.grph)$Dimension))

col <- c("#99D3CF", "#ffc300", "#ffe766","#F93822FF","#be99ea", "#5e8d5e", "#ddb7ac")


pdf("fig/Network_tests_label_2.pdf",
                width =8, height = 8)


set.seed(122)
plot(net.grph, layout=layout.fruchterman.reingold,
     vertex.label.color="black",
     vertex.size=5,
     vertex.label.cex = 0.8,
     vertex.label.dist = 0.2,
     repel=TRUE)
#     vertex.label=V(net.grph)$Test)
legend(x=1, y=1, legend=levels(as.factor(V(net.grph)$Dimension)), col=col, bty="n",x.intersp=0.25,text.width=0.045, pch=20, pt.cex=1.5)


dev.off()

oc <- cluster_(net.grph)

oc <- cluster_walktrap(net.grph)



oc

print(modularity(oc))



centrality(results.2)



results.2

centralityPlot(results.2, include = c("Degree","Strength","Closeness","Betweenness"))

bootnet_case_dropping <- bootnet(results.5,
                                       nBoots = 2500,
                                       type = "case",
                                       nCores = 10,
                                       statistics = c('strength',
                                                      'expectedInfluence',
                                                      'betweenness',
                                                      'closeness'))

plot(bootnet_case_dropping, 'all')


dim(net.df)

str(net.df)

histogram(net.df$locom_freq)

net.df$Batch <- as.integer(net.df$Batch)
                                        #net.df$Sow <- as.integer(net.df$Sow)
net.df$Sow <- NULL # removing Sow for now because the model requires at least 2 events
net.df$bias_w <- as.integer(net.df$bias_w)
net.df$bias_t <- as.integer(net.df$bias_t)
net.df$bias_f <- as.integer(net.df$bias_f)

net.type <- c("c", "p", "g", "p", "p", "g", "p", "g", "g", "g", "p", "g", "p", "g", "p", "g", "g", "p", "p", "g", "p", "p", "g", "g", "p", "p", "g", "c", "g", "c", "g", "c")

net.level <- c(5, rep(1, 26), 3, 1, 2, 1, 3)

fit.net <- mgm(data=as.matrix(net.df), type=net.type, level=net.level, k=2, lambdaSel="EBIC", lambdaGam=0.25, pbar=FALSE)

qgraph(input=fit.net$pairwise$wadj,
       nodeName=colnames(net.df))

colnames(net.df)

fit.net2 <- mgm(data=as.matrix(net.df), type=net.type, level=net.level, k=2, lambdaSel="EBIC", lambdaGam=0.4, pbar=FALSE)

### network with the only tests

### network with just bibago and laterality

fit.net


summary(fit.net)

length(net.type)
length(net.level)


results <- estimateNetwork(


    fit.net
    
