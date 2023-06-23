library(qgraph)
library(glasso)

library(bootnet)

library(mgm)

install.packages("GUniFrac")

net.df <- perI

net.df <- net.df[net.df$Test_Nr==1,]

#latI <- latI[,!names(latI)%in%c("Subject", "Batch", "Sow")]

#net.df <- merge(net.df, latI, by="PVC_Nr")

names(net.df)

net.df$Subject <- NULL
net.df$Test_Nr <- NULL

#net.df$bias_w <- NULL
#net.df$bias_f <- NULL
#net.df$bias_t <- NULL

key <- net.df$PVC_Nr

net.df$PVC_Nr <- NULL

#str(net.df)

net.df$Batch <- NULL
net.df$Sow <- NULL

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


class(results.2)

names(net.df)

str(results.2)

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

names(V(net.grph))


V(net.grph)$Test <- c("OFT","OFT", "OFT", "OFT", "NOT", "NOT", "NOT", "NOT", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "BIBAGO", "HAT", "HAT","HAT","HAT","HAT","HAT","HAT")

V(net.grph)$Dimension <- c("Boldness","Activity","Activity", "Sociability", "Exploration", "Exploration", "Boldness", "Boldness", "BAS", "BAS", "BAS", "BAS", "BIS", "BIS", "BIS", "BAS", "BIS", "BAS", "Exploration", "Exploration", "Boldness", "Boldness", "", "", "Exploration")
                      

E.color.Uni = edgew
E.color.Uni = ifelse(E.color.Uni>0, "chartreuse4",ifelse(E.color.Uni<0, "coral","grey"))
E(net.grph)$color = as.character(E.color.Uni)


V(net.grph)$Test

                                        #change edge width
E(net.grph)$width = abs(edgew)*20

unique(V(net.grph)$Dimension)

V(net.grph)$color <- "white"
V(net.grph)$color[which(V(net.grph)$Dimension=="Boldness")] <- "deeppink3"
#V(net.grph)$color[which(V(net.grph)$Dimension=="Sociability/Boldness")] <- "deeppink4"
V(net.grph)$color[which(V(net.grph)$Dimension=="Activity")] <- "darkolivegreen"
V(net.grph)$color[which(V(net.grph)$Dimension=="Exploration")] <- "darkorange"
V(net.grph)$color[which(V(net.grph)$Dimension=="Sociability")] <- "darkolivegreen1"
#V(net.grph)$color[which(V(net.grph)$Dimension=="Sociability/Exploration")] <- "darkkhaki"
V(net.grph)$color[which(V(net.grph)$Dimension=="BAS")] <- "darkgoldenrod1"
V(net.grph)$color[which(V(net.grph)$Dimension=="BIS")] <- "darkslategray"

levels(as.factor(V(net.grph)$Dimension))

levels(as.factor(V(net.grph)$color))

col <- c("white","darkolivegreen", "darkgoldenrod1",  "darkslategray", "deeppink3", "darkorange", "darkolivegreen1")

pdf("fig/Network_tests.pdf",
                width =8, height = 8)
set.seed(123)
plot(net.grph, layout=layout.fruchterman.reingold,
     vertex.label.color="black",
     vertex.label=V(net.grph)$Test)
legend(x=0.5, y=1.2, legend=levels(as.factor(V(net.grph)$Dimension)), col=col, bty="n",x.intersp=0.25,text.width=0.045, pch=20, pt.cex=1.5)
dev.off()

pdf("fig/Network_tests_label.pdf",
                width =8, height = 8)
set.seed(123)
plot(net.grph, layout=layout.fruchterman.reingold,
     vertex.label.color="black")
#     vertex.label=V(net.grph)$Test)
legend(x=0.5, y=1.2, legend=levels(as.factor(V(net.grph)$Dimension)), col=col, bty="n",x.intersp=0.25,text.width=0.045, pch=20, pt.cex=1.5)
dev.off()

     vertex.label=

plot(results.2,
     label.cex=1)

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
    