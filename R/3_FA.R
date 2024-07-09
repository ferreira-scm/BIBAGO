library(psych)
library(GPArotation)
library(ggcorrplot)

source("R/1_data_exploration.R")

rownames(Perso) <- Perso$ID
Perso$ID <- NULL


##### all together
# exclude sudden display dur, low KMO, doesn't cluster in the network
Perso$NPT_sudden_display_dur <- NULL

# scale and do the correlation matrix
Perso.s<-scale(Perso)
corr_P <- cor(Perso.s)
#ggcorrplot(BisBas.s)

cortest.mat(corr_P,n1=80)
cortest.bartlett(corr_P,n=80)
KMO(corr_P)

fa.parallel(corr_P, n.obs=80, fm="pa", fa="fa")
# 3-5

library(nFactors)

plot(nScree(x=corr_P, model="factors"))

Perso_sol <- fa(corr_P, nfactors=5, rotate="oblimin", fm="pa", max.iter = 100)

#Perso_dat <- fa(r=Perso.s, nfactors=5, rotate="oblimin", fm="pa", scores="regression")

#pairs.panels(Perso_dat$scores)


print(Perso_sol$loadings,cutoff = 0.3)

fa.diagram(Perso_sol)

Perso_sol$uniquenesses

Perso_scores <- factor.scores(Perso.s, Perso_sol)

Perso_scores <- as.data.frame(Perso_scores$scores)

scores.dim <- data.frame(BAS=Perso_scores$PA1, BIS=Perso_scores$PA2, Asoc=Perso_scores$PA3, Shy=Perso_scores$PA4)

head(scores.dim)


Perso_sol

################### only BIBAGO
#fa.congruence(Perso_sol, Perso_dat)


l <- labels[which(labels$Label%in%names(Perso)),]
# reordering
l <- l[match(names(Perso), l$Label),]
all(l$Label==names(Perso))# santiy check


#### based on the network, we now reclassify voc_freq as BIS
l$Hypothesis.dimension[l$Label=="OFT_voc_freq"] <- "Boldness"
l$Hypothesis.dimension[l$Label=="BIBAGO_voc_freq"] <- "BIS"
#l$Hypothesis.dimension[l$Label=="NPT_sudden_display_dur"] <- "BAS"


colnames(corr_P)

set.seed(42)
d <- sim.item(12)


R <- cor(d)
Ro <- corr_P[c(1,2,3,4,5,6,8,14, 17),c(1,2,3,4,5,6,8,14, 17)]
Roe <- corr_P[c(1,2,3,4,5,6,8,14, 17),c(7, 9:13, 15,16, 18,19)]

fo <- fa(Ro, nfactors=2, rotate="oblimin", fm="pa")
fe <- fa.extension(Roe, fo)

fe

pdf("fig/FA.pdf", width=10, height=12)
fa.diagram(fo, fe=fe)
dev.off()

fo

#ba5 <- bassAckward(Ro, nfactors =c(2,3,4,5),plot=FALSE)
#baf <- bassAckward.diagram(ba5)

print(fo$loadings, cutoff = 0.3)

BisBas <- Perso.s[,names(Perso)%in%l$Label[l$Hypothesis.dimension%in%c("BAS", "BIS")]]

DimP <- Perso.s[,!names(Perso)%in%l$Label[l$Hypothesis.dimension%in%c("BAS", "BIS")]]

# scale and do the correlation matrix
BisBas.s<-scale(BisBas)

corr_BisBas <- cor(BisBas)
#ggcorrplot(BisBas.s)

cortest.mat(corr_BisBas,n1=80)

cortest.bartlett(corr_BisBas,n=80)
KMO(corr_BisBas)

fa.parallel(corr_BisBas, n.obs=80)
# 2

BisBas_sol <- fa(corr_BisBas, nfactors=2, rotate="oblimin", fm="pa")

fe <- fa(corr_BisBas, nfactors=2, rotate="oblimin", fm="pa")

BisBas_sol

BisBas_scores <- factor.scores(BisBas.s, BisBas_sol)

fa.extension(DimP, BisBas_sol,correct=TRUE)

print(BisBas_sol$loadings,cutoff = 0.3)

fa.diagram(BisBas_sol)
