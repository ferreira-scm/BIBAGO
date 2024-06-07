library(psych)
library(GPArotation)
library(ggcorrplot)

source("R/1_data_exploration.R")

rownames(Perso) <- Perso$ID
Perso$ID <- NULL

l <- labels[which(labels$Label%in%names(Perso)),]
# reordering
l <- l[match(names(Perso), l$Label),]
all(l$Label==names(Perso))# santiy check

#### based on the network, we now reclassify voc_freq as BIS
l$Hypothesis.dimension[l$Label=="OFT_voc_freq"] <- "Boldness"
l$Hypothesis.dimension[l$Label=="BIBAGO_voc_freq"] <- "BIS"
l$Hypothesis.dimension[l$Label=="NPT_sudden_display_dur"] <- "BAS"

BisBas <- Perso[,names(Perso)%in%l$Label[l$Hypothesis.dimension%in%c("BAS", "BIS")]]

# scale and do the correlation matrix
BisBas.s<-scale(BisBas)
corr_BisBas <- cor(BisBas.s)
#ggcorrplot(BisBas.s)

cortest.mat(corr_BisBas,n1=80)
cortest.bartlett(corr_BisBas,n=80)
KMO(corr_BisBas)

fa.parallel(corr_BisBas, n.obs=80)
# 2

BisBas_sol <- fa(corr_BisBas, nfactors=2, rotate="oblimin", fm="pa")

BisBas_sol

BisBas_scores <- factor.scores(BisBas.s, BisBas_sol)

print(BisBas_sol$loadings,cutoff = 0.3)

fa.diagram(BisBas_sol)

### now with all the other dimensions
Dimensions <- Perso[,!names(Perso)%in%l$Label[l$Hypothesis.dimension%in%c("BAS", "BIS")]]

# scale and do the correlation matrix
Dimensions.s<-scale(Dimensions)
corr_Dimensions <- cor(Dimensions.s)
#ggcorrplot(BisBas.s)

cortest.mat(corr_Dimensions,n1=80)
cortest.bartlett(corr_Dimensions,n=80)
KMO(corr_Dimensions)

fa.parallel(corr_Dimensions, n.obs=80)
# 3

Dimensions_sol <- fa(corr_Dimensions, nfactors=4, rotate="oblimin", fm="pa")

Dimensions_sol

ggcorrplot(cor(scores.dim),
           method = "circle",
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)


##### all together
# exclude sudden display dur

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

Perso_sol <- fa(corr_P, nfactors=4, rotate="oblimin", fm="pa")

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
