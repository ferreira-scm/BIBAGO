library(psych)
library(GPArotation)
library(ggcorrplot)

source("R/1_data_exploration.R")

rownames(Perso) <- Perso$ID
Perso$ID <- NULL


# scale and do the correlation matrix
Perso.s<-scale(Perso)
corr_P <- cor(Perso.s)
#ggcorrplot(corr_P)

cortest.mat(corr_P,n1=80)
cortest.bartlett(corr_P,n=80)
KMO(corr_P)
## remove this one variable and calculate correlation matrix again
Perso$NPT_sudden_display_dur <- NULL
Perso.s<-scale(Perso)
corr_P <- cor(Perso.s)

################### extended factor analysis
l <- labels[which(labels$Label%in%names(Perso)),]

l

colnames(corr_P)

R <- cor(d)
Ro <- corr_P[c(1,2,3,4,5,6,8,14, 17),c(1,2,3,4,5,6,8,14, 17)]
Roe <- corr_P[c(1,2,3,4,5,6,8,14, 17),c(7, 9:13, 15,16, 18,19)]

fa.parallel(Ro, n.obs=80, fm="pa", fa="fa")

fo <- fa(Ro, nfactors=2, rotate="oblimin", fm="pa")
fe <- fa.extension(Roe, fo)

pdf("fig/FA.pdf", width=6, height=8)
fa.diagram(fo, fe=fe)
dev.off()

print(fo$loadings, cutoff = 0.3)

print(fe, digits=3)

### scores
BisBas_scores <- factor.scores(Perso.s[,c(1,2,3,4,5,6,8,14, 17)], fo)

BisBas_scores <- as.data.frame(BisBas_scores$scores)

head(BisBas_scores)


################### laterality testing

Perso.s <- as.data.frame(Perso.s)
Perso.s$ID <- as.factor(rownames(Perso.s))
Perso.s <- cbind(Perso.s, BisBas_scores)
latS <- merge(latI, Perso.s, by="ID")

#clustering based on laterality by Charlotte
# scaling
#latS<- latS %>% mutate_at(c("LI_w", "LI_f", "LI_t"), ~(scale(.) %>% as.vector))

km <- kmeans(latS[, c("LI_f", "LI_t")], centers=4, nstart=10)# we leave LI_w out because distribution is not binomial, therefore not a good indicator of laterality. We expect 4 clusters (2*2)

library(tidyverse)

centroids <- as_tibble(km$centers, rownames="cluster")

centroids# cluster 3 is RR and cluster 2 is LL

latS$km <- as.factor(km$cluster)

latK <- latS[latS$km%in%c("2", "3"),]

# just to make it pretty, I want a variable with 0 and 1
latK$Lat[latK$km==2] <- 0
latK$Lat[latK$km==3] <- 1

latK$Lat2[latK$km==3] <- "Right"
latK$Lat2[latK$km==2] <- "Left"


ggplot(latK, aes(LI_f, LI_t, colour=km))+geom_point()

#df <- df[, c("chew_dur", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq")]

##### compare only LL and RR #####
names(latK)

summary(as.factor(latK$Lat2))

#BIBLat <- glm(Lat~chew_dur+ freez_dur+ expl_wob_dur+BIBAGO_inter_voc_dur + BIBAGO_voc_freq+choco_eaten, data = latK, family=binomial)
#anova(BIBLat)
#summary(BIBLat)

library(brms)

library(scales)

latK$PA1_s <- rescale(latK$PA1)
latK$PA2_s <- rescale(latK$PA2)
latK$OFT_voc_freq_s <- rescale(latK$OFT_voc_freq)

latK$expl_obj_dur_s <- rescale(latK$expl_obj_dur)

names(latK)

Lat.m <- brm(Lat~PA1_s+PA2_s+OFT_voc_freq_s+expl_obj_dur_s, data = latK, family=bernoulli)

summary(Lat.m)

library(marginaleffects)

Char1 <- plot_predictions(Lat.m, condition="OFT_voc_freq_s")+
    theme_classic()


Char2 <- plot_predictions(Lat.m, condition="expl_obj_dur_s")+
    theme_classic()


Char3 <- plot_predictions(Lat.m, condition="PA2_s")+
    theme_classic()

Char4 <- plot_predictions(Lat.m, condition="PA1_s")+
    theme_classic()

library(cowplot)

Lat_Ten <- plot_grid(Char1, Char2, Char3, Char4)

ggsave("fig/Lat_tendencies.pdf", Lat_Ten, width = 180, height = 180, dpi = 300, units="mm")

       
BIBLatS <- glm(Lat~PA1+PA2, data = latK, family=binomial)
anova(BIBLatS)
summary(BIBLatS)

## Bonus
BIBBAS_OFT <- glm(Lat~PA1_s+PA2_s+OFT_voc_freq_s, data = latK, family=quasibinomial)

summary(BIBBAS_OFT)

anova(BIBBAS_OFT, test="LRT")


Fig3A <- ggplot(BisBas_scores, aes(PA1, PA2))+
    geom_point(size=4, alpha=0.6)+
    theme_classic()

Fig3B <- ggplot(latK, aes(Lat2, PA1, colour=Lat2))+
    geom_boxplot(outlier.shape = NA)+
    geom_jitter(width=0.2, height=NULL, size=4, alpha=0.6)+
    labs(x="Laterality", y="PA1-BAS/FFFs")+
    scale_colour_manual(values=c("#FFC107", "#004D40"))+
    guides(color = "none", size = "none")+
    theme_classic()


Fig3C <- ggplot(latK, aes(Lat2, PA2, colour=Lat2))+
    geom_boxplot(outlier.shape = NA)+
    geom_jitter(width=0.2, height=NULL, size=4, alpha=0.6)+
    labs(x="Laterality", y="PA2-BIS/FFFs")+
    scale_colour_manual(values=c("#FFC107", "#004D40"))+
    guides(color = "none", size = "none")+
    theme_classic()

library(cowplot)

BC <- cowplot::plot_grid(Fig3B, Fig3C, labels=c("B", "C"))

Fig3 <- cowplot::plot_grid(Fig3A, BC, ncol=1, labels=c("A", ""), rel_heights=c(2,1))
ggsave("fig/Figure3.pdf", Fig3, width = 180, height = 180, dpi = 300, units="mm")
