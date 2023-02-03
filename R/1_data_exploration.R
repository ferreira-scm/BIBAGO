# loading datasets
lat.df <- read.csv("data/laterality_dataset.csv")
per.df <- read.csv("data/personality_dataset.csv")

names(lat.df)
names(per.df)

per.df$PVC_Nr[which(!unique(lat.df$PVC_Nr)%in%unique(per.df$PVC_Nr))]# these piggies had only the personality experiments

new_perso <- per.df[,c("chew_dur", "chew_freq", "expl_wob_freq", "expl_wob_dur", "freez_dur", "freez_freq", "BIBAGO_inter_voc_dur", "BIBAGO_voc_freq", "expl_wob_lat", "no_expl_wob_dur", "no_expl_wob_freq", "wob_open")]

names(new_perso)

names(per.df)

nrow(lat.df)

nrow(per.df)

library(vegan)

lat.df

### First we need to imput

library("mice")
# checking the missing data per variable and per sample
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(latI,2,pMiss) # few variables have 6% misssing data.
apply(latI,1,pMiss)

apply(perI,2,pMiss)
apply(perI,1,pMiss)

mice::md.pattern(latI)

mice::md.pattern(perI)

library(VIM)

pdf("fig/Missing_val_Lat.pdf")
aggr(latI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(latI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()
pdf("fig/Missing_val_Per.pdf")
aggr(perI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(perI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()

# we remove the variables expl_dur expl_freq, because of too many missing values
perI[,c("expl_dur", "expl_freq")] <- NULL


latI_temp <- mice(latI,m=5,maxit=50,meth='pmm',seed=500, seed=1234)
summary(latI)
PerI_temp <- mice(perI,m=5,maxit=50,meth='pmm',seed=500, seed=1234)
summary(latI)


latI <- complete(latI_temp,1)

perI <- complete(PerI_temp,1)

pdf("fig/LatInputed_distr.pdf")
densityplot(latI_temp)
dev.off()
pdf("fig/PerInputed_distr.pdf")
densityplot(PerI_temp)
dev.off()



## question 1: are personality behaviours consistent over time?

Perso_beha <- perI[,c("chew_dur", "chew_freq", "expl_wob_freq", "expl_wob_dur", "freez_dur", "freez_freq", "BIBAGO_inter_voc_dur", "BIBAGO_voc_freq", "expl_wob_lat", "no_expl_wob_dur", "no_expl_wob_freq", "wob_open")]

library(vegan)

per_dis <- vegdist(Perso_beha, method="bray")

pcoa <- cmdscale(per_dis)

plot(pcoa)

NMDS <- metaMDS(per_dis, method="bray", k=2, trymax=500)

plot(NMDS)

ordiplot(pcoa, type="n")

perI$Test_Nr

#orditorp(NMDS, display="species", col="red", air=0.01)
ordihull(pcoa,groups=perI$Test_Nr,draw="polygon",col="grey90",label=F)
orditorp(pcoa, display="sites")

adonis2(per_dis~ perI$Test_Nr+
            perI$Sow+
            perI$Batch) # no difference of test?


distance
