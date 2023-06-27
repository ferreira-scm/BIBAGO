library("mice")
library(vegan)
library(VIM)
library("GUniFrac", lib="/usr/local/lib/R/site-library")
library(reshape)
library(ggplot2, )

# loading datasets
lat.df <- read.csv("data/laterality_dataset.csv")
per1.df <- read.csv("data/perso_datasets.csv")
per2.df <- read.csv("data/HAT_NPT_dataset.csv")
per3.df <- read.csv("data/outputs_to_be_imported_OFT_expl_corrected.csv", sep=";")

####cleaning
# removing these columns that are in per3.df
per1.df <- per1.df[,!names(per1.df)%in%c("expl_dur", "expl_freq", "no_expl_dur", "no_expl_freq")]

per2.df <- per2.df[!is.na(per2.df$Test_Nr),]
per1.df$unique <-paste("B", per1.df$Batch, "S", per1.df$Subject, "_T", per1.df$Test_Nr, sep="")
per2.df$unique <-paste("B", per2.df$Batch, "S", per2.df$Subject, "_T", per2.df$Test_Nr, sep="")
per3.df$unique <- paste("B", per3.df$Batch, "S", per3.df$Subject, "_T", per3.df$Test, sep="")
lat.df$unique <- paste("B", lat.df$Batch, "S", lat.df$Subject, "_T", lat.df$Test, sep="")


per1.df$ID <-paste("B", per1.df$Batch, "S", per1.df$Subject, sep="")
per2.df$ID <-paste("B", per2.df$Batch, "S", per2.df$Subject, sep="")
per3.df$ID <- paste("B", per3.df$Batch, "S", per3.df$Subject, sep="")
lat.df$ID <- paste("B", lat.df$Batch, "S", lat.df$Subject,  sep="")

# sanity check
setdiff(per2.df$unique, per1.df$unique) #
setdiff(per1.df$unique, per3.df$unique) #
setdiff(per2.df$unique, per3.df$unique) # 

# mergind datasets
per2.df <-per2.df[, !names(per2.df)%in%c("PVC_Nr", "Batch", "Subject", "Sow", "Test_Nr")]
per.df <- merge(per1.df, per2.df, by="unique")

#cleaning
per.df$ID <- per.df$ID.x
per.df$ID.x <- NULL
per.df$ID.y <- NULL
#merging
per3.df <-per3.df[, !names(per3.df)%in%c("Batch", "Subject", "Test", "ID")]
per.df <- merge(per.df, per3.df, by="unique")

labels <- read.csv("data/Labels.csv") # varialble and test labels
names(lat.df)%in%labels$Label # sanity check
names(per.df)%in%labels$Label

# adding unique and ID as important
uni <- data.frame(Importance=c(1,1), Test_name=c(NA, NA), Hypothesis.dimension=c(NA, NA), Label=c("unique", "ID"))
labels <- rbind(labels, uni)
labels <- labels[labels$Importance==1,] # we only analyse the important stuff
per.df <- per.df[,names(per.df)%in%labels$Label]# subseting the important variables
lat.df <-lat.df[,names(lat.df)%in%labels$Label]

### First we need to imput
# checking the missing data per variable and per sample
latI <- lat.df
perI <- per.df
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(latI,2,pMiss) # few variables have 6% misssing data.
#apply(latI,1,pMiss)
apply(perI,2,pMiss)
apply(perI,1,pMiss)
mice::md.pattern(latI)
mice::md.pattern(perI)
#aggr(latI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(latI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#aggr(perI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(perI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# we remove the variables that are more than 6% missing
perI[,names(perI)[apply(perI,2,pMiss)>6]] <- NULL
perI[,names(latI)[apply(latI,2,pMiss)>6]] <- NULL


# this needs to be a factor for mice (character is not recognizing na's)
latI$bias_f <- as.factor(latI$bias_f)
latI$bias_t <- as.factor(latI$bias_t)
latI$bias_w <- as.factor(latI$bias_w)

latI_temp <- mice(latI,m=5,maxit=50,meth='pmm',seed=500)
PerI_temp <- mice(perI,m=5,maxit=50,meth='pmm',seed=500)

perI <- complete(PerI_temp,1)
latI <- complete(latI_temp,1)

perI$Sow <- as.factor(perI$Sow)
perI$Batch <- as.factor(perI$Batch)
perI$PVC_Nr <- as.factor(perI$PVC_Nr)

## question 1: are personality behaviours consistent over time?
Perso_BIBAGO <- perI[,names(perI)%in%labels$Label[labels$Test_name%in%"BIBAGO"]]
Perso_NOT <- perI[,names(perI)%in%labels$Label[labels$Test_name%in%"NOT"]]
Perso_OFT <- perI[,names(perI)%in%labels$Label[labels$Test_name%in%"OFT"]]
Perso_HAT <-perI[,names(perI)%in%labels$Label[labels$Test_name%in%"HAT"]]

names(Perso_BIBAGO)
names(Perso_NOT)
names(Perso_OFT)
names(Perso_HAT)

#### different personality tests: bibago, open field, etc.
## seel it as a good one (bibago)
library(vegan)
bib_dis <- vegdist(Perso_BIBAGO, method="bray")
not_dis <- vegdist(Perso_NOT, method="bray")
oft_dis <- vegdist(Perso_OFT, method="bray")
hat_dis <- vegdist(Perso_HAT, method="bray")

bib_dis2 <- vegdist(Perso_BIBAGO, method="aitchison", pseudocount=1)
not_dis2 <- vegdist(Perso_NOT, method="aitchison", pseudocount=1)
oft_dis2 <- vegdist(Perso_OFT, method="aitchison", pseudocount=1)
hat_dis2 <- vegdist(Perso_HAT, method="aitchison", pseudocount=1)

bib_dis3 <- vegdist(Perso_BIBAGO, method="chisq")
not_dis3 <- vegdist(Perso_NOT, method="chisq")
oft_dis3 <- vegdist(Perso_OFT, method="chisq")
hat_dis3 <- vegdist(Perso_HAT, method="chisq")

# transforming to be closer to normality
bib_clr <- decostand(Perso_BIBAGO, method="clr", pseudocount=1)

# visualise correlation matrix
m <- cor(Perso_BIBAGO, method="pearson")
m <- melt(m)
names(m) <- c("X", "Y", "value")
ggplot2::ggplot(data=m, aes(x=X, y=Y, fill=value))+
    geom_tile()+
    geom_text(aes(label = round(value,2)), color = "white", size = 4)+
     scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))

# plotting distributions
bib <- melt(Perso_BIBAGO)
ggplot(data=bib, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales="free")

bib.clr <- melt(bib_clr)
ggplot(data=bib.clr, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales="free")

# can't use PCA's without transformation, data is skewed
bib_pcoa <- cmdscale(bib_dis2)
not_pcoa <- cmdscale(not_dis2)
oft_pcoa <- cmdscale(oft_dis2)
hat_pcoa <- cmdscale(hat_dis2)


bib_clr$PCoA1 <- bib_pcoa[,1]
bib_clr$PCoA2 <- bib_pcoa[,2]
bib_clr$ID <- perI$ID
bib_clr$Sow <- perI$Sow
bib_clr$Time <- as.factor(perI$Test_Nr)

Perso_NOT$ID <- perI$ID
Perso_NOT$PCoA1 <- not_pcoa[,1]
Perso_NOT$PCoA2 <- not_pcoa[,2]
Perso_NOT$Time <- as.factor(perI$Test_Nr)

Perso_OFT$ID <- perI$ID
Perso_OFT$PCoA1 <- oft_pcoa[,1]
Perso_OFT$PCoA2 <- oft_pcoa[,2]
Perso_OFT$Time <- as.factor(perI$Test_Nr)

Perso_HAT$ID <- perI$ID
Perso_HAT$PCoA1 <- hat_pcoa[,1]
Perso_HAT$PCoA2 <- hat_pcoa[,2]
Perso_HAT$Time <- as.factor(perI$Test_Nr)

library(rptR)
    
bib_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=bib_clr, datatype="Gaussian", nboot=1000)
#rpt(PCoA1~(1|ID), grname="ID", data=bib_clr, datatype="Gaussian", nboot=0, npermut=1000)
bib_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=bib_clr, datatype="Gaussian", nboot=1000)
#bib_rpt1.s <- rpt(PCoA1~(1|ID)+(1|Sow), grname=c("ID", "Sow"), data=bib_clr, datatype="Gaussian", nboot=1000) # sow explains a small variation given ID.
#summary(bib_rpt1.s)

not_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_NOT, datatype="Gaussian", nboot=1000)
not_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_NOT, datatype="Gaussian", nboot=1000)

oft_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_OFT, datatype="Gaussian", nboot=1000)
oft_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_OFT, datatype="Gaussian", nboot=1000)

hat_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_HAT, datatype="Gaussian", nboot=1000)
hat_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_HAT, datatype="Gaussian", nboot=1000)

summary(bib_rpt1)
summary(bib_rpt2)

summary(not_rpt1)
summary(not_rpt2)

summary(oft_rpt1)
summary(oft_rpt2)

summary(hat_rpt1)
summary(hat_rpt2)

time_palette <- c("#00203FFF", "#ADEFD1FF")


bib_mds <- ggplot(bib_clr, aes(x=PCoA1, y=PCoA2, fill=Time))+
    geom_point(shape=21, aes(fill=Time), size=4, stroke=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point") +
    scale_fill_manual(values = time_palette) +
    theme_bw(base_size=12)+
    theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(1.2, "lines"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold")
          )

not_mds <- ggplot(Perso_NOT, aes(x=PCoA1, y=PCoA2, fill=Time))+
    geom_point(shape=21, aes(fill=Time), size=4, stroke=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point") +
    scale_fill_manual(values = time_palette) +
    theme_bw(base_size=12)+
    theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(1.2, "lines"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold")
          )

oft_mds <- ggplot(Perso_OFT, aes(x=PCoA1, y=PCoA2, fill=Time))+
    geom_point(shape=21, aes(fill=Time), size=4, stroke=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point") +
    scale_fill_manual(values = time_palette) +
    theme_bw(base_size=12)+
    theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(1.2, "lines"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold")
          )
hat_mds <- ggplot(Perso_HAT, aes(x=PCoA1, y=PCoA2, fill=Time))+
    geom_point(shape=21, aes(fill=Time), size=4, stroke=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point") +
    scale_fill_manual(values = time_palette) +
    theme_bw(base_size=12)+
    theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(1.2, "lines"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold")
          )

library(cowplot)

Figure1 <- plot_grid(bib_mds, not_mds, hat_mds, oft_mds, labels="auto", nrow=2)

legend <- get_legend(bib_mds+  theme(legend.position="top"))

Figure1 <- plot_grid(legend, Figure1, nrow=2,  rel_heights=c(0.05,1))

ggsave("fig/Figure1.pdf", Figure1, width = 170, height = 170, dpi = 300, units="mm")

bib_NMDS <- metaMDS(bib_dis, method="bray", k=2, trymax=500)
not_NMDS <- metaMDS(not_dis, method="bray", k=2, trymax=500)
oft_NMDS <- metaMDS(oft_dis, method="bray", k=2, trymax=500)
hat_NMDS <- metaMDS(hat_dis, method="bray", k=2, trymax=500)
#plot(bib_NMDS)
#plot(oft_NMDS)
#plot(not_NMDS)

ordiplot(bib_pcoa, type="n")
ordihull(bib_pcoa,groups=perI$Test_Nr,draw="polygon",col="grey90",label=F)
orditorp(bib_pcoa, display="sites")

ordiplot(not_pcoa, type="n")
#orditorp(NMDS, display="species", col="red", air=0.01)
ordihull(not_pcoa,groups=perI$Test_Nr,draw="polygon",col="grey90",label=F)
orditorp(not_pcoa, display="sites")

ordiplot(oft_pcoa, type="n")
#orditorp(NMDS, display="species", col="red", air=0.01)
ordihull(oft_pcoa,groups=perI$Test_Nr,draw="polygon",col="grey90",label=F)
orditorp(oft_pcoa, display="sites")

ordiplot(hat_pcoa, type="n")
#orditorp(NMDS, display="species", col="red", air=0.01)
ordihull(hat_pcoa,groups=perI$Test_Nr,draw="polygon",col="grey90",label=F)
orditorp(hat_pcoa, display="sites")


# permanovas to test the effect of timepoint
adonis2(bib_dis2~
            perI$ID)

adonis2(bib_dis2~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        strata=perI$ID,
        by="margin") # no difference of test?

adonis2(not_dis2~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(not_dis2~
            perI$ID)

adonis2(hat_dis2~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(hat_dis2~
            perI$ID)

adonis2(oft_dis2~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(oft_dis2~
            perI$ID)



sd <- as.data.frame(model1$fit)

summary(sd$b_not)

################### using distance based ICC.
dICC(bib_dis, strata=perI$ID)
dICC(bib_dis2, strata=perI$ID)
dICC(bib_dis3, strata=perI$ID)

dICC(not_dis, strata=perI$ID)
dICC(not_dis2, strata=perI$ID)
dICC(not_dis3, strata=perI$ID)

dICC(oft_dis, strata=perI$ID)
dICC(oft_dis2, strata=perI$ID)
dICC(oft_dis3, strata=perI$ID)

dICC(hat_dis, strata=perI$ID)
dICC(hat_dis2, strata=perI$ID)
dICC(hat_dis3, strata=perI$ID)


### let's see if we have the same results for euclidean distances (pca)
bib_eu <- vegdist(Perso_BIBAGO, method="euclidean")
not_eu <- vegdist(Perso_NOT, method="euclidean")
oft_eu <- vegdist(Perso_OFT, method="euclidean")
hat_eu <- vegdist(Perso_HAT, method="euclidean")

adonis2(bib_eu~ perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(not_eu~ perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?
adonis2(oft_eu~ perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(hat_eu~ perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

## small differences. cool




#################
library(brms)

labels$Label[labels$Test_name%in%"BIBAGO"]
latI$PVC_Nr%in%perI$PVC_Nr

alldf <- merge(latI, perI, by="PVC_Nr")


M<-cor(alldf[,c("chew_dur", "expl_wob_dur", "expl_wob_freq", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq", "choco_eaten")])
corrplot(M, method = 'number')

M2<-cor(alldf[,c("chew_dur", "expl_wob_freq", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq")])

corrplot(M2, method = 'number')


# we have high colliniarity

# we remove expl_wob_dur, choco_eaten, and expl_wob_freq

## now we standardize

standardize <- function(x){
    (x - mean(x))/sd(x)
}

library(dplyr)

df <- alldf %>% mutate_at(c("chew_dur", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq"), ~(standardize(.) %>% as.vector))


df[, c("chew_dur", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq")]

#bform1 <- bf(mvbind(LI_w, LI_f, LI_t) ~chew_dur+ expl_wob_dur + expl_wob_freq + freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq + choco_eaten + (1|q|PVC_Nr)) + set_rescor(TRUE)# doesn't work

# ok let's do 3 univariate since our multivariate doesn't converge

fit_w <- brm(LI_w~chew_dur+ freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq+
                 (1|PVC_Nr),
             data = df,
             chains = 4,
             cores = 4,
             warmup=1000,
             iter=6000)

fit_w

fit_t <- brm(LI_t~chew_dur+ freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq+
                 (1|PVC_Nr),
             data = df,
             chains = 4,
             cores = 4,
             warmup=1000,
             iter=6000)


fit_f <- brm(LI_f~chew_dur+ freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq+
                 (1|PVC_Nr),
             data = df,
             chains = 4,
             cores = 4,
             warmup=1000,
             iter=6000)


fit_t


names(alldf)

install.packages("corrplot")

library(corrplot)


plot(alldf$expl_wob_freq, alldf$expl_wob_dur)

plot(alldf$chew_freq, alldf$chew_dur)

plot(alldf$BIBAGO_inter_voc_dur, alldf$BIBAGO_voc_freq)

plot(alldf$freez_freq, alldf$freez_dur)
    
+ set_rescor(TRUE)

fit1 <- brm(bform1, data = alldf, chains = 4, cores = 4, warmup=1000, iter=6000)

fit1

modelLAT<-brm(~1+ not+oft+
                (1|mm(IDA,IDB))+(1|sow)+(1|batch),
                data = data.dyad,
                family= "gaussian",
                warmup = 1000, iter = 3000,
                cores = 50, chains = 4,
                inits=0)


cor.test(alldf$LI_t, alldf$LI_f)


                                        # making key for the order of ID and sample
key <- data.frame(ID=paste(perI$PVC_Nr, perI$Test_Nr, sep="_"), ID2=perI$PVC_Nr)

bib_dis <- as.matrix(bib_dis)
rownames(bib_dis) <- key$ID
colnames(bib_dis) <- key$ID

dimnames(bib_dis)

not_dis <- as.matrix(not_dis)
rownames(not_dis) <- key$ID
colnames(not_dis) <- key$ID

oft_dis <- as.matrix(oft_dis)
rownames(oft_dis) <- key$ID
colnames(oft_dis) <- key$ID

bib_dis <- 1-bib_dis
not_dis <- 1-not_dis
oft_dis <- 1-oft_dis


Sow_frame <- perI[,c("Subject", "Sow")]
Sow_frame$Subject <- as.character(Sow_frame$Subject)
Sow_frame$Sow <- as.character(Sow_frame$Sow)
#Create an empty numeric matrix to fill with distances
SOWM<-array(0,c(nrow(Sow_frame),nrow(Sow_frame)))
#Derive matrix with binary Age similarity between each sample
for(i in 1:nrow(Sow_frame)){
    for(j in 1:nrow(Sow_frame)){
        if(Sow_frame$Sow[i]==Sow_frame$Sow[j]){
            SOWM[i,j]= 1
        } else{
            SOWM[i,j]= 0
        }
    }
}

#Note that AGE similarity matrix has rownames and colnames in the same order as key
all(rownames(SOWM)==key$ID)
rownames(SOWM)<-key$ID
colnames(SOWM)<-key$ID


Batch_frame <- perI[,c("Subject", "Batch")]
Batch_frame$Subject <- as.character(Batch_frame$Subject)
Batch_frame$Batch <- as.character(Batch_frame$Batch)
#Create an empty numeric matrix to fill with distances
BATM<-array(0,c(nrow(Batch_frame),nrow(Batch_frame)))
#Derive matrix with binary Batch similarity between each sample
for(i in 1:nrow(Batch_frame)){
    for(j in 1:nrow(Batch_frame)){
        if(Batch_frame$Batch[i]==Batch_frame$Batch[j]){
            BATM[i,j]= 1
        } else{
            BATM[i,j]= 0
        }
    }
}
#Note that batch similarity matrix has rownames and colnames in the same order as key
all(rownames(BATM)==key$ID)
rownames(BATM)<-key$ID
colnames(BATM)<-key$ID

######

BIB <- c(as.dist(bib_dis))
NOT <- c(as.dist(not_dis))
OFT <- c(as.dist(oft_dis))
SowM <- c(as.dist(SOWM))
BatchM <- c(as.dist(BATM))

data.dyad <- data.frame(bibago=BIB, not=NOT, oft=OFT, sow=SowM, batch=BatchM)

#Now all we need to do is add the identities of both individuals in each dyad as separate columns into the data frame and exclude self-comparisons (as these are not meaningful).
# extracting Individual-combinations present in the matrices
list<-expand.grid(key$ID,key$ID)

str(list)

# This created individual-to-same-individual pairs as well. Get rid of these:
list<-list[which(list$Var1!=list$Var2),]

# this still has both quantiles in--> add 'unique' key
list$key <- apply(list, 1, function(x)paste(sort(x), collapse='__'))
list<-subset(list, !duplicated(list$key))
# sanity check that the Individual name combinations are in the same exact order as the lower quantile value vector of the matrices


i=nrow(key)
    
bib_dis[which(rownames(bib_dis)==list$Var1[i]),which(colnames(bib_dis)==list$Var2[i])]==BIB[i]
# add the names of both individuals participating in each dyad into the data frame
data.dyad$IDA<-list$Var2
data.dyad$IDB<-list$Var1
# Make sure you have got rid of all self comparisons
data.dyad<-data.dyad[which(data.dyad$IDA!=data.dyad$IDB),]

names(data.dyad)

ggplot(data = data.dyad, aes(x=bibago , y= not))+
    geom_point(size= 1.2, alpha= .8, position= "jitter")+
    geom_smooth(method= lm, se= FALSE, col= "red", alpha= .8)+
    theme_bw()

ggplot(data = data.dyad, aes(x=bibago , y= sow))+
    geom_point(size= 1.2, alpha= .8, position= "jitter")+
    geom_smooth(method= lm, se= FALSE, col= "red", alpha= .8)+
    theme_bw()

#### This is our final model
model1<-brm(bibago~1+ not+oft+
                (1|mm(IDA,IDB))+(1|sow)+(1|batch),
                data = data.dyad,
                family= "gaussian",
                warmup = 1000, iter = 3000,
                cores = 50, chains = 4,
                inits=0)

saveRDS(model1, "tmp/BRMmodel1.rds")

model1 <- readRDS("tmp/BRMmodel1.rds")

model1

conditional_effects(model1)

bayes_R2(model1)

##### OK now 
############################# network


############################
library("BGGM")
library(ggplot2)

names(perI)

Perso_beha <- perI[,-c(1:4)]

Y <- Perso_beha+1



head(Y)

fit <- BGGM::estimate(Y, type="mixed", analytic=FALSE)



plot(select(fit))
