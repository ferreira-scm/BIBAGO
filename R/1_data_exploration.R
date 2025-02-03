library("mice")
library(vegan)
library(VIM)
library(reshape)
library(ggplot2)
library("GUniFrac", lib="/usr/local/lib/R/site-library")
library(rptR)

do_dataPrep <- FALSE
do_PERMANOVA <- FALSE
do_rptR <- FALSE

# cleaning labels
labels <- read.csv("data/Labels_21032024.csv") # varialble and test labels
labels[labels$Hypothesis.dimension%in%"BAS",]
labels[labels$Hypothesis.dimension%in%"BIS",]
labels[labels$Hypothesis.dimension%in%"Sociability",]
labels[labels$Hypothesis.dimension%in%"Exploration",]
labels[labels$Hypothesis.dimension%in%"Boldness",]

#some adjustments
labels$Importance[labels$Label%in%c("HAT_touching_not_visible_freq")] <- 0
labels$Importance[labels$Label=="HAT_pig_not_visible_dur"] <- 0
labels$Importance[labels$Label%in%c("NPT_facing_fence_dur")] <- 0
labels$Importance[labels$Label%in%c("NPT_facing_fence_freq")] <- 0

#labels[which(labels$Label%in%"NPT_latency_nose_nose_dur"),]
uni <- data.frame(Importance=c(1,1), Test_name=c(NA, NA), Hypothesis.dimension=c(NA, NA), Label=c("unique", "ID"))
labels <- rbind(labels, uni)
labels <- labels[labels$Importance==1,] # we only analyse the important stuff

if(do_dataPrep){

# loading datasets
lat.df <- read.csv("data/laterality_dataset.csv")
per1.df <- read.csv("data/perso_datasets.csv")
per2.df <- read.csv("data/HAT_NPT_dataset.csv")
per3.df <- read.csv("data/outputs_to_be_imported_OFT_expl_corrected.csv", sep=";")
per4.df <- read.csv("data/NPT_full_dataset.csv")

####cleaning
per4.df <- per4.df[!is.na(per4.df$Test_Nr),]

lat.df$bias_f <- NULL
lat.df$bias_w <- NULL
lat.df$bias_t <- NULL

# removing these columns that are in per3.df
per1.df <- per1.df[,!names(per1.df)%in%c("expl_dur", "expl_freq", "no_expl_dur", "no_expl_freq")]
per2.df <- per2.df[!is.na(per2.df$Test_Nr),]
per1.df$unique <-paste("B", per1.df$Batch, "S", per1.df$Subject, "_T", per1.df$Test_Nr, sep="")
per2.df$unique <-paste("B", per2.df$Batch, "S", per2.df$Subject, "_T", per2.df$Test_Nr, sep="")
per3.df$unique <- paste("B", per3.df$Batch, "S", per3.df$Subject, "_T", per3.df$Test, sep="")
per4.df$unique <- paste("B", per4.df$Batch, "S", per4.df$Subject, "_T", per4.df$Test_Nr, sep="")

#lat.df$unique <- paste("B", lat.df$Batch, "S", lat.df$Subject, "_T2", sep="")

per1.df$ID <-paste("B", per1.df$Batch, "S", per1.df$Subject, sep="")
per2.df$ID <-paste("B", per2.df$Batch, "S", per2.df$Subject, sep="")
per3.df$ID <- paste("B", per3.df$Batch, "S", per3.df$Subject, sep="")
per4.df$ID <- paste("B", per4.df$Batch, "S", per4.df$Subject, sep="")
lat.df$ID <- paste("B", lat.df$Batch, "S", lat.df$Subject,  sep="")

# sanity check
setdiff(per2.df$unique, per1.df$unique) #
setdiff(per1.df$unique, per3.df$unique) #
setdiff(per2.df$unique, per3.df$unique) #
setdiff(per4.df$unique, per3.df$unique) # 

# mergind datasets
per2.df <-per2.df[, !names(per2.df)%in%c("PVC_Nr", "Batch", "Subject", "Sow", "Test_Nr", "ID")]
per.df <- merge(per1.df, per2.df, by="unique")
per3.df <-per3.df[, !names(per3.df)%in%c("PVC_Nr", "Batch", "Subject", "Sow", "Test_Nr", "ID")]
per.df <- merge(per.df, per3.df, by="unique")
    
per4.df <-per4.df[, !names(per4.df)%in%c("PVC_Nr", "Batch", "Subject", "Sow", "Test_Nr", "ID")]
per.df <- merge(per.df, per4.df, by="unique")
per.df <- per.df[,!names(per.df)%in%names(per.df)[grep("\\.x", names(per.df))]]
names(per.df) <- gsub("\\.y", "", names(per.df))

#cleaning
#per.df$ID <- per.df$ID.x
#per.df$ID.x <- NULL
#per.df$ID.y <- NULL

#merging
#per3.df <-per3.df[, !names(per3.df)%in%c("Batch", "Subject", "Test", "ID")]
#per.df <- merge(per.df, per3.df, by="unique")

names(lat.df)%in%labels$Label # sanity check
names(per.df)%in%labels$Label
per.df <- per.df[,names(per.df)%in%labels$Label]# subseting the important variables
lat.df <-lat.df[,names(lat.df)%in%labels$Label]

############################ NPT batch 1 test 1 is missing.
### First we need to imput
# checking the missing data per variable and per sample
latI <- lat.df
perI <- per.df
pMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(latI,2,pMiss) # few variables have 10% misssing data.
apply(latI,1,pMiss)
apply(perI,2,pMiss) # the npt have 12.5% missing data, but it's not random. We won't impute those
apply(perI,1,pMiss)

#mice::md.pattern(latI)
#mice::md.pattern(perI)
#aggr(latI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(latI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#aggr(perI, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(perI), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# we remove the variables that are more than 10% missing, those are all NPT.
perI[,names(perI)[apply(perI,2,pMiss)>10]] <- NULL
latI[,names(latI)[apply(latI,2,pMiss)>10]]  <- NULL

# now we save
latI_temp <- mice(latI,m=10,maxit=50,meth='pmm',seed=500)
PerI_temp <- mice(perI,m=10,maxit=50,meth='pmm',seed=500)
perI <- complete(PerI_temp,1)
latI <- complete(latI_temp,1)

# character variables as factors
perI$Sow <- as.factor(perI$Sow)
perI$Batch <- as.factor(perI$Batch)
perI$PVC_Nr <- as.factor(perI$PVC_Nr)

# we need a complete dataset with NPT na's
perC <-merge(perI, per.df[,names(per.df)%in% c(labels$Label[labels$Test_name%in%"NPT"], "unique")], by="unique")
perC <- na.omit(perC)

# visualise correlation matrix
    m <- cor(perC[,c(7:24, 26:52)], method="spearman")
    m <- cor(perC[,c(7:20, 23:39)], method="spearman")
m <- melt(m)
names(m) <- c("X", "Y", "value")

not_cor <- ggplot(data = m, aes(x = X, y = Y, fill = value)) +
    geom_tile(color = "white", size = 0.2) +  # Add a white border around each tile
    geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), name = "Pearson's rho", labels = scales::comma) +
    xlab("")+
    ylab("")+
    theme_minimal() + 
    theme(legend.position = "right",  # Position the legend on the right side of the plot
          axis.text = element_text(size = 10),  # Adjust the size of axis text
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("fig/FigureS1.pdf", not_cor, width = 220, height = 220, dpi = 300, units="mm")


# removing variables with rho>0.8
per.df <- per.df[ , -which(names(per.df) %in% c("freez_freq", "chew_freq", "locom_freq", "NPT_walking_by_fence_freq", "NPT_sudden_display_freq", "NPT_facing_back_freq", "NPT_back_freq", "NPT_middle_freq", "NPT_facing_back_dur", "NPT_front_dur","NPT_latency_back_dur", "NPT_latency_middle_dur","expl_wob_freq","HAT_exploration_human_freq","expl_freq"))]

perC <- perC[ , -which(names(perC) %in% c("freez_freq", "chew_freq", "locom_freq", "NPT_walking_by_fence_freq", "NPT_sudden_display_freq", "NPT_facing_back_freq", "NPT_back_freq", "NPT_middle_freq", "NPT_facing_back_dur", "NPT_front_dur", "NPT_latency_back_dur", "NPT_latency_middle_dur","expl_wob_freq","HAT_exploration_human_freq","expl_freq"))]

perI <- perI[ , -which(names(perI) %in% c("freez_freq", "chew_freq", "locom_freq", "NPT_walking_by_fence_freq", "NPT_sudden_display_freq", "NPT_facing_back_freq", "NPT_back_freq", "NPT_middle_freq", "NPT_facing_back_dur", "NPT_front_dur", "NPT_latency_back_dur", "NPT_latency_middle_dur","expl_wob_freq","HAT_exploration_human_freq","expl_freq"))]

m <- cor(na.omit(per.df[,names(per.df)%in%labels$Label[labels$Test_name%in%"NPT"]]), method="spearman")
m <- melt(m)
names(m) <- c("X", "Y", "value")
sanity <- ggplot(data = m, aes(x = X, y = Y, fill = value)) +
    geom_tile(color = "white", size = 0.2) +  # Add a white border around each tile
    geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), name = "Pearson's rho", labels = scales::comma) +
    theme_minimal() + 
    theme(legend.position = "right",  # Position the legend on the right side of the plot
          axis.text = element_text(size = 10),  # Adjust the size of axis text
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
# looks good

#save
    saveRDS(perI, "tmp/perI.rds")
    saveRDS(per.df, "tmp/per.df.rds")
    saveRDS(perC, "tmp/perC.rds")
    saveRDS(latI, "tmp/latI.rds")
    saveRDS(lat.df, "tmp/lat.df.rds")
}

perI <- readRDS("tmp/perI.rds")
per.df <- readRDS("tmp/per.df.rds")
perC <- readRDS("tmp/perC.rds")
latI <- readRDS("tmp/latI.rds")
lat.df <- readRDS("tmp/lat.df.rds")

# separating dataframes for each test
Perso_BIBAGO <- perI[,names(perI)%in%labels$Label[labels$Test_name%in%"BIBAGO"]]
Perso_NOT <- perI[,names(perI)%in%labels$Label[labels$Test_name%in%"NOT"]]
Perso_OFT <- perI[,names(perI)%in%labels$Label[labels$Test_name%in%"OFT"]]
Perso_HAT <-perI[,names(perI)%in%labels$Label[labels$Test_name%in%"HAT"]]
Perso_NPT <-per.df[,names(per.df)%in% c(labels$Label[labels$Test_name%in%"NPT"], "unique", "Batch", "Sow", "Test_Nr", "ID")] # we take this one from the non imputed dataset, 20 animals missing.

names(Perso_BIBAGO)

names(Perso_NOT)

names(Perso_OFT)

names(Perso_HAT)

names(Perso_NPT)

Perso_NPT <- na.omit(Perso_NPT)


#### different personality tests: bibago, open field, etc.
## compute distances of behaviours within each test
library(vegan)
bib_dis <- vegdist(scale(Perso_BIBAGO), method="euclidean")
not_dis <- vegdist(scale(Perso_NOT), method="euclidean")
oft_dis <- vegdist(scale(Perso_OFT), method="euclidean")
hat_dis <- vegdist(scale(Perso_HAT), method="euclidean")
npt_dis <- vegdist(scale(Perso_NPT[,!names(Perso_NPT)%in%c("unique", "Batch", "Sow", "Test_Nr","ID")]), method="euclidean")

Perso_NPT$Sow <- as.factor(Perso_NPT$Sow)
Perso_NPT$Batch <- as.factor(Perso_NPT$Batch)
Perso_NPT$ID <- as.factor(Perso_NPT$ID)

if (do_PERMANOVA){
# permanovas to test the effect of timepoint
print(adonis2(bib_dis~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin"))

print(adonis2(not_dis~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin"))

print(adonis2(hat_dis~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin"))

print(adonis2(oft_dis~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin"))

print(adonis2(npt_dis~
            Perso_NPT$Test_Nr+
            Perso_NPT$Sow+
            Perso_NPT$Batch,
        by="margin"))
}


#### doing PCA for each test
pca_BIB <- prcomp(Perso_BIBAGO, scale=TRUE)
pca_NPT <- prcomp(Perso_NPT[,!names(Perso_NPT)%in%c("unique", "Batch", "Sow", "Test_Nr","ID")], scale=TRUE)
pca_OFT <- prcomp(Perso_OFT, scale=TRUE)
pca_HAT <- prcomp(Perso_HAT, scale=TRUE)
pca_NOT <- prcomp(Perso_NOT, scale=TRUE)

library(ggfortify)

pca_df <- as.data.frame(pca_BIB$x)[,c(1,2)]
names(pca_df) <- c("bib1", "bib2")
pca_df$Time <- as.factor(perI$Test_Nr)
pca_df$OFT1 <- pca_OFT$x[,1]
pca_df$OFT2 <- pca_OFT$x[,2]
pca_df$HAT1 <- pca_HAT$x[,1]
pca_df$HAT2 <- pca_HAT$x[,2]
pca_df$NOT1 <- pca_NOT$x[,1]
pca_df$NOT2 <- pca_NOT$x[,2]

npt_df <- as.data.frame(pca_NPT$x[,c(1,2)])
npt_df$Time <- as.factor(Perso_NPT$Test_Nr)

nptPCA <- ggplot(npt_df, aes(PC1, PC2, colour=Time))+
    geom_point(size=2)+
    stat_ellipse(aes(group=Time, colour=Time),level=0.8)+
    scale_color_manual(values=c("#8ebec4", "#7e5f7a"))+
    scale_fill_manual(values=c("#8ebec4", "#7e5f7a"))+
    theme_classic()

bibPCA <- ggplot(pca_df, aes(bib1, bib2, colour=Time))+
    geom_point(size=2)+
    stat_ellipse(aes(group=Time, colour=Time),level=0.8)+
    scale_color_manual(values=c("#8ebec4", "#7e5f7a"))+
    scale_fill_manual(values=c("#8ebec4", "#7e5f7a"))+
    theme_classic()

oftPCA <- ggplot(pca_df, aes(OFT1, OFT2, colour=Time))+
    geom_point(size=2)+
    stat_ellipse(aes(group=Time, colour=Time),level=0.8)+
    scale_color_manual(values=c("#8ebec4", "#7e5f7a"))+
    scale_fill_manual(values=c("#8ebec4", "#7e5f7a"))+
    theme_classic()

hatPCA <- ggplot(pca_df, aes(HAT1, HAT2, colour=Time))+
    geom_point(size=2)+
    stat_ellipse(aes(group=Time, colour=Time),level=0.8)+
    scale_color_manual(values=c("#8ebec4", "#7e5f7a"))+
    scale_fill_manual(values=c("#8ebec4", "#7e5f7a"))+
    theme_classic()

notPCA <- ggplot(pca_df, aes(NOT1, NOT2, colour=Time))+
    geom_point(size=2)+
    stat_ellipse(aes(group=Time, colour=Time),level=0.8)+
    scale_color_manual(values=c("#8ebec4", "#7e5f7a"))+
    scale_fill_manual(values=c("#8ebec4", "#7e5f7a"))+
    theme_classic()

library(cowplot)

Fig_S2 <- plot_grid(bibPCA, nptPCA, hatPCA, notPCA, oftPCA, labels="auto")
ggsave("fig/FigureS2.pdf", Fig_S2, width = 200, height = 200, dpi = 300, units="mm")

################### using distance based ICC.
dICC.SE.bt(as.matrix(bib_dis), strata=perI$ID, B=1000)
dICC.SE.bt(as.matrix(oft_dis), strata=perI$ID, B=1000)
dICC.SE.bt(as.matrix(hat_dis), strata=perI$ID, B=1000)
dICC.SE.bt(as.matrix(npt_dis), strata=Perso_NPT$ID, B=1000)
dICC.SE.bt(as.matrix(not_dis), strata=perI$ID, B=1000)

## question 1: are personality behaviours consistent over time?
### consistency for each predictor
if(do_rptR){
    names(Perso_BIBAGO)
    Rchew_dur <- rpt(chew_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    Rexpl_wob_dur <- rpt(expl_wob_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    Rfreez_dur <- rpt(freez_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    RBIBAGO_inter_voc_dur<-rpt(BIBAGO_inter_voc_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    Rexpl_wob_lat <- rpt(expl_wob_lat~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    RBIBAGO_voc_freq <- rpt(BIBAGO_voc_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    Rchoco_eaten <- rpt(choco_eaten~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)

    Rchew_dur
    Rexpl_wob_dur
    Rfreez_dur
    RBIBAGO_inter_voc_dur
    Rexpl_wob_lat # NOT REPEATABLE
    RBIBAGO_voc_freq
    Rchoco_eaten

names(Perso_NOT)

    Rexpl_obj_dur<-rpt(expl_obj_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    Rexpl_obj_freq <- rpt(expl_obj_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    RNOT_inter_voc_dur <- rpt(NOT_inter_voc_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    Rlat_expl_obj_dur <- rpt(lat_expl_obj_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)

    Rexpl_obj_dur
    Rexpl_obj_freq # not significant but also not 0
    RNOT_inter_voc_dur
    Rlat_expl_obj_dur # NOT REPEATABLE

    names(Perso_HAT)

    RHAT_exploration_human_dur<-rpt(HAT_exploration_human_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    RHAT_jumping_freq <- rpt(HAT_jumping_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    RHAT_latency_to_touch_the_human_dur <- rpt(HAT_latency_to_touch_the_human_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
    RHAT_tail_wagging_freq <- rpt(HAT_tail_wagging_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)

    RHAT_exploration_human_dur # NOT REPEATABLE
    RHAT_jumping_freq #NOT REPEATABLE
    RHAT_latency_to_touch_the_human_dur
    RHAT_tail_wagging_freq

    names(Perso_OFT)
    
Rjump_freq<-rpt(jump_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
Rlocom_dur<-rpt(locom_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
ROFT_voc_freq <- rpt(OFT_voc_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)
ROFT_expl_dur <- rpt(expl_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=perI)

Rjump_freq # NOT REPEATABLE
Rlocom_dur # NOT REPEATABLE
ROFT_voc_freq # NOT REPEATABLE
ROFT_expl_dur
    
names(Perso_NPT)

    RNPT_latency_nose_nose_dur<-rpt(NPT_latency_nose_nose_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_nose_nose_freq<-rpt(NPT_nose_nose_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_back_dur<-rpt(NPT_back_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_climbing_freq<-rpt(NPT_climbing_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_front_freq<-rpt(NPT_front_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_latency_front_dur<-rpt(NPT_latency_front_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_middle_dur<-rpt(NPT_middle_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_sudden_display_dur<-rpt(NPT_sudden_display_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_tail_wagging_freq<-rpt(NPT_tail_wagging_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_turn_back_freq<-rpt(NPT_turn_back_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)
    RNPT_walking_by_fence_dur<-rpt(NPT_walking_by_fence_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=Perso_NPT)

    RNPT_latency_nose_nose_dur # NOT REPEATABLE 
    RNPT_nose_nose_freq # NOT REPEATABLE 
    RNPT_back_dur
    RNPT_climbing_freq # NOT REPEATABLE
    RNPT_front_freq # NOT REPEATABLE
    RNPT_latency_front_dur # NOT REPEATABLE
    RNPT_middle_dur
    RNPT_sudden_display_dur
    RNPT_tail_wagging_freq
    RNPT_turn_back_freq
    RNPT_walking_by_fence_dur
}


Perso_NOT <- Perso_NOT[,!names(Perso_NOT)%in%c("expl_obj_freq", "lat_expl_obj_dur")]
Perso_HAT <-Perso_HAT[, !names(Perso_HAT)%in%c("HAT_exploration_human_dur", "HAT_jumping_freq")]
Perso_NPT <-Perso_NPT[, !names(Perso_NPT)%in%c("NPT_latency_nose_nose_dur", "NPT_nose_nose_freq", "NPT_climbing_freq", "NPT_front_freq", "NPT_latency_front_dur")]
Perso_BIBAGO <-Perso_BIBAGO[, !names(Perso_BIBAGO)%in%c("expl_wob_lat")]

names(Perso_BIBAGO)
names(Perso_NOT)
names(Perso_OFT)
names(Perso_HAT)
names(Perso_NPT)


################# now we need to make some decisions,
# NPT has missing data from animals of one batch and one time point
# We will use the first time they are tested, and for the few animals of NPT (batch1) we will use the time point 2. this is fine since the behaviours have high repeatability

Perso <- (cbind(Perso_BIBAGO, Perso_NOT, Perso_OFT, Perso_HAT))
Perso$ID <- perI$ID
Perso$Test_Nr <- perI$Test_Nr
Perso <- Perso[Perso$Test_Nr==1,]
Perso$Test_Nr <- NULL
NPT1 <- (Perso_NPT[Perso_NPT$Test_Nr==1,])
NPT2 <- (Perso_NPT[Perso_NPT$Test_Nr==2,])
all(NPT1$ID==NPT2$ID)
NPT_j <- rbind(NPT2[which(!NPT2$ID %in% NPT1$ID),], NPT1)

all(NPT_j$ID==Perso$ID)
NPT_j$ID <- NULL
NPT_j$Sow <- NULL
NPT_j$Batch <- NULL
NPT_j$unique <- NULL
NPT_j$Test_Nr <- NULL
Perso <- cbind(Perso, NPT_j)

names(Perso)

#### Correlation matrix of all the behaviours that are kept
m <- cor(Perso[,-15], method="pearson")
m <- melt(m)
names(m) <- c("X", "Y", "value")

cor_Perso <- ggplot(data = m, aes(x = X, y = Y, fill = value)) +
    geom_tile(color = "white", size = 0.2) +  # Add a white border around each tile
    geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), name = "Pearson's rho", labels = scales::comma) +
    xlab("")+
    ylab("")+
    theme_minimal() + 
    theme(legend.position = "right",  # Position the legend on the right side of the plot
          axis.text = element_text(size = 10),  # Adjust the size of axis text
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
#cor_Perso


