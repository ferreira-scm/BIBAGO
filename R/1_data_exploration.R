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
labels <- read.csv("data/Labels.csv") # varialble and test labels
#some adjustments
labels$Hypothesis.dimension[labels$Label%in%c("chew_dur", "expl_wob_dur", "expl_wob_freq", "choco_eaten", "expl_wob_lat")] <- "BAS"
labels$Hypothesis.dimension[labels$Label%in%c("freez_dur", "BIBAGO_inter_voc_dur", "BIBAGO_voc_freq")] <- "BIS"
labels$Hypothesis.dimension[labels$Label%in%c("HAT_exploration_human_dur", "HAT_exploration_human_freq", "NPT_sudden_display_dur")] <- "Exploration"
labels$Hypothesis.dimension[labels$Label%in%c("HAT_latency_to_touch_the_human_dur")] <- "Boldness"
labels$Hypothesis.dimension[labels$Label%in%c("HAT_tail_wagging_freq", "NPT_tail_wagging_freq", "NPT_sudden_display_dur")] <- "BAS_Sociability"
labels$Importance[labels$Label%in%c("HAT_touching_not_visible_freq")] <- 0
labels$Importance[labels$Label=="HAT_pig_not_visible_dur"] <- 0
labels[which(labels$Label%in%"NPT_latency_nose_nose_dur"),]
# adding unique and ID as important
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


## question 1: are personality behaviours consistent over time?
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
## seel it as a good one (bibago)
library(vegan)
bib_disb <- vegdist(Perso_BIBAGO, method="bray")
not_disb <- vegdist(Perso_NOT, method="bray")
oft_disb <- vegdist(Perso_OFT, method="bray")
hat_disb <- vegdist(Perso_HAT, method="bray")
npt_disb <- vegdist(Perso_NPT[, names(Perso_NPT)%in% labels$Label[labels$Test_name%in%"NPT"]], method="bray")

bib_disa <- vegdist(Perso_BIBAGO, method="aitchison", pseudocount=1)
not_disa <- vegdist(Perso_NOT, method="aitchison", pseudocount=1)
oft_disa <- vegdist(Perso_OFT, method="aitchison", pseudocount=1)
hat_disa <- vegdist(Perso_HAT, method="aitchison", pseudocount=1)
npt_disa <- vegdist(Perso_NPT[, names(Perso_NPT)%in% labels$Label[labels$Test_name%in%"NPT"]], method="aitchison", pseudocount=1)

bib_disc <- vegdist(Perso_BIBAGO, method="chisq")
not_disc <- vegdist(Perso_NOT, method="chisq")
oft_disc <- vegdist(Perso_OFT, method="chisq")
hat_disc <- vegdist(Perso_HAT, method="chisq")
npt_disc <- vegdist(Perso_NPT[, names(Perso_NPT)%in% labels$Label[labels$Test_name%in%"NPT"]], method="chisq")

# plotting distributions
bib <- melt(Perso_BIBAGO)
ggplot(data=bib, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales="free")
# can't use PCA's without transformation, data is skewed

bib_pcoa <- cmdscale(bib_disa)
not_pcoa <- cmdscale(not_disa)
oft_pcoa <- cmdscale(oft_disa)
hat_pcoa <- cmdscale(hat_disa)
npt_pcoa <- cmdscale(npt_disa)

Perso_BIBAGO$PCoA1 <- bib_pcoa[,1]
Perso_BIBAGO$PCoA2 <- bib_pcoa[,2]
Perso_BIBAGO$ID <- perI$ID
Perso_BIBAGO$Sow <- perI$Sow
Perso_BIBAGO$Time <- as.factor(perI$Test_Nr)

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

Perso_NPT$PCoA1 <- npt_pcoa[,1]
Perso_NPT$PCoA2 <- npt_pcoa[,2]
Perso_NPT$Time <- as.factor(Perso_NPT$Test_Nr)

Perso_NPT$Sow <- as.factor(Perso_NPT$Sow)
Perso_NPT$Batch <- as.factor(Perso_NPT$Batch)

# quick vizualization

if(do_rptR){
bib_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_BIBAGO, datatype="Gaussian", nboot=1000)
bib_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_BIBAGO, datatype="Gaussian", nboot=1000)
not_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_NOT, datatype="Gaussian", nboot=1000)
not_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_NOT, datatype="Gaussian", nboot=1000)
oft_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_OFT, datatype="Gaussian", nboot=1000)
oft_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_OFT, datatype="Gaussian", nboot=1000)
hat_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_HAT, datatype="Gaussian", nboot=1000)
hat_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_HAT, datatype="Gaussian", nboot=1000)
npt_rpt1 <- rpt(PCoA1~(1|ID), grname="ID", data=Perso_NPT, datatype="Gaussian", nboot=1000)
npt_rpt2 <- rpt(PCoA2~(1|ID), grname="ID", data=Perso_NPT, datatype="Gaussian", nboot=1000)
(bib_rpt1)
(bib_rpt2)
(not_rpt1)
(not_rpt2)
(oft_rpt1)
(oft_rpt2)
(hat_rpt1)
(hat_rpt2)
npt_rpt1
npt_rpt2
}

# visualizing time effect
time_palette <- c("#00203FFF", "#ADEFD1FF")


bib_mds <- ggplot(Perso_BIBAGO, aes(x=PCoA1, y=PCoA2, fill=Time))+
    geom_point(shape=21, aes(fill=Time), size=2, stroke=0.4, alpha=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point", title="BIBAGO") +
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
    geom_point(shape=21, aes(fill=Time), size=2, stroke=0.4, alpha=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point", title="NOT") +
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
    geom_point(shape=21, aes(fill=Time),  size=2, stroke=0.4, alpha=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point", title="OFT") +
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
    geom_point(shape=21, aes(fill=Time), size=2, stroke=0.6, alpha=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point", title="HAT") +
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

npt_mds <- ggplot(Perso_NPT, aes(x=PCoA1, y=PCoA2, fill=Time))+
    geom_point(shape=21, aes(fill=Time), size=2, stroke=0.4, alpha=0.8)+
#    geom_line(aes(group=ID), alpha=0.7)+
    stat_ellipse(aes(group=Time), geom="polygon", colour="black", alpha=0.2, linetype = "dashed", size = 0.6)+
    labs(x = "PCoA axis 1", y = "PCoA axis 2", fill = "Time point", title="NPT") +
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
legend <- get_legend(
              bib_mds + theme(legend.position="right")
              )

Figure1 <- plot_grid(bib_mds, npt_mds, not_mds, hat_mds, oft_mds,legend, labels="auto", nrow=2)
ggsave("fig/Figure1.pdf", Figure1, width = 170, height = 130, dpi = 300, units="mm")


if (do_PERMANOVA){
# permanovas to test the effect of timepoint

    adonis2(bib_disa~
            perI$ID)

adonis2(bib_disa~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        strata=perI$ID,
        by="margin") # no difference of test?

adonis2(not_disa~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(not_disa~
            perI$ID)

adonis2(hat_disa~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

    adonis2(hat_disa~
            perI$ID)

adonis2(oft_disa~
            perI$Test_Nr+
            perI$Sow+
            perI$Batch,
        by="margin") # no difference of test?

adonis2(oft_disa~
            perI$ID)

adonis2(npt_disa~
            Perso_NPT$Test_Nr+
            Perso_NPT$Sow+
            Perso_NPT$Batch,
        by="margin") # no difference of test?

adonis2(npt_dis2~
            Perso_NPT$ID)
    
}

################### using distance based ICC.
dICC(bib_disa, strata=perI$ID)
dICC(not_disa, strata=perI$ID)
dICC(oft_disa, strata=perI$ID)
dICC(hat_disa, strata=perI$ID)
dICC(npt_disa, strata=Perso_NPT$ID)

npt_disa <- vegdist(Perso_NPT[, names(Perso_NPT)%in% labels$Label[labels$Test_name%in%"NPT"]], method="aitchison", pseudocount=1)

Perso_NPT[,names(Perso_NPT)%in% labels$Label[labels$Test_name%in%"NPT"]]

npt_disa


################# now we need to make some decisions,
# NPT has missing data from animals of one batch and one time point
# what do we do? Either we remove those animals and have smaller dataset.
# We can also do the mean for each time point. This is fine for NPT since time point explains very little of the variance. Let's do that
perA <- perI
perA$PVC_Nr <- NULL
perA$unique <- NULL
perA$Test_Nr <- NULL
perA$Batch <- as.numeric(perA$Batch)
perA$Sow <- as.numeric(perA$Sow)
perA$Subject <- NULL
id <- perA$ID
perA$ID <- NULL
perA <- aggregate(perA, by=list(id), FUN=mean)
NPT <- Perso_NPT
NPT$unique <- NULL
NPT$Test_Nr <- NULL
NPT$Time <- NULL
NPT$PCoA1 <- NULL
NPT$PCoA2 <- NULL
NPT$Batch <- NULL
NPT$Sow <- NULL
id <- NPT$ID
NPT$ID <- NULL
NPT <- aggregate(NPT, by=list(id), FUN=mean)
head(Perso_NPT)
# and merge
perA <- merge(perA, NPT, by="Group.1")


