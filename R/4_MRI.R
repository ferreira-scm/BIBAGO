library(mice)

mri <- read.csv("data/BIBAGO_MRI_dataset.csv")

head(mri)

# remove NA's
mri <- mri[!is.na(mri$Subject),]
mri <- mri[!is.na(mri$Pen),]

pMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(mri,2,pMiss) # no variable with more than 10% misssing data.
apply(mri,1,pMiss)

# now we save
mri_temp <- mice(mri,m=10,maxit=50,meth='pmm',seed=500)

mriI <- complete(mri_temp,1)

summary(as.factor(mri$PVC_Nr))

names(mriI)

mriI[,8:17]

## repeatability
mriI$ID <- as.factor(mriI$PVC_Nr)
str(mri$PVC_Nr)

str(mriI)

names(mriI)
Rexpl_wob_dur <- rpt(expl_wob_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=mriI)

Rfreez_dur <- rpt(freez_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=mriI)
RBIBAGO_inter_voc_dur<-rpt(BIBAGO_inter_voc_dur~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=mriI)
Rexpl_wob_lat <- rpt(expl_wob_lat~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=mriI)
RBIBAGO_voc_freq <- rpt(BIBAGO_voc_freq~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=mriI)
Rchoco_eaten <- rpt(choco_eaten~(1|ID), grname="ID", datatype="Gaussian", nboot=1000, data=mriI)

Rfreez_dur
RBIBAGO_inter_voc_dur
Rexpl_wob_lat
RBIBAGO_voc_freq
Rchoco_eaten


bib_dis <- vegdist(mriI[,8:17], method="aitchison", pseudocount=1)

dICC(bib_dis, strata=mriI$PVC_Nr)



adonis2(bib_dis~
            mriI$Test_Nr+
            mriI$Sow+
            mriI$Batch,
        strata=mriI$PVC_Nr,
        by="margin") # no difference of test?

adonis2(bib_dis~
          mriI$PVC_Nr,
        by="margin") # no difference of test?


net.df <- mriI

net.df <- net.df[,8:17]

net.df$no_expl_wob_dur <- NULL
net.df$no_expl_wob_freq <- NULL
net.df$expl_wob_freq <- NULL
net.df$freez_freq <- NULL

names(Perso_BIBAGO)

names(net.df)



id <- mriI$PCV_Nr

ncol(net.df) # number of nodes

############ first a simple network
results.MRI <- estimateNetwork(net.df,
                default="EBICglasso",
                corMethod="spearman",
                tuning=0.19)


plot(results.MRI)
