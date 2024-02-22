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

bib_dis <- vegdist(mriI[,8:17], method="aitchison", pseudocount=1)

dICC(bib_dis, strata=mriI$PVC_Nr)

mriI$PVC_Nr <- as.factor(mriI$PVC_Nr)

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
