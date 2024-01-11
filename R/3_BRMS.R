library(brms)

#M<-cor(alldf[,c("chew_dur", "expl_wob_dur", "expl_wob_freq", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq", "choco_eaten")])
#corrplot(M, method = 'number')

#M2<-cor(alldf[,c("chew_dur", "expl_wob_freq", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq")])

#library(corrplot)
#corrplot(M2, method = 'number')
# we have high colliniarity
# we remove expl_wob_dur, choco_eaten, and expl_wob_freq
## now we standardize

#standardize <- function(x){
#    (x - mean(x))/sd(x)
#}

#library(dplyr)

#df <- alldf %>% mutate_at(c("chew_dur", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq"), ~(standardize(.) %>% as.vector))


#df <- df[, c("chew_dur", "freez_dur", "BIBAGO_inter_voc_dur", "expl_wob_lat", "BIBAGO_voc_freq")]

df

##### compare only LL and RR #####

# ok let's do 3 univariate since our multivariate doesn't converge
#fit_w <- brm(LI_w~chew_dur+ freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq+
#                 (1|Sow),
#             data = df,
#             chains = 4,
#             cores = 4,
#             warmup=1000,
#             iter=6000)
#fit_w

#fit_t <- brm(LI_t~chew_dur+ freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq,
#             data = df,
#             chains = 4,
#             cores = 4,
#             warmup=1000,
#             iter=6000)

#fit_t

#fit_f <- brm(LI_f~chew_dur+ freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq,
#             data = df,
#             chains = 4,
#             cores = 4,
#             warmup=1000,
#             iter=6000)


#fit_t


#names(alldf)

#install.packages("corrplot")

#library(corrplot)


#plot(alldf$expl_wob_freq, alldf$expl_wob_dur)

#plot(alldf$chew_freq, alldf$chew_dur)

#plot(alldf$BIBAGO_inter_voc_dur, alldf$BIBAGO_voc_freq)

#plot(alldf$freez_freq, alldf$freez_dur)

#bform1 <- bf(mvbind(LI_w, LI_f, LI_t) ~chew_dur+ expl_wob_dur + expl_wob_freq + freez_dur+ BIBAGO_inter_voc_dur + expl_wob_lat + BIBAGO_voc_freq + choco_eaten + (1|q|PVC_Nr)) + set_rescor(TRUE)# doesn't work
#fit1 <- brm(bform1, data = alldf, chains = 4, cores = 4, warmup=1000, iter=6000)

#fit1
#
#modelLAT<-brm(~1+ not+oft+
#                (1|mm(IDA,IDB))+(1|sow)+(1|batch),
#                data = data.dyad,
#                family= "gaussian",
#                warmup = 1000, iter = 3000,
#                cores = 50, chains = 4,
#                inits=0)


#cor.test(alldf$LI_t, alldf$LI_f)


# making key for the order of ID and sample
#key <- data.frame(ID=paste(perI$PVC_Nr, perI$Test_Nr, sep="_"), ID2=perI$PVC_Nr)

#bib_dis <- as.matrix(bib_dis)
#rownames(bib_dis) <- key$ID
#colnames(bib_dis) <- key$ID

#dimnames(bib_dis)

#not_dis <- as.matrix(not_dis)
#rownames(not_dis) <- key$ID
#colnames(not_dis) <- key$ID

#oft_dis <- as.matrix(oft_dis)
#rownames(oft_dis) <- key$ID
#colnames(oft_dis) <- key$ID

#bib_dis <- 1-bib_dis
#not_dis <- 1-not_dis
#oft_dis <- 1-oft_dis


#Sow_frame <- perI[,c("Subject", "Sow")]
#Sow_frame$Subject <- as.character(Sow_frame$Subject)
#Sow_frame$Sow <- as.character(Sow_frame$Sow)
#Create an empty numeric matrix to fill with distances
#SOWM<-array(0,c(nrow(Sow_frame),nrow(Sow_frame)))
#Derive matrix with binary Age similarity between each sample
#for(i in 1:nrow(Sow_frame)){
#    for(j in 1:nrow(Sow_frame)){
#        if(Sow_frame$Sow[i]==Sow_frame$Sow[j]){
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
