# plotting distributions
bib_pcoa <- cmdscale(bib_disb)
not_pcoa <- cmdscale(not_disb)
oft_pcoa <- cmdscale(oft_disb)
hat_pcoa <- cmdscale(hat_disb)
npt_pcoa <- cmdscale(npt_disb)

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


###
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(psych)

str(Perso_BIBAGO)
bib_s<-scale(Perso_BIBAGO)
corr_matrixBIB <- cor(bib_s)
ggcorrplot(corr_matrixBIB)
cortest.mat(corr_matrixBIB,n1=80)
cortest.bartlett(corr_matrixBIB,n=80)
KMO(corr_matrixBIB)

bib.pca <- princomp(corr_matrixBIB)
summary(bib.pca)

bib.pca$loadings[, 1:2]
fviz_eig(bib.pca, addlabels = TRUE)
fviz_pca_var(bib.pca, col.var = "black")
fviz_cos2(bib.pca, choice = "var", axes = 1)
fviz_cos2(bib.pca, choice = "var", axes = 2)
fviz_pca_var(bib.pca, col.var = "cos2",
                   gradient.cols = c("black", "orange", "green"),
                               repel = TRUE)

not_s<-scale(Perso_NOT)

corr_matrixNOT <- cor(not_s)
ggcorrplot(corr_matrixNOT)

not.pca <- princomp(corr_matrixNOT)
summary(not.pca)

not.pca$loadings[, 1:3]

fviz_eig(not.pca, addlabels = TRUE)

fviz_pca_var(not.pca, col.var = "black")

fviz_cos2(not.pca, choice = "var", axes = 1)

fviz_cos2(not.pca, choice = "var", axes = 2)

fviz_pca_var(not.pca, col.var = "cos2",
                   gradient.cols = c("black", "orange", "green"),
                               repel = TRUE)


oft_s<-scale(Perso_OFT)
corr_matrixOFT <- cor(oft_s)
ggcorrplot(corr_matrixOFT)

oft.pca <- princomp(corr_matrixOFT)
summary(oft.pca)

oft.pca$loadings[, 1:3]

fviz_eig(oft.pca, addlabels = TRUE)

fviz_pca_var(oft.pca, col.var = "black")

fviz_cos2(oft.pca, choice = "var", axes = 1)

fviz_cos2(oft.pca, choice = "var", axes = 2)

fviz_pca_var(oft.pca, col.var = "cos2",
                   gradient.cols = c("black", "orange", "green"),
                               repel = TRUE)


hat_s<-scale(Perso_HAT)
corr_matrixHAT <- cor(hat_s)
ggcorrplot(corr_matrixHAT)

hat.pca <- princomp(corr_matrixHAT)
summary(hat.pca)

oft.pca$loadings[, 1:3]

fviz_eig(hat.pca, addlabels = TRUE)

fviz_pca_var(hat.pca, col.var = "black")

fviz_cos2(hat.pca, choice = "var", axes = 1)

fviz_cos2(hat.pca, choice = "var", axes = 2)

fviz_pca_var(hat.pca, col.var = "cos2",
                   gradient.cols = c("black", "orange", "green"),
                               repel = TRUE)

npt_s<-na.omit(scale(Perso_NPT[, names(Perso_NPT)%in% labels$Label[labels$Test_name%in%"NPT"]]))
corr_matrixNPT <- cor(npt_s)
ggcorrplot(corr_matrixNPT)

npt.pca <- princomp(corr_matrixNPT)
summary(npt.pca)

npt.pca$loadings[, 1:3]

fviz_eig(npt.pca, addlabels = TRUE)

fviz_pca_var(npt.pca, col.var = "black")

fviz_cos2(npt.pca, choice = "var", axes = 1)

fviz_cos2(npt.pca, choice = "var", axes = 2)

fviz_pca_var(npt.pca, col.var = "cos2",
                   gradient.cols = c("black", "orange", "green"),
                               repel = TRUE)


