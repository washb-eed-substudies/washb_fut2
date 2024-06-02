
rm(list=ls())
library(tidyverse)
library(knitr)
library(here)
library(cowplot)
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_results.RDATA")
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_pooled_results.Rdata")

#Plot parameters
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



# Plots of associations between FUT2 status and growth outcomes (FUT2 as a risk factor)

### Child FUT2 status effect on continuous growth outcomes

#fig.width=12, fig.height=8}
plotdf <-data.frame(X= rownames(pooled_childFU2_RF_adj), pooled_childFU2_RF_adj)
plotdf$outcome <- substr(plotdf$X, 1,3)
plotdf$X <- factor(plotdf$X, levels=c("Lactulose","Mannitol","L/M ratio",
                                       "Neopterin","Myeloperoxidase","Alpha-1 antitrypsin",
                                       "Regenerating gene 1B"))
plotdf <- plotdf %>% arrange(X)
p1 <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=ATE, fill=X, color=X), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=X),
                       alpha=0.5, size = 3) +
        scale_y_continuous(n.breaks=6) +
        labs(x = "", y = "Difference in concentrations") +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 20, hjust = 0.85)) +
  ggtitle("Child FUT2 status effect\non EED outcomes")


### Mother FUT2 status effect on continuous growth outcomes
plotdf <-data.frame(X= rownames(pooled_motherFU2_RF_adj), pooled_motherFU2_RF_adj)
plotdf$outcome <- sapply(strsplit(as.character(plotdf$X), " -", fixed=T), `[`, 1)

plotdf$X <- factor(plotdf$X, levels=c("Lactulose","Mannitol","L/M ratio",
                                      "Neopterin","Myeloperoxidase","Alpha-1 antitrypsin",
                                      "Regenerating gene 1B"))
plotdf <- plotdf %>% arrange(X)

p2 <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=ATE, fill=X, color=X), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=X),
                       alpha=0.5, size = 3) +
        labs(x = "", y = "Difference in concentrations") +
        scale_y_continuous( n.breaks=6) +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 20, hjust = 0.85)) +
  ggtitle("Maternal FUT2 status effect\non EED outcomes")



p <- cowplot::plot_grid(p1, p2, ncol=2, labels="AUTO")
p
ggsave(p, file=paste0(here(),"/figures/FUT2_figure2.png"), width=12, height=4)

#Figure legend:
 #In panels A and C, the Y-axis is the difference in concentrations for FUT2 Negative (non-secretor) minus FUT2 Positive (secretor) children.
 #In panels B and C, the Y-axis is the prevalence ratio between  FUT2 Negative (non-secretor) over FUT2 Positive (secretor) children.

