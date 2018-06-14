#### Helper functions required for all plots


###############################################################
##### Figure 1 ############

source("scriptForPlots_helper.R")
pdf("Figures/Figure1.pdf",onefile = TRUE,width = 10, height =5,family="ArialMT")
par(cex=1,oma = c(0, 0, 0, 0),mar = c(3.5,3,1.5, 0) )
getDefinitionPlot()
dev.off()


###############################################################
####### Figure 2 ###########

source("scriptForPlots_helper.R")
pdf("Figures/Figure2.pdf",onefile = TRUE,width = 12, height = 6,family="ArialMT")
par(cex=1,oma = c(0, 0, 1, 0),mar = c(3.5,3,2, 0) )
getSpectrumPlot1()
dev.off()

###############################################################
####### Figure 3a, 3b, 3c ###############################

source("scriptForPlots_helper.R")
load("Figure3.Rdata")

dfs1_PI <- melt(R1_PIplate[[2]],id=c("DRow","DCol"))
dfs1_PI$DRow <- factor(dfs1_PI$DRow, levels = rev(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")))
dfs2_PI <- melt(R2_PIplate[[2]],id=c("DRow","DCol"))
dfs2_PI$DRow <- factor(dfs2_PI$DRow, levels = rev(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")))

dfs1_GR <- melt(R1_GRplate[[2]],id=c("DRow","DCol"))
dfs1_GR$DRow <- factor(dfs1_GR$DRow, levels = rev(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")))
dfs2_GR <- melt(R2_GRplate[[2]],id=c("DRow","DCol"))
dfs2_GR$DRow <- factor(dfs2_GR$DRow, levels = rev(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")))

dfs1_NDR <- melt(R1_NDRplate[[2]],id=c("DRow","DCol"))
dfs1_NDR$DRow <- factor(dfs1_NDR$DRow, levels = rev(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")))
dfs2_NDR <- melt(R2_NDRplate[[2]],id=c("DRow","DCol"))
dfs2_NDR$DRow <- factor(dfs2_NDR$DRow, levels = rev(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")))


pdf("Figures/Figure3a.pdf",onefile = TRUE,width = 10, height = 4 )
limNum= c(-25,100)
grid.arrange(ggplot(dfs1_PI, aes( DCol,DRow )) +
               geom_tile(aes(fill = value), color = "white") +
               scale_fill_gradient2(low = "green", high = "red",mid="white",limits=limNum,space="Lab") + ggtitle("Run1-PI values") +
               theme_void(),# + theme(legend.position="none"),
             ggplot(dfs1_GR, aes( DCol,DRow )) +
               geom_tile(aes(fill = value), color = "white") +
               scale_fill_gradient2(low = "green", high = "red",mid="white",limits=limNum,space="Lab") + ggtitle("Run1-GR values") +
               theme_void(),# + theme(legend.position="none"),
             ggplot(dfs1_NDR, aes( DCol,DRow )) +
               geom_tile(aes(fill = value), color = "white") +
               scale_fill_gradient2(low = "green", high = "red",mid="white",limits=limNum,space="Lab") + ggtitle("Run1-NDR values") +
               theme_void(),# + theme(legend.position="none"),
             ggplot(dfs2_PI, aes( DCol,DRow )) +
               geom_tile(aes(fill = value), color = "white") +
               scale_fill_gradient2(low = "green", high = "red",mid="white",limits=limNum,space="Lab") + ggtitle("Run2-PI values") +
               theme_void(),# + theme(legend.position="none"),
             ggplot(dfs2_GR, aes( DCol,DRow )) +
               geom_tile(aes(fill = value), color = "white") +
               scale_fill_gradient2(low = "green", high = "red",mid="white",limits=limNum,space="Lab") + ggtitle("Run2-GR values") +
               theme_void(),# + theme(legend.position="none"),
             ggplot(dfs2_NDR, aes( DCol,DRow )) +
               geom_tile(aes(fill = value), color = "white") +
               scale_fill_gradient2(low = "green", high = "red",mid="white",limits=limNum,space="Lab") + ggtitle("Run2-NDR values") +
               theme_void(), #+ theme(legend.position="none"),
             ncol=3)
dev.off()

grid.arrange(qplot(dfs1_PI$value,dfs2_PI$value,xlab=expression("PI"[Run1]),ylab=expression("PI"[Run2])),qplot(dfs1_GR$value,dfs2_GR$value,xlab=expression("GR"[Run1]),ylab=expression("GR"[Run2])),qplot(dfs1_NDR$value,dfs2_NDR$value,xlab=expression("NDR"[Run1]),ylab=expression("NDR"[Run2])),ncol=3)

pdf("Figures/Figure3b.pdf",onefile = TRUE,width = 4, height = 2 )
resTable= melt(data.frame(PI= abs(R2_PIplate[[2]]$PI-R1_PIplate[[2]]$PI),GR= abs(R2_GRplate[[2]]$PI- R1_GRplate[[2]]$PI),NDR= abs(R2_NDRplate[[2]]$PI - R1_NDRplate[[2]]$PI))) 
ggplot(resTable,aes(value,fill=variable,group=variable)) +geom_density(color=0,alpha=0.5) + theme_classic() + ylab("Density") + xlab("Absolute difference between well readouts")+ labs(fill="") 
dev.off()

wilcox.test(resTable$value[resTable$variable=="NDR"],resTable$value[resTable$variable=="GR"])
wilcox.test(resTable$value[resTable$variable=="NDR"],resTable$value[resTable$variable=="PI"])
wilcox.test(resTable$value[resTable$variable=="GR"],resTable$value[resTable$variable=="PI"])

pdf("Figures/Figure3c.pdf",onefile = TRUE,width = 6, height = 2 )
resTable= melt(data.frame(expNumber=c("Run1-Plate 1", "Run1-Plate 2","Run2-Plate 1", "Run2-Plate 2"),NDR=c(R1_NDRplate[[3]],R2_NDRplate[[3]]),GR=c(R1_GRplate[[3]],R2_GRplate[[3]]),PI=c(R1_PIplate[[3]],R2_PIplate[[3]])),id=c("expNumber"))
ggplot(resTable,aes(expNumber,value,fill=variable)) + geom_bar(stat="identity",position= "dodge") + xlab("Experiment") + ylab("Z'-factor") + labs(fill="") + theme_classic()
dev.off()

###############################################################
####### Figure 4a and 4b ###############################

source("scriptForPlots_helper.R")
load("Figure4.Rdata")

idx=6:7
t1= cor.test(rowMeans(MIA_PAC_Act$pactivation_NDR[MIA_PAC_Act$pactivation_NDR$Drug %in% drugNames,idx]),rowMeans(MIA_PAC_Act$pinhibition[MIA_PAC_Act$pinhibition$Drug %in% drugNames,idx]))
t2= cor.test(rowMeans(MB231_Act$pactivation_NDR[,idx]),rowMeans(MB231_Act$pinhibition[,idx]))
t3= cor.test(rowMeans(MCF7_R2_Act$pactivation_NDR[,idx]),rowMeans(MCF7_R2_Act$pinhibition[,idx]))
t4= cor.test(rowMeans(HDQP1_Act$pactivation_NDR[,idx]),rowMeans(HDQP1_Act$pinhibition[,idx]))
t5= cor.test(rowMeans(MB361_Act$pactivation_NDR[,idx]),rowMeans(MB361_Act$pinhibition[,idx]))

pdf("Figures/Figure4a.pdf",onefile = TRUE,width = 10, height = 2 )
grid.arrange(qplot(rowMeans(MIA_PAC_Act$pinhibition[MIA_PAC_Act$pinhibition$Drug %in% drugNames,idx]),rowMeans(MIA_PAC_Act$pactivation_NDR[MIA_PAC_Act$pactivation_NDR$Drug %in% drugNames,idx]),xlab="Average toxicity",ylab="Average viability",color=I("blue"),alpha=0.5, xlim=c(-0.15,1.15), ylim=c(-0.15,1.15)) + annotate("text", x = 0.75, y = 1, label = paste("R2=",round((t1$estimate)^2,2))) + theme_classic() + theme(legend.position="none") ,
             qplot(rowMeans(MB231_Act$pinhibition[,idx]),rowMeans(MB231_Act$pactivation_NDR[,idx]),xlab="Average toxicity",ylab="",color=I("red"),alpha=0.5, xlim=c(-0.15,1.15), ylim=c(-0.15,1.15)) + annotate("text", x = 0.75, y = 1, label = paste("R2=",round((t2$estimate)^2,2))) + theme_classic() + theme(legend.position="none"),
             qplot(rowMeans(MCF7_R2_Act$pinhibition[,idx]),rowMeans(MCF7_R2_Act$pactivation_NDR[,idx]),xlab="Average toxicity",ylab="",color=I("chocolate1"),alpha=0.5, xlim=c(-0.15,1.15), ylim=c(-0.15,1.15)) + annotate("text", x = 0.75, y = 1, label = paste("R2=",round((t3$estimate)^2,2))) + theme_classic() + theme(legend.position="none"),
             qplot(rowMeans(HDQP1_Act$pinhibition[,idx]),rowMeans(HDQP1_Act$pactivation_NDR[,idx]),xlab="Average toxicity",ylab="",color=I("green"),alpha=0.5, xlim=c(-0.15,1.15), ylim=c(-0.15,1.15)) + annotate("text", x = 0.75, y = 1, label = paste("R2=",round((t4$estimate)^2,2))) + theme_classic() + theme(legend.position="none"),
             qplot(rowMeans(MB361_Act$pinhibition[,idx]),rowMeans(MB361_Act$pactivation_NDR[,idx]),xlab="Average toxicity",ylab="",color=I("black"),alpha=0.5, xlim=c(-0.15,1.15), ylim=c(-0.15,1.15)) + annotate("text", x = 0.75, y = 1, label = paste("R2=",round((t5$estimate)^2,2))) + theme_classic() + theme(legend.position="none"),
             ncol=5)
dev.off()


pdf("Figures/Figure4b.pdf",onefile = TRUE,width = 8, height = 4,family='ArialMT')
drugs= c("Filanesib","Omacetaxine","Pevonedistat","Tipifarnib")
plotDrugresponse(HDQP1_cellNumbers,HDQP1_Act,MB231_Act,MIA_PAC_Act,MCF7_R2_Act,MB361_Act,drugs)
dev.off()

###############################################################
##### Figure 5a and 5b ##############################
source("scriptForPlots_helper.R")
load("Figure5.Rdata")

plots <- getRMSDplot(resTable)
pdf("Figures/Figure5a.pdf",onefile = TRUE,width = 10, height = 3.5 )
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],ncol=5)
dev.off()

plots <- getDistToZero(resTable)
pdf("Figures/Figure5b.pdf",onefile = TRUE,width = 10, height = 3.5 )
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],ncol=5)
dev.off()


###############################################################
###### Figure 6a, 6b and 6c #########################
source("scriptForPlots_helper.R")
load("Figure6.RData")

GR_DMSO <- cellNumbers$Final_cells[cellNumbers$DRUG_NAME=="DMSO"]/cellNumbers$Init_cells[cellNumbers$DRUG_NAME=="DMSO"]
GR_BzCl <- cellNumbers$Final_cells[cellNumbers$DRUG_NAME=="BzCl"]/cellNumbers$Init_cells[cellNumbers$DRUG_NAME=="BzCl"]

dfs <- rbind(data.frame(Condition="Negative control",value=GR_DMSO),data.frame(Condition="Final drug conc.",value=concChange))
lowerSD= mean(GR_DMSO)-2*sd(GR_DMSO)
upperSD= mean(GR_DMSO)+2*sd(GR_DMSO)
flevels <- levels(as.factor(seq(-1,6,1)))

pdf("Figures/Figure6a.pdf",onefile = TRUE,width = 7, height =3)
ggplot(dfs, aes(x=value,fill=Condition))+ geom_density(alpha=0.25)+ scale_x_continuous(name="Fold change of readout",limits=c(-1, 3), breaks=c(-1,1,3))+ ylim(0,2.75)+ geom_vline(xintercept =lowerSD ,linetype="dotted")+ geom_vline(xintercept = upperSD,linetype="dotted")+ geom_vline(xintercept = 1,linetype="dotted") + theme_classic()+ theme(legend.justification=c(0,0), legend.position=c(0.05,0.65)) + ylab("Density") + theme(legend.title = element_blank()) +
  annotate("segment", x = -0.5, xend = 1, y = 0.4, yend = 0.4, lwd=0.25, arrow=arrow(ends="last", angle=90, length=unit(.2,"cm"))) + annotate("text", x = 0.2, y = 0.5, label="Lethal", family="serif", fontface="italic", colour="darkred", size=3) +
  annotate("segment", x = 1, xend = lowerSD, y = 0.6, yend = 0.6, lwd=0.25, arrow=arrow(ends="both", angle=90, length=unit(.2,"cm"))) + annotate("text", x = 1.1, y = 0.7, label="Sub-effective", family="serif", fontface="italic", colour="darkred", size=3) +
  annotate("segment", x = lowerSD, xend = upperSD, y = 2.5, yend = 2.5, lwd=0.25, arrow=arrow(ends="both", angle=90, length=unit(.2,"cm"))) + annotate("text", x = 1.75, y = 2.6, label="Non-effective", family="serif", fontface="italic", colour="darkred", size=3) +
  annotate("segment", x = upperSD, xend =2.5, y = 0.9, yend = 0.9, lwd=0.25, arrow=arrow(ends="first", angle=90, length=unit(.2,"cm"))) + annotate("text", x = 2.5, y = 1, label="Growth-stimulatory", family="serif", fontface="italic", colour="darkred", size=3) 
dev.off()


pdf("Figures/Figure6b.pdf",onefile = TRUE,width = 7, height = 3 )
plotActivityCurve(MB361_Act$pactivation_NDR,MB361_Act$pinhibition)
dev.off()


tempTable <- subset(resTable, (Cell_line %in% c("MDA-MB-361")))
drugTypes <- c("Lethal","Effective","Non-effective","Growth-stimulative")
drugTypes_labels <- c("Lethal","Sub-effective","Non-effective","Growth-stimulative")
tempTable$drugType <-factor(tempTable$drugType,levels=drugTypes)

pdf("Figures/Figure6c.pdf",onefile = TRUE,width = 7, height = 3 )
par(cex=0.9,oma = c(0, 0, 1, 0),mar = c(2.5,3.5,0, 1) )
plot(1,xlim = c(0.5, 4.5), ylim=c(-20, 45),
     xaxt="n",type="l",las=1,xlab="",ylab="")
axis(1, at=c(1:4),labels=drugTypes_labels)
title(ylab=expression("DSS"[NDR]),line=2)

colorByMML <- as.character(ifelse(tempTable$Cell_line=='MIA PaCa-2',"blue",ifelse(tempTable$Cell_line=='MDA-MB-231',"red",ifelse(tempTable$Cell_line=='MCF-7-R1',"chocolate2",ifelse(tempTable$Cell_line=='HDQ-P1',"green","black")))))
beeswarm(as.numeric(DSS)~as.factor(drugType), data = tempTable,corral="wrap",side=0, pch=16,col =c(2,"orange","blue","green"),pwcol=colorByMML,add=T,alpha=0.5)
plotSegment(1,42,1.5,-1.5)
plotSegment(2,32,2.5,-3.5)
plotSegment(3,22,3.5,-5.5)
dev.off()


wilcox.test(tempTable$DSS[tempTable$drugType=="Effective"],tempTable$DSS[tempTable$drugType=="Lethal"])
wilcox.test(tempTable$DSS[tempTable$drugType=="Effective"],tempTable$DSS[tempTable$drugType=="Non-effective"])
wilcox.test(tempTable$DSS[tempTable$drugType=="Non-effective"],tempTable$DSS[tempTable$drugType=="Growth-stimulative"])





