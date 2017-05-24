##### Figure 1 ############
source("simulatedData_helper.R")
pdf("Figure1.pdf",onefile = TRUE,width = 10, height =5,family="ArialMT")
par(cex=1,oma = c(0, 0, 0, 0),mar = c(3.5,3,1.5, 0) )
getDefinitionPlot()
dev.off()

###############################################################
####### Figure 2 ###########

pdf("Figure2.pdf",onefile = TRUE,width = 12, height = 6,family="ArialMT")
par(cex=1,oma = c(0, 0, 1, 0),mar = c(3.5,3,2, 0) )
getSpectrumPlot()
dev.off()