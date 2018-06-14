library(reshape)
library(ggplot2)
library(gridExtra)
library(beeswarm)
source("colorBar.r")


getSpectrumPlot <- function(){
  ## get numCells for drug treatment 
  par(fig=c(0,0.225,0,1))
  GR_Drug= seq(-0.01,0.04,0.005)
  drawDrugPlot(GR_Drug,2)
  
  ## get numCells for positive control
  par(fig=c(0.25,0.475,0.5,0.98), new= TRUE)
  GR_BzCl= seq(-0.015,-0.005,0.0025)
  GR_DMSO= 0.03
  plot(1,type="l",xlim=c(0,120),ylim=c(0,100), las=1,xlab="", ylab="")
  title(xlab="Time (h)",line=2.5,cex.lab=0.8)
  title(ylab="Readout (aribitary unit)",line=2.2,cex.lab=0.8)
  title(main="Dynamic positive control,\nconstant negative control",cex.main=0.9,line=0.5)
  timeNum= c(1:120)
  colfunc <- colorRampPalette(c("red", "orange"))
  colors <- colfunc(length(GR_BzCl))
  
  for( i in 1:length(GR_BzCl)){
    lines((10*2^(timeNum*GR_BzCl[i])),col=colors[i])
  }
  lines((10*2^(timeNum*GR_DMSO)),col="green")
  #legend(3, 55, c("Growth rate in\nnegative control 0.03"), bty = "n",cex=0.8, lwd=2,lty=1,col="black")
  legend(0, 65, c("Negative control "), bty = "n",cex=0.7, lwd=2,lty=1,col="green")
  mtext("b", side=3,line=.75, at=-25,cex=1.25)
  
  par(fig=c(0.3,0.395,0.81,0.91), new= TRUE)
  #par(fig=c(0.465,0.56,0.495,0.575), new= TRUE)
  color.bar(colors, -0.015, -0.005, title='Growth rate in\npositive control', nticks=2)
  #subplot(color.bar(colors, 0.1, 0.8, title='Positive control', nticks=2),20,3,size=c(0.5,0.15),vadj=1,hadj=0.25)
  
  par(fig=c(0.5,0.725,0.5,0.98), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,1,1,colors)
  mtext("c", side=3,line=0.75, at=-0.03,cex=1.25)
  
  par(fig=c(0.75,0.975,0.5,0.98), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,3,1,colors)
  mtext("d", side=3,line=0.75, at=-0.03,cex=1.25)
  
  
  par(fig=c(0.25,0.475,0,0.48), new= TRUE)
  GR_DMSO= seq(0.01,0.055,0.01)
  GR_BzCl= -0.01
  ## get numCells for negative control
  plot(1,type="l",xlim=c(0,120),ylim=c(0,140), las=1,xlab="", ylab="")
  title(xlab="Time (h)",line=2.5,cex.lab=0.8)
  title(ylab="Readout (aribitary unit)",line=2.2,cex.lab=0.8)
  title(main="Dynamic negative control,\nconstant positive control",cex.main=0.9,line=0.5)
  timeNum= c(1:120)
  
  colfunc <- colorRampPalette(c("green4", "green"))
  colors <- colfunc(length(GR_DMSO))
  for( i in 1:length(GR_DMSO)){
    lines((10*2^(timeNum*GR_DMSO[i])),col=colors[i])
  }
  lines((10*2^(timeNum*GR_BzCl)),col="red")
  #legend(3, -3.5, c("Growth rate in\npositive control -0.01"), bty = "n",cex=0.8, lwd=2,lty=1,col="black")
  legend(0, 95, c("Positive control"), bty = "n",cex=0.7, lwd=2,lty=1,col="red")
  
  mtext("e", side=3,line=0.75, at=-25,cex=1.25)
  
  par(fig=c(0.3,0.395,0.31,0.41), new= TRUE)
  #par(fig=c(0.465,0.56,0.05,0.13), new= TRUE)
  color.bar(colors, 0.01, 0.055, title='Growth rate in\nnegative control', nticks=2)
  
  #subplot(color.bar(colors, 2, 8, title='Negative control', nticks=2),20,2,size=c(0.5,0.15),vadj=1,hadj=0.25)
  
  par(fig=c(0.5,0.725,0,0.48), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,1,2,colors)
  mtext("f", side=3,line=0.75, at=-0.03,cex=1.25)
  par(fig=c(0.75,0.975,0,0.48), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,3,2,colors)
  mtext("g", side=3,line=0.75, at=-0.03,cex=1.25)
  
}
getSpectrumPlot1 <- function(){
  ## get numCells for drug treatment 
  
  GR_Drug= seq(-0.01,0.04,0.005)
  #drawDrugPlot(GR_Drug,2)
  
  ## get numCells for positive control
  par(fig=c(0,0.225,0.5,0.98))
  GR_BzCl= seq(-0.015,-0.005,0.0025)
  GR_DMSO= 0.03
  plot(1,type="l",xlim=c(0,120),ylim=c(0,100), las=1,xlab="", ylab="")
  title(xlab="Time (h)",line=2.5,cex.lab=0.8)
  title(ylab="Readout (aribitary unit)",line=2.2,cex.lab=0.8)
  title(main="Dynamic positive control,\nconstant negative control",cex.main=0.9,line=0.5)
  timeNum= c(1:120)
  colfunc <- colorRampPalette(c("red", "orange"))
  colors <- colfunc(length(GR_BzCl))
  
  for( i in 1:length(GR_BzCl)){
    lines((10*2^(timeNum*GR_BzCl[i])),col=colors[i])
  }
  lines((10*2^(timeNum*GR_DMSO)),col="green")
  #legend(3, 55, c("Growth rate in\nnegative control 0.03"), bty = "n",cex=0.8, lwd=2,lty=1,col="black")
  legend(0, 65, c("Negative control "), bty = "n",cex=0.7, lwd=2,lty=1,col="green")
  mtext("a", side=3,line=.75, at=-25,cex=1.25)
  
  par(fig=c(0.05,0.145,0.81,0.91), new= TRUE)
  #par(fig=c(0.465,0.56,0.495,0.575), new= TRUE)
  color.bar(colors, -0.015, -0.005, title='Growth rate in\npositive control', nticks=2)
  #subplot(color.bar(colors, 0.1, 0.8, title='Positive control', nticks=2),20,3,size=c(0.5,0.15),vadj=1,hadj=0.25)
  
  par(fig=c(0.25,0.475,0.5,0.98), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,3,1,colors)
  mtext("b", side=3,line=0.75, at= 0.06,cex=1.25)
  
  par(fig=c(0.5,0.725,0.5,0.98), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,2,1,colors)
  mtext("c", side=3,line=0.75, at=0.06,cex=1.25)
  
  par(fig=c(0.75,0.975,0.5,0.98), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,1,1,colors)
  mtext("d", side=3,line=0.75, at=0.06,cex=1.25)
  
  
  par(fig=c(0,0.225,0,0.48), new= TRUE)
  GR_DMSO= seq(0.01,0.055,0.01)
  GR_BzCl= -0.01
  ## get numCells for negative control
  plot(1,type="l",xlim=c(0,120),ylim=c(0,140), las=1,xlab="", ylab="")
  title(xlab="Time (h)",line=2.5,cex.lab=0.8)
  title(ylab="Readout (aribitary unit)",line=2.2,cex.lab=0.8)
  title(main="Dynamic negative control,\nconstant positive control",cex.main=0.9,line=0.5)
  timeNum= c(1:120)
  
  colfunc <- colorRampPalette(c("green4", "green"))
  colors <- colfunc(length(GR_DMSO))
  for( i in 1:length(GR_DMSO)){
    lines((10*2^(timeNum*GR_DMSO[i])),col=colors[i])
  }
  lines((10*2^(timeNum*GR_BzCl)),col="red")
  #legend(3, -3.5, c("Growth rate in\npositive control -0.01"), bty = "n",cex=0.8, lwd=2,lty=1,col="black")
  legend(0, 95, c("Positive control"), bty = "n",cex=0.7, lwd=2,lty=1,col="red")
  
  mtext("e", side=3,line=0.75, at=-25,cex=1.25)
  
  par(fig=c(0.05,0.145,0.31,0.41), new= TRUE)
  #par(fig=c(0.465,0.56,0.05,0.13), new= TRUE)
  color.bar(colors, 0.01, 0.055, title='Growth rate in\nnegative control', nticks=2)
  
  #subplot(color.bar(colors, 2, 8, title='Negative control', nticks=2),20,2,size=c(0.5,0.15),vadj=1,hadj=0.25)
  par(fig=c(0.25,0.475,0,0.48), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,3,2,colors)
  mtext("f", side=3,line=0.75, at=0.06,cex=1.25)
  par(fig=c(0.5,0.725,0,0.48), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,2,2,colors)
  mtext("g", side=3,line=0.75, at=0.06,cex=1.25)
  par(fig=c(0.75,0.975,0,0.48), new= TRUE)
  drawPlot(GR_BzCl,GR_DMSO,1,2,colors)
  mtext("h", side=3,line=0.75, at=0.06,cex=1.25)
  
}

getDefinitionPlot <- function(){
  par(fig=c(0,0.31,0,1))
  drawPRdefinition()
  
  par(fig=c(0.34,0.65,0,1), new= TRUE)
  GR_Drug= seq(-0.01,0.04,0.005)
  drawDrugPlot(GR_Drug,1)
  
  par(fig=c(0.67,0.98,0,1), new= TRUE)
  drawPRvalues(GR_Drug,1)
}

drawPlot <- function(GR_BzCl,GR_DMSO,method,control,colors){
  
  GR_DMSO1= 2^(GR_DMSO*72)
  GR_BzCl1= 2^(GR_BzCl*72)
  meth= ifelse(method==1,"NDR normalized value",ifelse(method==2,"GR normalized value","PI normalized value"))
  if(control==1){
    #colors <- rainbow(length(GR_BzCl1)) 
    looper <- GR_BzCl1
    rateBzCl <- GR_BzCl
    rateBzCl1 <- GR_BzCl1
    rateDMSO <- rep(GR_DMSO1,length(GR_BzCl1))
    titleText <- "Growth rate in\npositive control"
  }
  else{
    #colors <- terrain.colors(length(GR_DMSO1)) 
    looper <- GR_DMSO1
    rateBzCl <- rep(GR_BzCl,length(GR_DMSO))
    rateBzCl1 <- rep(GR_BzCl1,length(GR_DMSO1))
    rateDMSO <- GR_DMSO1
    titleText <- "Growth rate in\n negative control"
  }
  
  if (method == 3){
    plot(1,type="l",xlim=c(0.045,-0.015),ylim=c(0,100), xlab="", ylab="",las=1)
    title(xlab=expression("Drug-induced growth rate (h"^-1*")"),line=2.5,cex.lab=0.8)
    title(ylab=meth,line=2.1,cex.lab=0.8)
    xaxislab= seq(0.045,-0.015,by=-0.005)
  }
  else{
    plot(1,type="l",xlim=c(0.045,-0.015),ylim=c(-1.1,2), xlab="", ylab="",las=1)
    title(xlab=expression("Drug-induced growth rate (h"^-1*")"),line=2.5,cex.lab=0.8)
    title(ylab=meth,line=2.1,cex.lab=0.8)
    xaxislab= seq(0.045,-0.015,by=-0.005)
    polygon(c(xaxislab,rev(xaxislab)), c(rep(1,length(xaxislab)),rep(2,length(xaxislab))),
            col = "gray100", border = NA)
    polygon(c(xaxislab,rev(xaxislab)), c(rep(0,length(xaxislab)),rep(1,length(xaxislab))),
            col = "gray90", border = NA)
    polygon(c(xaxislab,rev(xaxislab)), c(rep(0,length(xaxislab)),rep(-1,length(xaxislab))),
            col = "gray70", border = NA)
    polygon(c(xaxislab,rev(xaxislab)), c(rep(-1,length(xaxislab)),rep(-2,length(xaxislab))),
            col = "gray50", border = NA)
  }
  
  for( i in 1:length(looper)){
    GR_Drug= seq(0.04,rateBzCl[i],-0.0005)#seq(rateBzCl[i],0.04,0.0005)
    GR_Drug1= 2^(GR_Drug*72)
    
    if (method == 1)
      growth_rate1= pmax(-1,((1-2^(log2(GR_Drug1)/log2(rateBzCl1[i]))) / (1-2^(log2(rateDMSO[i])/log2(rateBzCl1[i])))))
    else if (method == 2)
      growth_rate1= (2^(log2(GR_Drug1)/log2(rateDMSO[i]))-1)
    else{
      numDrug= GR_Drug1
      numBzCl= rateBzCl1[i]
      numDMSO= rateDMSO[i]
      growth_rate1= 100*(1-((numDMSO-numDrug)/(numDMSO-numBzCl)))
      #growth_rate1[growth_rate1 < 0]= 0
      #growth_rate1= -growth_rate1
    }
    lines((GR_Drug),(growth_rate1),lwd=1,col=colors[i])
  }
  if (method != 3){
    lines(c(-0.015,0.05),c(0,0),lwd=1,lty=3)
    lines(c(-0.015,0.045),c(1,1),lwd=1,lty=3)
    lines(c(-0.015,0.045),c(-1,-1),lwd=1,lty=3)
  }
  #legend(2, -0.2, looper, bty = "n",cex=1, lty=1,col=colors, title=titleText)
  
}

drawDrugPlot<- function(GR_Drug,plotNum){
  plot(1,type="l",xlim=c(0,120),ylim=c(-2,120), las=1,xlab="", ylab="")
  
  if (plotNum == 1){
    posBar= 0.4
    plotLeg= 'b'
    DMSO_points= log2(5)/(72*60)
    BzCl_points= log2(0.5)/72
    lines((DMSO_points),lwd=1,col="green")
    lines((BzCl_points),lwd=1,col="red")
    labcex=1
  }
  else if (plotNum == 2){
    posBar= 0.05
    plotLeg= 'a'
    labcex=0.8
  }
  title(xlab="Time (h)",line=2.5,cex.lab=labcex)
  title( ylab="Readout (aribitary unit)",line=2,cex.lab=labcex)
  title(main="Drug-induced effects",cex.main=0.9,line=0.5)
  timeNum= c(1:120)
  DMSO_points= (10*2^(timeNum*0.032))#log2(5)/(72*60)2
  BzCl_points= (10*2^(timeNum*-0.012))#log2(0.5)/72
  #GR_Drug= append(seq(0.1,0.9,0.1),seq(1,10,1)) #seq(0.1,8,by=0.1)
  #GR_Drug= seq(-0.05,0.05,0.01)
  #colors <- topo.colors(length(GR_Drug))
  colfunc <- colorRampPalette(c("blue", "skyblue"))
  colors <- colfunc(length(GR_Drug))
  for( i in 1:length(GR_Drug)){
    lines((10*2^(timeNum*(GR_Drug[i]))),col=colors[i],lwd=2)
  }
  
  
  mtext(plotLeg, side=3,line=0, at=-23,cex=1.25)
  #lines(c(0,120),c(0,0),lwd=1,lty=3)
  
  #par(fig=c(0.45,0.555,0.175,0.26),new= TRUE)
  par(fig=c(posBar,posBar+0.105,0.8,0.8+0.125),new= TRUE)
  color.bar(colors, -0.01, 0.04, title='Drug-induced\ngrowth rate', nticks=2)
  #subplot(color.bar(colors, 0.1, 8, title='Drug effect', nticks=2),20,2,size=c(1,0.25),vadj=1,hadj=0.25)
}

drawPRdefinition <- function(){
  plot(1,type="l",xlim=c(0,120),ylim=c(-2,120), las=1,xlab="", ylab="")
  title(xlab="Time (h)",line=2.5)
  title(ylab="Readout (aribitary unit)",line=2)
  title(main="Definition of NDR metric",cex.main=0.9,line=0.5)
  DMSO_points= (10*2^(1:120*0.032))#log2(5)/(72*60)2
  Drug_points= (10*2^(1:120*0.02))#log2(2)/72
  BzCl_points= (10*2^(1:120*-0.012))#log2(0.5)/72
  ExpBzCl_points= (10*2^(1:120*-0.2))
  
  lines((DMSO_points),lwd=2,col="green")
  lines((Drug_points),lwd=2,col="#3652F7")
  lines((BzCl_points),lwd=2,col="red")
  lines(ExpBzCl_points,lwd=2,lty=3,col="red")
  #lines(c(0,120),c(0,0),lwd=1,lty=3,col="red")
  lines(c(80,80),c(-2.5,(DMSO_points[80])),lwd=1,lty=2)
  #arrows(x0= 72,y0=log2(BzCl_points[72]),x1=72,y1=log2(Drug_points[72]),lty=2,lwd=1,col="black",length=0.15)
  #arrows(x0= 90,y0=log2(BzCl_points[90]),x1=90,y1=log2(DMSO_points[90]),lty=2,lwd=1,col="black",length=0.15)
  points(80,(BzCl_points[80]),type="p",pch=15)
  points(80,(Drug_points[80]),type="p",pch=15)
  points(80,(DMSO_points[80]),type="p",pch=15)
  
  mtext(expression(paste("N= ","N"[80]*"/N"[0])), side=3,line=-11, at=97,cex=0.75)
  mtext(expression(paste("D= ","D"[80]*"/D"[0])), side=3,line=-15, at=97,cex=0.75)
  mtext(expression(paste("P= ","P"[80]*"/P"[0])), side=3,line=-18, at=97,cex=0.75)
  
  mtext(expression(paste("NDR = max(-1,",frac("1 - 2"^("log"[2]*"D/log"[2]*"P"),"1 - 2"^("log"[2]*"N/log"[2]*"P")),")")), side=3,line=-9, at=40,cex=0.75)
  
  mtext("a", side=3,line=0, at=-25,cex=1.25)
  legend(2, 120, c("Negative control ","Drug-induced condition","Observed positive control","Expected positive control"), bty = "n",cex=0.75, lty=c(1,1,1,3),col=c("green","#3652F7","red","red"))
  
}

drawPRvalues <- function(GR_in,method){
  if(method== 1){
    GR_Drug1= 2^(GR_in*72)
    GR_DMSO= 2^(0.03*72)
    GR_BzCl= 2^(-0.01*72)
  }
  else if(method==2){
    GR_Drug1= 2^(-0.0001*72)
    GR_DMSO= 2^(GR_in*72)
    GR_BzCl= 2^(-0.01*72)
  }
  else{
    GR_Drug1= 2^(0.01*72)
    GR_DMSO= 2^(0.03*72)
    GR_BzCl= 2^(GR_in*72)
  }
  
  
  plot(1,type="l",xlim=c(max(GR_in)+0.005,min(GR_in)-0.005),ylim=c(-1.1,1.1), las=1,xlab="", ylab="")
  #plot(1,type="l",xlim=c(0.045,-0.015),ylim=c(-2,2), xlab="", ylab="",las=1)
  
  title(xlab=expression("Drug-induced growth rate (h"^-1*")"), line= 2.5)
  title(ylab="NDR normalized value",line=2)
  title(main="Drug response quantification",cex.main=0.9,line=0.5)
  xaxislab= seq(max(GR_in)+0.005,min(GR_in)-0.005,by=-0.005)
  polygon(c(xaxislab,rev(xaxislab)), c(rep(1,  length(xaxislab)),rep(2,length(xaxislab))),
          col = "gray100", border = NA)
  polygon(c(xaxislab,rev(xaxislab)), c(rep(0,length(xaxislab)),rep(1,length(xaxislab))),
          col = "gray90", border = NA)
  polygon(c(xaxislab,rev(xaxislab)), c(rep(0,length(xaxislab)),rep(-1,length(xaxislab))),
          col = "gray70", border = NA)
  polygon(c(xaxislab,rev(xaxislab)), c(rep(-1,length(xaxislab)),rep(-2,length(xaxislab))),
          col = "gray50", border = NA)
  growth_rate1= pmax(-1,((1-2^(log2(GR_Drug1)/log2(GR_BzCl))) / (1-2^(log2(GR_DMSO)/log2(GR_BzCl)))))
  
  #growth_rate1= ((1-2^(0.72/-0.72)) / (1-2^(2.16/-0.72)))
  
  colfunc <- colorRampPalette(c("blue", "skyblue"))
  colors <- colfunc(length(GR_in))
  lines(GR_in,(growth_rate1),lwd=1,type="p",pch = 21,bg=colors,col=colors,cex=2)
  
  lines(c(-0.015,0.045),c(0,0),lwd=1,lty=3)
  lines(c(-0.015,0.045),c(1,1),lwd=1,lty=3)
  lines(c(-0.015,0.045),c(-1,-1),lwd=1,lty=3)
  mtext("c", side=3,line=0, at=-0.03,cex=1.25)
  
  mtext("Growth-stimulative", side=3,line=-1, at=0.02,cex=0.7)
  mtext("Effective", side=3,line=-6.5, at=0.02,cex=0.7)
  mtext("Lethal", side=3,line=-15.5, at=0.02,cex=0.7)
  #mtext("Complete killing", side=3,line=-17, at=0.02,cex=0.7)
}

getResult <- function(pinData){
  result= data.frame(Condition=character(),DSS= double(),IC50=double())
  numConc= 5
  sequence=seq(from=1, to= dim(pinData)[1], by= numConc)
  plotList= list("plot")
  k= 1;
  
  for (j in 1:length(sequence)){
    for (i in 4:9){
      GI= pinData[sequence[j]:(sequence[j]+numConc-1),i]
      conc= as.numeric(pinData[sequence[j]:(sequence[j]+numConc-1),3])
      drugResponse= data.frame(conc=(conc), effect=GI)
      resDss= CALC_IC50_EC50_DSS(drugResponse,2)
      result= rbind(result,data.frame(Drug=pinData[sequence[j],1],Method= pinData[sequence[j],2],Time= colnames(pinData)[i], DSS= resDss$data$dssScore, IC50=resDss$data$IC50,goodness=sqrt(sum((GI-resDss$line)^2/length(GI)))))
      plotList[[k]]=resDss$plot
      k=k+1;
    }
  }
  result$Method<- factor(result$Method,
                         levels = c("NDR", "GR", "PI"))
  return(result)
}

plotDrugresponse <- function(HDQP1_cellNumbers,HDQP1_Act,MB231_Act,MIA_PAC_Act,MCF7_Act,MB361_Act,drugNames){
  lineWidth= 2
  par(mfrow=c(2,8),mar=c(2,1, 3, 2),oma = c(2, 3, 0.2, 0.1))
  
  for (i in 1:length(drugNames)){
    
    drugName= as.character(drugNames[i])
    #drugSymbol= as.character(drugSymbols[i])
    conc= sort(as.numeric(unlist(subset(HDQP1_cellNumbers,DRUG_NAME==drugName,select=c("Conc")))))
    #par(mfrow=c(1,2),mar=c(1,1, 5, 2),oma = c(2, 2, 0.2, 0.1))
    yLim= c(-20,120)
    plot(log10(conc),100*as.numeric(MIA_PAC_Act$pactivation_PI[MIA_PAC_Act$pactivation_PI$Drug==drugName,3:7]),ylim=yLim,type="o",col="blue",xlab="Conc (nM)",ylab="Viability",lwd=lineWidth,xaxt = "n",yaxt="n")
    lines(log10(conc),100*as.numeric(MB231_Act$pactivation_PI[MB231_Act$pactivation_PI$Drug==drugName,3:7]),ylim=yLim,col="red",lwd=lineWidth,type= "o")
    lines(log10(conc),100*as.numeric(MCF7_Act$pactivation_PI[MCF7_Act$pactivation_PI$Drug==drugName,3:7]),ylim=yLim,col="chocolate1",lwd=lineWidth,type= "o")
    lines(log10(conc),100*as.numeric(HDQP1_Act$pactivation_PI[HDQP1_Act$pactivation_PI$Drug==drugName,3:7]),ylim=yLim,col="green",lwd=lineWidth,type= "o")
    lines(log10(conc),100*as.numeric(MB361_Act$pactivation_PI[MB361_Act$pactivation_PI$Drug==drugName,3:7]),ylim=yLim,col="black",lwd=lineWidth,type= "o")
    lines(log10(conc),rep(100,length(conc)),ylim=yLim,lty=3)
    lines(log10(conc),rep(0,length(conc)),ylim=yLim,lty=3)
    axis(1, at=log10(conc[1]):log10(conc[5]), labels=conc)
    if(i== 1 | i == 3){
      axis(2, at=c(0,50,100), labels=c(0.0,50,100),las=1)
    }
    
    title("Viability", outer=F, line = 0.2,cex.main=0.9) 
    #mtext("Title for Two Plots", outer = F, cex = 1.5,line=-2)
    mtext(paste(drugName,sep=""), side=3, line=0.9, at=5)
    #text(x=3,y=1.5, drugName, pos=3, offset=.2, cex=1.5)
    
    
    plot(log10(conc),100*as.numeric(MIA_PAC_Act$pinhibition[MIA_PAC_Act$pinhibition$Drug==drugName,3:7]),ylim=yLim,type="o",col="blue",xlab="Conc (nM)",ylab ="Toxicity",lwd=lineWidth,xaxt = "n",yaxt="n")
    lines(log10(conc),100*as.numeric(MB231_Act$pinhibition[MB231_Act$pinhibition$Drug==drugName,3:7]),ylim=yLim,col="red",lwd=lineWidth,type= "o")
    lines(log10(conc),100*as.numeric(MCF7_Act$pinhibition[MCF7_Act$pinhibition$Drug==drugName,3:7]),ylim=yLim,col="chocolate1",lwd=lineWidth,type= "o")
    lines(log10(conc),100*as.numeric(HDQP1_Act$pinhibition[HDQP1_Act$pinhibition$Drug==drugName,3:7]),ylim=yLim,col="green",lwd=lineWidth,type= "o")
    lines(log10(conc),100*as.numeric(MB361_Act$pinhibition[MB361_Act$pinhibition$Drug==drugName,3:7]),ylim=yLim,col="black",lwd=lineWidth,type= "o")
    lines(log10(conc),rep(100,length(conc)),ylim=yLim,lty=3)
    lines(log10(conc),rep(0,length(conc)),ylim=yLim,lty=3)
    axis(1, at=log10(conc[1]):log10(conc[5]), labels=conc)
    title("Toxicity", outer=F, line = 0.2,cex.main=0.9) 
    
    yLim= c(-1.2,1.2)
    plot(log10(conc),as.numeric(MIA_PAC_Act$pactivation_GR[MIA_PAC_Act$pactivation_GR$Drug==drugName,3:7]),ylim=yLim,type="o",col="blue",xlab="Conc (nM)",ylab="Viability",lwd=2.5,xaxt = "n",las=1)
    lines(log10(conc),as.numeric(MB231_Act$pactivation_GR[MB231_Act$pactivation_GR$Drug==drugName,3:7]),ylim=yLim,col="red",lwd=lineWidth,type= "o")
    lines(log10(conc),as.numeric(HDQP1_Act$pactivation_GR[HDQP1_Act$pactivation_GR$Drug==drugName,3:7]),ylim=yLim,col="green",lwd=lineWidth,type= "o")
    lines(log10(conc),as.numeric(MB361_Act$pactivation_GR[MB361_Act$pactivation_GR$Drug==drugName,3:7]),ylim=yLim,col="black",lwd=lineWidth,type= "o")
    lines(log10(conc),as.numeric(MCF7_Act$pactivation_GR[MCF7_Act$pactivation_GR$Drug==drugName,3:7]),ylim=yLim,col="chocolate1",lwd=lineWidth,type= "o")
    lines(log10(conc),rep(1,length(conc)),ylim=yLim,lty=3)
    lines(log10(conc),rep(0,length(conc)),ylim=yLim,lty=3)
    lines(log10(conc),rep(-1,length(conc)),ylim=yLim,lty=3)
    axis(1, at=log10(conc[1]):log10(conc[5]), labels=conc)
    title("GR", outer=F, line = 0.2,cex.main=0.9) 
    
    plot(log10(conc),as.numeric(MIA_PAC_Act$pactivation_NDR[MIA_PAC_Act$pactivation_NDR$Drug==drugName,3:7]),ylim=yLim,type="o",col="blue",xlab="Conc (nM)",ylab ="Toxicity",lwd=lineWidth,xaxt = "n",yaxt="n")
    lines(log10(conc),as.numeric(MB231_Act$pactivation_NDR[MB231_Act$pactivation_NDR$Drug==drugName,3:7]),ylim=yLim,col="red",lwd=lineWidth,type= "o")
    lines(log10(conc),as.numeric(MCF7_Act$pactivation_NDR[MCF7_Act$pactivation_NDR$Drug==drugName,3:7]),ylim=yLim,col="chocolate1",lwd=lineWidth,type= "o")
    lines(log10(conc),as.numeric(HDQP1_Act$pactivation_NDR[HDQP1_Act$pactivation_NDR$Drug==drugName,3:7]),ylim=yLim,col="green",lwd=lineWidth,type= "o")
    lines(log10(conc),as.numeric(MB361_Act$pactivation_NDR[MB361_Act$pactivation_NDR$Drug==drugName,3:7]),ylim=yLim,col="black",lwd=lineWidth,type= "o")
    lines(log10(conc),rep(1,length(conc)),ylim=yLim,lty=3)
    lines(log10(conc),rep(0,length(conc)),ylim=yLim,lty=3)
    axis(1, at=log10(conc[1]):log10(conc[5]), labels=conc)
    title("NDR", outer=F, line = 0.2,cex.main=0.9) 
    
  }
  mtext('Concentration (nM)', side = 1, outer = T, line = 1)
  mtext('Fraction of readout', side = 2, outer = T, line = 1)
}

getRMSDplot <- function(resTable){
  ubound= 30
  df1 <- data.frame(a = c(1, 1:3,3), b = c(25, 26, 26, 26, 25))
  #df2 <- data.frame(a = c(1, 1,2, 2), b = c(19, 20, 20, 19))
  df3 <- data.frame(a = c(2, 2, 3, 3), b = c(28, 29, 29, 28))
  
  
  tempTable= resTable[resTable$Cell_line=="MIA PaCa-2" & resTable$DSS !=0 & resTable$GRDSS !=0 & resTable$oldDSS !=0,c("DrugID","oldRMSD","GRRMSD","RMSD")]
  colnames(tempTable) <- c("DrugID","PI","GR","PR")
  dfs <- stack(tempTable)
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="PI"])
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="GR"])
  p1 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape= NA)+ scale_x_discrete(limits=c("PI","GR","PR")) + xlab("")+ ylab("RMSD")+ theme_classic()+ ylim(0,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2, y = 26.1, label = "***", size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2.5, y = 29.5, label = "***", size = 5)
  
  
  tempTable= resTable[resTable$Cell_line=="MDA-MB-231" & resTable$DSS !=0 & resTable$GRDSS !=0 & resTable$oldDSS !=0,c("DrugID","oldRMSD","GRRMSD","RMSD")]
  colnames(tempTable) <- c("DrugID","PI","GR","PR")
  dfs <- stack(tempTable)
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="PI"])
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="GR"])
  p2 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape = NA)+ scale_x_discrete(limits=c("PI","GR","PR"))+ xlab("")+ ylab("")+ theme_classic()+ ylim(0,ubound)+ geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2, y = 26.1, label = "***", size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2.5, y = 29.5, label = "***", size = 5)
  
  tempTable= resTable[resTable$Cell_line=="MCF-7-R1" & resTable$DSS !=0 & resTable$GRDSS !=0 & resTable$oldDSS !=0,c("DrugID","oldRMSD","GRRMSD","RMSD")]
  colnames(tempTable) <- c("DrugID","PI","GR","PR")
  dfs <- stack(tempTable)
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="PI"])
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="GR"])
  p3 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape = NA)+ scale_x_discrete(limits=c("PI","GR","PR"))+ xlab("") + ylab("")+ theme_classic()+ ylim(0,ubound)+ geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2, y = 26.1, label = "***", size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2.5, y = 29.5, label = "***", size = 5)
  
  tempTable= resTable[resTable$Cell_line=="HDQ-P1" & resTable$DSS !=0 & resTable$GRDSS !=0 & resTable$oldDSS !=0,c("DrugID","oldRMSD","GRRMSD","RMSD")]
  colnames(tempTable) <- c("DrugID","PI","GR","PR")
  dfs <- stack(tempTable)
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="PI"])
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="GR"])
  p4 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape = NA)+ scale_x_discrete(limits=c("PI","GR","PR"))+ xlab("")+ ylab("")+ theme_classic()+ ylim(0,ubound)+ geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2, y = 26.1, label = "*", size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2.5, y = 29.5, label = "***", size = 5)
  
  tempTable= resTable[resTable$Cell_line=="MDA-MB-361" & resTable$DSS !=0 & resTable$GRDSS !=0 & resTable$oldDSS !=0,c("DrugID","oldRMSD","GRRMSD","RMSD")]
  colnames(tempTable) <- c("DrugID","PI","GR","PR")
  dfs <- stack(tempTable)
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="PI"])
  wilcox.test(dfs$values[dfs$ind=="PR"],dfs$values[dfs$ind=="GR"])
  p5 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape = NA) + scale_x_discrete(limits=c("PI","GR","PR"))+ xlab("") + ylab("")+ theme_classic()+ ylim(0,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2, y = 26.1, label = "**", size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = 2.5, y = 29.5, label = "***", size = 5)
  
  newList <- list(p1,p2,p3,p4,p5)
  return(newList)
}

getDistToZero <- function(resTable){
  ubound=0.3
  df1 <- data.frame(a = c(1, 1:3,3), b = c(0.24, 0.25, 0.25, 0.25, 0.24))
  #df2 <- data.frame(a = c(1, 1,2, 2), b = c(19, 20, 20, 19))
  df3 <- data.frame(a = c(2, 2, 3, 3), b = c(0.28, 0.29, 0.29, 0.28))
  
  tempTable <- data.frame(Drug= MIA_PAC_Act$pactivation_PI[MIA_PAC_Act$pactivation_PI$Drug %in% drugNames,1],PI=(1-MIA_PAC_Act$pactivation_PI[MIA_PAC_Act$pactivation_PI$Drug %in% drugNames,3]),GR=(1-MIA_PAC_Act$pactivation_GR[MIA_PAC_Act$pactivation_GR$Drug %in% drugNames,3]),NDR=(1-MIA_PAC_Act$pactivation_NDR[MIA_PAC_Act$pactivation_NDR$Drug %in% drugNames,3]))
  dfs <- stack(tempTable)
  t1 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="PI"])
  t2 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="GR"])
  sigText1 <- ifelse(t1$p.value < 0.005,"***",ifelse(t1$p.value < 0.05,"**","*"))
  sigText2 <- ifelse(t2$p.value < 0.005,"***",ifelse(t2$p.value < 0.05,"**","*"))
  p1 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape= NA)+ scale_x_discrete(limits=c("PI","GR","NDR")) + xlab("")+ ylab("Lowest concentration distance to zero")+ theme_classic()+ ylim(-ubound,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df1$a), y = df1$b[2]+0.01, label = sigText1, size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df3$a), y = df3$b[2]+0.01, label = sigText2, size = 5)
  
  tempTable <- data.frame(Drug= MB231_Act$pactivation_NDR[,1],PI=(1-MB231_Act$pactivation_PI[,3]),GR=(1-MB231_Act$pactivation_GR[,3]),NDR=(1-MB231_Act$pactivation_NDR[,3]))
  dfs <- stack(tempTable)
  t1 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="PI"])
  t2 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="GR"])
  sigText1 <- ifelse(t1$p.value < 0.005,"***",ifelse(t1$p.value < 0.05,"**","*"))
  sigText2 <- ifelse(t2$p.value < 0.005,"***",ifelse(t2$p.value < 0.05,"**","*"))
  p2 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape= NA)+ scale_x_discrete(limits=c("PI","GR","NDR")) + xlab("")+ ylab("")+ theme_classic()+ ylim(-ubound,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df1$a), y = df1$b[2]+0.01, label = sigText1, size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df3$a), y = df3$b[2]+0.01, label = sigText2, size = 5)
  
  tempTable <- data.frame(Drug= MCF7_R1_Act$pactivation_NDR[,1],PI=(1-MCF7_R1_Act$pactivation_PI[,3]),GR=(1-MCF7_R1_Act$pactivation_GR[,3]),NDR=(1-MCF7_R1_Act$pactivation_NDR[,3]))
  dfs <- stack(tempTable)
  t1 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="PI"])
  t2 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="GR"])
  sigText1 <- ifelse(t1$p.value < 0.005,"***",ifelse(t1$p.value < 0.05,"**","*"))
  sigText2 <- ifelse(t2$p.value < 0.005,"***",ifelse(t2$p.value < 0.05,"**","*"))
  p3 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape= NA)+ scale_x_discrete(limits=c("PI","GR","NDR")) + xlab("")+ ylab("")+ theme_classic()+ ylim(-ubound,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df1$a), y = df1$b[2]+0.01, label = sigText1, size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df3$a), y = df3$b[2]+0.01, label = sigText2, size = 5)
  
  tempTable <- data.frame(Drug= HDQP1_Act$pactivation_NDR[,1],PI=(1-HDQP1_Act$pactivation_PI[,3]),GR=(1-HDQP1_Act$pactivation_GR[,3]),NDR=(1-HDQP1_Act$pactivation_NDR[,3]))
  dfs <- stack(tempTable)
  t1 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="PI"])
  t2 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="GR"])
  sigText1 <- ifelse(t1$p.value < 0.005,"***",ifelse(t1$p.value < 0.05,"**","*"))
  sigText2 <- ifelse(t2$p.value < 0.005,"***",ifelse(t2$p.value < 0.05,"**","*"))
  p4 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape= NA)+ scale_x_discrete(limits=c("PI","GR","NDR")) + xlab("")+ ylab("")+ theme_classic()+ ylim(-ubound,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df1$a), y = df1$b[2]+0.01, label = sigText1, size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df3$a), y = df3$b[2]+0.01, label = sigText2, size = 5)
  
  tempTable <- data.frame(Drug= MB361_Act$pactivation_NDR[,1],PI=(1-MB361_Act$pactivation_PI[,3]),GR=(1-MB361_Act$pactivation_GR[,3]),NDR=(1-MB361_Act$pactivation_NDR[,3]))
  dfs <- stack(tempTable)
  t1 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="PI"])
  t2 <- var.test(dfs$values[dfs$ind=="NDR"],dfs$values[dfs$ind=="GR"])
  sigText1 <- ifelse(t1$p.value < 0.005,"***",ifelse(t1$p.value < 0.05,"**","*"))
  sigText2 <- ifelse(t2$p.value < 0.005,"***",ifelse(t2$p.value < 0.05,"**","*"))
  p5 <- ggplot(dfs , aes(x=ind,y=values))+geom_boxplot(outlier.shape= NA)+ scale_x_discrete(limits=c("PI","GR","NDR")) + xlab("")+ ylab("")+ theme_classic()+ ylim(-ubound,ubound) + geom_line(data = df1, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df1$a), y = df1$b[2]+0.01, label = sigText1, size = 5) +  geom_line(data = df3, aes(x = a, y = b),lwd=0.25) + annotate("text", x = mean(df3$a), y = df3$b[2]+0.01, label = sigText2, size = 5)
  
  newList <- list(p1,p2,p3,p4,p5)
  return(newList)
}

plotActivityCurve <- function(pactivation_NDR,pinhibition){
  drugTypeNames = c("Lethal","Effective","Non-effective","Growth-stimulative")
  par(mfrow=c(1,4),oma = c(3, 2.25, 0.2, 2.25),mar=c(0, 1, 2, 1))
  ylimNDR= c(-1.2,1.2)
  ylimTox= c(-10,110)
  for (i in 1:length(drugTypeNames)){
    plot(colMeans(pactivation_NDR[pactivation_NDR$Type==drugTypeNames[i],3:7]),ylim=ylimNDR,type="l",col="blue", axes=FALSE, xlab="", ylab="",lwd=2.5,main=drugTypeNames[i])
    if(i == 1)
      axis(2, ylim=ylimNDR,col.axis="blue",las=1)  ## las=1 makes horizontal labels
    box()
    par(new=TRUE)
    plot(100*colMeans(pinhibition[pinhibition$Type==drugTypeNames[i],3:7]),col="red",lwd=2.5,  xlab="", ylab="", ylim=ylimTox, axes=FALSE, type="l")
    if (i==4)
      axis(4, ylim=ylimTox,col.axis="red",las=1)
    axis(1,1:5)
  }
  
  mtext('Concentration points', side = 1, outer = TRUE, line = 2,cex=0.8)
  mtext('NDR', side = 2, outer = TRUE, line = 1.1,cex=0.8)
  mtext('Toxicity', side = 4, outer = TRUE, line = 1.2,cex=0.8)
}

plotSegment <- function(x,y,xpos,ypos){
  lines(x:(x+1), rep(y,2))
  lines(rep(x,3), (y-2):y)
  lines(rep(x+1,3), (y-2):y)
  mtext("***", side=3,line=ypos, at=xpos,cex=1.5)
}
