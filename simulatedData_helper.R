
source("colorBar.r")
getSpectrumPlot <- function(){
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
  meth= ifelse(method==1,"PR normalized value",ifelse(method==2,"GR normalized value","PI normalized value"))
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
    plot(1,type="l",xlim=c(0.045,-0.015),ylim=c(-2,2), xlab="", ylab="",las=1)
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
      growth_rate1= ((1-2^(log2(GR_Drug1)/log2(rateBzCl1[i]))) / (1-2^(log2(rateDMSO[i])/log2(rateBzCl1[i]))))
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
  title(main="Definition of PR normalization",cex.main=0.9,line=0.5)
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

  mtext(expression(paste("PR = ",frac("1 - 2"^("log"[2]*"D/log"[2]*"P"),"1 - 2"^("log"[2]*"N/log"[2]*"P")))), side=3,line=-9, at=40,cex=0.75)
  
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
  

  plot(1,type="l",xlim=c(max(GR_in)+0.005,min(GR_in)-0.005),ylim=c(-2,2), las=1,xlab="", ylab="")
  #plot(1,type="l",xlim=c(0.045,-0.015),ylim=c(-2,2), xlab="", ylab="",las=1)
  
  title(xlab=expression("Drug-induced growth rate (h"^-1*")"), line= 2.5)
  title(ylab="PR normalized value",line=2)
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
  growth_rate1= ((1-2^(log2(GR_Drug1)/log2(GR_BzCl))) / (1-2^(log2(GR_DMSO)/log2(GR_BzCl))))
  
  #growth_rate1= ((1-2^(0.72/-0.72)) / (1-2^(2.16/-0.72)))
  
  colfunc <- colorRampPalette(c("blue", "skyblue"))
  colors <- colfunc(length(GR_in))
  lines(GR_in,(growth_rate1),lwd=1,type="p",pch = 21,bg=colors,col=colors,cex=2)
  
  lines(c(-0.015,0.045),c(0,0),lwd=1,lty=3)
  lines(c(-0.015,0.045),c(1,1),lwd=1,lty=3)
  lines(c(-0.015,0.045),c(-1,-1),lwd=1,lty=3)
  mtext("c", side=3,line=0, at=-0.03,cex=1.25)
  
  mtext("Growth-stimulative", side=3,line=-3, at=0.02,cex=0.7)
  mtext("Effective", side=3,line=-8, at=0.02,cex=0.7)
  mtext("Lethal", side=3,line=-13, at=0.02,cex=0.7)
  mtext("Complete killing", side=3,line=-17, at=0.02,cex=0.7)
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
                          levels = c("PR", "GR", "PI"))
  return(result)
}

