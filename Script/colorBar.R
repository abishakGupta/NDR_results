color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  op <- par(mar = c(1,1,1.5,1),mgp=c(3, .3, 0))
  #dev.new(width=1.75, height=5)
  plot(c(min,max),c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
  title(title,cex.main=0.7,font.main=1,line = 0.15)
  axis(1, ticks, las=1,cex.axis=0.7)
  for (i in 1:(length(lut)-1)) {
    #y = (i-1)/scale + min
    x= (i-1)/scale + min
    #rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    rect(x,0,x+1/scale,1, col=lut[i], border=NA)
  }	
  par(op)
}
