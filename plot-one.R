########## plot function #############
plot_Func <- function(temp.x,temp.y,sens.loc,EStim_d,ylim,xlim){
  sens.loc = data.frame(sens.loc)
  
  # temp plot
  plot(temp.x,temp.y,col="red",cex = 0.7,pch = 19,ylim = ylim,
       xaxt = 'n',yaxt ='n',xlab = "",ylab = "")
  par(new = T) 
  
  # senser plot
  plot(sens.loc,pch = 19,cex = 1,xlab = "",ylab = "",ylim = ylim, xaxt = 'n',yaxt ='n')
  text(sens.loc,labels = rownames(sens.loc),pos = 3,col="black")  #図が重なる
  
}
plot_Func(temp.x,temp.y,sens.loc,dat1,ylim = c(-1.5,1.7),xlim = c(0,3))
