loc.GDM <- function(sensor.loc, r, loc.init, h, lambda, moment, tol){
  # locating a position of a receiver
  #-----------------------------------------
  # input argument
  #  sensor.loc: location of sensors
  #  r: distances between rec & sensors
  #  loc.init: initial guess
  #  h: parameter of numerical differentiation
  #  lambda: step size
  #  moment: weight for momentum
  #  tol: tolerance
  #-----------------------------------------
  grad = rep(10000,2)
  v = c(0,0)
  loc = loc.init
  while (max(abs(grad))>tol){
    # gradient for x
    loc.p = c(loc[1] + h, loc[2])
    plus = sum(((loc.p[1]-sensor.loc[,1])^2 + (loc.p[2]-sensor.loc[,2])^2 -
                  r^2)^2)
    loc.m = c(loc[1]- h, loc[2])
    minus = sum(((loc.m[1]-sensor.loc[,1])^2 + (loc.m[2]-sensor.loc[,2])^2
                 - r^2)^2)
    grad[1] = (plus-minus)/(2*h)
    # gradient for y
    loc.p = c(loc[1], loc[2] + h)
    plus = sum(((loc.p[1]-sensor.loc[,1])^2 + (loc.p[2]-sensor.loc[,2])^2 -
                  r^2)^2)
    loc.m = c(loc[1], loc[2] - h)
    minus = sum(((loc.m[1]-sensor.loc[,1])^2 + (loc.m[2]-sensor.loc[,2])^2
                 - r^2)^2)
    grad[2] = (plus-minus)/(2*h)
    v = moment*v - lambda*grad
    loc = loc + v
  }
  return(loc)
}


# loop
rec.locfun <- function(sens.zahyouD){
  n.data = nrow(route);
  temp.location = array(0,dim = c(4,2,n.data))
  r = matrix(0,nrow = 4,ncol = n.data)
  rec.loc = matrix(0,nrow = n.data,ncol = 2)
  t1 = matrix(0,nrow = n.data,ncol = 2)
  t2 = matrix(0,nrow = n.data,ncol = 2)
  t3 = matrix(0,nrow = n.data,ncol = 2)
  t4 = matrix(0,nrow = n.data,ncol = 2)
  d1 = rep(0,n.data)
  d2 = rep(0,n.data)
  d3 = rep(0,n.data)
  d4 = rep(0,n.data)
  for(i in 1:n.data){
    t1[i,] = c(sens.zahyouD$X.1[i],sens.zahyouD$Y.1[i])
    t2[i,] = c(sens.zahyouD$X.2[i],sens.zahyouD$Y.2[i])
    t3[i,] = c(sens.zahyouD$X.3[i],sens.zahyouD$Y.3[i])
    t4[i,] = c(sens.zahyouD$X.4[i],sens.zahyouD$Y.4[i])
    d1[i] = sens.zahyouD$D.1[i]
    d2[i] = sens.zahyouD$D.2[i]
    d3[i] = sens.zahyouD$D.3[i]
    d4[i] = sens.zahyouD$D.4[i]
    temp.location[,,i] = rbind(t1[i,],t2[i,],t3[i,],t4[i,])
    r[,i] = c(d1[i],d2[i],d3[i],d4[i])
  }
  for(i in 1:n.data){
    rec.loc[i,] <- loc.GDM(temp.location[,,i],t(r[,i]),c(0,1),1e-4,1e-2,0.1,0.01)
  }
  return(rec.loc)
}  
rec.loc = rec.locfun(sens.zahyouD)
#system.time(rec.locfun(sens.zahyouD))


# plot
plot(rec.loc,col= "red",xlim=c(-0.5,3.5),ylim=c(-1.5,2),
     pch = 15,ylab="",xlab="",xaxt = 'n',yaxt = 'n')
text(rec.loc,pos = 3,col="black",cex=1)
par(new=T)
plot(temp.x,temp.y,col="blue",type='l',cex = 0.7,pch = 19,xlim=c(-0.5,3.5),ylim=c(-1.5,2),
     xlab = "",ylab = "")




###############################################
######### rec.loc と route の距離 #############
Estim_d2 <- function(rec.loc,route){
  
  # initialization
  n.data = nrow(route)
  distance2 = rep(0,n.data)
  
  # loop
  for (i_loop in 1:n.data){
    distance2[i_loop] = dist(rbind(rec.loc[i_loop,],route[i_loop,]))
  }
  return(distance2)
}
D2 = Estim_d2(rec.loc,route)
plot(D2,ylab = "distance",xlab = "",pch = 19)
var(D2)
