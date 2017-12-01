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

pos  = c(5,5)
s1 = c(2,2)
s2 = c(0,3)
s3 = c(1,0)
s4 = c(4,4)
d1 = sqrt(sum((pos-s1)^2))
d2 = sqrt(sum((pos-s2)^2))
d3 = sqrt(sum((pos-s3)^2))
d4 = sqrt(sum((pos-s4)^2))
sensor.loc = rbind(s1,s2,s3,s4)
r = c(d1,d2,d3,d4)
rec.loc <- loc.GDM(sensor.loc,r,c(2,3),1e-4,1e-5,0.1,0.01)
