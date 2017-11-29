plot.circle <- function(x, y, r){
  theta <- seq(-pi, pi, length=100)
  points(x + r*cos(theta), y + r*sin(theta), type="l", asp=1)
}
plot(c(-3,3), c(-3,3), type="n", asp=1,xaxt = "n",yaxt = "n",ylab = "",xlab = "") # 作図領域を用意
plot.circle(0, -1, 2)       
plot.circle(-1.55, 1, 1.2) 
plot.circle( 0.8, 1.6, 1.3) 

# 中心のplot
par(new=T)
plot(0,-1,col = "red",pch = 19,ylim = c(-3,3),xlim = c(-3,3),xaxt = "n",yaxt = "n",ylab = "",xlab = "")
par(new=T)
plot(-1.1,0.95,col = "red",pch = 19,ylim = c(-3,3),xlim = c(-3,3),xaxt = "n",yaxt = "n",ylab = "",xlab = "")
par(new=T)
plot(0.58,1.55,col = "red",pch = 19,ylim = c(-3,3),xlim = c(-3,3),xaxt = "n",yaxt = "n",ylab = "",xlab = "")
