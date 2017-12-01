install.packages("RColorBrewer", dependencies=T)
library(RColorBrewer)

pos  = c(3,3)
s1 = c(2,2)
s2 = c(0,3)
s3 = c(1,0)
s4 = c(4,4)
d1 = sqrt(sum((pos-s1)^2))
d2 = sqrt(sum((pos-s2)^2))
d3 = sqrt(sum((pos-s3)^2))
d4 = sqrt(sum((pos-s4)^2))


circle =function(x, y, radius,border = NULL, col = NA, lty = 1, 
                 density = NULL,lwd = 1){
  angles=seq(0,2*pi,0.01)
  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  lines(xv,yv,type = 'l',col=color,lwd=2)
  polygon(xv, yv, border = border, col = col, lty = lty, 
            density = density, lwd = lwd)
}

Blue = paste(brewer.pal(8, "Blues")[1], "AA", sep="")
Purple = paste(brewer.pal(8, "Purples")[2], "AA", sep="")
Yelow = paste(brewer.pal(8, "YlOrBr")[1], "AA", sep="")
Gray = paste(brewer.pal(8, "Greys")[2], "AA", sep="")

plot(pos[1],pos[2],pch=20,col='orange',xlim=c(-1,5),ylim=c(-1,5),cex=5)
circle(s3[1],s3[2],d3,border=color[3],col=Yelow,lwd=2)
circle(s2[1],s2[2],d2,border=color[2],col=Purple,lwd=2)
circle(s4[1],s4[2],d4,border=color[4],col=Gray,lwd=2)
circle(s1[1],s1[2],d1,border=color[1],col=Blue,lwd=2)
points(s1[1],s1[2],pch=18,cex=3,col=color[1])
points(s2[1],s2[2],pch=18,cex=3,col=color[2])
points(s3[1],s3[2],pch=18,cex=3,col=color[3])
points(s4[1],s4[2],pch=18,cex=3,col=color[4])
par(new=T)
plot(pos[1],pos[2],pch=20,col='orange',xlim=c(-1,5),ylim=c(-1,5),cex=5)



