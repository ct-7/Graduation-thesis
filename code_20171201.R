# データ生成
temp.x = seq(0,3,0.1)
temp.y = 0.3*temp.x^2 - 1

sens.loc.X1 = sort(rep(seq(0,3,0.5),7))  #seq(0,3,0.1)のが31次元ベクトル
sens.loc.Y1 = rep(seq(-1.5,1.5,0.5),7)

# 互い違いのセンサー
#sens.loc.X1 = sort(rep(seq(0,3,1),7))
#sens.loc.X2 = sort(rep(seq(0.5,2.5,1),6))
#sens.loc.Y1 = rep(seq(-1.5,1.5,0.5),4)
#sens.loc.Y2 = rep(seq(-1.25,1.25,0.5),3)
#sens.loc = cbind(c(sens.loc.X1,sens.loc.X2),c(sens.loc.Y1,sens.loc.Y2))

route = cbind(temp.x,temp.y)
sens.loc = cbind(sens.loc.X1,sens.loc.Y1)
n.data = nrow(route)
n.sens = nrow(sens.loc)


#########################################################
###Estimation distance Function##########
# routeの各点からsensorの距離を出す
Estim_d <- function(temp.x,temp.y,route,sens.loc,alpha){
  
  # initialization
  n.data = nrow(route)
  n.sens = nrow(sens.loc)
  x.matrix = matrix(0,ncol = n.data + 1,nrow = n.sens)
  
  # loop
  for (i_loop in 1:n.sens){
    d = as.matrix(dist(rbind(route,sens.loc[i_loop,])))
    x.matrix[i_loop,] = 1/exp(alpha*abs(d[,n.data + 1])) + rnorm(1,mean=0.01,sd=0.1)
  }
  
  x.matrix_New = x.matrix[,1:n.data]
  x.matrix_New = round(x.matrix_New, digits = 3)
  return(x.matrix_New)
}
##dat1
dat1 = Estim_d(temp.x,temp.y,route,sens.loc,alpha = 2)
dat1 = abs(dat1 - 1)



#######################################################
# 1.route[i]と距離が近いsensor順に並び替え、そのセンサーの番号を返す
order_function <- function(dat1){
  dat.order <- matrix(0,nrow = n.data,ncol = n.sens)
  dat.orderRank = matrix(0,nrow = n.data,ncol = n.sens)
  for(i_col in 1:n.data){
    dat.order[i_col,] <- order(dat1[,i_col])
    dat.orderRank[i_col,] <- dat.order[i_col,1:n.sens]
  }
  return(dat.orderRank)
}
dat1.Rank = order_function(dat1)
dat1.Rank = data.frame(dat1.Rank)


# 2.dat1.Rankで選ばれたセンサーの座標を返す
# 3次元目の1にに1番距離が近いもの3次元目の2に二番目に距離が近い座標が入っている

zahyou_function <- function(dat1.Rank,sens.loc){
  zahyou = array(0,dim = c(n.data,2,n.sens))
  for(i in 1:n.data){
    for(j in 1:n.sens){
     zahyou[i,,j] = c(sens.loc[dat1.Rank[i,j],])
    }
  }
  return(zahyou)
}
sens.zahyou <- zahyou_function(dat1.Rank,sens.loc) 

# dat1の距離を出す
D.function <- function(dat1,dat1.Rank){
  D = matrix(0,nrow = n.data,ncol = n.sens)
  for(i in 1:n.data){
    for(j in 1:n.sens){
      D[i,j] = dat1[dat1.Rank[i,j],i]
    }
  }
  return(D)
}
D = D.function(dat1,dat1.Rank)



#  3.距離と座標を同じデータフレームに結合する
D_zahyou = function(sens.zahyou,D){
  sens.zahyouD = array(0,dim = c(n.data,3,n.sens))
    for(i in 1:n.sens){
      sens.zahyouD[,,i] = cbind(sens.zahyou[,,i],D[,i])
    }
  return(sens.zahyouD)
}
sens.zahyou_D = D_zahyou(sens.zahyou,D)
dimnames(sens.zahyou_D) <- list(NULL, c("X", "Y", "D"))
sens.zahyouD = data.frame(sens.zahyou_D)


###############################################################
# rnorm(1,mean=0.01,sd=0.1) #ノイズを加える
y <- jitter(y, factor = 30) #ノイズを加える
# signal
d = matrix(0,ncol = n.data+1,nrow = n.sens)
signal = matrix(0,ncol = n.data + 1,nrow = n.sens)
d.sqrt = matrix(0,ncol = n.data + 1,nrow = n.sens)
alpha=2
for (i_loop in 1:n.sens){
  d = as.matrix(dist(rbind(route,sens.loc[i_loop,])))
  signal[i_loop,] = 1/exp(alpha*abs(d[,n.data + 1]))
  d.sqrt[i_loop,] = sqrt(log(1/signal[i_loop,])/alpha)
}


###############################################################
##### plot ###########
plot_Func <- function(temp.x,temp.y,sens.loc,EStim_d,ylim,xlim){
  sens.loc = data.frame(sens.loc)
  
  # temp plot
  plot(temp.x,temp.y,col="red",cex = 0.7,pch = 19,ylim = ylim,
       xlab = "",ylab = "")
  par(new = T) 
  
  # senser plot
  plot(sens.loc,pch = 19,cex = 1,xlab = "",ylab = "",ylim = ylim, xaxt = 'n',yaxt ='n')
  text(sens.loc,labels = rownames(sens.loc),pos = 3,col="black")  #図が重なる
  
}
plot_Func(temp.x,temp.y,sens.loc,dat1,ylim = c(-1.5,1.7),xlim = c(0,3))
