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



#########################################################
###Estimation distance Function##########
# routeの各点からsensorの距離を出す
n.data = nrow(route)
n.sens = nrow(sens.loc)
Estim_d <- function(temp.x,temp.y,route,sens.loc,alpha){
  
  # initialization
  n.data = nrow(route)
  n.sens = nrow(sens.loc)
  x.matrix = matrix(0,ncol = n.data + 1,nrow = n.sens)
  
  # loop
  for (i_loop in 1:n.sens){
    d = as.matrix(dist(rbind(route,sens.loc[i_loop,])))
    x.matrix[i_loop,] = 1/exp(alpha*abs(d[,n.data + 1]))
  }
  
  x.matrix_New = x.matrix[,1:n.data]
  x.matrix_New = round(x.matrix_New, digits = 3)
  return(x.matrix_New)
}
##dat1
route = cbind(temp.x,temp.y)
sens.loc = cbind(sens.loc.X1,sens.loc.Y1)
dat1 = Estim_d(temp.x,temp.y,route,sens.loc,alpha = 2)


#######################################################
# 1.route[i]と距離が近い三番目までの座標とroute[i]までの距離を取得
order_function <- function(dat1){
  dat.order <- matrix(0,nrow = n.data,ncol = n.sens)
  dat.orderRank = matrix(0,nrow = n.data,ncol = 3)
  for(i_col in 1:n.data){
    dat.order[i_col,] <- order(-dat1[,i_col])
    dat.orderRank[i_col,] <- dat.order[i_col,1:3]
  }
  return(dat.orderRank)
}
dat1.Rank = order_function(dat1)
dat1.Rank = data.frame(dat1.Rank)

################## ランク3が全て同じ直線上(同じgroup)にあるか探す関数作成中 #############
# 2.order sensorのx軸の位置ごとでgroupに分ける
Order = data.frame(matrix(1:n.sens,nrow = 7,ncol= 7))
colnames(Order) = c("g1","g2","g3","g4","g5","g6","g7")

# 3.出したランク3が全て同じ直線上(同じgroup)にあるか探す
logical.function <- function(dat1,Order){
  
  # initialization
  logical1 = matrix(0,ncol = 1,nrow = n.data)
  logical2 = matrix(0,ncol = 1,nrow = n.data)
  logical3 = matrix(0,ncol = 1,nrow = n.data)
  logical4 = matrix(0,ncol = 1,nrow = n.data)
  logical5 = matrix(0,ncol = 1,nrow = n.data)
  logical6 = matrix(0,ncol = 1,nrow = n.data)
  logical7 = matrix(0,ncol = 1,nrow = n.data)
  logical = matrix(0,nrow = 7,ncol = n.data)
  
  # loop
  for(i in 1:n.data){
    logical1[i] = dat1.Rank[i,1] %in% Order$g1 && dat1.Rank[i,2] %in% Order$g1 && dat1.Rank[i,3] %in% Order$g1
    logical2[i] = dat1.Rank[i,1] %in% Order$g2 && dat1.Rank[i,2] %in% Order$g2 && dat1.Rank[i,3] %in% Order$g2
    logical3[i] = dat1.Rank[i,1] %in% Order$g3 && dat1.Rank[i,2] %in% Order$g3 && dat1.Rank[i,3] %in% Order$g3
    logical4[i] = dat1.Rank[i,1] %in% Order$g4 && dat1.Rank[i,2] %in% Order$g4 && dat1.Rank[i,3] %in% Order$g4
    logical5[i] = dat1.Rank[i,1] %in% Order$g5 && dat1.Rank[i,2] %in% Order$g5 && dat1.Rank[i,3] %in% Order$g5
    logical6[i] = dat1.Rank[i,1] %in% Order$g6 && dat1.Rank[i,2] %in% Order$g6 && dat1.Rank[i,3] %in% Order$g6
    logical7[i] = dat1.Rank[i,1] %in% Order$g7 && dat1.Rank[i,2] %in% Order$g7 && dat1.Rank[i,3] %in% Order$g7
    logical[1,i] = logical1[i]
    logical[2,i] = logical2[i]
    logical[3,i] = logical3[i]
    logical[4,i] = logical4[i]
    logical[5,i] = logical5[i]
    logical[6,i] = logical6[i]
    logical[7,i] = logical7[i]
  }
  logical = t(logical)
  return(logical)
}
logic = logical.function(dat1,Order)

# logicのTRUEを探す 一つでもTRUEがあればそれは同じgroupの座標が3つ入ってしまっている
find_true = function(logic){
  for(i in 1:n.data){
    for(j in 1:7){
      if(logic[i,j] %in% 1){
        return(c(i,j))
      }
    }
  }
}
logic_where = find_true(logic)
dat1.Rank[logic_where[1],]

# とりあえずdat1.Rank[1,3]をdat1[1,9]に置き換える
dat1.Rank[logic_where[1],3] = 9

###############################################################
# dat1.Rankで選ばれたセンサーの座標を返す
# 3次元目の1にに1番距離が近いもの3次元目の2に二番目に距離が近い座標が入っている
point = matrix(0,nrow = n.data,ncol = 2)
point_function <- function(point,dat1.Rank,sens.loc){
  point = array(0,dim = c(n.data,2,3))
  for(i in 1:n.data){
    for(j in 1:3){
     point[i,,j] = c(sens.loc[dat1.Rank[i,j],])
    }
  }
  return(point)
}
sens.zahyou <- point_function(point,dat1.Rank,sens.loc) 

# ランク３のdat1の距離を出す
D.function <- function(dat1,dat1.Rank){
  D = matrix(0,nrow = n.data,ncol = 3)
  for(i in 1:n.data){
    for(j in 1:3){
      D[i,j] = dat1[dat1.Rank[i,j],i]
    }
  }
  return(D)
}
D = D.function(dat1,dat1.Rank)

#  距離と座標を同じデータフレームに結合する
sens.zahyou_D1 = cbind(sens.zahyou[,,1],D[,1])
sens.zahyou_D2 = cbind(sens.zahyou[,,2],D[,2])
sens.zahyou_D3 = cbind(sens.zahyou[,,3],D[,3])
sens.zahyouD = cbind(sens.zahyou_D1,sens.zahyou_D2,sens.zahyou_D3)
colnames(sens.zahyouD) = c("X1","Y1","D1","X2","Y2","D2","X3","Y3","D3")
sens.zahyouD

#### 三点測位　#######
# 円の方程式を利用した三点測位法?
x = 0;y = 0
(x - sens.zahyouD$X1[i])^2 + (y - sens.zahyouD$Y1[i])^2 = sens.zahyouD$D1[i]^2 
(x - sens.zahyouD$X2[i])^2 + (y - sens.zahyouD$Y2[i])^2 = sens.zahyouD$D2[i]^2
(x - sens.zahyouD$X3[i])^2 + (y - sens.zahyouD$Y3[i])^2 = sens.zahyouD$D3[i]^2
