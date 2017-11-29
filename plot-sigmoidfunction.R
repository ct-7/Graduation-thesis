##################################################
#  function plot
x = seq(-5,5,0.1)
sigmoid.function <- function(x){
  1/(1+exp(-x))
}
sigmoid.abs.function <- function(x){
  1/(1+exp(abs(x)))
}
y = sigmoid.function(x)
y1 = sigmoid.abs.function(x)

# 累積密度関数 sigmoid
plot(y,type="l",xlab="x",main="sigmoid function",col=4)
plot(y1,type="l",xlab="x",main="sigmoid.abs function",col=3)
p <- ggplot(data=data.frame(x=c(-7,7)), aes(x=x))
p <- p + stat_function(fun=sigmoid.function)
p

# 累積密度関数 sigmoid.abs
plot(y,type="l",xlab="x",main="sigmoid.abs function",col=4)
p <- ggplot(data=data.frame(x=c(-7,7)), aes(x=x))
p <- p + stat_function(fun=sigmoid.abs.function)
p


# 確率密度関数
p <- ggplot(data=data.frame(x=c(-9,9)), aes(x=x))
p <- p + theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15))
p <- p + theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))
# p <- p + labs(title="sigmoid function")

p <- p + stat_function(fun=dlogis)
p <- p + stat_function(fun = dnorm,color = "orange")
p

install.packages("animation")
library(animation)

