getwd()
library(MASS)
library(dplyr)
library(ggplot2)
library(plotly)
library(kdensity)
x = data.frame(read.csv("wine.csv", sep = ",", header = T))
x = x[c("Wine", "Acl", "OD")]
x$Wine = as.factor(x$Wine)
?kde2d

#kroswalizacja n krotna
Acl_1=x$Acl[x$Wine == "1"]
OD_1=x$OD[x$Wine == "1"]
Acl_2=x$Acl[x$Wine == "2"]
OD_2=x$OD[x$Wine == "2"]
Acl_3=x$Acl[x$Wine == "3"]
OD_3=x$OD[x$Wine == "3"]

reklas
for(i in 1:nrow(x)){
  g1_cross=kde2d(Acl_1[-i], OD_1[-i], lims=c(x$Acl[i],x$Acl[i], x$OD[i],x$OD[i]),n=1)

  g2_cross=kde2d(Acl_2[-i],OD_2[-i], lims=c(x$Acl[i],x$Acl[i], x$OD[i],x$OD[i]),n=1)

  g3_cross=kde2d(Acl_3[-i], OD_3[-i], lims=c(x$Acl[i],x$Acl[i], x$OD[i],x$OD[i]),n=1)

  if(g1_cross$z>g2_cross$z & g1_cross$z>g3_cross$z){reklas[i]="1"}
  if(g2_cross$z>g1_cross$z & g2_cross$z>g3_cross$z){reklas[i]="2"}
  if(g3_cross$z>g1_cross$z & g3_cross$z>g2_cross$z){reklas[i]="3"}
  reklas[i]
}
reklas


gest1 = kde2d(x$Acl[x$Wine == "1"], x$OD[x$Wine == "1"], n = 50)
gest2 = kde2d(x$Acl[x$Wine == "2"], x$OD[x$Wine == "2"], n = 50)
gest3 = kde2d(x$Acl[x$Wine == "3"], x$OD[x$Wine == "3"], n = 50)
ggplot(x, aes(x = Acl, y = OD))+
  geom_point(aes(color = Wine))+
geom_contour(obszar1)
#wyznaczamy punkty z siatki dla ktorych gestosc setosa ma najwieksza wartosc
obszar1=ifelse(gest1$z>gest2$z & gest1$z>gest3$z,1,0)
#wyznaczamy punkty z siatki dla ktorych gestosc versicolor ma najwieksza wartosc
obszar2=ifelse(gest2$z>gest1$z & gest2$z>gest3$z,1,0)
#wyznaczamy punkty z siatki dla ktorych gestosc virginica ma najwieksza wartosc
obszar3=ifelse(gest3$z>gest1$z & gest3$z>gest2$z,1,0)
#obszar klasyfikacji dla setosy

plot(x$Acl,x$OD,pch = 19, col=x$Wine,cex=1.2, xlab = "Acl", ylab = "OD",
     main = "Wykres rozrzutu dla win\nwraz z gęstością dwuwymiarową")


contour(gest1,lwd=0.5,method="simple", col = "black",
        xlab = "Acl",
        ylab = 'OD',
        labels ="",
        main = "Gęstości dwuwymiarowe\ndla poszczególnych kateogrii win",
        xlim = c(10,26),
        ylim = c(1.2,4))
contour(gest2,lwd=0.5,method="simple", col = "red",
                          labels="", add =T)
contour(gest3,lwd=0.5,method="simple", col = "green",
        labels="",add =T)
points(x$Acl[x$Wine == "1"], x$OD[x$Wine == "1"],pch=19,col='black',cex=1.2) 
points(x$Acl[x$Wine == "2"], x$OD[x$Wine == "2"],pch=19,col='red',cex=1.2) 
points(x$Acl[x$Wine == "3"], x$OD[x$Wine == "3"],pch=19,col='green',cex=1.2)
legend(10, 1.75,legend=c("Wine 1", "Wine 2", "Wine 3"), pch = 19, col = c("black", "red", "green"), title =  "Rodzaj wina:")

plot(x$Acl,x$OD,pch = 19, col=x$Wine,cex=1.2, xlab = "Acl", ylab = "OD",
     main = "Wykres rozrzutu dla win \nwraz z obszarem klasyfikacji",
     sub = "obszary klasyfikacji wyznaczone na podstawie estymatora jądrowego",
     cex.sub = 0.7)
contour(gest1$x,gest1$y,obszar1,add = T,lwd=2,levels = 0.5,method="simple", col = "black",
        labels="")
#obszar klasyfikacji dla versicolor
contour(gest2$x,gest2$y,obszar2,add=T,lwd=2,levels = 0.5,method="simple", col = "red",
        labels="")
#obszar klasyfikacji dla virginici
contour(gest3$x,gest3$y,obszar3,add=T,lwd=2,levels =0.5,method="simple", col = "green",
        labels="")
legend(10, 1.75,legend=c("Wine 1", "Wine 2", "Wine 3"), pch = 19, col = c("black", "red", "green"), title =  "Rodzaj wina:")

library(ks)
kda1=kda(x[,2:3],x$Wine,gridsize = 200)
plot(x$Acl,x$OD,pch=19,col=x$Wine,cex=1)
plot(kda1)
points(x$Acl, x$OD, col = x$Wine)


reklas=vector(length = nrow(x))

#przypisujemy do wspolrzednych wektora reklas (do kolejnych obserwacji)
#gatunek wynikajacy z reguly klasyfikacyjnej
for(i in 1:nrow(x)){
  g1=kde2d(x$Acl[x$Wine == "1"], x$OD[x$Wine == "1"],
           lims=c(x$Acl[i],x$Acl[i],
                  x$OD[i],x$OD[i]),n=1)
  #gestosc dla setosy
  #dla kolejnych obserwacji z ramki iris
  g2=kde2d(x$Acl[x$Wine == "2"], x$OD[x$Wine == "2"],lims=c(x$Acl[i],x$Acl[i],
                            x$OD[i],x$OD[i]),n=1)
  #gestosc dla versicolor
  #dla kolejnych obserwacji z ramki iris
  g3=kde2d(x$Acl[x$Wine == "3"], x$OD[x$Wine == "3"],lims=c(x$Acl[i],x$Acl[i],
                                                            x$OD[i],x$OD[i]),n=1)
  #gestosc dla virginici
  #dla kolejnych obserwacji z ramki iris
  if(g1$z>g2$z & g1$z>g3$z){reklas[i]="1"}
  if(g2$z>g1$z & g2$z>g3$z){reklas[i]="2"}
  if(g3$z>g1$z & g3$z>g2$z){reklas[i]="3"}
}
reklas

x$Acl[1]
x$Alc[1]

table(reklas,x$Wine)
sum(diag(table(reklas,x$Wine)))/nrow(x)



library(mixtools)
mu1Acl=mean(x$Acl[x$Wine == '1'])
mu1OD=mean(x$OD[x$Wine == '1'])
mu_1 = c(mu1Acl, mu1OD)
mu_1

mu2Acl=mean(x$Acl[x$Wine == '2'])
mu2OD=mean(x$OD[x$Wine == '2'])
mu_2 = c(mu2Acl, mu2OD)
mu_2

mu3Acl=mean(x$Acl[x$Wine == '3'])
mu3OD=mean(x$OD[x$Wine == '3'])
mu_3 = c(mu3Acl, mu3OD)
mu_3

var_1=var(x[x$Wine == '1', 2:3])
var_1

var_2=var(x[x$Wine == '2', 2:3])
var_2

var_3=var(x[x$Wine == '3', 2:3])
var_3
gest1=function(x){dmvnorm(x,mu=mu_1,sigma=var_1)}
gest2=function(x){dmvnorm(x,mu=mu_2,sigma=var_2)}
gest3=function(x){dmvnorm(x,mu=mu_3,sigma=var_3)}

range(x$OD)
N=300
x1=seq(from=10,to=30,length.out = N) 
y=seq(from=1,to=4,length.out = N)
siatka=expand.grid(x=x1,y=y)

wek1=gest1(as.matrix(siatka))
norm1_z=matrix(wek1,nrow=N,ncol=N)
  
wek2=gest2(as.matrix(siatka))
norm2_z=matrix(wek2,nrow=N,ncol=N)

wek3=gest3(as.matrix(siatka))
norm3_z=matrix(wek3,nrow=N,ncol=N)

plot(x$Acl[x$Wine == "1"], x$OD[x$Wine == "1"],pch=19,col='black',cex=1.2,
     xlim = c(8,30),
     ylim = c(1.2,5),
     main = "Wykres rozrzutu dla win \nwraz z dwuwymiarowym rozkładem normalnym",
     xlab = 'Acl',
     ylab = 'OD') 
points(x$Acl[x$Wine == "2"], x$OD[x$Wine == "2"],pch=19,col='red',cex=1.2) 
points(x$Acl[x$Wine == "3"], x$OD[x$Wine == "3"],pch=19,col='green',cex=1.2)
legend('topleft',legend=c("Wine 1", "Wine 2", "Wine 3"), pch = 19, col = c("black", "red", "green"), title =  "Rodzaj wina:")

contour(x1,y,norm1_z,add=T,col="black")
contour(x1,y,norm2_z,add=T,col="red")
contour(x1,y,norm3_z,add=T,col="green")

clas1=ifelse(norm1_z>norm2_z & norm1_z>norm3_z,1,0)
clas2=ifelse(norm2_z>norm1_z & norm2_z>norm3_z,1,0)
clas3=ifelse(norm3_z>norm1_z & norm3_z>norm2_z,1,0)

contour(x1,y,clas1,add=T,method="simple",labels="",lwd=2, col = 'black')
contour(x1,y,clas2,add=T,method="simple",labels="",lwd=2, col = 'red')
contour(x1,y,clas3,add=T,method="simple",labels="",lwd=2, col = 'green')

reklas=vector(length=nrow(x))
for(i in 1:nrow(x)){
  arg=as.numeric(x[i,2:3])
  if(gest1(arg)>gest2(arg) & gest1(arg)>gest3(arg)){reklas[i]="1"}
  if(gest2(arg)>gest1(arg) & gest2(arg)>gest3(arg)){reklas[i]="2"}
  if(gest3(arg)>gest2(arg) & gest3(arg)>gest1(arg)){reklas[i]="3"}
}
reklas 

table(reklas,x$Wine)

sum(diag(table(reklas,x$Wine)))/nrow(x)











plot(x$Acl[x$Wine == "1"], x$OD[x$Wine == "1"],pch=19,col='black',cex=2,
     xlim = c(10,30),
     ylim = c(1.2,4))
points(vePL,vePW,pch=19,col=rgb(1,0,0,0.5),cex=2) #obserwacje dla versicolor
points(viPL,viPW,pch=19,col=rgb(0,0,1,0.5),cex=2)




mVe1=mean(iris$Petal.Length[51:100])
mVe2=mean(iris$Petal.Width[51:100])
mVe=c(mVe1,mVe2)








table(kda1$x.group.estimate,x$Wine)
sum(diag(table(kda1$x.group.estimate,x$Wine)))/nrow(x)
library(klaR)
library(caret)
train(Wine~Acl+OD,
      data=x,method='knn',
      trControl=trainControl('cv',number=10),
      tuneGrid = expand.grid(k = seq(from = 1, to = 15, by = 2)))


# wykres dla 3 gatunkow - gestosc dwuwymiarowa
contour(kd1,col=rgb(0,1,0,0.5), ylim = c(0,5))
contour(kd2,col=rgb(1,0,0,0.5),add=T)
contour(kd3,col=rgb(0,0,1,0.5),add=T)
x


fig1 = ggplot(x, aes(x = Acl, y = OD))+
  geom_point(aes(color = Wine), size = 2, alpha = 0.7)+
  geom_density_2d(aes(color = Wine), lwd = 0.75, alpha = 0.7)+
  ggthemes::theme_fivethirtyeight()+
  ylim(1, 4.25)+
  labs(title = "Gęstość dwuwymiarowa dla trzech kategorii win",
       subtitle = "na podstawie zmiennych Acl i OD",
       color = "Kategoria wina: ",
       y = "Acl",
       x = 'OD')+
  theme(plot.title =element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = 10))
fig1
ggplotly(fig1)|>
  layout(title = list(text = paste0('Gęstość dwuwymiarowa dla trzech kategorii win',
                             '<br>',
                             '<sup>',
                             'na podstawie zmiennych Acl i OD',
                             '</sup>')))
fig1

# wykres dla 3 gatunkow razem 3d
gest_all=kde2d(x$Acl, x$OD, n=50,h=c(2.8,1))

axx <- list(
  title = "Acl")
axy <- list(
  title = "OD")
axz <- list(
  title = "gęstość")

fig <- plot_ly(x = gest_all$x, y = gest_all$y, z = gest_all$z)|>
  add_surface(contours = list(z = list(
    show=F,
    usecolormap=TRUE,
    highlightcolor="#ff0000",
    project=list(z=TRUE))))|>
  layout(scene = list(camera = list(eye = list(x = -1.75, y = 1.75, z = 1.75)), xaxis = axx, yaxis = axy, zaxis = axz))
fig


