getwd()
library(MASS)
library(dplyr)
library(ggplot2)
library(plotly)
library(kdensity)
x = data.frame(read.csv("wine.csv", sep = ",", header = T))
x = x[c("Wine", "Acl", "OD")]
x$Wine = as.factor(x$Wine)
ggplot(x, aes(x = Acl, y = OD, color = Wine))+
  geom_point()
gest = kde2d(x$Acl, x$OD, n = 5, h=c(0.5,0.5))
gest = expand.grid(gest)
?kde2d
kd <- kde2d(x$Acl, x$OD, n = 50)
kd1 = kde2d(x$Acl[x$Wine == "1"], x$OD[x$Wine == "1"], n = 50)
kd2 = kde2d(x$Acl[x$Wine == "2"], x$OD[x$Wine == "2"], n = 50)
kd3 = kde2d(x$Acl[x$Wine == "3"], x$OD[x$Wine == "3"], n = 50)

contour(kd1,col=rgb(0,1,0,0.5), ylim = c(0,5))
contour(kd2,col=rgb(1,0,0,0.5),add=T)
contour(kd3,col=rgb(0,0,1,0.5),add=T)
x

library(ggplot2)
library(plotly)

fig1 = ggplot(x, aes(x = Acl, y = OD))+
  geom_point(aes(color = Wine), size = 2, alpha = 0.7)+
  geom_density_2d(aes(color = Wine), lwd = 0.75, alpha = 0.7)+
  ggthemes::theme_fivethirtyeight()+
  ylim(1, 4.25)+
  labs(title = "Gęstość dwuwymiarowa dla trzech kategorii win",
       subtitle = "na podstawie zmiennych Acl i OD",
       color = "Kategoria wina: ")+
  theme(plot.title =element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"))
t <- list(
  family = "sans serif",
  size = 14,
  color = 'blue')
ggplotly(fig1)|>
  layout(title = list(text = paste0('Gęstość dwuwymiarowa dla trzech kategorii win',
                             '<br>',
                             '<sup>',
                             'na podstawie zmiennych Acl i OD',
                             '</sup>')))
  
?ggplotly
fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
fig
stat

fig <- plot_ly(
  x = iris$Petal.Length,
  y = iris$Petal.Width,
  z = kde2d(iris$Petal.Length, iris$Petal.Width, n = 50),
  type = "contour" 
)

fig
