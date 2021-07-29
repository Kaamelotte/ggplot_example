
rm(list = ls()) ; graphics.off()

require(reshape2)
require(ggplot2)
library(tidyverse)


load("data.rda")


lambda = data$lambda
yield = data$yield
nirs = data$nirs %>% t %>%  scale(scale = F) %>% t


#______________________________________________________________________________#  
#On calcule les quantiles du rendement
q <- quantile(yield, probs = seq(0,1, 0.25))

#On récupère les nom des quantiles
nom <- paste(names(q[1:4]), 'à', names(q[2:5]))

#colonne contenant la couleur de chaque nirs
col = yield %>% sapply(function(x) 1+ sum(x > q[-1]) ) %>%
  rep( each = length(lambda) ) %>%
  as.factor



#Passage en data.frame et rajout des lambda
nirs %>% as.data.frame %>% mutate(lambda) %>%
  melt(id = 'lambda') %>% #ggplot veut avoir ses donnée en colonne, on passe donc à trois colonne : 
                          #lambda, value = nirs, et variable  = nom des lignes ie desvariété
  mutate(color = col ) %>%
  #Graphique :
  ggplot(aes(x = lambda, y = value, col = color, group = variable)) + geom_line() +
  scale_color_manual(values = c('mediumblue', 'dodgerblue', 'orange', 'red' ),
                     labels = nom) +
  labs(title = 'nirs en fonction du rendement')

#______________________________________________________________________________#  
  
#référence pour ggplot 
#1) liste des types de graphe : https://www.r-graph-gallery.com/index.html
#2) tuto très/trop complet : http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html




