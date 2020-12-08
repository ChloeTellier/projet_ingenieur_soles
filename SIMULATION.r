###SIMULATIONS

##IMPORTATION DES PACKAGES

library(spatstat)
library(tidyverse)
library(ggplot2)

## DEFINITION DES 4 SOUS ZONES

#On définit 4 sous zones dans la zone d'étude, chaque sous zone correspondant
#à un type de sol : par exemple sable, boue, zoche et gravier
#on attribue une valeur fixée (1,2,3 ou 4) a chacune des 4 modalités
#la fonction zonage prend pour argument les coordonnées x et y d'un point de la grille
zonage <- function(x,y, xmed = 5, ymed = 5){
  ifelse(x<xmed, 1, 2) + ifelse(y<ymed, 0, 2) 
}


## CALCUL DU CHAMP LATENT

#dans le champ latent, on ne prend pas en compte l'effet spatial aléatoire,
#et on considere qu'une seule covariable (type de sol)
alpha_s <- -0.5 #on fixe l'intercept à -0,5
betas_s <- c(0, 1, -1, 2) #on fixe les beta correspondant a chacune des modalités de la
#covariable type de sol
#la fonction du champ latent prend pour argument les coordonnées x et y d'un point
#de la grille, les valeurs fixées de alpha_s et des beta_s et renvoie la valeur du 
#champ latent en ce point
latent_field_function = function(x,y, alpha_s, betas_s){
  exp(alpha_s +  betas_s[zonage(x,y)])
}

## REPRESENTATION GRAPHIQUE DU CHAMP LATENT DANS LA GRILLE

dta <- data.frame(x = runif(n = 100, 0,10 ), y = runif(100, 0, 10)) %>% 
  mutate(z= latent_field_function(x, y, alpha_s, betas_s))
ggplot(data= dta) + geom_point(aes(x=x, y=y, col = z))


## CALCUL DE L'INTENSITE LAMBDA

alpha_x <- 1
b <- 1  # b peut aussi valoir 0 ou 3

#fonction de calcul du lambda
lambda_function <- function(x, y, alpha_x, b, alpha_s, betas_s){
  lambda <- exp(alpha_x + b * log(latent_field_function(x,y, alpha_s, betas_s)))
}

## GENERATION DES POINTS DE PECHE

# on utilise la fonction rpoispp pour générer les points de peche
# La fonction rpoispp prend comme arguments :
# - lambda : la fonction de densite
# - lmax : pour fixer un maximum pour la densite, eviter qu'elle n'explose
# - win : la fenetre dans laquelle on simule nos points de densite, pour nous c'est 10*10
# - les autres arguments utilises par lambda (sauf x et y qui du coup sont definis par la fenetre win)

pp = rpoispp(lambda_function, lmax=4.5, win=owin(c(0,10),c(0,10)),
             alpha_x=alpha_x, b=b, alpha_s= alpha_s, betas_s = betas_s)
#On représente graphiquement les points de peche dans les 4 sous zones
plot(pp)
