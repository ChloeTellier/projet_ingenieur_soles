# Packages

# Ligne de modification pour le test

library(spatstat) # For rpoispp function


# Définition de la zone sur laquelle on va travailler

nx = 10
ny = 10
ncells = nx * ny


# Une covariable pour cette zone, les sédiments

sediments = matrix(nrow=nx, ncol=ny)

for (i in 1:10)
{
  for (j in 1:10)
  {
    if (i<=5 & j<=5)
    {
      sediments[i,j] = 1
    }
    else if (i<=5 & j>5)
    {
      sediments[i,j] = 2
    }
    else if (i>5 & j<=5)
    {
      sediments[i,j] = -1
    }
    else
    {
      sediments[i,j] = -2
    }
  }
}


# Génération du champ latent

alphas = 1
betas = 2
latent_field_s = matrix(nrow=nx, ncol=ny)

latent_field_function = function(cov, alphas, betas)
{
  latent_field_s = exp(alphas + cov * betas)
}

latent_field_s = latent_field_function(sediments, alphas, betas)


# Générer les points de pêche

# On définit lambda, la fonction de densité

#### Exemples d'utilisation de la fonction rpoispp

# uniform Poisson process with intensity 100 in the unit square
pp <- rpoispp(100)
plot(pp)

# uniform Poisson process with intensity 1 in a 10 x 10 square
pp <- rpoispp(1, win=owin(c(0,10),c(0,10)))
plot(pp)
# plots should look similar !

# inhomogeneous Poisson process in unit square
# with intensity lambda(x,y) = 100 * exp(-3*x)
# Intensity is bounded by 100
pp <- rpoispp(function(x,y) {100 * exp(-3*x)}, 100)
plot(pp)

# How to tune the coefficient of x
lamb <- function(x,y,a) { 100 * exp( - a * x)}
pp <- rpoispp(lamb, 100, a=3)
plot(pp)

# pixel image
Z <- as.im(function(x,y){100 * sqrt(x+y)}, unit.square())
pp <- rpoispp(Z)
plot(pp)

# randomising an existing point pattern
pp = rpoispp(intensity(cells), win=Window(cells))
plot(pp)
pp = rpoispp(ex=cells)
plot(pp)

#### Essais persos

# La fonction rpoispp prend comme arguments :
# - lambda : la fonction de densité
# - lmax : pour fixer un maximum à la densité, éviter qu'elle n'explose
# - win : la fenetre dans laquelle on simule nos points de densité, pour nous c'est 10*10
# - les autres arguments utilisés par lambda (sauf x et y qui du coup sont définis par la fenetre win)

#  Essai n°1

alphax = 1
b = 1 # Plus tard il faudra le faire varier : c(0, 1, 3)
lambda_s = matrix(nrow=nx, ncol=ny)

lambda_function = function(alphax, b, latent_field_s)
{
  lambda = exp(alphax + b * log(latent_field_s))
}

lambda_s = lambda_function(alphax, b, latent_field_s)
# Ca nous crée une matrice avec la valeur de la fonction de densité, mais ce n'est pas ce qu'on veut,
# nous on veut juste la fonction de densité

# Essai n°2

lambda_function2 = function(x, y, alphax, b)
{
  exp(alphax + b * log(latent_field_s[x,y]))
}

test_essain2 = lambda_function2(1:10, 1:10, 1, 1)
# On applique la fonction à une matrice 10*10 pour voir ce que ça nous retourne
# Ca nous retourne une matrice 10*10, logique

pp = rpoispp(lambda_function2, lmax=10, win=owin(c(1,10),c(1,10)), alphax=alphax, b=b)
# Et non, du coup ça marche pas
# Error: Index out of bounds in [.ppp
plot(pp)

# Essai n°3

lambda_function3 = function(x, y, alphax, b)
{
  exp(alphax + b * log(latent_field_s[x,2])) # On fixe y à 2 juste pour vérifier
}

test_essain3 = lambda_function3(1:10, 1:10, 1, 1)
# On applique la fonction à une matrice 10*10 pour voir ce que ça nous retourne
# Ca nous retourne un vecteur 10, logique car on a fixé y à 2

pp = rpoispp(lambda_function3, lmax=10, win=owin(c(1,10),c(1,10)), alphax=alphax, b=b)
plot(pp)
# Et la évidemment ça marche ...
# Conclusion : il faut que la fonction de densité, appliquée à notre matrice 10*10, nous retourne un
# vecteur  de taille 10 et pas une matrice 10*10

# Je comprends pas pq, je maitrise pas trop ce code, + je sais pas comment faire
# Autre chose : on a regardé rapidement les codes de Baptiste et pas vu d'utilisation de cette fonction rpoispp
# Comment il fait pour générer ses points de pêche ? On n'a pas bien compris
