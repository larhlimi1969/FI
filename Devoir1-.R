############################################################3
# # effaçons tout ce qui a été créé préalablement


rm(list=ls())


#############---------------Question 1----------------------########################

library(astsa)
#changeant le répertoire de travail   et chargeant les données
setwd("D:/Cours 2022-2023/AYA- 2022-2023/Micro 3/UOttawa/MacroIII/ECO35522022/FallTerm/12sep2022")




donnees <- read.delim("cpidata.txt",stringsAsFactors = FALSE)
# créations des séries
TSIPC <- as.ts(donnees$All.items)
xE <-ts(donnees$yyyy.mm)
TSIPCXFE<- as.ts(donnees$All.items.excluding.food.and.energy)
d<-cbind(TSIPC,TSIPCXFE)

########################################################

# A) traçons sur un même graphique IPC et IPCXFE
# superposé sur le même axe y
x11();tsplot(d, col = c(2,3),
       main = "IPC total et IPC calculé en excluant le prix de la nourriture et de l’énergie",
       lwd=2.5,  
       ylab='',  spaghetti=TRUE)
legend("topleft", legend=c("IPC","IPCXFE"), col=c(2,3), lty=2, bty="n")

# Séparément 
#tsplot(d)

#Calculons le taux d’inflation annuel mesuré en utilisant l’indice IPC et IPCXFE. 
#les données sur les indices de prix à notre disposition commencent
#en janvier 1992, nos mesures d’inflation commenceront en janvier 1993.

# le taux d’inflation annuel mesuré en utilisant l’indice IPC 

# en utilisant lag()
IPCA    <-    ts.intersect(lag(TSIPC,-12),TSIPC)
TIPC   <-    100*(IPCA[,2]-IPCA[,1]) / IPCA[,1]
# Autrement en utilisant diff()
zIPC <-100*diff(TSIPC,12)/TSIPC

# le taux d’inflation annuel mesuré en utilisant l’indice IPCXFE.
# en utilisant lag()
IPCXFEA <-    ts.intersect(lag(TSIPCXFE,-12),TSIPCXFE)
TIPCXFE   <-    100*(IPCXFEA[,2]-IPCXFEA[,1]) / IPCXFEA[,1]
# Autrement en utilisant diff()
zIPCXFE <-100*diff(TSIPCXFE,12)/TSIPCXFE

######################################33
# B) Tracé graphique de le taux d’inflation mesuré par l’IPC et par l’IPCXFE

d<-cbind(TIPC,TIPCXFE)
# superposé sur le même axe y 

x11();tsplot(d, col = c(2,3),
       main = "Taux d’inflation mesuré par l’IPC et par l’IPCXFE",
       lwd=2,  
       ylab='',  spaghetti=TRUE)
legend("topleft", legend=c("Taux IPC","Taux IPCXFE"), col=c(2,3), lty=1, pch=c(0,1), bty="n")

# Séparément 
#tsplot(d)


###################################################################
# Caracteristiques du taux de l'IPC
summary(TIPC)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.9532  1.2461  1.8629  1.9193  2.3953  7.8723 


# Caracteristiques du taux de l'IPCXFE
summary(TIPCXFE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.344   1.278   1.580   1.666   1.947   5.244 

# Interpretation 
#Non, la Banque du Canada  n’a pas été en mesure de rencontrer son mandat.
#D’après ces statistiques, le taux d’inflation annuel a dépassé la marge de 2 pourcent
#avec des fluctuations acceptables dans une fourchette allant de 1 à 3 pourcents que
#la Banque de Canada s’est fixe dans le cadre de sa politique monétaire. 


#################################################################33
#C)
# Calcul de la variance la variance de l’inflation mesurée par l’IPC et l’IPCXFE
#sur l’ensemble de l’échantillon disponible
var(TIPC)
#1.37717
var(TIPCXFE)
# 0.5803774

# le taux d’inflation annuel mesuré en utilisant l’indice IPC est plus volatil
# car var(TIPC)= 1.37717 est supérieur à var(TIPCXFE)= 0.5803774






# Creation de l'indice IPCTAR
#E) Créez un indice de prix IPCTAR, allant de janvier 1992 jusqu’à juillet 2022, dont la valeur en janvier
#1995 est 100, et qui représente ce qu’aurait été le niveau des prix si la Banque du Canada avait été en mesure
#de parfaitement contrôler l’inflation à 2 pourcent. Ainsi, en février 1995, l’indice de prix devrait être de
#100.1651581, en mars 1995 de 100.330589 etc. . .

#valeur_base= IPC janvier 95 a partir du janvier 92----> 12+12+12+1=37

IPCTAR=c(1:length(TSIPC))
for (i in 1:length(TSIPC)){
  IPCTAR[i] =100*1.02^((i-37)/12)
}



####################################################
#G) Trace sur un même graphique IPC, IPCXFE et IPCTAR apres normalisation

# Normalisation des series 
# en divisant par la valeur qu’elle prennent en janvier 1995 et en de remultipliant
# par 100.
NIPCTAR     <-  100*IPCTAR/IPCTAR[37]
NTSIPC      <-  100*TSIPCXFE/TSIPCXFE[37]
NIPCXFE     <-  100*TSIPC/TSIPC[37]


d<-cbind(NTSIPC,NIPCXFE,NIPCTAR)
# superposé sur le même axe y 

x11();tsplot(d, col = c(2:4),pch=c(0,1,2),
       main = "Taux IPC , IPCXFE et IPCTAR",
       lwd=2,
       ylab='Taux',  spaghetti=TRUE)
legend("topleft", legend=c("Taux IPC","Taux IPCXFE","Taux IPCTAR"), col=c(2:4), 
       lty=1, pch=c(0,1), bty="n")

# Séparément 
#tsplot(d)


###############################################################################3
#H) Sur la base de votre réponse en G), le niveau des prix actuel,
#tel que mesuré par l’IPC et IPCXFE, est-il
#plus élevé ou moins élevé que ce qu’il aurait dû être si la Banque du
#Canada avait été en mesure de contrôler
#parfaitement l’inflation?







#############---------------Question 2----------------------########################
#La base de données pibrev.txt provient de Statistiques Canada et contient les données sur le PIB trimestriel
#en termes de revenus, tableau CANSIM 36-10-0103-01, du premier trimestre de 1961 au second trimestre de
#2022 (données désaisonnalisées au taux annuel). Vous pouvez utilisez file -> Import Dataset -> From Text
#(base) dans RStudio pour importez vos données.
#Il y a présentement un vigoureux débat quant à savoir si la montée fulgurante de l’inflation observée depuis
#2021 n’est pas le résultat des entreprises qui profitent du contexte mondial actuel plutôt difficile pour tenter
#d’augmenter leur marge de profit. Une façon de vérifier si effectivement les entreprises font davantage de
#profits est de regarder la part du facteur travail dans les revenus totaux de l’économie. Dit autrement,
#quelle proportion du PIB, en %, revient aux travailleurs et quelle proportion retournes aux détenteurs des
#entreprises (profits). Si effectivement les entreprises arrivent à augmenter leurs profits, la part du facteur
#travail devrait être à la baisse et celle des profits devrait être à la hausse.


#################################################################
rm(list=ls())
#a) Dans un premier temps, tracez l’évolution de la variable rémunération 
#des salariés, du premier #trimestre de 2012 jusqu’à la dernière donnée disponible.
library(astsa)
#changeant le répertoire de travail   et chargeant les données
setwd("D:/Cours 2022-2023/AYA- 2022-2023/Micro 3/UOttawa/MacroIII/ECO35522022/FallTerm/12sep2022")
donnees4 <- read.delim("pibrev.txt",stringsAsFactors = FALSE,header = TRUE)







########################################################

# A) traçons sur un même graphique Rémunération des salariés Dollars
# créations des séries
ren<- as.ts(donnees4$Rémunération.des.salariés..Dollars)

# rémunération des salariés, du premier trimestre de 2012 jusqu’à la dernière donnée
#disponible
ren <-tail(ren, -204)
x11();tsplot(ren, col = c(2,3),
             main = "Rémunération des salariés en Dollars",
             lwd=2.5,  
             ylab='',  spaghetti=FALSE)



#B) traçons le graphique de l’évolution, pour la même période, des «profits» 
#des entreprises, que nous
#définissions comme la somme de l’excédent d’exploitation brut, du revenu mixte brut,
#des impôts moins les #subventions sur la production,
#des impôts moins les subventions sur les produits et importations et de la
#divergence statistique (statistical divergence)

profit   <-  as.ts (donnees4$Excédent.d.exploitation.brut..Dollars+donnees4$Revenu.mixte.brut..Dollars+
        donnees4$Impôts.moins.les.subventions.sur.la.production..Dollars+
        donnees4$Impôts.moins.les.subventions.sur.la.production..Dollars+
        donnees4$Divergence.statistique..Dollars)

profit   <-  tail(profit,-204)

x11();tsplot(profit, col = c(2,3),
             main = "Evolution des «profits» des entreprises",
             lwd=2.5,  
             ylab='',  spaghetti=FALSE)



#C) Sur un même graphique, traçons la part (rémunération des salairés divisée par
# PIB au prix du marché) 
# et
#la part des profits sur la période allant du premier trimestre de 2012 au deuxième trimestre de 2022. Est-ce
#qu’effectivement, nous observons une hausse de la part des profits depuis l’année 2021? Pensez-vous que
#ceux qui prétendent que la hausse de l’inflation provient d’un comportement opportuniste des entreprises ont
#effectivement raison?

#PIB
PIB_global    <-    as.ts(donnees4$Produit.intérieur.brut.aux.prix.du.marché..Dollars)
PIB_relatif   <-    tail(PIB_global,-204)

# part des Rémunération des salariés en Dollars dans le PIB au prix du marché

z1            <-    100*ren/PIB_relatif

# part des profits des entreprises en Dollars

z2  <-100*profit/PIB_relatif


d4<-cbind(z1,z2)

########################################################

# traçons sur un même graphique 
x11();tsplot(d4, col = c(2,3),
             main = "Rémunération des salariés et Part des Profits des entreprises par
             rapport et PIB au prix du marché",
             lwd=2.5,  
             ylab='',  spaghetti=TRUE)
legend("topleft", legend=c("Rémunération des salariés","Profits des entreprises"), col=c(2,3), lty=2, bty="n")

#########################################################################
# D) Nous allons refaire l’exercice en C) mais cette fois en utilisant l’ensemble
#des données disponibles, ie du premier
#trimestre de 1961 jusqu’au dernier deuxième trimestre de 2022.

rm(list=ls())
#a) Dans un premier temps, tracez l’évolution de la variable rémunération 
#des salariés, du premier #trimestre de 2012 jusqu’à la dernière donnée disponible.
library(astsa)
#changeant le répertoire de travail   et chargeant les données
setwd("D:/Cours 2022-2023/AYA- 2022-2023/Micro 3/UOttawa/MacroIII/ECO35522022/FallTerm/12sep2022")
donnees4 <- read.delim("pibrev.txt",stringsAsFactors = FALSE,header = TRUE)

# créations des séries
ren<- as.ts(donnees4$Rémunération.des.salariés..Dollars)
x11();tsplot(ren, col = c(2,3),
             main = "Rémunération des salariés pour toute la periode",
             lwd=2.5,  
             ylab='',  spaghetti=TRUE)

profit   <-  as.ts(donnees4$Excédent.d.exploitation.brut..Dollars+donnees4$Revenu.mixte.brut..Dollars+
  donnees4$Impôts.moins.les.subventions.sur.la.production..Dollars+
  donnees4$Impôts.moins.les.subventions.sur.la.production..Dollars+
  donnees4$Divergence.statistique..Dollars)
x11();tsplot(profit, col = c(2,3),
             main = "Profits des entreprises pour toute la periode",
             lwd=2.5,  
             ylab='',  spaghetti=TRUE)


PIB_global    <-    as.ts(donnees4$Produit.intérieur.brut.aux.prix.du.marché..Dollars)

# part des Rémunération des salariés en Dollars dans le PIB au prix du marché
Globalz1  <-100*ren/PIB_global

# part des profits des entreprises en Dollars
Globalz2  <-100*profit/PIB_global


Globald4<-cbind(Globalz1,Globalz2)

# traçons sur un même graphique 
x11();tsplot(Globald4, col = c(2,3),
             main = "Rémunération des salariés et Part des Profits des entreprises par
             rapport et PIB au prix du marché pour toute la periode",
             lwd=2.5,  
             ylab='',  spaghetti=TRUE)
legend("topleft", legend=c("Rémunération des salariés","Profits des entreprises"), col=c(2,3), lty=2, bty="n")




#############################################Question 3#################################
rm(list=ls())

#A) Construisons, en R, la matrice D.


s       <- 0.25     # s est le taux d’épargne constant des ménages
A       <-  2        # A est le niveau de productivité dans l’économie
alpha   <- 1/3   # alpha est la part du capital dans la production
gamma   <- 0.1   # gamma est le taux de dépréciation du capital productif.

D <- matrix (0L,6,6)
D[1,2]=1.00;D[1,3]=-1.00
D[2,1]=1.00;
D[3,1]=-(1-s);D[3,4]=1.0
D[4,1]=1.00;D[4,3]=-1.00;D[4,4]=-1.0
D[5,5]=1.00;
D[6,1]=-(1-alpha);D[6,6]=1.00

#B) Assumons maintenant que le stock de capital initial k0 = 1.
k0    <-1.00
b <- matrix (0L,6,1)
b[1,1] <- (1-gamma)*k0
b[2,1] <- A+(k0^alpha)
b[5,1] <- A*alpha*k0^(alpha-1)


#c) En utilisant vos résultats en A) et B), 
# calculons la valeur des variables endogènes à la période t = 1,
x    <-     solve(D)%*%b
print(x)

# À partir de cette réponse en C), qui nous donne le stock de capital à la fin de 
#la période t = 1, nous calculons le nouveau vecteur b et
# nous calculez la valeur des variables endogènes à la période t = 2
k0 = x[2,1]
b[1,1] <- (1-gamma)*k0
b[2,1] <- A+(k0^alpha)
b[5,1] <- A*alpha*k0^(alpha-1)

x    <-     solve(D)%*%b
print(x)




#E) En utilisant une boucle tel que vu dans les notes de cours, nous utilisons
#la procédure proposée en D) pour calculer la valeur des variables endogènes
# pour les périodes t = 1 à t = 200.  

D <- matrix (0L,6,6)
D[1,2]=1.00;D[1,3]=-1.00
D[2,1]=1.00;
D[3,1]=-(1-s);D[3,4]=1.0
D[4,1]=1.00;D[4,3]=-1.00;D[4,4]=-1.0
D[5,5]=1.00;
D[6,1]=-(1-alpha);D[6,6]=1.00

#B) Assumons maintenant que le stock de capital initial k0 = 1.
k0    <-1.00
b <- matrix (0L,6,200)
b[1,1] <- (1-gamma)*k0
b[2,1] <- A+(k0^alpha)
b[5,1] <- A*alpha*k0^(alpha-1)

z <- matrix(0L,6,200)
z
for (i in 2:200){
  z[,i] <- solve(D)%*%b[,i-1]
  k0 = z[2,i]
  b[1,i] <- (1-gamma)*k0
  b[2,i] <- A*(k0^alpha)
  b[5,i] <- A*alpha*k0^(alpha-1)
  
}
# solution 
z[,200]

###################################################################################
##########################################################Questions supp##########3

# Question 1
# a)
Z <- matrix(c(500,350,320,360),2,2,byrow=TRUE)
X <- matrix(c(1000,800))
I <- matrix(c(1,1))
f <- X- Z%*%I
f

#b)

A <-matrix(c(Z[1,1]/X[1],Z[1,2]/X[2],Z[2,1]/X[1],Z[2,2]/X[2]),2,2,byrow=TRUE)
f <- matrix(c(200,100))
I <- diag(3)
X <- solve(I-A)%*%f

X

#Question 2
# 2a)
X=matrix(c(1000,500,1000))
Z <- matrix(c(350,0,0,50,250,150,200,150,550),3,3,byrow=TRUE)

A <- matrix(c(Z[1,1]/X[1],Z[1,2]/X[2],Z[1,3]/X[3],Z[2,1]/X[1],Z[2,2]/X[2],Z[2,3]/X[3],
              Z[3,1]/X[1],Z[3,2]/X[2],Z[3,3]/X[3]),3,3,byrow=TRUE)
A

#
# le tableau des exigences totales ou l’inverse de Leontief L

L <- solve(I-A)
print(L)


#2b)
#la demande finale associée à ces données
f= solve(L)%*%X
print(f)


#2c) Supposons que nous prévoyons f :
f <- matrix(c(1300,100,200))
X <- solve(I-A)%*%f

print(X)

# Question 3
# a)

Z <- matrix(c(2,8,6,4),2,2,byrow=TRUE)

f <- matrix(c(20,20))
I <- matrix(c(1,1))
X <- Z%*%I + f
print(X)

#b)
A <-matrix(c(Z[1,1]/X[1],Z[1,2]/X[2],Z[2,1]/X[1],Z[2,2]/X[2]),2,2,byrow=TRUE)
print(A)



#c)
f <- matrix(c(15,18))
I<-diag(2)
X <- solve(I-A)%*%f
print(X)

# Zij=aij*xj
Z<-matrix(0L,2,2)
Z[1,1]=A[1,1]*X[1]
Z[1,2]=A[1,2]*+X[2]
Z[2,1]=A[2,1]*X[1]
Z[2,2]=A[2,2]*X[2]
print(Z)

# Question 4

#a
Z <- matrix(c(6,2,4,2),2,2,byrow=TRUE)
X <- matrix(c(20,15))
A<-matrix(c(Z[1,1]/X[1],Z[1,2]/X[2],Z[2,1]/X[1],Z[2,2]/X[2]),2,2,byrow=TRUE)

I<-diag(2)


f= (I-A)%*%X
print(f)


#b
Xtilde <- matrix(0L,2,10)

X<-f
Xtilde[,1]=X
AA <- diag(2)
for (i in 1:10){
  AA<-AA %*% A
  X <-X +AA%*% f
  Xtilde[,i]=X

}
print(Xtilde)



# Question 5
# a)


X <- matrix(c(22, 18,31))
f <- matrix(c(22-3-8-6, 18-2-4-5,31-7-3-9))
print(f)

Z <- matrix(c(3,8,6,2,4,5,7,3,9),3,3,byrow=TRUE)

A <- matrix(c(Z[1,1]/X[1],Z[1,2]/X[2],Z[1,3]/X[3],Z[2,1]/X[1],Z[2,2]/X[2],Z[2,3]/X[3],
              Z[3,1]/X[1],Z[3,2]/X[2],Z[3,3]/X[3]),3,3,byrow=TRUE)
#"A"
print(A)


# "L"
I <-diag(3)
L <-solve(I-A)
print(L)


# b)

i <- 0
suite <-TRUE


  
X<-f
Xtilde[,1]=X
AA <- diag(2)
1 <-0
while(i < 500)
{
  AA<-AA %*% A
  if (all(AA==0)) break
  X <-X +AA%*% f
  Xtilde[,i]=X
  i <-i+1
  
  
}
print(Xtilde)
  

