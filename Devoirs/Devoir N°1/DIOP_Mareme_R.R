# script pours cours 11/03


#les types d'objets dans R

#VECTEUR
  vect_pays<-c("Sénégal","Mali","Guinée","Tchad","Rwanda", "Cameroun")
  vect_pays

# FACTEUR
  x=c("CEDEAO","CEMAC")
  fact= factor(rep(x, each= 3), levels=c("CEDEAO","CEMAC"))
  fact             
  ?factor

#LISTE ET DATA FRAME

liste <- list(
  pays= vect_pays,
  Zone_eco= fact)
print(liste)

#CREATION D'UN DATAFRAME DE 100 OBSERVATIONS QUI SONT DES TROUPEAUX
df <- data.frame( 
  Region = factor(sample(c("Nord", "Sud", "Est", "Ouest"), 
                         size = 100, 
                         replace = TRUE)),
  
  Systeme_Elevage = factor(sample(c("Pastoral", "Agropastoral", "Transhumant"), 
                                  size = 100, 
                                  replace = TRUE, 
                                  prob = c(0.3, 0.4, 0.3))),
  
  Composition_Troupeau = factor(sample(c("Bovins", "Ovins", "Caprins", "Mixte"), 
                                       size = 100,
                                       replace = TRUE,
                                       prob = c(0.3, 0.2, 0.2, 0.3))),
  
  cout_alim_annuel = rnorm(100, mean = 300, sd = 100),
  
  Taille_troup = rpois(100, lambda = 45) + 5,
  
  Acces_Services_Veterinaires = factor(sample(c("Non", "Limité", "Régulier"), 
                                              size = 100, 
                                              replace = TRUE, 
                                              prob = c(0.4, 0.4, 0.2)))
)

df
#Ajout de colonne dans le dataframe
df$revnus= rnorm(100, mean = 400, sd= 100 )
df

#MATRICES
?matrix
# Résumé sommaire du df avec des stats desc
summary(df)

#quelques statistiques pertinentes

mean(df$cout_alim_annuel)               # Moyenne
median(df$cout_alim_annuel)             # Médiane
min(df$cout_alim_annuel)                # Minimum
max(df$cout_alim_annuel)                # Maximum
range(df$cout_alim_annuel)              # Étendue (min et max)
sd(df$cout_alim_annuel)                 # Écart-type
var(df$cout_alim_annuel)   
  
## ou encore plus simplement

summary(df$revnus)
summary(df$cout_alim_annuel)



