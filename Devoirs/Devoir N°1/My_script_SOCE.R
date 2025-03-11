# Création du DataFrame avec 10 variables
my_df <- data.frame(
  Rang = c(1:17),  # Variable numérique
  Nom = factor(c("Barcelone", "Atlético Madrid", "Real Madrid", "Atlétic Bilbao", "Villarreal", "Réal Betis", "Rayo Vallecano", "Majorque", "Real Sociedad", "Celta Vigo", "Osasuna", "Séville", "Girona", "Getafe", "Espanyol", "Leganés", "Las Palmas")),  # Facteur
  MJ = rep(26, 17),  # Variable numérique
  MG = c(18, 16, 16, 13, 12, 10, 9, 10, 10, 9, 7, 8, 9, 7, 7, 6, 6),  # Variable numérique
  MN = c(3, 8, 6, 9, 8, 8, 9, 6, 4, 6, 12, 9, 5, 9, 6, 9, 6),  # Variable numérique
  MP = c(5, 2, 4, 4, 5, 8, 8, 10, 12, 11, 7, 9, 12, 10, 12, 11, 14),  # Variable numérique
  BM = c(71, 43, 55, 44, 48, 34, 28, 25, 23, 38, 32, 31, 34, 21, 24, 23, 30),  # Variable numérique
  BE = c(25, 16, 25, 23, 35, 33, 27, 32, 27, 40, 37, 36, 39, 21, 36, 38, 44),  # Variable numérique
  Stade = factor(c("Camp Nou", "Wanda Metropolitano", "Santiago Bernabéu", "San Mamés", "Estadio de la Cerámica", "Benito Villamarín", "Vallecas", "Visit Mallorca Estadi", "Anoeta", "Balaídos", "El Sadar", "Ramón Sánchez Pizjuán", "Montilivi", "Coliseum Alfonso Pérez", "RCDE Stadium", "Butarque", "Gran Canaria")),  # Facteur
  Conférence = factor(c("Nord", "Sud", "Nord", "Nord", "Est", "Sud", "Sud", "Est", "Nord", "Ouest", "Nord", "Sud", "Est", "Sud", "Est", "Ouest", "Ouest"))  # Facteur
)

# Ajout de la colonne "GD" (Goal Difference)
my_df['GD'] <- my_df['BM'] - my_df['BE']

# Ajout de la colonne "Points" (3 points par victoire, 1 point par match nul)
my_df['Points'] <- 3 * my_df['MG'] + my_df['MN']

# Ajout de la colonne Ratio_but (Buts marqués par match)
my_df['Ratio_but'] <- my_df['BM'] / my_df['MJ']

# Affichage du DataFrame
print(my_df)

# Vérification des types de données
print(typeof(my_df$Rang))  # "integer"
print(class(my_df$Nom))    # "factor"

# Opérateurs arithmétiques
print(my_df$BM + my_df$BE)  # Somme des buts marqués et encaissés

# Opérateurs de comparaison
print(my_df$MG > my_df$MN)  # Vérifie si les matchs gagnés sont supérieurs aux matchs nuls

# Opérateurs logiques
print(my_df$Points > 30 & my_df$GD > 10)  # Vérifie si les points sont > 30 et la différence de buts > 10

# Indexation
print(my_df[, c("Nom", "Points")])  # Sélectionne les colonnes "Nom" et "Points"
my_df_30pts<- my_df[my_df$Points > 30, ]  # Filtre les équipes avec plus de 30 points
print(my_df_30pts)

# Création de séquences
# Répéter la séquence pour qu'elle ait la même longueur que le DataFrame
my_df$Sequence <- rep(seq(1, nrow(my_df), by = 2), length.out = nrow(my_df))
# Affichage du DataFrame
print(my_df)

# Répétition
print(my_df$Repetition <- rep(c("A", "B"), length.out = nrow(my_df)))  # Répète "A" et "B" sur 17 lignes

# Fonctions statistiques de base
print(mean(my_df$Points))  # Moyenne des points
print(median(my_df$GD))    # Médiane de la différence de buts
print(sd(my_df$Ratio_but)) # Écart-type du ratio de buts

# Gestion des valeurs manquantes (exemple)
my_df$BM[1] <- NA  # Ajout d'une valeur manquante pour l'exemple
print(is.na(my_df$BM))  # Détecter les valeurs manquantes
print(sum(is.na(my_df$BM)))  # Compter les valeurs manquantes
my_df$BM[is.na(my_df$BM)] <- 0  # Remplacer les NA par 0

# Charger dplyr
library(dplyr)
# Filtrer et sélectionner des colonnes avec dplyr
filtered_df <- my_df %>%
  dplyr::filter(Points > 30) %>%
  dplyr::select(Nom, Points, GD)
# Affichage du résultat
print(filtered_df)

# Ajouter une nouvelle colonne conditionnelle
my_df <- my_df %>%
  mutate(Categorie = if_else(Points > 30, "Haut", "Bas"))
print(my_df)

# Affichage final du DataFrame
print(my_df)