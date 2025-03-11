

# Création du DataFrame avec des données réelles sur 10 pays
df <- data.frame(
  Pays = c("Sénégal", "France", "États-Unis", "Chine", "Inde", "Brésil", "Nigéria", 
           "Allemagne", "Japon", "Afrique du Sud"),
  Capitale = c("Dakar", "Paris", "Washington", "Pékin", "New Delhi", "Brasília", 
               "Abuja", "Berlin", "Tokyo", "Pretoria"),
  Population = c(18.5, 68.0, 332.0, 1412.0, 1428.6, 216.4, 223.8, 83.2, 125.7, 60.0), # En millions
  PIB = c(28.5, 3000.0, 27000.0, 18000.0, 3700.0, 2200.0, 504.0, 4600.0, 4300.0, 399.0), # En milliards $
  Superficie = c(196722, 551695, 9833517, 9596961, 3287263, 8515767, 923769, 357022, 
                 377975, 1219090), # En km²
  Continent = c("Afrique", "Europe", "Amérique du Nord", "Asie", "Asie", "Amérique du Sud", 
                "Afrique", "Europe", "Asie", "Afrique"),
  Monnaie = c("Franc CFA", "Euro", "Dollar US", "Yuan", "Roupie", "Réal", "Naira", 
              "Euro", "Yen", "Rand"),
  Esperance_Vie = c(68, 82, 79, 78, 71, 76, 55, 81, 84, 65),
  IDH = c(0.512, 0.903, 0.921, 0.768, 0.633, 0.754, 0.539, 0.950, 0.925, 0.713),
  Langue = c("Français", "Français", "Anglais", "Mandarin", "Hindi", "Portugais", 
             "Anglais", "Allemand", "Japonais", "Anglais")
)

# 1️⃣ Vérification des types de données
str(df)  # Afficher les types de données
class(df$Population)  # Vérifier le type d'une colonne spécifique
class(df$Continent)

# 2️⃣ Opérateurs en R appliqués au DataFrame
df$Population <- df$Population + 10  # Ajouter 10 millions d’habitants à chaque pays
df$PIB_Habitant <- df$PIB * 1e9 / (df$Population * 1e6)  # Calcul du PIB par habitant
df$GrandePopulation <- df$Population > 100  # TRUE si la pop > 100M
df$BonIDH <- df$IDH > 0.6 & df$Continent == "Afrique"  # Sélection IDH + Afrique
df$RangPIB <- rank(-df$PIB)  # Classement du PIB

# 3️⃣ Manipulation des données
head(df)  # Afficher les 6 premières lignes
tail(df)  # Afficher les 6 dernières lignes
df[df$IDH > 0.8, ]  # Filtrer les pays avec un IDH > 0.8
df[, c("Pays", "IDH")]  # Sélectionner seulement les colonnes Pays et IDH
df$Catégorie_IDH <- ifelse(df$IDH > 0.7, "Élevé", "Faible")  # Ajouter une colonne conditionnelle

# 4️⃣ Statistiques de base
mean(df$Population)  # Moyenne de la population
sd(df$Population)  # Écart-type de la population
summary(df)  # Résumé statistique

# 5️⃣ Gestion des valeurs manquantes
df$IDH[2] <- NA  # Simuler une valeur manquante
sum(is.na(df$IDH))  # Nombre de valeurs manquantes
df$IDH[is.na(df$IDH)] <- mean(df$IDH, na.rm = TRUE)  # Remplacement des NA par la moyenne

# 6️⃣ Exportation du DataFrame
write.csv(df, "data_pays.csv", row.names = FALSE)

# 7️⃣ Inspection des données
dim(df)  # Nombre de lignes et de colonnes
nrow(df)  # Nombre de lignes
ncol(df)  # Nombre de colonnes
