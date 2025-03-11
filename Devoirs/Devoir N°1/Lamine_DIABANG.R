# 1. Création de données
employes_df <- data.frame(
  id = 1:5,
  nom = c("Habib", "Paco", "Souleye", "Moussa", "Awa"),
  age = c(21, 22, 35, 40, 28),
  actif = c(TRUE, FALSE, TRUE, TRUE, FALSE)
)
print(employes_df)

# Création d'une matrice de performances
perf_matrix <- matrix(1:9, nrow = 3, ncol = 3)
print(perf_matrix)

# Création d'un array (tableau multidimensionnel)
perf_array <- array(1:24, dim = c(2,3,4))
print(perf_array)

# 2. Vérifications et opérations logiques
print(is.data.frame(employes_df))  # Vérifie si employes_df est un data.frame
print(is.matrix(perf_matrix))  # Vérifie si perf_matrix est une matrice

print(TRUE & FALSE)
print(TRUE | FALSE)
print(!TRUE)
print(c(TRUE, FALSE, TRUE) & c(TRUE, TRUE, FALSE))
print(TRUE && FALSE)
print(TRUE || FALSE)

# 3. Manipulation de données avec les pipes
library(magrittr)

print(1:10 %>% mean() %>% round(2))  # Pipe classique %>%
print(1:10 |> mean() |> round())  # Pipe natif |>

# Création de séquences
print(seq(1, 10, by = 2))
print(seq(0, 1, length.out = 5))

# Répétition de séquences
print(rep(1:3, times = 2))
print(rep(1:3, each = 2))

# 4. Indexation et Sélection
x <- c(10, 20, 30, 40, 50)
print(x[1])       # Premier élément
print(x[c(1,3)])  # 1ère et 3ème position
print(x[-2])      # Tous sauf le 2ème

# Sélection de colonnes et filtrage dans employes_df
library(dplyr)

print(employes_df$age)  # Affiche la colonne âge
print(employes_df[, "age"])  # Même chose que la ligne précédente
print(employes_df[, 3])  # Sélection par position de colonne

# Sélection et filtrage avec dplyr
df_filtre <- employes_df %>%
  select(id, age) %>%
  filter(age > 30)
print(df_filtre)

# Ajout d'une nouvelle colonne avec mutate
employes_df <- employes_df %>%
  mutate(categorie = if_else(age > 30, "expérimenté", "jeune"))
print(employes_df)

# 5. Statistiques descriptives
donnees <- c(12, 15, 21, 18, 24, 19, 22)
print(mean(donnees))   # Moyenne
print(median(donnees)) # Médiane
print(sd(donnees))     # Écart-type
print(var(donnees))    # Variance
