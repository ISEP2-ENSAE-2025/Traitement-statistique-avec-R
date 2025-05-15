library(haven)    # lecture .dta
library(dplyr)    # manipulation
library(tidyr)    # gestion des NA
library(ggplot2)  # graphiques


#####


##1. Préparation du workspace

zip_path <- "/Users/HP/Downloads/SEN2018_menage.zip"
out_dir  <- "/Users/HP/Downloads/SEN2018_menage"
if (!dir.exists(out_dir)) {
  if (file.exists(zip_path)) {
    utils::unzip(zipfile = zip_path, exdir = out_dir)
    message("Extraction réussie dans ", out_dir)
  } else {
    stop("Fichier zip introuvable : ", zip_path)
  }
} else {
  message("Le dossier existe déjà : ", out_dir)
}
dta_files <- list.files(out_dir, pattern = "\\.dta$", full.names = TRUE, recursive = TRUE)
if (length(dta_files) == 0) stop("Aucun .dta trouvé dans ", out_dir)


##2. Lecture et exploration initiale

# On lit tous les fichiers .dta et on les stocke dans une liste nommée 'liste_data'.
# setNames() attribue à chaque élément de la liste le nom de son fichier sans son chemin.
liste_data <- setNames(
  lapply(dta_files, read_dta),   # Parcourt chaque chemin de fichier et lit le .dta
  basename(dta_files)            # Extrait seulement le nom du fichier pour nommer la liste
)

# Boucle pour afficher un aperçu et les colonnes de chaque table
for (nom in names(liste_data)) {
  # Affiche une ligne titre avec le nom de la table
  cat("--- Table :", nom, "---
")
  
  # head() affiche les 5 premières lignes pour voir la structure des données
  print(head(liste_data[[nom]], 5))
  
  # names() récupère tous les noms de colonnes;
  # paste(..., collapse = ", ") les transforme en une seule chaîne séparée par des virgules
  cat(
    "Colonnes :", 
    paste(names(liste_data[[nom]]), collapse = ", "),
    "

"
  )
}


###3. Identification des variables clés

# 1. On définit une fonction qui cherche dans les noms de colonnes ceux correspondant à un motif donné
grep_vars <- function(pattern, df) {
  grep(pattern,                # expression régulière à chercher
       names(df),              # vecteur des noms de colonnes
       value = TRUE,           # retourner les noms qui correspondent
       ignore.case = TRUE)     # ne pas tenir compte de la casse
}

# 2. Pour chaque table de la liste 'liste_data'
for (nom in names(liste_data)) {
  cat("Table :", nom, "
")    # affiche le nom de la table
  
  # Recherche des variables liées à la consommation alimentaire
  conso_vars <- grep_vars("conso|alim|food", liste_data[[nom]])
  cat("  Conso :", toString(conso_vars), "
")
  
  # Recherche des variables liées à la zone géographique
  zone_vars <- grep_vars("zone|urbain|rural|locale", liste_data[[nom]])
  cat("  Zone  :", toString(zone_vars), "

")
}


###4. Construction du jeu de données de travail. Construction du jeu de données de travail

# 1. Pour chaque table, on ajoute une colonne 'id' qui combine 'grappe' et 'menage' pour créer
#    un identifiant unique par ménage.
df_list <- lapply(
  liste_data,
  function(df) df %>% mutate(id = paste(grappe, menage, sep = "_"))
)

# 2. On sélectionne la table des zones de résidence ('s00_me_SEN2018.dta').
#    Cette table contient la variable 's00q04' codée 1 pour Urbaine, 2 pour Rurale.
tbl_zone <- df_list[["s00_me_SEN2018.dta"]] %>%
  # On ne garde que 'id' et la variable 's00q04'
  select(id, s00q04) %>%
  # On renomme 's00q04' en 'zone_code' pour plus de lisibilité
  rename(zone_code = s00q04) %>%
  # On transforme le code en libellé textuel : 1 → "Urbaine", 2 → "Rurale"
  mutate(zone = ifelse(zone_code == 1, "Urbaine", "Rurale")) %>%
  # On ne conserve que 'id' et le nouveau libellé 'zone'
  select(id, zone)

# 3. On sélectionne la table des consommations ('s08b1_me_SEN2018.dta').
tbl_conso_raw <- df_list[["s08b1_me_SEN2018.dta"]]

# 4. On repère toutes les colonnes commençant par 's08b02' qui indiquent les quantités
food_vars <- grep("^s08b02", names(tbl_conso_raw), value = TRUE)

# 5. On calcule la consommation totale par ménage :
#    - on ne garde que 'id' et les colonnes de quantité (food_vars)
#    - on fait la somme de ces colonnes ligne à ligne (rowSums), en ignorant les NA
#    - on sélectionne ensuite 'id' et le nouveau calcul 'conso_total'
tbl_conso <- tbl_conso_raw %>%
  select(id, all_of(food_vars)) %>%
  mutate(conso_total = rowSums(across(all_of(food_vars)), na.rm = TRUE)) %>%
  select(id, conso_total)

# 6. Enfin, on fusionne les deux tables 'tbl_conso' et 'tbl_zone' en une seule
#    sur la colonne commune 'id', pour obtenir un dataframe 'df'
#    contenant : id, conso_total et zone.
df <- inner_join(tbl_conso, tbl_zone, by = "id")

# 7. On vérifie la structure finale avec glimpse (aperçu compact).
glimpse(df)


###Après avoir construit le jeu de données, on enlève les ménages sans consommation
# Retirer tous les ménages dont la consommation totale est nulle
library(dplyr)
df <- df %>% filter(conso_total > 0)
message("Nombre de ménages après suppression des conso à zéro : ", nrow(df))
View(df)


###5. Nettoyage des données
dups <- df %>% group_by(id) %>% filter(n()>1)
if(nrow(dups)>0) {
  message("Doublons : ", nrow(dups), ". Suppression.")
  df <- df %>% distinct(id, .keep_all=TRUE)
}


###5.2 Valeurs manquantes

na_summary <- df %>% summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to="var", values_to="n_na") %>%
  mutate(pct_na = n_na/nrow(df)*100)
print(na_summary)
# Suppression imputation
vars_drop <- na_summary %>% filter(pct_na>0 & pct_na<5) %>% pull(var)
if(length(vars_drop)>0) df <- df %>% drop_na(all_of(vars_drop))
if(any(is.na(df$conso_total))) {
  med <- median(df$conso_total, na.rm=TRUE)
  df <- df %>% mutate(conso_total=replace_na(conso_total, med))
}
View(na_summary)


###6. Analyse descriptive
df %>% group_by(zone) %>% summarise(n=n(), moyenne=mean(conso_total), sd=sd(conso_total))
ggplot(df, aes(zone, conso_total)) + geom_boxplot() +
  labs(title="Conso alimentaire par zone", x="Zone", y="Conso totale")


# 1. Calcul des statistiques descriptives par zone :
#    - group_by(zone) regroupe les données par zone (Urbaine vs Rurale)
#    - summarise() calcule pour chaque groupe : effectif, moyenne et écart-type
stats_zone <- df %>%
  group_by(zone) %>%                    # Regrouper par zone
  summarise(
    n = n(),                            # Nombre de ménages par zone
    moyenne = mean(conso_total),        # Moyenne de la consommation totale
    sd = sd(conso_total)                # Écart-type de la consommation totale
  )
print(stats_zone)                       # Afficher les résultats
View(stats_zone)

# 2. Création d'un boxplot pour visualiser la distribution :
#    - aes(x=zone, y=conso_total) définit les axes
#    - geom_boxplot() dessine la boîte à moustaches
#    - labs() ajoute un titre et des étiquettes d'axes
ggplot(df, aes(x = zone, y = conso_total)) +
  geom_boxplot() +                      # Boîte à moustaches par zone
  labs(
    title = "Distribution de la consommation alimentaire par zone",
    x = "Zone (Urbaine vs Rurale)",
    y = "Consommation totale"
  )




###7. Tests statistiques

###7.1 Normalité

sh_u <- shapiro.test(df$conso_total[df$zone=="Urbaine"])
sh_r <- shapiro.test(df$conso_total[df$zone=="Rurale"])
sh_u; sh_r


###7.2 Homogénéité des variances

var_test <- var.test(conso_total~zone, data=df)
var_test


###7.3 Comparaison des moyennes

if(sh_u$p.value>0.05 & sh_r$p.value>0.05 & var_test$p.value>0.05) {
  res <- t.test(conso_total~zone, data=df)
  method <- "t-test"
} else {
  res <- wilcox.test(conso_total~zone, data=df)
  method <- "Wilcoxon"
}
print(res)
cat("Méthode :", method)



###7.4. Test de Levene pour l’égalité des variances

# avec le package car
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)
leveneTest(conso_total ~ zone, data = df)


###7.5. Test de Kolmogorov–Smirnov pour comparer les distributions

ks.test(
  x = df$conso_total[df$zone == "Urbaine"],
  y = df$conso_total[df$zone == "Rurale"],
  alternative = "two.sided"
)



###7.6. Taille d’effet (Cohen's d) avec rstatix

# Installation et chargement du package rstatix
if (!requireNamespace("rstatix", quietly = TRUE)) {
  install.packages("rstatix")
}
library(rstatix)

# Calcul de Cohen's d pour chaque groupe
res_cohen <- df %>%
  cohens_d(conso_total ~ zone, var.equal = FALSE)  # var.equal selon test de variances

# Affichage des résultats
print(res_cohen)

# Visualisation avec View
View(res_cohen)






###7.8. Test de permutation sur la différence des cumulatives

# Fonction pour calculer la statistique KS manuelle
obs_stat <- max(abs(ecdf(df$conso_total[df$zone=="Urbaine"])(df$conso_total[df$zone=="Rurale"]) -
                      ecdf(df$conso_total[df$zone=="Rurale"])(df$conso_total[df$zone=="Urbaine"])))

set.seed(123)
B <- 2000
perm_stats <- replicate(B, {
  perm_zone <- sample(df$zone)
  max(abs(ecdf(df$conso_total[perm_zone=="Urbaine"])(df$conso_total[perm_zone=="Rurale"]) -
            ecdf(df$conso_total[perm_zone=="Rurale"])(df$conso_total[perm_zone=="Urbaine"])))
})

p_perm <- mean(perm_stats >= obs_stat)
cat("Statistique observée =", round(obs_stat,4), "
")
cat("P-value par permutation (~ ", p_perm, ")
")



###7.9. Test du Khi² sur distribution discrète

# Discrétiser la consommation en classes (ex. tertiles)
df <- df %>% mutate(
  conso_cat = ntile(conso_total, 3)  # 3 classes : faible, moyen, élevé
)

# Tableau de contingence
tab <- table(df$conso_cat, df$zone)
print(tab)

# Test du Khi²
test_chi2 <- chisq.test(tab)
print(test_chi2)

# Calcul de la taille d'effet (Phi²)
phi2 <- test_chi2$statistic / sum(tab)
cat("Phi² =", round(phi2, 4), "
")

#- Classe 1 (faible conso) : plus de ménages ruraux (1441) que urbains (938).
#- Classe 2 (moyenne conso) : ménages urbains (1408) légèrement supérieurs aux ruraux (971).
#- Classe 3 (forte conso) : nettement plus de ménages urbains (1579) que ruraux (800).

#- **X-squared = 374.21, df = 2, p-value < 2.2e-16** :
 # - La statistique de test est **très élevée** et la p-value est **très faible**, bien en-dessous de 0,05.
#- **Hypothèse nulle** : répartition des ménages selon les catégories de conso est **indépendante** de la zone.
#- **Conclusion** : on **rejette** l’hypothèse nulle. Il existe une **association significative** entre la catégorie de consommation et la zone.

#- **Phi² = 0.0524** :
#  - Cette taille d’effet (Phi carré) est **faible** (< 0,1), ce qui signifie que bien que la relation soit statistiquement significative, l’ampleur de l’association reste **limité**.

#En résumé, le test Khi² confirme que la **distribution des ménages** dans les classes de consommation **diffère** entre zones, avec une proportion plus élevée de gros consommateurs en milieu urbain, mais l’effet global reste de **faible intensité**.




###7.10. ANOVA et η² (R²) pour mesurer la part de variance expliquée

# ANOVA
aov_res <- aov(conso_total ~ zone, data = df)
summary(aov_res)

# Calcul de l'eta carré (effect size) avec rstatix
if (!requireNamespace("rstatix", quietly = TRUE)) {
  install.packages("rstatix")
}
library(rstatix)

# La fonction eta_squared de rstatix peut ne pas accepter 'partial'; appel sans argument
etasq <- eta_squared(aov_res)
print(etasq)

#Conclusion ANOVA : il existe une différence hautement significative des moyennes de consommation entre zones.

#Interprétation de l’η² (R² interne) :
  
#etasq = 0.0390 signifie que 3.9 % de la variation totale de consommation est expliquée par la différence entre zones.

#C’est un effet de taille faible à modéré ; la zone influence la consommation, mais la majeure partie de la variance reste due à d’autres facteurs non étudiés.










