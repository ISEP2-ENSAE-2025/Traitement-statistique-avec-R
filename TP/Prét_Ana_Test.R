# pretraitement_consommation.R
# ------------------------------------------------------------
# Objectif : Préparer les données SEN2018 pour analyse de consommation
#  - Renommage des variables
#  - Calcul des totaux (alimentaire & non alimentaire)
#  - Gestion des valeurs manquantes
#  - Filtrage des ménages à consommation nulle et doublons
#  - Ajout de la variable zone (Urbaine / Rurale)
# ------------------------------------------------------------

# 0. Chargement des librairies
library(haven)    # Lecture des fichiers .dta
library(dplyr)    # Manipulation de données
library(tidyr)    # Pivot et gestion des NA
library(stringr)  # Traitement des chaînes

# 1. Extraction et lecture des données
zip_path <- "/Users/HP/Downloads/SEN2018_menage.zip"
out_dir  <- "/Users/HP/Downloads/SEN2018_menage"
if (!dir.exists(out_dir) && file.exists(zip_path)) {
  unzip(zip_path, exdir = out_dir)
}
dta_files <- list.files(out_dir, pattern = "\\.dta$", full.names = TRUE, recursive = TRUE)
data_list <- setNames(lapply(dta_files, read_dta), basename(dta_files))

# 2. Renommage des variables
# 2.1 Alimentaire : s08b02a...s08b02j -> food_cereal, food_vegetable, ..., food_condiment
food_map <- c(
  a = "cereal", b = "vegetable", c = "dairy", d = "meat", e = "fish",
  f = "oil",     g = "fruit",     h = "sugary_drink", i = "sugar", j = "condiment"
)
cons_food <- data_list[["s08b1_me_SEN2018.dta"]] %>%
  rename_with(.fn = function(x) {
    key <- str_match(x, "^s08b02([a-j])$")[,2]
    ifelse(!is.na(key), paste0("food_", food_map[key]), x)
  })

# 2.2 Non alimentaire : s09bq01...s09bq08 -> nonfood_1 ... nonfood_8
cons_nonfood <- data_list[["s09b_me_SEN2018.dta"]] %>%
  rename_with(.fn = function(x) {
    key <- str_match(x, "^s09bq0([1-8])$")[,2]
    ifelse(!is.na(key), paste0("nonfood_", key), x)
  })

# 3. Création d'un identifiant ménage unique
cons_food    <- cons_food    %>% mutate(id = paste(grappe, menage, sep = "_"))
cons_nonfood <- cons_nonfood %>% mutate(id = paste(grappe, menage, sep = "_"))

# 4. Calcul des totaux par ménage
# 4.1 Total alimentaire
df_food_totals <- cons_food %>%
  select(id, starts_with("food_")) %>%
  mutate(food_total = rowSums(across(starts_with("food_")), na.rm = TRUE))
# 4.2 Total non alimentaire
df_nonfood_totals <- cons_nonfood %>%
  select(id, starts_with("nonfood_")) %>%
  mutate(nonfood_total = rowSums(across(starts_with("nonfood_")), na.rm = TRUE))

# 5. Jointure des totaux
df_totals <- inner_join(df_food_totals, df_nonfood_totals, by = "id")

# 6. Gestion des valeurs manquantes
# 6.1 Calcul du pourcentage de NA par variable
na_pct <- df_totals %>%
  summarise(across(-id, ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na")
# 6.2 Imputation médiane pour variables ayant <= 5% de NA
vars_imp <- na_pct %>% filter(pct_na > 0, pct_na <= 0.05) %>% pull(variable)
for (v in vars_imp) {
  med <- median(df_totals[[v]], na.rm = TRUE)
  df_totals[[v]][is.na(df_totals[[v]])] <- med
}
# 6.3 Suppression des variables avec > 20% de NA
vars_drop <- na_pct %>% filter(pct_na > 0.20) %>% pull(variable)
if (length(vars_drop) > 0) {
  df_totals <- df_totals %>% select(-all_of(vars_drop))
}

# 7. Filtrage des ménages à consommation nulle et suppression des doublons
#    On garde un seul enregistrement par id

df_clean <- df_totals %>%
  filter(food_total > 0, nonfood_total > 0) %>%
  distinct(id, .keep_all = TRUE)

# 8. Ajout de la variable zone (Urbaine / Rurale)
df_zone <- data_list[["s00_me_SEN2018.dta"]] %>%
  mutate(id = paste(grappe, menage, sep = "_")) %>%
  select(id, s00q04) %>%
  rename(zone_code = s00q04) %>%
  mutate(zone = if_else(zone_code == 1, "Urbaine", "Rurale")) %>%
  select(id, zone)

# 9. base d'application
df_final <- inner_join(df_clean, df_zone, by = "id")

View(df_final)



# 02_descriptives.R ---------------------------------------------------
# Objectif : analyses descriptives avant tests

# 1. Statistiques de base par zone
desc_zone <- df_final %>%
  group_by(zone) %>%
  summarise(
    n_menages = n(),
    mean_food = mean(food_total), sd_food = sd(food_total),
    mean_nonfood = mean(nonfood_total), sd_nonfood = sd(nonfood_total)
  )
print(desc_zone)
View(desc_zone)

# 2. Histogrammes
hist(df_final$food_total, breaks=30,
     main="Distribution consommation alimentaire", xlab="food_total")
hist(df_final$nonfood_total, breaks=30,
     main="Distribution consommation non-alimentaire", xlab="nonfood_total")

# 3. Boxplots par zone
ggplot(df_final, aes(zone, food_total)) +
  geom_boxplot(fill="lightgreen") +
  labs(title="Conso alimentaire par zone", x="Zone", y="food_total")
ggplot(df_final, aes(zone, nonfood_total)) +
  geom_boxplot(fill="lightblue") +
  labs(title="Conso non-alimentaire par zone", x="Zone", y="nonfood_total")

# 4. Tests de normalité (Shapiro)
sh_food_u <- shapiro.test(df_final$food_total[df_final$zone=="Urbaine"])
sh_food_r <- shapiro.test(df_final$food_total[df_final$zone=="Rurale"])
sh_nonfood_u <- shapiro.test(df_final$nonfood_total[df_final$zone=="Urbaine"])
sh_nonfood_r <- shapiro.test(df_final$nonfood_total[df_final$zone=="Rurale"])
print(sh_food_u); print(sh_food_r)
print(sh_nonfood_u); print(sh_nonfood_r)

# 5. Homogénéité des variances (Levene)
print(leveneTest(food_total~zone, df_final))
print(leveneTest(nonfood_total~zone, df_final))


# 6. Effectif du ménage
roster <- data_list[["s01_me_SEN2018.dta"]] %>%
  mutate(id = paste(grappe, menage, sep = "_"))

df_indiv <- roster %>%
  group_by(id) %>% summarise(n_indiv = n(), .groups = "drop") %>%
  left_join(df_final, by = "id") %>%
  mutate(food_per_indiv = food_total / n_indiv)

# 7. Analyse : consommation vs taille ménage
df_plot <- df_indiv %>%
  filter(!is.na(n_indiv), !is.na(food_per_indiv), is.finite(food_per_indiv))

plot1 <- ggplot(df_plot, aes(x = n_indiv, y = food_per_indiv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Conso alimentaire par taille de ménage", x = "Taille du ménage", y = "Conso indiv")
print(plot1)

# 8. Test de corrélation
# Corrélation de Spearman avec note sur les ties
res_cor <- cor.test(df_plot$n_indiv, df_plot$food_per_indiv, method = "spearman")
print(res_cor)
cat("Note : la p-value est approximative à cause de valeurs identiques (ties).")



# 9. Comparaison entre zones
shapiro.test(df_indiv$food_per_indiv[df_indiv$zone == "Urbaine"])
shapiro.test(df_indiv$food_per_indiv[df_indiv$zone == "Rurale"])

# 10. Levene
leveneTest(food_per_indiv ~ zone, data = df_indiv)

# 11. Wilcoxon
wilcox.test(food_per_indiv ~ zone, data = df_indiv)

# 12 Taille d'effet
cohens_d(df_indiv, food_per_indiv ~ zone)



# 13. Modèle multivarié
model <- lm(log(food_per_indiv + 1) ~ zone + n_indiv, data = df_indiv)
summary(model)

