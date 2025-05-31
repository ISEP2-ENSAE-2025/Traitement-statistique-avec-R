# 📦 Chargement des packages
library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(FactoMineR)
library(factoextra)

# 1. Chargement des bases et sélection des variables utiles
s03 <- read_dta("C:/Users/moust/Downloads/s03_me_SEN2018 (1).dta") %>%
  select(vague, grappe, menage, s01q00a, s03q01, s03q02, s03q03, s03q04, s03q05, s03q06,
         s03q07, s03q08, s03q09, s03q39, s03q40) %>%
  rename(id_r = s01q00a)

menages_s03 <- s03 %>% select(vague, grappe, menage, id_r)

s01 <- read_dta("C:/Users/moust/Downloads/s01_me_SEN2018 (1).dta") %>%
  select(vague, grappe, menage, s01q00a, s01q01, s01q04a, s01q07) %>%
  rename(id_r = s01q00a) %>%
  semi_join(menages_s03, by = c("vague", "grappe", "menage", "id_r"))

s02 <- read_dta("C:/Users/moust/Downloads/s02_me_SEN2018 (1).dta") %>%
  select(vague, grappe, menage, s01q00a, s02q03, s02q04, s02q05, s02q06, s02q29) %>%
  rename(id_r = s01q00a) %>%
  semi_join(menages_s03, by = c("vague", "grappe", "menage", "id_r"))

s04 <- read_dta("C:/Users/moust/Downloads/s04_me_SEN2018 (1).dta") %>%
  select(vague, grappe, menage, s01q00a, s04q43, s04q57) %>%
  rename(id_r = s01q00a) %>%
  semi_join(menages_s03, by = c("vague", "grappe", "menage", "id_r"))

s11 <- read_dta("C:/Users/moust/Downloads/s11_me_SEN2018.dta") %>%
  select(vague, grappe, menage, s11q00, s11q01, s11q02, s11q04, s11q05, s11q32,
         s11q33__1, s11q33__2, s11q33__3, s11q33__4, s11q33__5, s11q33__6, s11q33__7) %>%
  rename(id_r = s11q00) %>%
  semi_join(menages_s03, by = c("vague", "grappe", "menage", "id_r"))

# 2. Fusion des bases
base_final <- s03 %>%
  left_join(s01, by = c("vague", "grappe", "menage", "id_r")) %>%
  left_join(s02, by = c("vague", "grappe", "menage", "id_r")) %>%
  left_join(s04, by = c("vague", "grappe", "menage", "id_r")) %>%
  left_join(s11, by = c("vague", "grappe", "menage", "id_r"))

stopifnot(nrow(base_final) == nrow(s03))

# 3. Imputation simple des valeurs manquantes
mode_impute <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(x)
  mode_val <- names(sort(table(ux), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_val
  return(x)
}

base_final_imputed <- base_final %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), mode_impute)) %>%
  mutate(across(where(is.factor), ~ forcats::fct_explicit_na(., na_level = "Inconnu")))

# 4. Renommage variables clés (exemple)
base_final_imputed <- base_final_imputed %>%
  rename(
    probleme_sante = s03q01,
    type_probleme = s03q02,
    a_consulte = s03q03,
    type_structure = s03q07,
    satisfaction_soins = s03q09
  )

# 5. Analyses descriptives simples
print(table(base_final_imputed$probleme_sante))
print(round(prop.table(table(base_final_imputed$probleme_sante)) * 100, 2))

malades <- filter(base_final_imputed, probleme_sante == 1)
print(sort(table(malades$type_probleme), decreasing = TRUE))

print(table(malades$a_consulte))
print(round(prop.table(table(malades$a_consulte)) * 100, 2))

malades_consultes <- filter(malades, a_consulte == 1)
print(table(malades_consultes$type_structure))
print(round(prop.table(table(malades_consultes$type_structure)) * 100, 2))

print(table(malades_consultes$satisfaction_soins))
print(round(prop.table(table(malades_consultes$satisfaction_soins)) * 100, 2))

# 6. Graphiques simples
ggplot(base_final_imputed, aes(x = probleme_sante)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Avez-vous eu un problème de santé ?", x = "Réponse", y = "Nombre d'individus") +
  theme_minimal()

ggplot(malades, aes(x = a_consulte)) +
  geom_bar(fill = "seagreen") +
  labs(title = "Recours aux soins chez les malades", x = "A consulté ?", y = "Nombre d'individus") +
  theme_minimal()

ggplot(malades_consultes, aes(x = type_structure)) +
  geom_bar(fill = "orange") +
  labs(title = "Type de structure consultée", x = "Structure", y = "Nombre de patients") +
  theme_minimal()

ggplot(malades_consultes, aes(x = satisfaction_soins)) +
  geom_bar(fill = "purple") +
  labs(title = "Satisfaction vis-à-vis des soins", x = "Satisfaction", y = "Nombre de patients") +
  theme_minimal()

# 7. Variables santé et socio-économiques sélectionnées
vars_sante <- c("probleme_sante", "type_probleme", "a_consulte", "s03q04", "s03q05", 
                "s03q06", "type_structure", "s03q08", "satisfaction_soins", "s03q39", "s03q40")

vars_socioeco <- c("s01q01", "s01q04a", "s01q07", "s02q29", "s04q43", "s04q57",
                   "s11q01", "s11q02", "s11q04", "s11q05", "s11q32")

# 8. Préparation dataframe ACP : retirer lignes avec NA sur variables santé + variable groupe s04q57
library(tidyr)

df_acp <- base_final_imputed %>%
  select(all_of(vars_sante), s04q57) %>%
  drop_na()

# 9. Sous-échantillonnage pour ACP et visualisation
set.seed(123)
n_samples <- min(1000, nrow(df_acp))
sample_indices <- sample(nrow(df_acp), n_samples)

# 10. ACP sur sous-échantillon (variables santé)
acp_res_sub <- PCA(df_acp[sample_indices, vars_sante], scale.unit = TRUE, ncp = 5, graph = FALSE)

# 11. Visualisation ACP avec habillage par variable socio-éco s04q57
fviz_pca_ind(acp_res_sub,
             geom = "point",
             habillage = factor(df_acp$s04q57[sample_indices]),
             palette = "jco",
             addEllipses = TRUE,
             label = "none")

# 12. Corrélations santé vs socio-éco sur l’ensemble (sans NA)
df_selected <- base_final_imputed %>%
  select(all_of(c(vars_sante, vars_socioeco))) %>%
  drop_na() %>%
  mutate(across(everything(), as.numeric))

cor_mat <- cor(df_selected[, vars_sante], df_selected[, vars_socioeco], use = "pairwise.complete.obs")
print(round(cor_mat, 2))
