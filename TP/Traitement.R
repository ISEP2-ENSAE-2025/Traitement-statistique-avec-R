## Définition du chemin vers les données EHCVM du Sénégal
senedata <- "C:/Users/HP/Desktop/ISEP2/Semestre2/Logiciel R/Cours/EHCVM_Données_SENEGAL"



library(haven)      
library(tidyverse)   
library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------



# Module 0 : Données d'identification
Identification_data <- read_dta(paste0(senedata, "/s00_me_SEN2018.dta"))


# Module 1A et 1B : Caractéristiques sociodémographiques
socio_data <- read_dta(paste0(senedata, "/s01_me_SEN2018.dta"))

# Module 2 : Éducation
education_data <- read_dta(paste0(senedata, "/s02_me_SEN2018.dta"))

# Module 4A, 4B, 4C : Emploi
emploi_data <- read_dta(paste0(senedata, "/s04_me_SEN2018.dta"))

# Module 5 : Revenu hors emploi
revenu_data <- read_dta(paste0(senedata, "/s05_me_SEN2018.dta"))

# Module 6 : Épargne et Crédit
epargne_credit_data <- read_dta(paste0(senedata, "/s06_me_SEN2018.dta"))

# Module 11 : Caractéristiques du logement
logement_data <- read_dta(paste0(senedata, "/s11_me_SEN2018.dta"))

# Module 13A : Transferts reçus
transfert_data <- read_dta(paste0(senedata, "/s13a_1_me_SEN2018.dta"))





## Exploration de epargne_credit_data

# Pour chaque dataframe, on génère les identifiants
# Identification_data
Identification_data <- Identification_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_"),
  )
names(Identification_data)

# Sélection des colonnes utiles 
Identification_data <- Identification_data[, c("id_menage", "s00q01", "s00q04")]

# Renommage pour plus de lisibilité
colnames(Identification_data) <- c("id_menage","Region", "Milieu_de_residence")

















# Socio_data
socio_data <- socio_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_"),
    id_individu = if ("s01q00a" %in% names(socio_data)) {
      paste(vague, grappe, menage, s01q00a, sep = "_")
    } else {
      NA_character_
    }
  )

names(socio_data)


# Sélection des colonnes utiles (ajout de la catégorie socioprofessionnelle du père)
socio_data <- socio_data[, c("id_menage", "id_individu", "s01q01", "s01q03c", "s01q07", 
                             "s01q10", "s01q14", "s01q15", "s01q16", "s01q36", "s01q27")]

# Renommage pour plus de lisibilité
colnames(socio_data) <- c("id_menage", "id_individu", "sexe", "annee_naissance", 
                          "situation_matrimoniale", "age_premier_mariage", 
                          "religion", "nationalite", "ethnie", "telephone", "socio_pro_pere")

# Création de l'âge en se basant sur l'année actuelle (2025)
socio_data$age <- 2018 - as.numeric(socio_data$annee_naissance)









# Education_data
education_data <- education_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_"),
    id_individu = if ("s01q00a" %in% names(education_data)) {
      paste(vague, grappe, menage, s01q00a, sep = "_")
    } else {
      NA_character_
    }
  )



# Sélection des colonnes utiles

education_data <- education_data[, c("id_menage", "id_individu", 
                                     "s02q03", "s02q29", "s02q33")]

# Renommage pour plus de lisibilité
colnames(education_data) <- c("id_menage", "id_individu", 
                              "etude_formelle", "niveau_etudes_max", "diplome_obtenu")

# Création de la variable d'alphabétisation (1 si sait lire/écrire, 0 sinon)
education_data$alphabetisation <- ifelse(
  education_data$etude_formelle == 1 & !is.na(education_data$niveau_etudes_max), 1, 0)


















# Emploi_data
emploi_data <- emploi_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_"),
    id_individu = if ("s01q00a" %in% names(emploi_data)) {
      paste(vague, grappe, menage, s01q00a, sep = "_")
    } else {
      NA_character_
    }
  )


# Sélection des colonnes utiles avec les noms d'origine
emploi_data <- emploi_data[, c("id_menage", "id_individu", 
                               "s04q39",    # Catégorie socioprofessionnelle
                               "s04q30c",   # Branche d'activité principale
                               "s04q43")]   # Salaire principal (montant)

# Renommage des colonnes pour plus de lisibilité
colnames(emploi_data) <- c("id_menage", "id_individu", 
                           "categorie_socio_pro", "branche_activite", "salaire_total")














# Revenu_data
revenu_data <- revenu_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_"),
    id_individu = if ("s01q00a" %in% names(revenu_data)) {
      paste(vague, grappe, menage, s01q00a, sep = "_")
    } else {
      NA_character_
    }
  )




# Création des indicatrices
revenu_data <- revenu_data %>%
  mutate(
    # 1. Pensions : retraite, veuvage, invalidité, alimentaire
    a_recu_pension = if_else(s05q01 == 1 | s05q03 == 1 | s05q05 == 1 | s05q07 == 1, 1, 0),
    
    # 2. Revenus locatifs : loyers de maison d'habitation
    a_recu_loyer = if_else(s05q09 == 1, 1, 0),
    
    # 3. Revenus financiers : mobiliers, financiers, placements
    a_recu_revenu_financier = if_else(s05q11 == 1, 1, 0),
    
    # 4. Autres revenus : loterie, héritage, vente de biens, etc.
    a_recu_autre_revenu = if_else(s05q13 == 1, 1, 0)
  )



# Sélection des colonnes utiles avec les noms d'origine
revenu_data <- revenu_data %>%
  select(id_menage, id_individu, 
         s05q02, s05q04, s05q06, s05q08, # Pensions
         s05q10,                         # Loyer
         s05q12,                         # Revenu financier
         s05q14,                         # Autres revenus
         a_recu_pension, a_recu_loyer, a_recu_revenu_financier, a_recu_autre_revenu)

# Calcul du revenu total hors emploi (somme des montants)
revenu_data <- revenu_data %>%
  mutate(
    revenu_hors_emploi_total = rowSums(across(c(s05q02, s05q04, s05q06, s05q08, s05q10, s05q12, s05q14), ~replace_na(., 0))),
    a_revenu_hors_emploi = if_else(revenu_hors_emploi_total > 0, 1, 0)
  )

# Renommage des colonnes pour plus de clarté
colnames(revenu_data) <- c("id_menage", "id_individu", 
                           "pension_retraite", "pension_veuvage", 
                           "pension_inval", "pension_alimentaire", 
                           "loyer_annuel", "revenu_mobilier", 
                           "autres_revenus",
                           "pension", "loyer", 
                           "revenu_financier", "autre_revenu",
                           "revenu_hors_emploi_total", "a_revenu_hors_emploi")












# Logement_data
logement_data <- logement_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_")
  )




# Sélection des colonnes utiles avec les noms d'origine
logement_data <- logement_data[, c("id_menage", 
                                   "s11q04",    # Statut d'occupation
                                   "s11q09a",   # Titre de propriété
                                   "s11q16",    # Crédit immobilier en cours
                                   "s11q22",    # Accès à l'eau courante
                                   "s11q34",    # Accès à l'électricité
                                   "s11q46")]   # Accès à Internet

# Création de la variable "acces_infrastructures"
logement_data$acces_infrastructures <- ifelse(rowSums(logement_data[, c("s11q22", "s11q34", "s11q46")], 
                                                      na.rm = TRUE) > 0, 1, 0)

# Renommage pour plus de clarté
colnames(logement_data) <- c("id_menage", 
                             "statut_occupation", 
                             "titre_propriete", 
                             "credit_immobilier", 
                             "eau_courante", 
                             "electricite", 
                             "internet", 
                             "acces_infrastructures")

















# Transfert_data
transfert_data <- transfert_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_")
  )



# Sélection des colonnes utiles avec les noms d'origine
transfert_data <- transfert_data[, c("id_menage", 
                                     "s13aq01",   # Transfert d'argent de la famille
                                     "s13aq02",   # Transfert d'argent d'une autre personne
                                     "s13aq03",   # Transfert via Mobile Money
                                     "s13aq04")]  # Indicateur de réception de transfert

# Création des nouvelles variables
transfert_data$transfert_total <- ifelse(transfert_data$s13aq04 == 1, 1, 0)

transfert_data$type_transfert <- dplyr::case_when(
  transfert_data$s13aq01 == 1 ~ "Famille",
  transfert_data$s13aq02 == 1 ~ "Non-membre",
  transfert_data$s13aq03 == 1 ~ "Mobile Money",
  TRUE ~ "Aucun"
)



# Renommage pour plus de clarté
colnames(transfert_data) <- c("id_menage", 
                              "transfert_famille", 
                              "transfert_non_membre", 
                              "transfert_mobile_money", 
                              "a_recu_transfert", 
                              "transfert_total", 
                              "type_transfert")











# Epargne_credit_data
epargne_credit_data <- epargne_credit_data %>%
  mutate(
    id_menage = paste(vague, grappe, menage, sep = "_"),
    id_individu = if ("s01q00a" %in% names(epargne_credit_data)) {
      paste(vague, grappe, menage, s01q00a, sep = "_")
    } else {
      NA_character_
    }
  )



# Création des indicateurs d'accès à l'épargne
epargne_credit_data$a_epargne <- ifelse(
  (epargne_credit_data$s06q01__1 == 1 |  # Banque classique
     epargne_credit_data$s06q01__2 == 1 |  # Poste
     epargne_credit_data$s06q01__3 == 1 |  # Caisse rurale / IMF
     epargne_credit_data$s06q01__4 == 1 |  # Mobile Banking
     epargne_credit_data$s06q01__5 == 1) & # Carte prépayée
    epargne_credit_data$s06q02 == 1, 1, 0
)

# Création des indicateurs d'accès au crédit
epargne_credit_data$a_credit <- ifelse(
  (epargne_credit_data$s06q03 == 1 & epargne_credit_data$s06q05 == 1) | # Crédit demandé et obtenu dans les 12 derniers mois
    epargne_credit_data$s06q09 == 1 |                                    # Crédit non remboursé dans le passé
    epargne_credit_data$s06q10 > 0,                                      # Nombre de crédits non remboursés > 0
  1, 0
)

# Gestion des NA : Remplacer les NA par 0 (indique l'absence d'accès)
epargne_credit_data$a_epargne[is.na(epargne_credit_data$a_epargne)] <- 0
epargne_credit_data$a_credit[is.na(epargne_credit_data$a_credit)] <- 0

# Vérification des résultats
table(epargne_credit_data$a_epargne)
table(epargne_credit_data$a_credit)





























## Fusion


library(dplyr)

#  Étendre les données "Logement" et "Transferts" à tous les membres du ménage
logement_data_extended <- logement_data %>%
  left_join(socio_data[, c("id_menage", "id_individu")], by = "id_menage")

transfert_data_extended <- transfert_data %>%
  left_join(socio_data[, c("id_menage", "id_individu")], by = "id_menage")

Identification_data_extended <- Identification_data %>%
  left_join(socio_data[, c("id_menage", "id_individu")], by = "id_menage")


#  Merge des données individuelles (niveau individu)
data_individu <- socio_data %>%
  left_join(education_data, by = c("id_menage", "id_individu")) %>%
  left_join(emploi_data, by = c("id_menage", "id_individu")) %>%
  left_join(epargne_credit_data, by = c("id_menage", "id_individu")) %>%
  left_join(revenu_data, by = c("id_menage", "id_individu"))

#  Merge avec les données étendues au niveau ménage
data_individu <- data_individu %>%
  left_join(logement_data_extended, by = c("id_menage", "id_individu")) %>%
  left_join(transfert_data_extended, by = c("id_menage", "id_individu")) %>%
  left_join(Identification_data_extended, by = c("id_menage", "id_individu"))




variables_selectionnees <- c(
  "id_menage", "id_individu", "sexe", "age", "Region", "Milieu_de_residence", "situation_matrimoniale", "telephone",
  "socio_pro_pere", "alphabetisation", "niveau_etudes_max", "categorie_socio_pro",
  "salaire_total", "a_credit", "a_epargne","religion",
  "titre_propriete","pension", "a_revenu_hors_emploi",
  "a_recu_transfert"
)

# Sélection dans le dataframe
data_individu <- data_individu[, variables_selectionnees]

data_individu$age[data_individu$age < 0] <- NA

table(data_individu$a_credit)
table(data_individu$a_epargne)
nrow(data_individu)

prop.table(table(data_individu$a_credit))
prop.table(table(data_individu$a_epargne))

## Gestion des valeur manquantes
# Suppression des lignes où id_menage ET id_individu sont tous les deux NA
data_individu <- data_individu[!(is.na(data_individu$id_menage) & is.na(data_individu$id_individu)), ]
data_individu <- data_individu[data_individu$age >= 15, ]
nrow(data_individu)



# Nettoyage des valeurs manquantes et uniformisation
# Vérification des modalités pour chaque variable catégorielle
unique(data_individu$Region)
unique(data_individu$Milieu_de_residence)
unique(data_individu$situation_matrimoniale)
unique(data_individu$telephone)
unique(data_individu$religion)
unique(data_individu$niveau_etudes_max)
unique(data_individu$categorie_socio_pro)
unique(data_individu$socio_pro_pere)
unique(data_individu$titre_propriete)
unique(data_individu$a_recu_transfert)
unique(data_individu$a_revenu_hors_emploi)






data_individu$tranche_age <- cut(data_individu$age, 
                                 breaks = c(15, 24, 34, 44, 54, 64, Inf), 
                                 labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"), 
                                 right = TRUE)

data_individu$Region <- factor(data_individu$Region, levels = 1:14, 
                               labels = c("Dakar", "Ziguinchor", "Diourbel", "Saint-Louis", "Tambacounda", "Kaolack", 
                                          "Thiès", "Louga", "Fatick", "Kolda", "Matam", "Kaffrine", "Kédougou", "Sédhiou"))


data_individu$Milieu <- factor(data_individu$Milieu_de_residence, levels = c(1, 2), labels = c("Urbain", "Rural"))


data_individu$statut_matrimonial <- case_when(
  data_individu$situation_matrimoniale %in% 1 ~ "Célibataire",
  data_individu$situation_matrimoniale %in% c(2, 3) ~ "Marié(e)",
  data_individu$situation_matrimoniale %in% c(4, 5, 6, 7) ~ "Autre",
  TRUE ~ NA_character_
)


data_individu$telephone <- factor(data_individu$telephone, levels = c(1, 2), labels = c("Oui", "Non"))


data_individu$religion_cat <- case_when(
  data_individu$religion == 1 ~ "Musulman",
  data_individu$religion == 2 ~ "Chrétien",
  data_individu$religion %in% c(3, 4, 5) ~ "Autre/Sans",
  TRUE ~ NA_character_
)



data_individu$niveau_etude_cat <- case_when(
  data_individu$niveau_etudes_max %in% c(NA, 1) ~ "Aucun",
  data_individu$niveau_etudes_max == 2 ~ "Primaire",
  data_individu$niveau_etudes_max %in% 3:6 ~ "Secondaire",
  data_individu$niveau_etudes_max %in% 7:8 ~ "Supérieur",
  TRUE ~ NA_character_
)



data_individu$csp_cat <- case_when(
  data_individu$categorie_socio_pro %in% c(1, 2, 3) ~ "Cadres/Qualifiés",
  data_individu$categorie_socio_pro %in% c(4, 5, 6) ~ "Non qualifiés",
  data_individu$categorie_socio_pro %in% c(7, 8) ~ "Aide familiale/Apprenti",
  data_individu$categorie_socio_pro %in% c(9, 10) ~ "Indépendants",
  TRUE ~ NA_character_
)



data_individu$titre_propriete <- factor(data_individu$titre_propriete, levels = c(1, 2), labels = c("Oui", "Non"))
data_individu$a_recu_transfert <- factor(data_individu$a_recu_transfert, levels = c(0, 1), labels = c("Non", "Oui"))
data_individu$a_revenu_hors_emploi <- factor(data_individu$a_revenu_hors_emploi, levels = c(0, 1), labels = c("Non", "Oui"))




# Garder 1 = Oui, 0 = Non pour ces variables binaires
#data_individu$telephone <- ifelse(data_individu$telephone == 1, 1,
#                                 ifelse(data_individu$telephone == 2, 0, NA))

#data_individu$titre_propriete <- ifelse(data_individu$titre_propriete == 1, 1,
 #                                       ifelse(data_individu$titre_propriete == 2, 0, NA))

#data_individu$a_recu_transfert <- ifelse(data_individu$a_recu_transfert == 1, 1,
#                                         ifelse(data_individu$a_recu_transfert == 0, 0, NA))

#data_individu$a_revenu_hors_emploi <- ifelse(data_individu$a_revenu_hors_emploi == 1, 1,
#                                             ifelse(data_individu$a_revenu_hors_emploi == 0, 0, NA))




## selection finale 


# Suppression des observations avec au moins une NA sur les variables retenues
vars_model <- c("id_menage", "id_individu", "a_credit", "a_epargne", "tranche_age", "sexe", "niveau_etude_cat", 
                "statut_matrimonial", "telephone", "religion_cat", "Milieu", "Region", 
                "titre_propriete", "a_recu_transfert", "a_revenu_hors_emploi", "csp_cat")

data_model <- data_individu %>%
  select(all_of(vars_model)) 

# Aperçu du % de valeurs manquantes
sapply(data_model[, vars_model], function(x) mean(is.na(x)) * 100)






vars_model <- c("a_credit", "a_epargne", "tranche_age", "sexe", "niveau_etude_cat", 
                "statut_matrimonial", "telephone", "religion_cat", "Milieu", "Region", 
                "a_recu_transfert", "a_revenu_hors_emploi")



sapply(data_model[, vars_model], function(x) mean(is.na(x)) * 100)



# Nettoyage avec sélection stricte
data_model <- data_individu %>%
  select(all_of(vars_model)) %>%
  drop_na()


sapply(data_model[, vars_model], function(x) mean(is.na(x)) * 100)



## Répartion des variables catégorielle

## Aperçu de la répartition des principales variables

library(ggplot2)
theme_set(theme_minimal())

# Fonction de graphe pour variables catégorielles
## 6.1 Visualisation des distributions (en pourcentage)

plot_bar <- function(var, data, title = NULL) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    count(var = .data[[var]]) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ggplot(aes(x = var, y = pct)) +
    geom_bar(stat = "identity", fill = "#2c7fb8") +
    geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 3.5) +
    labs(title = title, x = NULL, y = "Pourcentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Tracer quelques variables clés
plot_bar("tranche_age", data_model, "Répartition par tranche d'âge")
plot_bar("sexe", data_model, "Répartition par sexe")
plot_bar("niveau_etude_cat", data_model, "Niveau d'études atteint")
plot_bar("statut_matrimonial", data_model, "Statut matrimonial")
plot_bar("Milieu", data_model, "Milieu de résidence")
plot_bar("Region", data_model, "Région de résidence")
plot_bar("religion_cat", data_model, "Religion")
plot_bar("telephone", data_model, "Possession d’un téléphone")
plot_bar("a_epargne", data_model, "Accès à l’épargne")
plot_bar("a_credit", data_model, "Accès au crédit")










## Ajustements avant modélisation

data_model <- data_model %>%
  mutate(across(c(tranche_age, niveau_etude_cat, statut_matrimonial,
                  religion_cat, Milieu, Region), as.factor))

data_model$sexe <- factor(data_model$sexe, levels = c(1, 2), labels = c("Homme", "Femme"))



## Modélisaton
# accés_credit

modele_credit <- glm(
  a_credit ~ tranche_age + sexe + niveau_etude_cat + statut_matrimonial +
    telephone + religion_cat + Milieu + Region +
    a_recu_transfert + a_revenu_hors_emploi,
  data = data_model,
  family = binomial(link = "logit")
)

summary(modele_credit)

exp(cbind(OR = coef(modele_credit), confint(modele_credit)))






# accés_epargne
modele_epargne <- glm(
  a_epargne ~ tranche_age + sexe + niveau_etude_cat + statut_matrimonial +
    telephone + religion_cat + Milieu + Region +
    a_recu_transfert + a_revenu_hors_emploi,
  data = data_model,
  family = binomial(link = "logit")
)

summary(modele_epargne)

exp(cbind(OR = coef(modele_epargne), confint(modele_epargne)))















# Créer un tableau de contingence
tab <- table(data_model$a_credit, data_model$a_epargne)

# Afficher le tableau
print(tab)

# Test du Khi-deux
chi2_test <- chisq.test(tab)

# Résultats
print(chi2_test)


correlation_phi <- cor(data_model$a_credit, data_model$a_epargne, method = "pearson")
print(correlation_phi)


write.csv(data_model, "data_model.csv", row.names = FALSE)


