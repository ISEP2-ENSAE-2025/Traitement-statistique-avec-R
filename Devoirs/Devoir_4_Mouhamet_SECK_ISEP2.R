# Préambule : Chargement des packages

library(haven)      # Pour lire les fichiers .dta
library(dplyr)      # Pour la manipulation de données
library(ggplot2)    # Pour les graphiques
library(tidyr)      # Pour le reshape des données
sendata <- "C:/Users/HP/Desktop/ISEP2/Semestre2/Logiciel R/Cours/EHCVM_Données_SENEGAL"



### 1. SUppression des doublons 
s01 <- read_dta(paste0(sendata, "/s01_me_SEN2018.dta"))
View(s01)

dim(s01)
s01 <- s01 %>%
  group_by(grappe, menage, s01q00a) %>%
  distinct_all()
dim(s01)



### 2. Informations sur les membres du menage 
# Trier par âge croissant
s01 <- s01 %>% arrange(grappe, menage, s01q00a)

s01 <- s01 %>%
  mutate(ifmember = ifelse(s01q12==1 | s01q13==1,1,0))
sum(s01$ifmember, na.rm = T) # 66109 < 66119


# 3.Taille et composition des ménages (s01_me_MLI2018.dta)

taille_menages <- s01 %>%
  group_by(grappe, menage) %>%
  summarise(nb_membres = sum(ifmember, na.rm = TRUE))  # uniquement les vrais membres
View(taille_menages)
mean(taille_menages$nb_membres)

# 4.Structure par sexe et classe d'âge

s01 <- s01 %>%
  mutate(age_group = cut(s01q04a, breaks = c(0, 14, 24, 64, Inf),
                         labels = c("0-14", "15-24", "25-64", "65+")))

table_sexe_age <- s01 %>%
  group_by(s01q01, age_group) %>%
  summarise(n = n()) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))
View(table_sexe_age)

library(ggplot2)

ggplot(table_sexe_age, aes(x = age_group, y = pourcentage, fill = factor(s01q01))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Répartition par sexe et âge",
    x = "Tranche d'âge",
    y = "Pourcentage",
    fill = "Sexe"
  ) +
  scale_fill_manual(values = c("steelblue", "tomato"), labels = c("Homme", "Femme")) +
  theme_minimal()


# 5. Taux d’alphabétisation (s02_me_MLI2018.dta)

s02 <- read_dta(paste0(sendata, "/s02_me_SEN2018.dta"))
View(s02)

s02 <- s02 %>%
  mutate(
    sait_lire = s02q01__1 == 1 | s02q01__2 == 1 | s02q01__3 == 1,
    sait_ecrire = s02q02__1 == 1 | s02q02__2 == 1 | s02q02__3 == 1,
    alpha = ifelse(sait_lire & sait_ecrire, 1, 0)
  )

# Taux d'alphabétisation global
taux_alpha <- mean(s02$alpha, na.rm = TRUE) * 100

print(taux_alpha)


