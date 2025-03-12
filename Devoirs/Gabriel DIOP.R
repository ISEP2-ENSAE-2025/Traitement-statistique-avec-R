#creer un data frame
data <- data.frame(
  ID = 1:10,  # Variable quantitative discrète (Identifiant)
  Age = c(25, 30, 22, 40, 35, 28, 26, 33, 27, 31), # Quantitative continue
  Sexe = factor(c("F", "M", "M", "M", "F", "M", "F", "M", "F", "M")), # Nominal transformé en facteur
  Salaire = c(2500, 3200, 2100, 4500, 3700, 2900, 2600, 3400, 2800, 3100), # Quantitative continue
  Satisfaction = factor(c("Mauvaise", "Bonne", "Excellente", "Bonne", "Mauvaise", "Excellente", "Bonne", "Mauvaise", "Bonne", "Excellente"), 
                        levels = c("Mauvaise", "Bonne", "Excellente"), ordered = TRUE), # Ordinale
  Département = factor(c("RH", "IT", "Finance", "IT", "RH", "Finance", "RH", "IT", "Finance", "RH")), # Nominal
  Expérience = c(3, 7, 1, 10, 6, 4, 2, 8, 5, 7), # Quantitative discrète
  Temps_de_travail = c(35, 40, 30, 50, 45, 38, 36, 42, 37, 39), # Quantitative continue
  Télétravail = factor(c("Oui", "Non", "Non", "Oui", "Oui", "Non", "Non", "Oui", "Oui", "Non")) # Nominal transformé en facteur
)

head(data)
#Afficher la colonne experience
print(data$Expérience)
# rendre la colonne experience en facteur
data$Expérience <- cut(data$Expérience, breaks = c(0, 3, 6, 10), labels = c("Débutant", "Intermédiaire", "Expérimenté"), ordered_result = TRUE)
print(data$Expérience)
head(data)
# Afficher la structure du data frame
str(data)
#Calculer la moyenne des salaires

library(magrittr)
data$Salaire %>% mean() %>% round(2)

Salaire<-c(data$Salaire)

sd(data$Salaire)
summary(data$Expérience)

#supplement

library(ggplot2)

ggplot(data, aes(x = Salaire)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black") +
  labs(title = "Distribution des salaires", x = "salaire (%)", y = "Effectif") +
  theme_minimal()


corr_matrix <- cor(data[sapply(data, is.numeric)], use = "complete.obs")
print(corr_matrix)
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8) # faire la correlation entre les variables numeriques sous forme de graph (corrélogramme)


library(GGally)

ggpairs(data, columns = c("Salaire", "Age", "Temps_de_travail", "Expérience"))

