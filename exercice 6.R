data <- read.table("D:/Projects/ESGI/MathR/s3/sympathique.txt", header = TRUE, sep = " ", row.names = 1)
# Partie 1
# Question 1

#pourcentage de chaque caractéristique d'une personne sympathique
pourcentage_carac <- data["TOTAL", , drop = FALSE]/data["TOTAL", "TOTAL"]
print(round(pourcentage_carac,2))
#pourcentage des catégories professionnelles
pourcentage_pro <- data[ , "TOTAL", drop = FALSE]/data["TOTAL", "TOTAL"]
print(round(pourcentage_pro, 2))

# Question 2
# On a 1200 votes au total avec 3 votes par personne donc 1200/3 = 400 personnes sondées

#Donner la proportion des employés pour qui être honnête rend sympathique
pourcentage_empl_honn <- data["EMPL", "HONN"]/(data["EMPL", "TOTAL"]/3)
print(round(pourcentage_empl_honn,2))
#Quelle est la proportion d'employés parmi les gens qui pensent qu'être honnête rend sympathique ?
pourcentage_empl_sachant_honn <- data["EMPL", "HONN"]/data["TOTAL", "HONN"]
print(round(pourcentage_empl_sachant_honn,2))




#Partie 2
# Installer et charger les bibliothèques nécessaires
if (!require(FactoMineR, quietly = TRUE)) {
  install.packages("FactoMineR")
  library(FactoMineR)
} else {
  library(FactoMineR)
}

# Transposer les données (car AFC nécessite des variables en lignes et des individus en colonnes)
data_transposed <- t(data[-nrow(data), -ncol(data)])  # Enlever la colonne 'TOTAL'

# Effectuer l'AFC
afc_result <- FactoMineR::CA(data_transposed, graph = TRUE)

# Afficher les résultats de l'AFC
print(afc_result$eig)
