#importer les packages
library(FactoMineR)
library(factoextra)

#importer le dataset
library(readr)
dataset <- read.csv("C:/Users/Ilhem/Desktop/TP SID/TP ANAD/TP AFCM/dataset/diabetes_data_upload.csv", head=TRUE)

#__________________________________________________________________________

#traiter les valeurs manquantes
sum(is.na(dataset))
dataset$age[is.na(dataset$age)] <- median(dataset$age, na.rm = TRUE)

#__________________________________________________________________________
#Etudier les statistiques des données
head(dataset)
str(dataset)
summary(dataset)
library(dplyr)

# Histogramme pour la variable numérique âge
dev.new()
dataset$AgeGroup <- cut(dataset$Age, breaks=c(15, 34, 54, 74), labels=c("15-34", "35-54", "55-74"), right=TRUE)
barplot(table(dataset$AgeGroup), main="Répartition des âges", xlab="Tranches d'âge", col="lightblue", 
        names.arg=c("15-34", "35-54", "55-74"), las=1)


# Barplots pour les variables catégorielles
dev.new()
par(mfrow=c(2,3), mar=c(3, 3, 2, 1)) #pour afficher tout dans une seule fenêtre
barplot(table(dataset$Gender), main="Répartition des sexes", col=c("lightpink", "lightblue"))
barplot(table(dataset$Polyuria), main="Fréquence de Polyuria", col=c("lightgreen", "orange"))
barplot(table(dataset$Polydipsia), main="Fréquence de Polydipsia", col=c("yellow", "blue"))
barplot(table(dataset$Polyphagia), main="Fréquence de Polyphagia", col=c("lightblue", "orange"))
barplot(table(dataset$Alopecia), main="Fréquence de Alopecia", col=c("peachpuff", "lightgrey"))
#__________________________________________________________________________
# Cleaning and standardizing data for consistency


dataset <- dataset %>% select(Age, Gender, Polyuria, Polydipsia, Polyphagia, Alopecia)

dataset$Gender <- trimws(tolower(dataset$Gender))  # Standardize Gender values
dataset$Polyuria <- trimws(tolower(dataset$Polyuria))  # Standardize Polyuria values
dataset$Polydipsia <- trimws(tolower(dataset$Polydipsia))  # Standardize Polydipsia values
dataset$Polyphagia <- trimws(tolower(dataset$Polyphagia))  # Standardize Polyphagia values
dataset$Alopecia <- trimws(tolower(dataset$Alopecia))  # Standardize Alopecia values

# Classes d'age
dataset$Age <- as.numeric(as.character(dataset$Age))
if (any(is.na(dataset$Age))) {
  print("Warning: NA values found in Age column. Please check data.")
}
dataset$AgeClass <- cut(dataset$Age, breaks = c(15, 35, 55, 75), labels = c("15-34", "35-54", "55-74"), right = FALSE)

# organiser les colonnes
dataset <- dataset %>% select(AgeClass, Gender, Polyuria, Polydipsia, Polyphagia, Alopecia)

# fonction qi calcule les effectifs et les frequences
calculate_stats <- function(column) {
  effective_counts <- table(column)
  frequency_table <- prop.table(effective_counts)
  return(list(effective = effective_counts, frequency = frequency_table))
}

#Calculer les stats pour chaque variable
ageclass_stats <- calculate_stats(dataset$AgeClass)
gender_stats <- calculate_stats(dataset$Gender)
polyuria_stats <- calculate_stats(dataset$Polyuria)
polydipsia_stats <- calculate_stats(dataset$Polydipsia)
polyphagia_stats <- calculate_stats(dataset$Polyphagia)
alopecia_stats <- calculate_stats(dataset$Alopecia)

# Display results 
print("Statistics for Age Classes:")
print(ageclass_stats)

print("Statistics for Gender:")
print(gender_stats)

print("Statistics for Polyuria (Yes/No):")
print(polyuria_stats)

print("Statistics for Polydipsia (Yes/No):")
print(polydipsia_stats)

print("Statistics for Polyphagia (Yes/No):")
print(polyphagia_stats)

print("Statistics for Alopecia (Yes/No):")
print(alopecia_stats)



#__________________________________________________________________________

#Transformer le tableau de données en tableau disjonctif complet
dataset_dummies <- model.matrix(~.-1, data = dataset)

#__________________________________________________________________________

#AFCM
afcm <- MCA(dataset, graph = FALSE)

# Tableau des valeurs propres
print("Eigenvalues (Vectors Propres) of the MCA:")
print(afcm$eig)

# Visualizsation
fviz_mca_biplot(afcm, repel = TRUE, ggtheme = theme_minimal())


#__________________________________________________________________________

#Étudier le tableau des contributions 

contributions <- afcm$var$contrib

# Convertir les contributions 
contributions_df <- as.data.frame(contributions)
rownames(contributions_df) <- afcm$var$coord[, 1]  # Optional: add row names if available

print("Contributions des variables a chaque axe:")
print(contributions_df)

# Visualiser les contributions 
fviz_contrib(afcm, choice = "var", axes = 1, top = 10)
fviz_contrib(afcm, choice = "var", axes = 2, top = 10)
#__________________________________________________________________________



#  Projection des individus
fviz_mca_ind(afcm, 
             label = "none", 
             habillage = "none", 
             repel = TRUE, 
             ggtheme = theme_minimal())

# 2. Projection des variables
fviz_mca_var(afcm, 
             repel = TRUE, 
             ggtheme = theme_minimal())



#______________________________________________________________________

# Visualiser le plot des modalités
fviz_mca_var(afcm, 
             repel = TRUE,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal(),
             axes = c(1, 2))

#______________________________________________________________________
#AFC
# Créer un tableau de contingence entre AgeGroup et Polyuria


table_contingence <- table(dataset$AgeClass, dataset$Polyuria)
colnames(table_contingence) <- c("PolyuriaNo", "PolyuriaYes")
table_contingence

#Test de chi2
test_chi2 <- chisq.test(table_contingence)
print(test_chi2)

afc <- CA(table_contingence, graph = FALSE)

# Résumé des valeurs propres, inertie et inertie cumulée
valeurs_propres <- afc$eig
cat("Valeurs propres, Inertie, Inertie cumulée :\n")
print(valeurs_propres)

# Projections et contributions des colonnes sur l'axe 1
cat("\nProjections et Contributions des Colonnes sur Dim.1 :\n")
projection_cols <- afc$col$coord[, 1] 
contrib_cols <- afc$col$contrib[, 1] 
cos2_cols <- afc$col$cos2[, 1]       
cat("Projections des colonnes :\n")
print(projection_cols)
cat("Contributions des colonnes :\n")
print(contrib_cols)
cat("Cos² des colonnes :\n")
print(cos2_cols)

# Projections et contributions des lignes sur l'axe 1
projection_lines <- afc$row$coord  
cat("Projections des lignes sur Dim.1 :\n")
print(projection_lines)
contrib_lines <- afc$row$contrib  
cat("Contributions des lignes sur Dim.1 :\n")
print(contrib_lines)
cos2_lines <- afc$row$cos2 
cat("Cos² des lignes sur Dim.1 :\n")
print(cos2_lines)

#affichage des rprésentations graphiques
library(ggplot2)
library(gridExtra)

projection_cols <- afc$col$coord[, 1]  
contrib_cols <- afc$col$contrib[, 1]  
cos2_cols <- afc$col$cos2[, 1]        

projection_lines <- afc$row$coord  
contrib_lines <- afc$row$contrib 
cos2_lines <- afc$row$cos2       

df_cols <- data.frame(
  Category = names(projection_cols),
  Projection = projection_cols,
  Contribution = contrib_cols,
  Cos2 = cos2_cols
)

df_lines <- data.frame(
  Age_Group = names(projection_lines),
  Projection = projection_lines,
  Contribution = contrib_lines,
  Cos2 = cos2_lines
)

# Créer les graphes pour chaque type de données
dev.new()
# Projections des colonnes
plot_cols_projection <- ggplot(df_cols, aes(x = Category, y = Projection)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Projections des colonnes (AFC)", x = "Catégories", y = "Projection")

# Contributions des colonnes
plot_cols_contrib <- ggplot(df_cols, aes(x = Category, y = Contribution)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(title = "Contributions des colonnes (AFC)", x = "Catégories", y = "Contribution (%)")

# Projections des lignes
plot_lines_projection <- ggplot(df_lines, aes(x = Age_Group, y = Projection)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  theme_minimal() +
  labs(title = "Projections des tranches d'âges (AFC)", x = "Tranches d'âges", y = "Projection")

# Contributions des lignes
plot_lines_contrib <- ggplot(df_lines, aes(x = Age_Group, y = Contribution)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_minimal() +
  labs(title = "Contributions des tranches d'âges (AFC)", x = "Tranches d'âges", y = "Contribution (%)")

# Afficher tous les graphiques dans la même fenêtre
grid.arrange(plot_cols_projection, plot_cols_contrib, plot_lines_projection, plot_lines_contrib, ncol = 2)


