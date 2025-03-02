library(FactoMineR)
library(factoextra)

# Lecture des fichiers excel
library(readxl)
pv_data <- read_excel("C:/Users/admin/Documents/RAYANE DOSSIER ETUDES/ANAD/TP2 ANAD/PV_2CP_2022.xlsx")

# Dans la liste affectation se trouve 4 différents sheet
sheet1 <- read_excel("C:/Users/admin/Documents/RAYANE DOSSIER ETUDES/ANAD/TP2 ANAD/LISTE_AFFECTATION.xlsx",sheet=1)
sheet2 <- read_excel("C:/Users/admin/Documents/RAYANE DOSSIER ETUDES/ANAD/TP2 ANAD/LISTE_AFFECTATION.xlsx",sheet=2)
sheet3 <- read_excel("C:/Users/admin/Documents/RAYANE DOSSIER ETUDES/ANAD/TP2 ANAD/LISTE_AFFECTATION.xlsx",sheet=3)
sheet4 <- read_excel("C:/Users/admin/Documents/RAYANE DOSSIER ETUDES/ANAD/TP2 ANAD/LISTE_AFFECTATION.xlsx",sheet=4)

# Création d'une seule liste d'affectation en réalisant une fusion
library(dplyr)
affectation_data <- bind_rows(sheet1, sheet2, sheet3, sheet4)

# Traitement de pv_data et retrait des colonnes non concernés par l'étude
pv_data <- pv_data %>%
  select(-c("Groupe_S1", "SFSD", "ARCH2", "UEF5", "ANAL3", "ALG3", "UEF6", "ELEF2", 
            "PRST1", "UEM2", "ANG2", "UET3", "ECON", "UED2", "Ne_S1", "Rang_S1", 
            "Moy_S1", "Moy_rachatS1", "Crd_S1", "Groupe_S2", "Ne_S2", "Rang_S2",
            "Moy_S2",	"Moy_rachatS2",	"Crd_S2",	"Rang_annuel", "Moy_annuelle",
            "Moy_rachat",	"Crd_annuel",	"Decision_jury",
            "UEF7", "UEM3", "UEM4", "UET4", "UEF8"))

# Fusion des notes avec les affectations selon la colonne Matricule
data <- left_join(pv_data, affectation_data, by = "Matricule")

#--> Traitements des étudiants sans affectations (redoublants, concours SBA bejaia ect ...) + Détection des valeurs aberrantes
nb_vides <- colSums(is.na(data))
data <- data[!is.na(data$Affectation), ]
numeric_data <- data %>% select(-c(Matricule, Affectation)) %>% select(where(is.numeric))
outliers <- apply(numeric_data, 2, function(x) {
  quantiles <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  IQR <- diff(quantiles)
  lower_bound <- quantiles[1] - 1.5 * IQR
  upper_bound <- quantiles[2] + 1.5 * IQR
  x < lower_bound | x > upper_bound
})
data <- data[!apply(outliers, 1, any), ]

#--> Cas ou on ne nettoie pas les données, on ne fait que la ligne ci dessous
#data$Affectation[is.na(data$Affectation)] <- "Inconnu"

# La colonne Matricule ne nous sert plus à rien car nous allons étiquetter chaque étudiant par sa spécialité
data_old <- data
data <- data %>%
  select(-Matricule)

# Nous pouvons à présent faire notre ACP 
ACP <- PCA(data %>% select(-Affectation), scale.unit = TRUE, graph = FALSE)

# Les valeurs propres + Inerties 
val_propres <- ACP$eig
print(val_propres)
fviz_screeplot(ACP, addlabels = TRUE, ylim = c(0, 50))

# Projection des individus selon un code couleur
fviz_pca_ind(ACP, 
             geom.ind = "point", 
             col.ind = data$Affectation,
             palette = c("purple", "pink", "brown", "cyan"), #,"grey"),
             #addEllipses = TRUE,
             legend.title = "Affectation"
)

# Contributions des individus pour les 5 premiers axes (Inerties cumulées > 80%)
fviz_contrib(ACP, choice = "ind", axes = 1, top = 10)
fviz_contrib(ACP, choice = "ind", axes = 2, top = 10)
fviz_contrib(ACP, choice = "ind", axes = 3, top = 10)
fviz_contrib(ACP, choice = "ind", axes = 4, top = 10)
fviz_contrib(ACP, choice = "ind", axes = 5, top = 10)

# Projections des variables 
fviz_pca_var(ACP, 
             col.var = "black", 
             axes = c(1, 2), 
             addlabels = TRUE, 
             legend.title = "Variables"
)

# Contributions des variables pour les 4 premiers axes (Inerties cumulées > 80%)
fviz_contrib(ACP, choice = "var", axes = 1, top = 10)
fviz_contrib(ACP, choice = "var", axes = 2, top = 10)
fviz_contrib(ACP, choice = "var", axes = 3, top = 10)
fviz_contrib(ACP, choice = "var", axes = 4, top = 10)
fviz_contrib(ACP, choice = "var", axes = 5, top = 10)

# Positionnement sur le nuage 
mon_matricule <- "21/0173"
index <- which(data_old$Matricule == mon_matricule)
mon_coord <- ACP$ind$coord[index, ]

mon_matricule1 <- "21/0030"
index1 <- which(data_old$Matricule == mon_matricule1)
mon_coord1 <- ACP$ind$coord[index1, ]

fviz_pca_ind(ACP, 
             geom.ind = "point", 
             col.ind = data$Affectation,
             palette = c("purple", "pink", "brown", "cyan"), 
             label = data_old$Matricule,
             legend.title = "Affectation"
) +
  annotate("point", x = mon_coord[1], y = mon_coord[2], color = "red", size = 4) +
  annotate("text", x = mon_coord[1], y = mon_coord[2], label = "RAYANE", vjust = -1, color = "red") +
  annotate("point", x = mon_coord1[1], y = mon_coord1[2], color = "blue", size = 4) + 
  annotate("text", x = mon_coord1[1], y = mon_coord1[2], label = "ILHEM", vjust = -1, color = "blue")
