library(data.table)
library(readxl)
library(purrr)
library(stringr)
library(gt)
library(ggplot2)


# Importation de la table
lire <- read_excel("donnees/romans@lire_indexation_2023-01-24.xlsx") |> setDT()

# Création d'une colonne catégorielle
lire$roman_nouvelle <- ifelse(lire$`Genre littéraire` %ilike% "roman", "roman", "nouvelle") |> factor()
str(lire)

# Distribution des documents selon la forme (roman/nouvelle)
table(lire$roman_nouvelle)

# Nombre de titres uniques
lire[!duplicated(lire$Titre), .N]

# Élimination d'une colonne créée par erreur dans l'importation des données
lire[, ...12:=NULL]

# Création d'une table ne contenant que les noms des champs de la table brute
lireCol <- data.table(Champs = colnames(lire))

# Affichage élégant de cette table pour exportation
lire_champs <- gt(lireCol) |> cols_label(Champs = "Nom des champs")
lire_champs |> gtsave(filename = "resultats/202304_PB_table_champs.png")


# Remplacement d'erreurs (4) dans le champ Année de publication. Remplacement des modalités problématiques par NA
lire[, `Année de publication`:=ifelse(lire$`Année de publication` %ilike% "[0-9]+", as.integer(lire$`Année de publication`), NA)]

# Observation de la distribution
table(lire$`Année de publication`)

# Fonction pour agréger les documents par décennie
slicing_f <- function(x){
  decennies <- factor(paste0(str_sub(x, 1,3),0))
  return(decennies)
}

# Création d'une table avec la fonction
distrib_decennies <- lire[!is.na(`Année de publication`), .(`Numéro de séquence`, `Année de publication`)][
  , decennies:=slicing_f(`Année de publication`)][
    ,.N, by="decennies"][
      order(decennies, decreasing = TRUE)]


# Création d'un diagramme avec cette table
distrib_docs_decennies <- ggplot(distrib_decennies,
                                 aes(x=decennies, y=N))+
  geom_col()+
  geom_text(aes(label = N),
            hjust = 0.5,
            vjust = -0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Distribution chronologique des romans exotopiques, par décennies\nNombre brut de documents",
       caption = "Données: BANQ, 2023")+
  ylab(NULL)+
  xlab("Décennies")

ggsave("resultats/20230418_PB_DistribChronologiqueDecennies.png", dpi=300)

# Création d'une table élégante pour exportation
table_distrib_chrono_gt <- gt(distrib_decennies) |> cols_label(
  decennies = "Décennies",
  N = "Nombre de documents")|>
  tab_source_note(
    source_note = md("Données: BAnQ, 2023")
  )
table_distrib_chrono_gt |> gtsave(filename = "resultats/20230418_PB_table_distrib_chrono_gt.png")

# Exportation de la table sous forme de .csv
fwrite(distrib_decennies, "resultats/20230418_PB_DistribChronologiqueDecennies.csv")








