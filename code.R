library(data.table)
library(readxl)
library(purrr)
library(stringr)
library(gt)
library(ggplot2)
library(tibble)

#### Préliminaires ----

# Importation de la table
lire <- read_excel("donnees/romans@lire_indexation_2023-01-24.xlsx") |> setDT()

# Élimination d'une colonne créée par erreur dans l'importation des données
lire[, ...12:=NULL]


##### Distribution des documents selon la forme (roman/nouvelle) ----
# Création d'une colonne catégorielle
lire$roman_nouvelle <- ifelse(lire$`Genre littéraire` %ilike% "roman", "roman", "nouvelle") |> factor()
str(lire)

# Distribution
table(lire$roman_nouvelle)

#### Titres uniques ----
lire[!duplicated(lire$Titre), .N]


#### Champs de la table ----
# Création d'une table ne contenant que les noms des champs de la table brute
lireCol <- data.table(Champs = colnames(lire))

# Affichage élégant de cette table pour exportation
lire_champs <- gt(lireCol) |> cols_label(Champs = "Nom des champs")
lire_champs |> gtsave(filename = "resultats/202304_PB_table_champs.png")



#### Année de publication ----

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
  xlab("Décennies")+
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5, hjust=0.5))

ggsave("resultats/diagrammes/20230418_PB_DistribChronologiqueDecennies.png", dpi=300)

# Création d'une table élégante pour exportation
table_distrib_chrono_gt <- gt(distrib_decennies) |> cols_label(
  decennies = "Décennies",
  N = "Nombre de documents")|>
  tab_source_note(
    source_note = md("Données: BAnQ, 2023")
  )
table_distrib_chrono_gt |> gtsave(filename = "resultats/tables/20230418_PB_table_distrib_chrono_gt.png")

# Exportation de la table sous forme de .csv
fwrite(distrib_decennies, "resultats/tables/20230418_PB_DistribChronologiqueDecennies.csv")


#### Distribution par pays ----
# Observation
names(table(lire$Pays))

# Corrections (la modalité indiquée d'abord est remplacée par la seconde):
lire[Pays == "FR", Pays:="fr"]

lire[Pays == "cc", Pays:="ch"]

lire[Pays == "enk", Pays:="uk"]

lire[Pays == "xxk", Pays:="uk"]


pays_distrib <- data.table(sigle = names(table(lire$Pays)),
                           N = as.vector(table(lire$Pays)))

# Créer une table d'équivalences pour les lieux de publication
equiv_pays <- tribble(
  ~sigle, ~lieuSigleNomsComplet, ~paysAssocie, ~type,
  "abc", "Alberta", "Canada", "région",
  "bbc", "Colombie Britannique", "Canada", "région",
  "be", "Belgique", "Belgique", "pays",
  "cau", "Californie", "États-Unis", "région",
  "cf", "République centrafricaine", "République centrafricaine", "pays",
  "ch", "Chine", "Chine", "pays",
  "cm", "Cameroun", "Cameroun", "pays",
  "cq", "Comores", "Comores", "pays",
  "dcu", "Washington, D.C.", "États-Unis", "région",
  "dk", "Danemark", "Danemark", "pays",
  "fg", "Guyane", "France", "région",
  "flu", "Floride", "États-Unis", "région",
  "fp", "Tahiti", "France", "région",
  "fr", "France", "France", "pays",
  "gp", "Guadeloupe", "France", "région",
  "gr", "Grèce", "Grèce", "pays",
  "gw", "Allemagne", "Allemagne", "pays",
  "gy", "Guyane", "France", "région",
  "ilu", "Illinois", "États-Unis", "région",
  "inu", "Indiana", "États-Unis", "région",
  "it", "Italie", "Italie", "pays",
  "iv", "Côte-d'Ivoire", "Côte-d'Ivoire", "pays",
  "le", "Liban", "Liban",  "pays",
  "lu", "Luxembourg", "Luxembourg", "pays",
  "mau", "Massachusetts", "États-Unis", "région",
  "mbc", "Manitoba", "Canada", "région",
  "mc", "Monaco", "Monaco", "pays",
  "mdu", "Maryland", "États-Unis", "région",
  "meu", "Maine", "États-Unis", "région",
  "mg", "Madagascar", "Madagascar", "pays",
  "mq", "Martinique", "France", "région",
  "mr", "Maroc", "Maroc", "pays",
  "nju", "New Jersey", "États-Unis", "région",
  "nkc", "Nouveau-Brunswick", "Canada", "région",
  "nl", "Nouvelle-Calédonie", "France", "région",
  "nsc", "Nouvelle-Écosse", "Canada", "région",
  "nyu", "New York", "États-Unis", "région",
  "onc", "Ontario", "Canada", "région",
  "oru", "Oregon", "États-Unis", "région",
  "pic", "Île-du-Prince-Édouard", "Canada", "région",
  "pl", "Pologne", "Pologne", "pays",
  "po", "Portugal", "Portugal", "pays",
  "quc", "Québec", "Canada", "région",
  "re", "Réunion", "France", "région",
  "rm", "Roumanie", "Roumanie", "pays",
  "ru", "Russie", "Russie", "pays",
  "sg", "Sénégal", "Sénégal", "pays",
  "snc", "Saskatchewan", "Canada", "région",
  "sp", "Espagne", "Espagne", "pays",
  "sz", "Suisse", "Suisse", "pays",
  "ti", "Tunisie", "Tunisie", "pays",
  "uk", "Grande-Bretagne", "Grande-Bretagne", "pays",
  "vm", "Vietnam", "Vietnam", "pays",
  "wau", "Washington", "États-Unis", "région",
  "xxu", NA, NA, NA
)

# Croisement des tables
PaysPublication <- merge.data.table(lire[, .(Pays)], equiv_pays, by.x = "Pays", by.y = "sigle")

PaysPublication_N <- PaysPublication[, .N, by="paysAssocie"][order(N, decreasing = TRUE)]

DistribPaysPublication <- ggplot(PaysPublication_N[N>10],
       aes(x = reorder(paysAssocie, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Distribution des romans exotopiques selon les principaux Pays de publication",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()
  
ggsave("resultats/diagrammes/20230418_PB_DistribPaysPublication.png", dpi=300)

fwrite(PaysPublication_N, "resultats/tables/20230418_PB_DistribPaysPublication.csv")




