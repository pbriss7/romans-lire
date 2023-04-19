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


# Make clean names
names(lire) <- janitor::make_clean_names(names(lire))


#### Année de publication ----

# Remplacement d'erreurs (4) dans le champ Année de publication. Remplacement des modalités problématiques par NA
lire[, annee_de_publication:=ifelse(lire$annee_de_publication %ilike% "[0-9]+", as.integer(lire$annee_de_publication), NA)]

# Observation de la distribution
table(lire$annee_de_publication)

# Fonction pour agréger les documents par décennie
slicing_f <- function(x){
  decennies <- factor(paste0(str_sub(x, 1,3),0))
  return(decennies)
}

# Création d'une table avec la fonction
distrib_decennies <- lire[!is.na(annee_de_publication), .(numero_de_sequence, annee_de_publication)][
  , decennies:=slicing_f(annee_de_publication)][
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
# fwrite(distrib_decennies, "resultats/tables/20230418_PB_DistribChronologiqueDecennies.csv")


#### Distribution par pays ----
# Observation
names(table(lire$pays))

# Corrections (la modalité indiquée d'abord est remplacée par la seconde):
lire[pays == "FR", pays:="fr"]

lire[pays == "cc", pays:="ch"]

lire[pays == "enk", pays:="uk"]

lire[pays == "xxk", pays:="uk"]


pays_distrib <- data.table(sigle = names(table(lire$pays)),
                           N = as.vector(table(lire$pays)))

# Créer une table d'équivalences pour les lieux de publication
equiv_pays <- tribble(
  ~sigle, ~lieuSigleNomsComplet, ~paysAssocie, ~type,
  "abc", "Alberta", "Canada", "autre",
  "bbc", "Colombie Britannique", "Canada", "autre",
  "be", "Belgique", "Belgique", "pays",
  "cau", "Californie", "États-Unis", "autre",
  "cf", "République centrafricaine", "République centrafricaine", "pays",
  "ch", "Chine", "Chine", "pays",
  "cm", "Cameroun", "Cameroun", "pays",
  "cq", "Comores", "Comores", "pays",
  "dcu", "Washington, D.C.", "États-Unis", "autre",
  "dk", "Danemark", "Danemark", "pays",
  "fg", "Guyane", "France", "autre",
  "flu", "Floride", "États-Unis", "autre",
  "fp", "Tahiti", "France", "autre",
  "fr", "France", "France", "pays",
  "gp", "Guadeloupe", "France", "autre",
  "gr", "Grèce", "Grèce", "pays",
  "gw", "Allemagne", "Allemagne", "pays",
  "gy", "Guyane", "France", "autre",
  "ilu", "Illinois", "États-Unis", "autre",
  "inu", "Indiana", "États-Unis", "autre",
  "it", "Italie", "Italie", "pays",
  "iv", "Côte-d'Ivoire", "Côte-d'Ivoire", "pays",
  "le", "Liban", "Liban",  "pays",
  "lu", "Luxembourg", "Luxembourg", "pays",
  "mau", "Massachusetts", "États-Unis", "autre",
  "mbc", "Manitoba", "Canada", "autre",
  "mc", "Monaco", "Monaco", "pays",
  "mdu", "Maryland", "États-Unis", "autre",
  "meu", "Maine", "États-Unis", "autre",
  "mg", "Madagascar", "Madagascar", "pays",
  "mq", "Martinique", "France", "autre",
  "mr", "Maroc", "Maroc", "pays",
  "nju", "New Jersey", "États-Unis", "autre",
  "nkc", "Nouveau-Brunswick", "Canada", "autre",
  "nl", "Nouvelle-Calédonie", "France", "autre",
  "nsc", "Nouvelle-Écosse", "Canada", "autre",
  "nyu", "New York", "États-Unis", "autre",
  "onc", "Ontario", "Canada", "autre",
  "oru", "Oregon", "États-Unis", "autre",
  "pic", "Île-du-Prince-Édouard", "Canada", "autre",
  "pl", "Pologne", "Pologne", "pays",
  "po", "Portugal", "Portugal", "pays",
  "quc", "Québec", "Canada", "autre",
  "re", "Réunion", "France", "autre",
  "rm", "Roumanie", "Roumanie", "pays",
  "ru", "Russie", "Russie", "pays",
  "sg", "Sénégal", "Sénégal", "pays",
  "snc", "Saskatchewan", "Canada", "autre",
  "sp", "Espagne", "Espagne", "pays",
  "sz", "Suisse", "Suisse", "pays",
  "ti", "Tunisie", "Tunisie", "pays",
  "uk", "Grande-Bretagne", "Grande-Bretagne", "pays",
  "vm", "Vietnam", "Vietnam", "pays",
  "wau", "Washington", "États-Unis", "autre",
  "xxu", NA, NA, NA
)

# Croisement des tables
paysPublication <- merge.data.table(lire[, .(numero_de_sequence, pays)], equiv_pays, by.x = "pays", by.y = "sigle")

paysPublication_N <- paysPublication[, .N, by="paysAssocie"][order(N, decreasing = TRUE)]

DistribPaysPublication <- ggplot(paysPublication_N[N>10],
       aes(x = reorder(paysAssocie, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Distribution des romans exotopiques selon les principaux pays de publication",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()
  
ggsave("resultats/diagrammes/20230418_PB_DistribPaysPublication.png", dpi=300)

# Exportation de la table sous forme de .csv
# fwrite(PaysPublication_N, "resultats/tables/20230418_PB_DistribPaysPublication.csv")


#### Littérature nationale ----
# Création d'une nouvelle table comprenant les champs de paysPublication + champs de contenu
lireEnrichi <- merge(lire[, -c("pays")], PaysPublication, by = "numero_de_sequence")

colnames(lire)
personnages_principaux <- strsplit(lire$personnage_principal[1:10], ";") |> unlist()


