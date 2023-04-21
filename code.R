# Statistiques descriptives de la base de données Romans à lire (BANQ)
# Auteur: Pascal Brissette (U. McGill)

# Structure du répertoire
if(!dir.exists("resultats")) dir.create("resultats")
if(!dir.exists("resultats/diagrammes")) dir.create("resultats/diagrammes")
if(!dir.exists("resultats/tables")) dir.create("resultats/tables")

#### Importation et prétraitement des données ----
source("fonctions.R")
rm(list = setdiff(ls(), c("data", "slicing_f")))


#### Nombre de titres uniques ----
# Nombre de titres uniques
data[!duplicated(data$titre), .N]


#### Distribution selon la forme (roman/nouvelle) ----

##### Création d'une colonne catégorielle ----
data$roman_nouvelle <- ifelse(data$genre_litteraire %ilike% "roman", "roman", "nouvelle") |> factor()

# Création d'un diagramme avec cette table
distrib_docs_genre <- ggplot(data[, .(roman_nouvelle)][, .N, "roman_nouvelle"],
                                 aes(x=reorder(roman_nouvelle, N), y=N))+
  geom_col()+
  geom_text(aes(label = N),
            hjust = 0.5,
            vjust = -0.7,
            size = 2.5,
            colour = "black") +
  labs(title = "Distribution des documents selon le genre (roman/nouvelle)",
       caption = "Données: BANQ, 2023") +
  ylab(NULL)+
  xlab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribGenreLitt.png", dpi=300)


#### Distribution selon l'année de publication ----

# Création d'une table avec la fonction
distrib_decennies <- data[!is.na(annee_de_publication), .(numero_de_sequence, annee_de_publication)][
  , decennies:=slicing_f(annee_de_publication)][
    ,.N, by="decennies"][
      order(decennies, decreasing = TRUE)]


# Création d'un diagramme avec cette table
distrib_docs_decennies <- ggplot(distrib_decennies, aes(x=decennies, y=N))+
  geom_col()+
  geom_text(aes(label = N),
            hjust = 0.5,
            vjust = -0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Distribution chronologique des documents par décennies\nNombre brut de documents",
       caption = "Données: BANQ, 2023")+
  ylab(NULL)+
  xlab("Décennies")+
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5, hjust=0.5))+
  theme_classic()

# Exportation du diagramme
ggsave("resultats/diagrammes/20230418_PB_DistribChronologiqueDecennies.png", dpi=300)

# Exportation de la table sous forme de .csv
fwrite(distrib_decennies, "resultats/tables/20230418_PB_DistribChronologiqueDecennies.csv")


#### Distribution par pays ----

# Corrections (la modalité indiquée d'abord est remplacée par la seconde):
data[pays == "FR", pays:="fr"]
data[pays == "cc", pays:="ch"]
data[pays == "enk", pays:="uk"]
data[pays == "xxk", pays:="uk"]

# Création d'une table
pays_distrib <- data.table(sigle = names(table(data$pays)),
                           N = as.vector(table(data$pays)))

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
paysPublication <- merge.data.table(data[, .(numero_de_sequence, pays)], equiv_pays, by.x = "pays", by.y = "sigle")

paysPublication_N <- paysPublication[, .N, by="paysAssocie"][
  order(N, decreasing = TRUE)
  ]

DistribPaysPublication <- ggplot(paysPublication_N[N>10], aes(x = reorder(paysAssocie, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Distribution des documents selon les principaux pays de publication",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()
  
ggsave("resultats/diagrammes/20230418_PB_DistribPaysPublication.png", dpi=300)

# Exportation de la table sous forme de .csv
fwrite(paysPublication_N, "resultats/tables/20230418_PB_DistribPaysPublication.csv")


#### Littérature nationale (étiquettes originales et à deux termes (genre + nationalité)) ----

# Utilisation des étiquettes originales (631 étiquettes uniques)
litteratureNationaleEtiquettesOriginales_N <- data[, .(litterature_nationale)][
  , .N, "litterature_nationale"][
  order(N, decreasing = TRUE)
  ]

# Exportation de la table sous forme de .csv
fwrite(litteratureNationale_N, "resultats/tables/20230418_PB_litteratureNationaleEtiquettesOriginales.csv")

# Extraction des deux premiers termes des étiquettes originales (romans/nouvelles + nationalité)
litteratureNationaleEtiquettes2termes_N <- data[, .(litterature_nationale)][
  , etiquettes2termes:=str_extract(litterature_nationale, pattern = "^[[:alpha:]]+\\s[[:alpha:]-]+")][
  , .N, "etiquettes2termes"][
    order(N, decreasing = TRUE)]

# Exportation de la table sous forme de .csv
fwrite(litteratureNationaleEtiquettes2termes_N, "resultats/tables/20230418_PB_litteratureNationaleEtiquettes2termes_N.csv")


# Diagramme (10 premières entrées)
DistribLitteratureNationale <- ggplot(litteratureNationaleEtiquettes2termes_N[1:10], aes(x = reorder(etiquettes2termes, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Distribution des documents selon la nationalité de l'auteur·e",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribLittNationale2termes.png", dpi=300)


# 

