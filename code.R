# Statistiques descriptives de la base de données Romans à lire (BANQ) ----

# Auteur: Pascal Brissette (U. McGill) ----



# Structure du répertoire
if(!dir.exists("resultats")) dir.create("resultats")
if(!dir.exists("resultats/diagrammes")) dir.create("resultats/diagrammes")
if(!dir.exists("resultats/tables")) dir.create("resultats/tables")

#### Importation et prétraitement des données ----
source("fonctions.R")
rm(list = setdiff(ls(), c(
  "data",
  "slicing_f",
  "separerCompter_f"
  )))


#### Nombre de titres uniques ----
# Nombre de titres uniques
data[!duplicated(data$titre), .N]


#### Distribution selon la langue du texte ----
table(data$langue)

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


# Importation de la table des sigles (champs Pays) et des équivalences (noms complets, pays associés)
siglesPaysEquiv <- fread("donnees/202304_PB_siglesPaysEquiv.csv")


# Croisement des tables
paysPublication <- merge.data.table(data[, .(numero_de_sequence, pays)], siglesPaysEquiv, by.x = "pays", by.y = "sigle")

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


#### Oeuvres adaptées et primées ----

# Extraction des années des adaptations
data[, annee_adaptation:=as.numeric(str_extract(adaptation, "[0-9]+$"))]
DistribAdaptationAnnuelle <- data[!is.na(annee_adaptation), .N, "annee_adaptation"][order(annee_adaptation, decreasing = TRUE)]

DistribLittAdaptation<- ggplot(DistribAdaptationAnnuelle[annee_adaptation>1930], aes(x=annee_adaptation, y=N))+
  geom_bar(stat = "identity") +
  # geom_text(aes(label = N),
  #           hjust = 0.5,
  #           vjust = -0.7,
  #           size = 2.5,
  #           colour = "black")+
  labs(title = "Distribution chronologique des adaptations après 1930",
       caption = "Données: BANQ, 2023")+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(breaks = c(1930, 1950, 1970, 1990, 2010))+
  theme_classic()

ggsave("resultats/diagrammes/20230421_PB_DistribAdaptation.png", dpi=300)

fwrite(DistribAdaptationAnnuelle, "resultats/tables/20230421_PB_DistribAdaptation.csv")


# Calcul du nombre de prix
data$nombre_de_prix <- lapply(data$prix_litteraire, separerCompter_f) |> unlist()

NbrePrix <- data[, .(auteur, titre, annee_de_publication, pays, nombre_de_prix)][order(-nombre_de_prix)]

gtNbrePrix <- gt(NbrePrix[nombre_de_prix>3]) |> 
  tab_header(
    title = "Oeuvres ayant obtenu plus de trois prix",
    subtitle = "Données: BANQ, 2023"
  ) |> 
  cols_label(annee_de_publication = "Année de publication",
             nombre_de_prix = "Nombre de prix reçus")

gtsave(gtNbrePrix, "resultats/tables/20230421_PB_NbrePrixRecus.png")

fwrite(NbrePrix, "resultats/tables/20230421_PB_OuvragesPrimes.csv")


#### Genres et sous-genres  ----
NbreNoNaSousGenre <- data[!is.na(genre_litteraire) & !genre_litteraire == "Romans" & !genre_litteraire == "Nouvelles", .N]
NbreNoNaSousGenrePourcent <- NbreNoNaSousGenre/nrow(data)*100

data$genre_litteraire[1:10]
tousGenres <- strsplit(data$genre_litteraire, ";") |> unlist() |> table()
tousGenres_dt <- data.table(genre = names(tousGenres),
                                  N = as.vector(tousGenres),
                                  key = "N")

RomansSousGenres <- tousGenres_dt[genre %ilike% "roman"]
NouvellesSousGenres <- tousGenres_dt[genre %ilike% "nouvelles"]

DistribRomansSousGenres <- ggplot(RomansSousGenres[order(-N)][2:16], 
                                  aes(x = reorder(genre, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Principaux sous-genres (roman) de la base de données",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribRomansSousGenres.png", dpi=300)
fwrite(RomansSousGenres[order(-N)], "resultats/tables/20230421_PB_RomansSousGenres.csv")



DistribNouvellesSousGenres <- ggplot(NouvellesSousGenres[order(-N)][2:16], 
                                  aes(x = reorder(genre, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.8,
            size = 2.5,
            colour = "black")+
  labs(title = "Principaux sous-genres (nouvelles) de la base de données",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribNouvellesSousGenres.png", dpi=300)
fwrite(NouvellesSousGenres[order(-N)], "resultats/tables/20230421_PB_NouvellesSousGenres.csv")


#### Sujets  ----
NbreNoNa <- data[!is.na(sujets), .N]
NbreNoNaPourcent <- NbreNoNa/nrow(data)*100
tousSujets <- strsplit(data$sujets, ";") |> unlist() |> table()
tousSujets_dt <- data.table(sujet = names(tousSujets),
                            N = as.vector(tousSujets),
                            key = "N")

DistribSujets <- ggplot(tousSujets_dt[order(-N)][1:15], aes(x = reorder(sujet, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.7,
            size = 2.5,
            colour = "black")+
  labs(title = "Principaux sujets de la base de données",
       subtitle = "N = nombre de notice comportant l'étiquette",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribSujets.png", dpi=300)

fwrite(tousSujets_dt[order(-N)], "resultats/tables/20230421_PB_tousSujets.csv")


#### Catégories de personnages  ----
NbreNoNaPerso <- data[!is.na(categories_de_personnages), .N]
NbreNoNaPourcentPerso <- NbreNoNaPerso/nrow(data)*100
tousPersonnages <- strsplit(data$categories_de_personnages, ";") |> unlist() |> table()
tousPersonnagres_dt <- data.table(personnage = names(tousPersonnages),
                            N = as.vector(tousPersonnages),
                            key = "N")

DistribPersonnages <- ggplot(tousPersonnagres_dt[order(-N)][1:15], aes(x = reorder(personnage, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.8,
            size = 2.5,
            colour = "black")+
  labs(title = "Principales catégories de personnages de la base de données",
       subtitle = "N = nombre de notice comportant l'étiquette",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribPersonnages.png", dpi=300)

fwrite(tousPersonnagres_dt[order(-N)], "resultats/tables/20230421_PB_DistribPersonnages.csv")



#### Lieux de l'action  ----
NbreNoNaLieu <- data[!is.na(lieu_geographique), .N]
NbreNoNaLieuPourcent <- NbreNoNaLieu/nrow(data)*100

tousLieux <- strsplit(data$lieu_geographique, ";") |> unlist() |> table()
tousLieux_dt <- data.table(lieu = names(tousLieux),
                                  N = as.vector(tousLieux),
                                  key = "N")

tousLieux_dt[order(-N)][1:15]

DistribLieuxAction <- ggplot(tousLieux_dt[order(-N)][1:15], aes(x = reorder(lieu, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.8,
            size = 2.5,
            colour = "black")+
  labs(title = "Principaux lieux de l'action dans les ouvrages de la base de données",
       subtitle = "N = nombre de notice comportant l'étiquette",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribLieuxAction.png", dpi=300)

fwrite(tousLieux_dt[order(-N)], "resultats/tables/20230421_PB_DistribLieuxAction.csv")

#### Période historique  ----
NbreNoNaPeriode <- data[!is.na(periode_historique), .N]
NbreNoNaPeriodePourcent <- NbreNoNaPeriode/nrow(data)*100

tousPeriode <- strsplit(data$periode_historique, ";") |> unlist() |> table()
tousPeriode_dt <- data.table(periode = names(tousPeriode),
                           N = as.vector(tousPeriode),
                           key = "N")

tousPeriode_dt[order(-N)][1:15]

DistribPeriodeHist <- ggplot(tousPeriode_dt[order(-N)][1:15], aes(x = reorder(periode, N), y=N)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  geom_text(aes(label = N),
            hjust = 0,
            vjust = +0.8,
            size = 2.5,
            colour = "black")+
  labs(title = "Principales périodes historiques de la base de données",
       subtitle = "N = nombre de notice comportant l'étiquette",
       caption = "Données: BANQ, 2023")+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()

ggsave("resultats/diagrammes/20230418_PB_DistribPeriodeHist.png", dpi=300)

fwrite(tousLieux_dt[order(-N)], "resultats/tables/20230421_PB_DistribPeriodeHist.csv")





