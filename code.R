# Statistiques descriptives de la base de données Romans à lire (BANQ) ----

## Auteur: Pascal Brissette (U. McGill)
## Commentaires et questions: pascal.brissette@mcgill.ca


# Structure du répertoire
if(!dir.exists("resultats")) dir.create("resultats")
if(!dir.exists("resultats/diagrammes")) dir.create("resultats/diagrammes")
if(!dir.exists("resultats/tables")) dir.create("resultats/tables")

#### Importation et prétraitement des données ----
source("fonctions.R")
rm(list = setdiff(ls(), c(
  "data",
  "slicing_f",
  "separerCompter_f",
  "graphique_f"
  )))

#### Nombre de titres uniques ----
TitresUniques <- data.table(NbreObservationsTotal = data[, .N],
                            NbreTitresUniques = data[!duplicated(data$titre), .N])

fwrite(TitresUniques, "resultats/tables/20240423_PB_TitresUniques.csv")


#### Distribution selon la langue du texte ----
LangueTexte <- data.table(LangueTexte = names(table(data$langue)),
                          N = as.vector(table(data$langue)))

LangueTexte_order_dt <- LangueTexte[order(-N)]

fwrite(LangueTexte_order_dt, "resultats/tables/20230423_PB_DistribLangueTexte.csv")


#### Distribution selon le genre (roman/nouvelle) ----

# Création d'une colonne catégorielle roman/nouvelle
data$roman_nouvelle <- ifelse(data$genre_litteraire %ilike% "roman", "roman", "nouvelle") |> factor()

# Données pour tableau
DistribRomanNouvelle_dt <- data[, .(roman_nouvelle)][, .N, "roman_nouvelle"]

distrib_docs_genre<- graphique_f(DistribRomanNouvelle_dt,
                                 x = roman_nouvelle,
                                 y = N,
                                 flip = FALSE,
                                 reorder = FALSE,
                                 titre = "Distribution des documents selon le genre (roman/nouvelle)")

ggsave("resultats/diagrammes/20230418_PB_DistribGenreLitt.png", dpi=300)


#### Distribution selon l'année de publication ----

# Création d'une table avec la fonction
distrib_decennies <- data[!is.na(annee_de_publication), .(numero_de_sequence, annee_de_publication)][
  , decennies:=slicing_f(annee_de_publication)][
    ,.N, by="decennies"][order(decennies)]
      
# decennies:=as.factor(decennies, levels = c("1830", "1840", "1860", "1870", "1880", "1890","1900","1910","1920","1930",
#                                               "1940", "1950", "1960", "1970", "1990", "2000", "2010", "2020"))]


# Création d'un diagramme avec cette table
distrib_docs_decennies <-
  ggplot(distrib_decennies[1:19], aes(x = decennies, y = N)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution chronologique des documents par décennies",
       caption = "Données: BANQ, 2023") +
  xlab(NULL) +
  ylab(NULL) +
  theme_classic() +
  geom_text(
    aes(label = N),
    hjust = 0.5,
    vjust = -0.7,
    size = 2.5,
    colour = "black"
  )

# Exportation du diagramme
ggsave("resultats/diagrammes/20230418_PB_DistribChronologiqueDecennies.png", dpi=300)

# Exportation de la table sous forme de .csv
fwrite(distrib_decennies, "resultats/tables/20230418_PB_DistribChronologiqueDecennies.csv")


#### Distribution par pays de publication ----

# Corrections au jeu de données (la modalité indiquée d'abord est remplacée par la seconde):
data[pays == "FR", pays:="fr"]
data[pays == "cc", pays:="ch"]
data[pays == "enk", pays:="uk"]
data[pays == "xxk", pays:="uk"]

# Création d'une table
pays_distrib <- data.table(sigle = names(table(data$pays)),
                           N = as.vector(table(data$pays)))


# Importation de la table des sigles (champ Pays) et des équivalences (noms complets, pays associés)
siglesPaysEquiv <- fread("donnees/202304_PB_siglesPaysEquiv.csv")


# Croisement des tables
paysPublication <- merge.data.table(data[, .(numero_de_sequence, pays)], siglesPaysEquiv, by.x = "pays", by.y = "sigle")

paysPublication_N <- paysPublication[, .N, by="paysAssocie"][
  order(N, decreasing = TRUE)
  ]

paysPublication_order_dt <- paysPublication_N[N>3]

DistribPaysPublication <- graphique_f(paysPublication_order_dt,
                                      x = paysAssocie,
                                      y = N,
                                      titre = "Distribution des documents selon les principaux pays de publication")
  
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


# Données pour diagramme
litteratureNationaleEtiquettes2termes_order_dt_N <- litteratureNationaleEtiquettes2termes_N[1:15]

# Diagramme (10 premières entrées)
DistribLitteratureNationale <- graphique_f(litteratureNationaleEtiquettes2termes_order_dt_N,
                                           x = etiquettes2termes,
                                           y = N,
                                           titre = "Distribution des documents selon les modalités du champ `Littérature nationale`"
                                           )

ggsave("resultats/diagrammes/20230418_PB_DistribLittNationale2termes.png", dpi=300)


#### Oeuvres adaptées et primées ----

# Extraction des années des adaptations
data[, annee_adaptation:=as.numeric(str_extract(adaptation, "[0-9]+$"))]
DistribAdaptationAnnuelle <- data[!is.na(annee_adaptation), .N, "annee_adaptation"][order(annee_adaptation, decreasing = TRUE)]

DistribAdaptationAnnuelle_ord_dt <- DistribAdaptationAnnuelle[annee_adaptation>1980]

DistribLittAdaptation<- graphique_f(DistribAdaptationAnnuelle_ord_dt,
                                    x = annee_adaptation,
                                    y = N,
                                    titre = "Distribution chronologique des adaptations après 1980",
                                    flip = FALSE,
                                    reorder = FALSE)

ggsave("resultats/diagrammes/20230421_PB_DistribAdaptation.png", dpi=300)

fwrite(DistribAdaptationAnnuelle, "resultats/tables/20230421_PB_DistribAdaptation.csv")


#### Calcul du nombre de prix ----
data$nombre_de_prix <- lapply(data$prix_litteraire, separerCompter_f) |> unlist()

NbrePrix <- data[, .(auteur, titre, annee_de_publication, pays, nombre_de_prix)][order(-nombre_de_prix)]

# Tableau sous forme d'image (png)
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

# Séparation des sous-genres par genre (roman/nouvelle)
RomansSousGenres <- tousGenres_dt[genre %ilike% "roman"]
NouvellesSousGenres <- tousGenres_dt[genre %ilike% "nouvelles"]

# Données pour diagramme
RomansSousGenres_ord_dt <- RomansSousGenres[order(-N)][2:16]

# Diagramme (sous-genres romans)
DistribRomansSousGenres <- graphique_f(data = RomansSousGenres_ord_dt,
                                       x = genre,
                                       y = N,
                                       titre = "Principaux sous-genres (roman) de la base de données",
                                       flip = TRUE)

# Sauvegarde du graphique et des données
ggsave("resultats/diagrammes/20230418_PB_DistribRomansSousGenres.png", dpi=300)
fwrite(RomansSousGenres[order(-N)], "resultats/tables/20230421_PB_RomansSousGenres.csv")


# Diagramme (sous-genres nouvelles)
NouvellesSousGenres_ord_dt <- NouvellesSousGenres[order(-N)][2:16]

DistribNouvellesSousGenres <- graphique_f(NouvellesSousGenres_ord_dt,
                                          x = genre,
                                          y = N,
                                          flip = TRUE,
                                          titre = "Principaux sous-genres (nouvelles) de la base de données",
                                          )

ggsave("resultats/diagrammes/20230418_PB_DistribNouvellesSousGenres.png", dpi=300)
fwrite(NouvellesSousGenres[order(-N)], "resultats/tables/20230421_PB_NouvellesSousGenres.csv")


#### Sujets  ----
NbreNoNa <- data[!is.na(sujets), .N]
NbreNoNaPourcent <- NbreNoNa/nrow(data)*100

# Données pour graphique
tousSujets <- strsplit(data$sujets, ";") |> unlist() |> table()
tousSujets_dt <- data.table(sujet = names(tousSujets),
                            N = as.vector(tousSujets),
                            key = "N")

tousSujets_ord_dt <- tousSujets_dt[order(-N)][1:15]


# Diagramme
DistribSujets <- graphique_f(tousSujets_ord_dt,
                             x = sujet,
                             y = N,
                             flip = TRUE,
                             titre = "Principaux sujets de la base de données")


ggsave("resultats/diagrammes/20230418_PB_DistribSujets.png", dpi=300)

fwrite(tousSujets_dt[order(-N)], "resultats/tables/20230421_PB_tousSujets.csv")




#### Catégories de personnages  ----
NbreNoNaPerso <- data[!is.na(categories_de_personnages), .N]
NbreNoNaPourcentPerso <- NbreNoNaPerso/nrow(data)*100

# Données pour graphique
tousPersonnages <- strsplit(data$categories_de_personnages, ";") |> unlist() |> table()
tousPersonnagres_dt <- data.table(personnage = names(tousPersonnages),
                            N = as.vector(tousPersonnages),
                            key = "N")

tousPersonnagres_ord_dt <- tousPersonnagres_dt[order(-N)][1:15]

# Diagramme

DistribPersonnages <- graphique_f(tousPersonnagres_ord_dt,
                                  x = personnage,
                                  y = N,
                                  titre = "Principales catégories de personnages de la base de données",
                                  flip = TRUE)

ggsave("resultats/diagrammes/20230418_PB_DistribPersonnages.png", dpi=300)

fwrite(tousPersonnagres_dt[order(-N)], "resultats/tables/20230421_PB_DistribPersonnages.csv")




#### Lieux de l'action  ----
NbreNoNaLieu <- data[!is.na(lieu_geographique), .N]
NbreNoNaLieuPourcent <- NbreNoNaLieu/nrow(data)*100

tousLieux <- strsplit(data$lieu_geographique, ";") |> unlist() |> table()
tousLieux_dt <- data.table(lieu = names(tousLieux),
                                  N = as.vector(tousLieux),
                                  key = "N")

# Données pour graphique
tousLieux_ord_dt <- tousLieux_dt[order(-N)][1:15]

# Diagramme
DistribLieuxAction <- graphique_f(tousLieux_ord_dt,
                                  x = lieu,
                                  y = N,
                                  titre = "Principaux lieux de l'action dans les ouvrages de la base de données",
                                  flip = TRUE)

ggsave("resultats/diagrammes/20230418_PB_DistribLieuxAction.png", dpi=300)

fwrite(tousLieux_dt[order(-N)], "resultats/tables/20230421_PB_DistribLieuxAction.csv")



#### Période historique  ----
NbreNoNaPeriode <- data[!is.na(periode_historique), .N]
NbreNoNaPeriodePourcent <- NbreNoNaPeriode/nrow(data)*100

# Données pour graphique
tousPeriode <- strsplit(data$periode_historique, ";") |> unlist() |> table()
tousPeriode_dt <- data.table(periode = names(tousPeriode),
                           N = as.vector(tousPeriode),
                           key = "N")
tousPeriode_ord_dt = tousPeriode_dt[order(-N)][1:15]

# Diagramme
DistribPeriodeHist <- graphique_f(tousPeriode_ord_dt,
                                  x = periode,
                                  y = N,
                                  titre = "Principales périodes historiques de la base de données",
                                  flip = TRUE)


ggsave("resultats/diagrammes/20230418_PB_DistribPeriodeHist.png", dpi=300)

fwrite(tousLieux_dt[order(-N)], "resultats/tables/20230421_PB_DistribPeriodeHist.csv")
