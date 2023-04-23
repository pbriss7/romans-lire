# Fonctions d'importation et de prétraitement de la table

#### Fonction d'installation/activation des extensions ----
activate_packages_f <- function(){
  inst_ext_fun <- function(extension) {
    if(!extension %in% rownames(installed.packages())) {
      install.packages(extension, dependencies = TRUE)
    }
    require(extension, character.only = TRUE)
  }

  
  # Application aux extensions
  extensions <-
    c(
      "data.table",
      "readxl",
      "ggplot2",
      "stringr",
      "tibble",
      "janitor",
      "XLS",
      "gt"
    )
  
  # # Ajout aux extensions de base
  # autres_extensions <- readline("Les extensions suivantes et leurs dépendances seront installées:\ndata.table\nreadxl\nggplot2\nstringr\ntibble\njanitor\nXLS\nSouhaitez-vous installer d'autres extensions? (oui/non)  ")
  # if(substr(tolower(autres_extensions), 1, 1) == "o") {
  #   autres_extensions <- readline("S'il y a plus d'une extension, séparez les noms par une virgule:  ")
  #   autres_extensions <- strsplit(autres_extensions, ",") |> unlist()
  #   extensions <- append(extensions, autres_extensions)
  # }
  
  # Application de la fonction à chaque élément du vecteur`extensions`
  sapply(extensions, inst_ext_fun)
}

#### Fonction d'importation ----
import_f <- function() {
  # Demander le chemin du fichier
  # reponse <- readline("Importation des données. Fournissez le lien exact du fichier de données, sans guillemets:  ")
  reponse <- "donnees/lireAll.xlsx"
  # Vérifier si le chemin conduit au fichier
  if(!file.exists(reponse)) {
    stop("Vérifier le chemin du fichier")
  }
  
  # Vérifier si la table est dans un format approprié
  if(!grepl(pattern = "([ct]sv$)|(xlsx$)", reponse)) {
    stop("Le fichier doit avoir une extension .xlsx, .tsv ou .csv")
  }
  
  # Importation de la table brute
  if(!grepl(pattern = "xlsx$", reponse)) {
    x <- fread(reponse)
  } else {
    x <- read_excel(reponse, sheet = 1) |> setDT()
  }
  return(x)
}


#### Fonction de formatage ----
format_f <- function(x){
  # Nom des champs
  x <- x[, .(`Numéro de séquence`,
             Titre,
             Auteur,
             `Éditeur`,
             Description,
             Collection,
             ISBN,
             Langue,
             `Année de publication`,
             `Lieu de publication`,
             Pays,
             `Vedettes-matières`,
             Adaptation,
             `Personnage principal`,
             `Période historique`,
             Sujets,
             `Lieu géographique`,
             `Genre littéraire`,
             `Littérature nationale`,
             `Catégories de personnages`,
             `Prix littéraire`
  )]
  colnames(x) <- make_clean_names(names(x))
  
  # Types de données et création de doc_id
  if(all(sapply(x$numero_de_sequence, grepl, pattern = "[0-9]"))) {
    x$numero_de_sequence <- sapply(x$numero_de_sequence, as.integer)
    setkey(x, numero_de_sequence)
  } else {
    x$doc_id <- 1:nrow(x)
    setkey(x, doc_id)
    warning("Les numéros de séquence ne sont pas tous numériques. Des doc_id ont été créés.")
  }
  
  if(all(sapply(x$annee_de_publication, grepl, pattern = "[0-9]"))) {
    x$annee_de_publication <- sapply(x$annee_de_publication, as.integer)
  } else {
    x$annee_de_publication <- sapply(x$annee_de_publication, function(x) ifelse(grepl(pattern = "[^0-9]", x), NA, x)) |> as.integer()
    warning("Les valeurs non numériques de la variable annee_de_publication ont été remplacées par NA.")
  }

  return(x)
}


#### Fonction principale d'importation et de prétraitement ----
importAndClean_main_f <- function() {
  activate_packages_f()
  x <- import_f() |> format_f()
  return(x)
}

data <- importAndClean_main_f()


# # Pour les tests, deux tables comprenant 3 lignes
# xlsx = "donnees/testxlsx.xlsx"
# csv = "donnees/testcsv.csv"
# fonction_principale_f(xlsx)
# fonction_principale_f(csv)


# Fonction pour agréger les documents par décennie
slicing_f <- function(x) {
  decennies <- factor(paste0(str_sub(x, 1,3),0))
  return(decennies)
}

# Fonction permettant de séparer les éléments d'une chaine sur le point-virgule et de renvoyer le nombre d'éléments du vecteur ainsi composé
separerCompter_f <- function(x, split = ";") {
  if(!is.na(x)){
    x <- strsplit(x, split) |> unlist()
    y <- length(x)
  } else {
    y <- 0
  }
  return(y)
}


# Base du graphique à barres
graphique_f <- function(data, x, y, titre, flip=TRUE, reorder = TRUE){
  if(reorder){
    mapping = aes(x = reorder({{x}}, {{y}}), y = {{y}})
  } else {
    mapping = aes(x = {{x}}, y = {{y}})
  }
  graphique <- ggplot(data, mapping) +
    geom_bar(stat = "identity") +
    labs(title = titre,
         caption = "Données: BANQ, 2023")+
    xlab(NULL)+
    ylab(NULL)+
    theme_classic()
  if(flip) {
    graphique <- graphique + 
      coord_flip() +
      geom_text(aes(label = N),
                hjust = 0,
                vjust = +0.8,
                size = 2.5,
                colour = "black")
  } else {
    graphique <- graphique +
      geom_text(aes(label = N),
                hjust = 0.5,
                vjust = -0.7,
                size = 2.5,
                colour = "black")
  }
  return(graphique)
}
