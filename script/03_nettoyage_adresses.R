# Chargement des packages --------------------------------------------------
library(data.table)
library(dplyr)
library(ggmap)
library(stringr)


# 0- Chargement des donnees -------------------------------------------------

contexte <- readRDS("data/contexte.RDS") %>% as.data.table()
source("functions/methodes-mots.R")

# 1. Adresses en agglomérations ---------------------------------------------
adresses <- contexte[agg == "En agglomération", .(adr,
                                                  Code_Postal,
                                                  Code_INSEE,
                                                  com,
                                                  dep,
                                                  Commune,
                                                  Departement,
                                                  gps,
                                                  lat,
                                                  long)]


# Mise en forme des adresses
# Mise en majuscule
adresses[, adr := toupper(adr)]
# 1.1. Récupération du numéro de la rue---------------------------------------------

# Séparation avant/ après virgule
adr_num_voies <- strsplit(adresses$adr, split = ",")
adr_num_voies2 <- lapply(
  X = 1:length(adr_num_voies),
  FUN = function(x) {
    if (length(adr_num_voies[[x]]) == 0) {
      av_virgule = NA
      ap_virgule = NA
    } else{
      if (length(adr_num_voies[[x]]) == 1) {
        av_virgule = NA
        ap_virgule = adr_num_voies[[x]]
      } else {
        av_virgule = adr_num_voies[[x]][1]
        ap_virgule = adr_num_voies[[x]][2]
      }
    }
    return(list(av_virgule, ap_virgule))
  }
)


adresses[,
         av_virgule := sapply(
           X = 1:nrow(adresses),
           FUN = function(x)
             adr_num_voies2[[x]][[1]]
         )]

adresses[,
         ap_virgule := sapply(
           X = 1:nrow(adresses),
           FUN = function(x)
             adr_num_voies2[[x]][[2]]
         )]



# Test si numéro restant après virgule
adresses[is.na(av_virgule), av_virgule := str_extract(string = adr, pattern = "^([0-9]+).")]
adresses[, ap_virgule := gsub("^([0-9]+).", "", ap_virgule)]


# Traitement des sans numéros
adresses[av_virgule %in% c("SNR", "SANS N", "SANS", "SANS N°"), av_virgule := NA]
adresses[, ap_virgule := supprime_mot(phrase = ap_virgule, c("SNR", "SANS N", "SANS", "SANS N°"))]


# 1.2. Formatage des noms de rue ---------------------------------------------------

# On retire les caractères spéciaux
adresses[, ap_virgule := supprime_espaces(gsub(
  pattern = "[[:punct:]]|\\(|\\)",
  replacement = " ",
  x = ap_virgule
))]

# Suppression mots inutiles
adresses[, ap_virgule := supprime_mot(phrase = ap_virgule, c("DE", "DU", "DES", "LE", "LA", "L", "LES"))]


# Suppression des accents
adresses[, ap_virgule := supprime_accents(ap_virgule)]

# Remplacer ST par SAINT
adresses[, ap_virgule := remplace_mot(ap_virgule, "ST", "SAINT")]
adresses[, ap_virgule := remplace_mot(ap_virgule, "STE", "SAINTE")]

# Gérer les communes avec / / => mettre deux lignes ?



# Typologie des voies -----------------------------------------------------

typo_voies <-
  c("ALLEE",
    "BOULEVARD",
    "RUE",
    "AVENUE",
    "CHEMIN",
    "PLACE",
    "ROUTE",
    "LIEU DIT")

#AVENUE
adresses[detecte_mot(ap_virgule, "AVENUE") |
           detecte_mot(ap_virgule, "AV") |
           detecte_mot(ap_virgule, "AVE") |
           detecte_mot(ap_virgule, "AVEN") |
           detecte_mot(ap_virgule, "AVENU"), type_voie := "AVENUE"]
adresses[type_voie == "AVENUE", ap_virgule := supprime_mot(ap_virgule, c("AVENUE", "AV", "AVE", "AVEN", "AVENU"))]

#BOULEVARD
adresses[detecte_mot(ap_virgule, "BOULEVARD") |
           detecte_mot(ap_virgule, "BOULEVAR") |
           detecte_mot(ap_virgule, "BOULEVA") |
           detecte_mot(ap_virgule, "BOULE") |
           detecte_mot(ap_virgule, "BOULEV") |
           detecte_mot(ap_virgule, "BO") |
           detecte_mot(ap_virgule, "BVD") |
           detecte_mot(ap_virgule, "B") |
           detecte_mot(ap_virgule, "BD") , type_voie := "BOULEVARD"]
adresses[type_voie == "BOULEVARD", ap_virgule := supprime_mot(ap_virgule,
                                                              c(
                                                                "BOULEVARD",
                                                                "BOULEVAR",
                                                                "BOULEVA",
                                                                "BD",
                                                                "BOULE",
                                                                "BOULEV",
                                                                "BO",
                                                                "B",
                                                                "BVD"
                                                              ))]


#RUE
adresses[detecte_mot(ap_virgule, "RUE") |
           detecte_mot(ap_virgule, "R") |
           detecte_mot(ap_virgule, "RU") , type_voie := "RUE"]
adresses[type_voie == "RUE", ap_virgule := supprime_mot(ap_virgule, c("RUE", "R", "RU"))]

#ALLEE
adresses[detecte_mot(ap_virgule, "ALLEE") |
           detecte_mot(ap_virgule, "AL") |
           detecte_mot(ap_virgule, "ALL"), type_voie := "ALLEE"]
adresses[type_voie == "ALLEE", ap_virgule := supprime_mot(ap_virgule, c("ALLEE", "AL", "ALL"))]

#ROUTE
adresses[detecte_mot(ap_virgule, "ROUTE") |
           detecte_mot(ap_virgule, "RTE") , type_voie := "ROUTE"]
adresses[type_voie == "ROUTE", ap_virgule := supprime_mot(ap_virgule, c("ROUTE", "RTE"))]

#PLACE
adresses[detecte_mot(ap_virgule, "PLACE") |
           detecte_mot(ap_virgule, "PL") |
           detecte_mot(ap_virgule, "PLA"), type_voie := "PLACE"]
adresses[type_voie == "PLACE", ap_virgule := supprime_mot(ap_virgule, c("PLACE", "PL", "PLA"))]

#CHEMIN
adresses[detecte_mot(ap_virgule, "CHEMIN") , type_voie := "CHEMIN"]
adresses[type_voie == "CHEMIN", ap_virgule := supprime_mot(ap_virgule, c("CHEMIN"))]

# LIEU DIT
adresses[detecte_mot(ap_virgule, "LIEU DIT") , type_voie := "LIEU DIT"]
adresses[type_voie == "LIEU DIT", ap_virgule := supprime_mot(ap_virgule, c("LIEU DIT"))]

View(adresses[order(Code_Postal, type_voie, ap_virgule)])


# 1.3. Nettoyage des noms de voie ----------------------------------------------

noms_voie <-
  adresses[!(is.na(ap_virgule) |
               ap_virgule %in% c("", " ") |
               ap_virgule == 0), .(.N) , by = .(Code_INSEE, Code_Postal, type_voie, ap_virgule)]

noms_voie[as.numeric(Code_INSEE) < 97000, dep := substr(Code_INSEE, 1, 2)]
noms_voie[as.numeric(Code_INSEE) > 97000, dep := substr(Code_INSEE, 1, 3)]
noms_voie[, premier_mot := word(ap_virgule, 1)]
noms_voie[, dernier_mot := word(ap_virgule,-1)]

# Liste des adresses du BAN
liste_fichiers <- list.files("C:/Users/Aurelie/Documents/UnJourUneViz/BAN")
liste_fichiers <- liste_fichiers[grep(".csv", liste_fichiers)]

liste_fichiers_BAN  <- lapply(
  X = liste_fichiers,
  FUN = function(fichier_BAN) {
    
    rues <- fread(file = file.path("C:/Users/Aurelie/Documents/UnJourUneViz/BAN", fichier_BAN))
    rues <-
      rues[nom_voie != "" |
             nom_afnor != "", .(.N), by = .(
               code_insee =  formatC(
                 code_insee,
                 width = 5,
                 format = "d",
                 flag = "0"
               ),
               Code_Postal =  formatC(
                 code_post,
                 width = 5,
                 format = "d",
                 flag = "0"
               ),
               nom = supprime_mot(phrase = nom_afnor, c("DE", "DU", "DES", "LE", "LA", "L", "LES"))
             )]
  }
)

fichiers_BAN <- rbindlist(liste_fichiers_BAN)
fichiers_BAN[as.numeric(code_insee) < 97000, dep := substr(code_insee, 1, 2)]
fichiers_BAN[as.numeric(code_insee) > 97000, dep := substr(code_insee, 1, 3)]
fichiers_BAN[, sans_typo   := supprime_mot(fichiers_BAN$nom, typo_voies)]
fichiers_BAN[, dernier_mot := word(nom, -1)]

saveRDS(
  fichiers_BAN,
  "C:/Users/Aurelie/Documents/UnJourUneViz/BAN/adresses_toutes_communes.RDS"
)
fichiers_BAN <-
  readRDS("C:/Users/Aurelie/Documents/UnJourUneViz/BAN/adresses_toutes_communes.RDS")

# Correspondance par commune
COMMUNES <- unique(noms_voie$Code_INSEE)
COMMUNES <- COMMUNES[!is.na(COMMUNES)]
les_echappes_INSEE <- unique(noms_voie[is.na(Code_Postal)]$Code_INSEE)

liste_resultats <- lapply(
  X = COMMUNES,
  FUN = function(commune) {
    print(commune)
    if (commune %in% les_echappes_INSEE ){
      rues      <- fichiers_BAN[substr(dep, 1, 2) == substr(commune, 1, 2)]
      noms_voie <- noms_voie[substr(dep, 1, 2) == substr(commune, 1, 2)]
      noms_voie$nom_propre <-
        sapply(
          X = 1:nrow(noms_voie),
          FUN = function(i) {
            id <-
              pmatch(paste(noms_voie$type_voie[i],
                           noms_voie$ap_virgule[i]),
                     rues$nom)
            
            # sans le type de voie dans la même commune
            if (is.na(id)) {
              id <- pmatch(noms_voie$ap_virgule[i],
                           rues$sans_typo)
            }
            
            
            # Avec le dernier mot
            if (is.na(id)) {
              id <- match(noms_voie$dernier_mot[i],
                          rues$dernier_mot)
            }
            
            # Avec le premier mot
            if (is.na(id)) {
              id <- match(noms_voie$premier_mot[i],
                          rues$dernier_mot)
            }
            
            return(rues$nom[id])
          }
        )
    } else {
      rues      <- fichiers_BAN[code_insee == commune]
      noms_voie <- noms_voie[Code_INSEE == commune]
      noms_voie$nom_propre <-
        sapply(
          X = 1:nrow(noms_voie),
          FUN = function(i) {
            id <-
              pmatch(paste(noms_voie$type_voie[i],
                           noms_voie$ap_virgule[i]),
                     rues$nom)
            
            # sans le type de voie dans la même commune
            if (is.na(id)) {
              id <- pmatch(noms_voie$ap_virgule[i],
                           rues$sans_typo)
            }
            
            
            # Avec le dernier mot
            if (is.na(id)) {
              id <- match(noms_voie$dernier_mot[i],
                          rues$dernier_mot)
            }
            
            # Avec le premier mot
            if (is.na(id)) {
              id <- match(noms_voie$premier_mot[i],
                          rues$dernier_mot)
            }
            
            return(rues$nom[id])
          }
        )
    }
    
    return(noms_voie)
  }
)
noms_voie <- rbindlist(liste_resultats)
sum(is.na(noms_voie$nom_propre)) / nrow(noms_voie)


#9 Refaire par département pour les villes non attrapées : pb d'indentification code insee
test <- noms_voie[, sum(is.na(nom_propre)), by = Code_INSEE]$Code_INSEE
View(noms_voie[Code_INSEE %in% test & is.na(nom_propre)])
noms_voie[is.na(Code_Postal)]
