

# Chargement des libraries ------------------------------------------------

library(data.table)


# Chargement des fichiers -------------------------------------------------
chemin_fichiers <- "C:/Users/Aurelie/Documents/UnJourUneViz/BAN/"

liste_fichiers            <- list.files(chemin_fichiers)

# Chargement des fichiers
liste_fichiers <-
  liste_fichiers[grep(".csv", liste_fichiers)]

BAN_allfiles <-
  lapply(
    X   = liste_fichiers,
    FUN = function(x) {
      DT <- fread(file = paste0(chemin_fichiers, x))
      DT <-
        DT[!(is.na(nom_afnor) & is.na(nom_voie)),
           .(lat = min(lat),
             long = min(lon)) ,
           by = .(
             id_fantoir,
             numero,
             rep,
             nom_voie,
             nom_afnor,
             code_insee,
             code_post,
             libelle_acheminement
           )]
      DT <- unique(DT)
    }
  )
names(BAN_allfiles) <- liste_fichiers
