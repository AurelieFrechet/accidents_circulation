







# Chargement des packages --------------------------------------------------
library(data.table)
library(forcats)



# Chargement des fichiers -------------------------------------------------

liste_fichiers            <- list.files("data")


# Chargement caractéristiques ---------------------------------------------

# Chargement des fichiers
fichiers_caracteristiques <-
  liste_fichiers[grep("caracteristiques", liste_fichiers)]

allfiles_caracteristiques <-
  lapply(
    X   = fichiers_caracteristiques,
    FUN = function(x) {
      fread(file = paste0("data/", x))
    }
  )
caracteristiques <- rbindlist(allfiles_caracteristiques)

# Renommage des variables
str(caracteristiques)

caracteristiques[, c("lum", "agg", "int", "atm", "col") := lapply(.SD, function(x)
  as.factor(as.character(x))), .SDcols =
    c("lum", "agg", "int", "atm", "col")]

caracteristiques[, c("lat", "long") := lapply(.SD, as.numeric), .SDcols =
                   c("lat", "long")]

caracteristiques[, c("lum",
                     "agg",
                     "int",
                     "atm",
                     "col") := .(
                       fct_recode(
                         lum,
                         "Plein jour"                            = "1",
                         " Crépuscule ou aube"                   = "2",
                         "Nuit sans éclairage public"            = "3",
                         "Nuit avec éclairage public non allumé" = "4",
                         "Nuit avec éclairage public allumé"     = "5"
                       ),
                       fct_recode(
                         agg,
                         "Hors agglomération" = "1",
                         "En agglomération"   = "2"
                       ),
                       fct_recode(
                         int,
                         "Hors intersection"                 = "1",
                         "Intersection en X"                 = "2",
                         "Intersection en T"                 = "3",
                         "Intersection en Y"                 = "4",
                         "Intersection à plus de 4 branches" = "5",
                         "Giratoire"                         = "6",
                         "Place"                             = "7",
                         "Passage à niveau"                  = "8",
                         "Autre intersection"                = "9"
                       ),
                       fct_recode(
                         atm,
                         "Normale"             = "1",
                         "Pluie légère"        = "2",
                         "Pluie forte"         = "3",
                         " Neige - grêle"      = "4",
                         "Brouillard - fumée"  = "5",
                         "Vent fort - tempête" = "6",
                         "Temps éblouissant"   = "7",
                         "Temps couvert"       = "8",
                         "Autre"               = "9"
                       ),
                       fct_recode(
                         col,
                         "Deux véhicules - frontale"                       = "1",
                         "Deux véhicules – par l’arrière"                  = "2",
                         "Deux véhicules – par le coté"                    = "3",
                         "Trois véhicules et plus – en chaîne"             = "4",
                         "Trois véhicules et plus  - collisions multiples" = "5",
                         "Autre collision"                                 = "6",
                         "Sans collision"                                  = "7"
                       )
                     )]



# Chargement lieux ---------------------------------------------------------

fichiers_lieux            <-
  liste_fichiers[grep("lieux", liste_fichiers)]

allfiles_lieux <-
  lapply(
    X   = fichiers_lieux,
    FUN = function(x) {
      fread(file = paste0("data/", x))
    }
  )
lieux <- rbindlist(allfiles_lieux)


# Renommage des variables
str(lieux)

lieux[, c("catr",
          "circ",
          "vosp",
          "prof",
          "plan",
          "surf",
          "infra",
          "situ") := lapply(.SD, function(x)
            as.factor(as.character(x))), .SDcols =
        c("catr",
          "circ",
          "vosp",
          "prof",
          "plan",
          "surf",
          "infra",
          "situ")]

lieux[, c("catr",
          "circ",
          "vosp",
          "prof",
          "plan",
          "surf",
          "infra",
          "situ") := .(
            fct_recode(
              catr,
              "Autoroute"                                              = "1",
              "Route Nationale"                                        = "2",
              "Route Départementale"                                   = "3",
              "Voie Communale"                                         = "4",
              "Hors réseau public"                                     = "5",
              "Parc de stationnement ouvert à la circulation publique" = "6",
              "Autre"                                                  = "9"
            ),
            fct_recode(
              circ,
              "A sens unique"                     = "1",
              "Bidirectionnelle"                  = "2",
              "A chaussées séparées"              = "3",
              "Avec voies d’affectation variable" = "4"
            ),
            fct_recode(
              vosp,
              "Piste cyclable"  = "1",
              "Banque cyclable" = "2",
              "Voie réservée"   = "3"
            ),
            fct_recode(
              prof,
              "Plat"           = "1",
              "Pente"          = "2",
              "Sommet de côte" = "3",
              "En S"           = "4"
            ),
            fct_recode(
              plan,
              "Partie rectiligne"  = "1",
              "En courbe à gauche" = "2",
              "En courbe à droite" = "3",
              "En S"               = "4"
            ),
            fct_recode(
              surf,
              "normale"            = "1",
              "mouillée"           = "2",
              "flaques"            = "3",
              "inondée"            = "4",
              "enneigée"           = "5",
              "boue"               = "6",
              "verglacée"          = "7",
              "corps gras - huile" = "8",
              "autre"              = "9"
            ),
            fct_recode(
              infra,
              "Souterrain - tunnel"                     = "1",
              "Pont - autopont"                         = "2",
              "Bretelle d’échangeur ou de raccordement" = "3",
              "Voie ferrée"                             = "4",
              "Carrefour aménagé"                       = "5",
              "Zone piétonne"                           = "6",
              "Zone de péage"                           = "7"
            ),
            fct_recode(
              situ,
              "Sur chaussée"                = "1",
              "Sur bande d’arrêt d’urgence" = "2",
              "Sur accotement"              = "3",
              "Sur trottoir"                = "4",
              "Sur piste cyclable"          = "5"
            )
          )]

# Chargement usagers ------------------------------------------------------

fichiers_usagers          <-
  liste_fichiers[grep("usagers", liste_fichiers)]

allfiles_usagers <-
  lapply(
    X   = fichiers_usagers,
    FUN = function(x) {
      fread(file = paste0("data/", x))
    }
  )
usagers <- rbindlist(allfiles_usagers)


# Renommage des variables
str(usagers)

# secu1 et secu2
usagers[, c("secu1", "secu2") := .(substr(as.character(secu), 1, 1),
                                   substr(as.character(secu), 2, 2))]

usagers[, c("catu",
            "grav",
            "sexe",
            "trajet",
            "secu1",
            "secu2",
            "locp",
            "actp",
            "etatp") := lapply(.SD, function(x)
              as.factor(as.character(x))), .SDcols =
          c("catu",
            "grav",
            "sexe",
            "trajet",
            "secu1",
            "secu2",
            "locp",
            "actp",
            "etatp")]


usagers[, c("catu",
            "grav",
            "sexe",
            "trajet",
            "secu1",
            "secu2",
            "locp",
            "actp",
            "etatp") := .(
              fct_recode(
                catu,
                "Conducteur"                         = "1",
                "Passager"                           = "2",
                "Piéton"                             = "3",
                "Piéton en roller ou en trottinette" = "4"
              ),
              fct_recode(
                grav,
                "Indemne"            = "1",
                "Tué"                = "2",
                "Blessé hospitalisé" = "3",
                "Blessé léger"       = "4"
              ),
              fct_recode(sexe,
                         "Masculin" = "1",
                         "Féminin"  = "2"),
              fct_recode(
                trajet,
                "Domicile – travail"          = "1",
                "Domicile – école"            = "2",
                "Courses – achats"            = "3",
                "Utilisation professionnelle" = "4",
                "Promenade – loisirs"         = "5",
                "Autre"                       = "9"
              ),
              fct_recode(
                secu1,
                "Ceinture"                 = "1",
                "Casque"                   = "2",
                "Dispositif enfants"       = "3",
                "Equipement réfléchissant" = "4",
                "Autre"                    = "9"
              ),
              fct_recode(
                secu2,
                "Oui"              = "1",
                "Non"              = "2",
                "Non déterminable" = "3"
              ),
              fct_recode(
                locp,
                "Sur chaussée à +50 m du passage piéton"          = "1",
                "Sur chaussée à – 50 m du passage piéton"         = "2",
                "Sur passage piéton sans signalisation lumineuse" = "3",
                "Sur passage piéton avec signalisation lumineuse" = "4",
                "Sur trottoir"                                    = "5",
                "Sur accotement"                                  = "6",
                "Sur refuge ou BAU"                               = "7",
                "Sur contre allée"                                = "8"
              ),
              fct_recode(
                actp,
                "non renseigné ou sans objet"           = "0",
                "Se déplaçant sens véhicule heurtant"   = "1",
                "Se déplaçant sens inverse du véhicule" = "2",
                "Traversant"                            = "3" ,
                "Masqué"                                = "4",
                "Jouant – courant"                      = "5",
                "Avec animal"                           = "6",
                "Autre"                                 = "9"
              ),
              fct_recode(
                etatp,
                "Seul"       = "1" ,
                "Accompagné" = "2",
                "En groupe"  = "3"
              )
            )]


# Chargement vehicules ---------------------------------------------------

fichiers_vehicules        <-
  liste_fichiers[grep("vehicules", liste_fichiers)]

allfiles_vehicules <-
  lapply(
    X   = fichiers_vehicules,
    FUN = function(x) {
      fread(file = paste0("data/", x))
    }
  )
vehicules <- rbindlist(allfiles_vehicules)

# Renommage des variables
str(vehicules)



vehicules[, c("senc",
              "catv",
              "obs",
              "obsm",
              "choc",
              "manv") := lapply(.SD, function(x)
                as.factor(as.character(x))), .SDcols =
            c("senc",
              "catv",
              "obs",
              "obsm",
              "choc",
              "manv")]

vehicules[, c("senc",
              "catv",
              "obs",
              "obsm",
              "choc",
              "manv") := .(
                fct_recode(senc,
                           "croissant"   = "1",
                           "décroissant" = "2"),
                fct_recode(
                  catv,
                  "Bicyclette"                                         = "1",
                  "Cyclomoteur <50cm3"                                 = "2",
                  "Voiturette"                                         = "3",
                  "2006 (scooter immatriculé)"                         = "4",
                  "2006 (motocyclette)"                                = "5",
                  "2006 (side-car)"                                    = "6",
                  "VL seul"                                            = "7",
                  "Catégorie plus utilisée (VL + caravane)"            = "8",
                  "Catégorie plus utilisée (VL + remorque)"            = "9",
                  "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque" = "10",
                  "2006 (VU (10) + caravane)"                          = "11",
                  "2006 (VU (10) + remorque)"                          = "12",
                  "PL seul 3,5T <PTCA <= 7,5T"                         = "13",
                  "PL seul > 7,5T"                                     = "14",
                  "PL > 3,5T + remorque"                               = "15",
                  "Tracteur routier seul"                              = "16",
                  "Tracteur routier + semi-remorque"                   = "17",
                  "2006 (transport en commun)"                         = "18",
                  "2006 (tramway)"                                     = "19",
                  "Engin spécial"                                      = "20",
                  "Tracteur agricole"                                  = "21",
                  "Scooter < 50 cm3"                                   = "30",
                  "Motocyclette > 50 cm3   et <= 125 cm3"              = "31",
                  "Scooter  > 50 cm3  et <= 125 cm3"                   = "32",
                  "Motocyclette > 125 cm3"                             = "33",
                  "Scooter  > 125 cm3"                                 = "34",
                  "Quad léger  <= 50 cm3"                              = "35",
                  "Quad lourd > 50 cm3"                                = "36",
                  "Autobus"                                            = "37",
                  "Autocar"                                            = "38",
                  "Train"                                              = "39",
                  "Tramway"                                            = "40",
                  "Autre véhicule"                                     = "99"
                ),
                fct_recode(
                  obs,
                  "Véhicule en stationnement"                                     = "1",
                  "Arbre"                                                         = "2",
                  "Glissière métallique"                                          = "3",
                  "Glissière béton"                                               = "4",
                  "Autre glissière"                                               = "5",
                  "Bâtiment, mur, pile de pont"                                   = "6",
                  "Support de signalisation verticale ou poste d’appel d’urgence" = "7",
                  "Poteau"                                                        = "8",
                  "Mobilier urbain"                                               = "9",
                  "Parapet"                                                       = "10",
                  "Ilot, refuge, borne haute"                                     = "11",
                  "Bordure de trottoir"                                           = "12",
                  "Fossé, talus, paroi rocheuse"                                  = "13",
                  "Autre obstacle fixe sur chaussée"                              = "14",
                  "Autre obstacle fixe sur trottoir ou accotement"                = "15",
                  "Sortie de chaussée sans obstacle"                              = "16"
                ),
                fct_recode(
                  obsm,
                  "Piéton"            = "1",
                  "Véhicule"          = "2",
                  "Véhicule sur rail" = "4",
                  "Animal domestique" = "5",
                  "Animal sauvage"    = "6",
                  "Autre"             = "9"
                ),
                fct_recode(
                  choc,
                  "Avant"                      = "1",
                  "Avant droit"                = "2",
                  "Avant gauche"               = "3",
                  "Arrière"                    = "4",
                  "Arrière droit"              = "5",
                  "Arrière gauche"             = "6",
                  "Côté droit"                 = "7",
                  "Côté gauche"                = "8",
                  "Chocs multiples (tonneaux)" = "9"
                ),
                fct_recode(
                  manv,
                  "Sans changement de direction"              = "1",
                  "Même sens, même file"                      = "2",
                  "Entre 2 files"                             = "3" ,
                  "En marche arrière"                         = "4",
                  "A contresens"                              = "5",
                  "En franchissant le terre-plein central"    = "6",
                  "Dans le couloir bus, dans le même sens"    = "7",
                  "Dans le couloir bus, dans le sens inverse" = "8",
                  "En s’insérant"                             = "9",
                  "En faisant demi-tour sur la chaussée"      = "10",
                  "Changeant de file à gauche"                = "11",
                  "Changeant de file à droite"                = "12",
                  "Déporté à gauche"                          = "13",
                  "Déporté à droite"                          = "14",
                  "Tournant à gauche"                         = "15",
                  "Tournant à droite"                         = "16",
                  "Dépassant à gauche"                        = "17",
                  "Dépassant à droite"                        = "18",
                  "Traversant la chaussée"                    = "19",
                  "Manœuvre de stationnement"                 = "20",
                  "Manœuvre d’évitement"                      = "21",
                  "Ouverture de porte"                        = "22",
                  "Arrêté (hors stationnement)"               = "23",
                  "En stationnement (avec occupants)"         = "24"
                )
              )]


# Sauvegarde --------------------------------------------------------------

saveRDS(object = caracteristiques,
        file = "data/caracteristiques.RDS")

saveRDS(object = lieux,
        file = "data/lieux.RDS")

saveRDS(object = usagers,
        file = "data/usagers.RDS")

saveRDS(object = vehicules,
        file = "data/vehicules.RDS")
