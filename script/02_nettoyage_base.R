

# Chargement des packages --------------------------------------------------
library(data.table)
library(dplyr)
library(ggmap)
library(stringr)


# 0- Chargement des donnees -------------------------------------------------

caracteristiques <- readRDS("data/caracteristiques.RDS") %>% as.data.table()
lieux            <- readRDS("data/lieux.RDS")            %>% as.data.table()
usagers          <- readRDS("data/usagers.RDS")          %>% as.data.table()
vehicules        <- readRDS("data/vehicules.RDS")        %>% as.data.table()


# 1 - Contexte : caracteristique et lieu ----------------------------------------

# Jointure des tables
contexte <- merge(x = caracteristiques,
                  y = lieux,
                  by = "Num_Acc")

# Format datetime
contexte[, an   := as.character(2000 + an)]
contexte[, mois := formatC(mois, width = 2, format = "d", flag = "0")]
contexte[, jour := formatC(jour, width = 2, format = "d", flag = "0")]
contexte[, hrmn := formatC(hrmn, width = 4, format = "d", flag = "0")]
contexte[, date := paste(an, mois, jour, sep = "-")]
contexte[, datetime := as.POSIXct(paste(date, hrmn),
                                  format = "%Y-%m-%d %H%M",
                                  tz = "UTC")]

#1- Localisation GPS --------------------------------------------------------

# remise en format GPS ----------------------------------------------------#
contexte[, lat  := lat  / 100000]
contexte[, long := long / 100000]

# suppression des coordonnées de "Null Island"
contexte[lat == 0 & long == 0, c("lat", "long") := NA]

# coordonnées Métropole (C, P ou M) contenues dans un carré
contexte[gps %in% c("M", "C", "P") &
           !(long > -7 & long < 10 &
             lat  > 41 & lat  < 52)
         , c("lat", "long") := NA]


nrow(contexte[!is.na(lat) | !is.na(long)])
nrow(unique(contexte[!is.na(lat) | !is.na(long), .(lat, long)]))

# 1.1 code commune INSEE-------------------------------------------------------

contexte[, dep := formatC(dep, width = 3, format = "d", flag = "0")]
contexte[, com := formatC(com, width = 3, format = "d", flag = "0")]
contexte[, Code_INSEE := paste0(substr(dep, 1, 2), com)]

# https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/table/?flg=fr
code_commune_INSEE <- fread("data/correspondance-code-insee-code-postal.csv",
                            encoding = "UTF-8")  
colnames(code_commune_INSEE) <- gsub(pattern = " ", replacement = "_", x = colnames(code_commune_INSEE))
colnames(code_commune_INSEE) <- gsub(pattern = "é", replacement = "e", x = colnames(code_commune_INSEE))

contexte <- merge(x = contexte,
                  y = code_commune_INSEE[, .(Code_INSEE, Code_Postal, Commune, Departement)],
                  by = "Code_INSEE", 
                  all.x = TRUE)


saveRDS(object = contexte,
        file = "data/contexte.RDS")

# 1.2. Concordance des codes GPS ------------------------------------------

# P = Paris
# C = Couronne
# M = Metropole

# 1.2 reconstitution des adresses --------------------------------------------- 
adresses <- contexte[agg == "En agglomération", .(adr,
                                                  Code_Postal,
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





# adr renseignée que pour les accidents en agglomération
nrow(contexte[agg == "En agglomération" & is.na(lat) & is.na(long)])/nrow(contexte[agg == "En agglomération"]) # peu d'accident sans localisation
# 65% d'adresses à reconstituer

contexte[agg == "En agglomération", adresse := paste(adr,",", Code_Postal, Commune)]
# /!\ adresse tronquée

adresses_recupes <- unique(contexte[agg == "En agglomération" &
                                      is.na(lat) & is.na(long)]$adresse)

# recup_gg_adresses <-
#   sapply( 
#     X = adresses_recupes,
#     FUN = function(x) {
#       geocode(x, output = "latlona", source = "google")
#     }
#   )


# Ne fonctionne pas pour l'instant
# # catr, voie pr et pr1 pour le calcul des coordonnées hors agglomération
# contexte[agg == "Hors agglomération" &
#            catr == "Autoroute", adresse := paste0("A", voie, v2, " ", Code_Postal)]
# contexte[agg == "Hors agglomération" &
#            catr == "Route Nationale", adresse := paste0("RN", voie, v2, " ", Code_Postal)]
# contexte[agg == "Hors agglomération" &
#            catr == "Route Départementale", adresse := paste0("D", voie, v2, " ", Code_Postal)]
# contexte[agg == "Hors agglomération" &
#            catr == "Voie Communale", adresse := paste(catr, voie, v2, Code_Postal)]



# Attention c'est payant, 300 crédits googles
#geocode(contexte[agg == "En agglomération" & is.na(lat) & is.na(long)]$adresse[1], output = "latlona", source = "google", client = google_API)
# # origAddress data frame in new columns lat and lon
# for(i in 1:nrow(origAddress))
# {
#   # Print("Working...")
#   result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
#   origAddress$lon[i] <- as.numeric(result[1])
#   origAddress$lat[i] <- as.numeric(result[2])
#   origAddress$geoAddress[i] <- as.character(result[3])
# }
