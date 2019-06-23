

# Chargement des packages --------------------------------------------------
library(data.table)
library(ggmap)


# Chargement des donnees -------------------------------------------------

caracteristiques <- readRDS("data/caracteristiques.RDS") %>% as.data.table()
lieux            <- readRDS("data/lieux.RDS")            %>% as.data.table()
usagers          <- readRDS("data/usagers.RDS")          %>% as.data.table()
vehicules        <- readRDS("data/vehicules.RDS")        %>% as.data.table()


# Contexte : caracteristique et lieu ----------------------------------------

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

# Localisation GPS

# code commune INSEE
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

# Adresse renseignée que pour les accidents en agglomération

nrow(contexte[agg == "En agglomération" & is.na(lat) & is.na(long)])/nrow(contexte[agg == "En agglomération"]) # peu d'accident sans localisation
# 65% d'adresses à reconstituer

contexte[, adresse := paste(adr,",", Code_Postal, Commune)]
# /!\ adresse tronquée
length(contexte[is.na(lat) & is.na(long)]$adresse)
length(contexte[!(is.na(lat) & is.na(long))]$adresse)


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
