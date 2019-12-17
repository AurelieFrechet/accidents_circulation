# Chargement des packages --------------------------------------------------
library(data.table)
library(dplyr)

# 0- Chargement des donnees -------------------------------------------------

contexte <- readRDS("data/contexte.RDS") %>% as.data.table()


# 1 - Récriture des départements et communes ---------------------------------
contexte[, dep := formatC(dep, width = 3, format = "d", flag = "0")]
contexte[, com := formatC(com, width = 3, format = "d", flag = "0")]

# Paris
contexte[dep == "750", com := paste0("1", substr(com, 2, 3))]

# Corse
contexte[dep == "201", dep:= "2A0"]
contexte[dep == "201", dep:= "2B0"]

# Outre-Mer
contexte[dep == "971", com := paste0("1", substr(com, 2, 3))]
contexte[dep == "972", com := paste0("2", substr(com, 2, 3))]
contexte[dep == "973", com := paste0("3", substr(com, 2, 3))]
contexte[dep == "974", com := paste0("4", substr(com, 2, 3))]
contexte[dep == "975", com := paste0("5", substr(com, 2, 3))]
contexte[dep == "976", com := paste0("6", substr(com, 2, 3))]


# 2 - Jointure avec le code INSEE -----------------------------------------
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

sum(is.na(contexte$Code_Postal.y))
