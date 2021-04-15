#!/bin/Rscript
#
# Web scraping, downloading XLS datasets and uploading CSV file
#
# Author: alu0101371061@ull.edu.es
#

# Install packages
install.packages("tidyverse")
install.packages("readxl")
#install.packages("googledrive")

# Load libraries
library(tidyverse)
library(rvest)
library(readxl)
#library(googledrive)


# Web Scraping

nodes_mercancias <- read_html("http://risp.puertosdetenerife.org/dataset/trafico-de-mercancias") %>% html_nodes(".resource-url-analytics")
xls_mercancias <- nodes_mercancias %>% html_attr("href")
rm(nodes_mercancias)


# Prepare data

options(width = 230, height = 40)
full_mercancias = data.frame()
for (xls in xls_mercancias) {
  download.file(xls, "localfile.xls", quiet = TRUE)
  df <- read_excel("localfile.xls")
  full_mercancias <- rbind(full_mercancias, df)
}
full_mercancias$Mes <- case_when (
  full_mercancias$Mes == "ene" ~ "01",
  full_mercancias$Mes == "feb" ~ "02",
  full_mercancias$Mes == "mar" ~ "03",
  full_mercancias$Mes == "abr" ~ "04",
  full_mercancias$Mes == "may" ~ "05",
  full_mercancias$Mes == "jun" ~ "06",
  full_mercancias$Mes == "jul" ~ "07",
  full_mercancias$Mes == "ago" ~ "08",
  full_mercancias$Mes == "sep" ~ "09",
  full_mercancias$Mes == "oct" ~ "10",
  full_mercancias$Mes == "nov" ~ "11",
  full_mercancias$Mes == "dic" ~ "12"
)


# Traffic by type

uni_1_mercancias <- full_mercancias %>%
  unite("Mes", c("Año", "Mes"), sep = "-", remove = TRUE) %>%
  unite("Tipo", c("Tipo de Territorio", "Operación"), sep = "-", remove = TRUE)
sel_1_mercancias <- uni_1_mercancias %>%
  select(Mes, Puerto, Tipo, Toneladas) %>%
  group_by(Mes, Puerto, Tipo) %>%
  summarise(Sumatorio = sum(Toneladas), .groups = "drop")
rm(uni_1_mercancias)
sel_1_mercancias$Tipo <- gsub("Unión Europea", "UE", sel_1_mercancias$Tipo)
sel_1_mercancias$Tipo <- gsub(" ", "_", sel_1_mercancias$Tipo)
mercancias_1 <- sel_1_mercancias %>% pivot_wider(names_from = Tipo, values_from = Sumatorio, values_fill = 0)
rm(sel_1_mercancias)
mercancias_1[,"Resto-Cargadas"] <- rowSums(mercancias_1[,c("Resto_Territorios-Cargadas", "Resto_TMCD-Cargadas")])
mercancias_1[,c("Resto_Territorios-Cargadas", "Resto_TMCD-Cargadas")] <- NULL
mercancias_1[,"Resto-Descargadas"] <- rowSums(mercancias_1[,c("Resto_Territorios-Descargadas", "Resto_TMCD-Descargadas")])
mercancias_1[,c("Resto_Territorios-Descargadas", "Resto_TMCD-Descargadas")] <- NULL
mercancias_1[,"Resto-Tránsito"] <- rowSums(mercancias_1[,c("Resto_Territorios-Tránsito", "Resto_TMCD-Tránsito")])
mercancias_1[,c("Resto_Territorios-Tránsito", "Resto_TMCD-Tránsito")] <- NULL
mercancias_1 <- mercancias_1 %>% rename("Resto-Transbordos" = "Resto_Territorios-Transbordos")
mercancias_1 <- mercancias_1[,c("Mes","Puerto",
                                "Canarias-Cargadas", "Canarias-Descargadas", "Canarias-Tránsito",
                                "Nacional-Cargadas", "Nacional-Descargadas", "Nacional-Tránsito",
                                "UE-Cargadas", "UE-Descargadas", "UE-Tránsito", "UE-Transbordos",
                                "Resto-Cargadas", "Resto-Descargadas", "Resto-Tránsito", "Resto-Transbordos")]
str(mercancias_1)
mercancias_1_SPC <- mercancias_1 %>% filter(Puerto == "S/C DE LA PALMA")
mercancias_1_SPC$Puerto <- NULL
mercancias_1[1:20,]
mercancias_1_SPC[1:20,]


# Traffic by scope

uni_2_mercancias <- full_mercancias %>%
  unite("Tipo", c("Tipo de Territorio", "Operación"), sep = "-", remove = TRUE)
sel_2_mercancias <- uni_2_mercancias %>%
  select(Año, Puerto, Tipo, Toneladas) %>%
  group_by(Año, Puerto, Tipo) %>%
  summarise(Sumatorio = sum(Toneladas), .groups = "drop")
rm(uni_2_mercancias)
sel_2_mercancias <- sel_2_mercancias %>% filter(Año != 2021)
sel_2_mercancias$Tipo <- gsub("Unión Europea", "UE", sel_2_mercancias$Tipo)
sel_2_mercancias$Tipo <- gsub(" ", "_", sel_2_mercancias$Tipo)
sel_2_mercancias$Puerto <- gsub("LA ESTACA \\(EL HIERRO\\)", "HIE", sel_2_mercancias$Puerto)
sel_2_mercancias$Puerto <- gsub("LOS CRISTIANOS", "CRI", sel_2_mercancias$Puerto)
sel_2_mercancias$Puerto <- gsub("S/C DE LA PALMA", "SPC", sel_2_mercancias$Puerto)
sel_2_mercancias$Puerto <- gsub("S/C DE TENERIFE", "SCT", sel_2_mercancias$Puerto)
sel_2_mercancias$Puerto <- gsub("S/S DE LA GOMERA", "GOM", sel_2_mercancias$Puerto)
sel_2_mercancias$Año <- as.character(sel_2_mercancias$Año)
uni_2_mercancias <- sel_2_mercancias %>%
  unite("A_P", c("Año", "Puerto"), sep = "-", remove = TRUE)
rm(sel_2_mercancias)
mercancias_2 <- uni_2_mercancias %>% pivot_wider(names_from = Tipo, values_from = Sumatorio, values_fill = 0)
rm(uni_2_mercancias)
mercancias_2[,"Resto-Cargadas"] <- rowSums(mercancias_2[,c("Resto_Territorios-Cargadas", "Resto_TMCD-Cargadas")])
mercancias_2[,c("Resto_Territorios-Cargadas", "Resto_TMCD-Cargadas")] <- NULL
mercancias_2[,"Resto-Descargadas"] <- rowSums(mercancias_2[,c("Resto_Territorios-Descargadas", "Resto_TMCD-Descargadas")])
mercancias_2[,c("Resto_Territorios-Descargadas", "Resto_TMCD-Descargadas")] <- NULL
mercancias_2[,"Resto-Tránsito"] <- rowSums(mercancias_2[,c("Resto_Territorios-Tránsito", "Resto_TMCD-Tránsito")])
mercancias_2[,c("Resto_Territorios-Tránsito", "Resto_TMCD-Tránsito")] <- NULL
mercancias_2 <- mercancias_2 %>% rename("Resto-Transbordos" = "Resto_Territorios-Transbordos")
mercancias_2 <- mercancias_2[,c("A_P",
                                "Canarias-Cargadas", "Canarias-Descargadas", "Canarias-Tránsito",
                                "Nacional-Cargadas", "Nacional-Descargadas", "Nacional-Tránsito",
                                "UE-Cargadas", "UE-Descargadas", "UE-Tránsito", "UE-Transbordos",
                                "Resto-Cargadas", "Resto-Descargadas", "Resto-Tránsito", "Resto-Transbordos")]
str(mercancias_2)
mercancias_2[1:20,]


# Traffic by ports stream

#uni_3_mercancias <- full_mercancias %>%
#   unite("Origen", c("País Origen", "Puerto Origen"), sep = " - ", remove = TRUE) %>%
#   unite("Destino", c("Pais Destino", "Puerto Destino"), sep = " - ", remove = TRUE)
#sel_3_mercancias <- uni_3_mercancias %>%
#   select(Origen, Destino, Toneladas)  %>%
#   group_by(Origen, Destino) %>%
#   summarise(Sumatorio = sum(Toneladas), .groups = "drop")
uni_3_mercancias <- full_mercancias %>%
  rename(
          PuertoO = "Puerto Origen",
          PuertoD = "Puerto Destino",
          Scope = "Tipo de Territorio")
uni_3_mercancias <- uni_3_mercancias %>% filter(Scope == "Canarias")
sel_3_mercancias <- uni_3_mercancias %>%
  select(PuertoO, PuertoD, Toneladas) %>%
  group_by(PuertoO, PuertoD) %>%
  summarise(Sumatorio = sum(Toneladas), .groups = "drop")
rm(uni_3_mercancias)
mercancias_3 <- sel_3_mercancias %>%
  rename(
          Origen = PuertoO,
          Destino = PuertoD,
          Toneladas = Sumatorio)
rm(sel_3_mercancias)
str(mercancias_3)
mercancias_3[]


# Export to CSV

write.csv(mercancias_1, "../../MU_CID_3B_Vis/mercancias_1.csv", row.names = FALSE)
write.csv(mercancias_1_SPC, "../../MU_CID_3B_Vis/mercancias_1_SPC.csv", row.names = FALSE)
write.csv(mercancias_2, "../../MU_CID_3B_Vis/mercancias_2.csv", row.names = FALSE)
write.csv(mercancias_3, "../../MU_CID_3B_Vis/mercancias_3.csv", row.names = FALSE)

# Remove the dataframe objects

rm(full_mercancias)
rm(mercancias_1)
rm(mercancias_1_SPC)
rm(mercancias_2)
rm(mercancias_3)


# Log into G Drive and upload the CSV file

#drive_upload(media = "mercancias.csv",
#   path = "~/Colab Notebooks/Vis_2021/mercancias.csv",
#   overwrite = TRUE)
