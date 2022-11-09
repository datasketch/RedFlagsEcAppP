# install.packages("tidyverse")
# install.packages("usethis", dependencies = TRUE)
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("knitr")
 install.packages("archive")


library(tidyverse)
library(archive)
library(dplyr)
#library(rgdal)
options(timeout=100000)
download.file("http://corporatetrails.com/ec/flagfetti01.tar.gz", destfile="flagfetti01.tar.gz")
download.file("http://corporatetrails.com/ec/flagfetti00.tar.gz", destfile="flagfetti00.tar.gz")
# download.file("https://corporatetrails.com/ec/contractFlagsJson.tar.gz", destfile="contractFlagsJson.tar.gz")
# download.file("https://corporatetrails.com/ec/contractFlags.tar.gz", destfile="contractFlags.tar.gz")

results <- read_csv(archive_read("flagfetti00.tar.gz"), quote = "'")
summaries <- read.csv(archive_read("flagfetti01.tar.gz"), skip = 1, quote = "'",sep = ",")
# temp= read.csv("flagfetti01.csv",skip = 1,sep=",",quote = "'")
# write.csv(summaries,"flag01.csv")
colnames(summaries) = c("RUC","description.party","description.locality","description.region","description.role","category","categoryValue", "year")
colnames(results) = c("RUC","description.party","description.locality","description.region","description.role","flag","category","result", "year", "categoryValue")
nrow(summaries)
# summary(summaries$categoryValue)
# results1 = results   %>% filter(!is.na(categoryValue))
# results2 = results   %>% filter(is.na(categoryValue))
# results=results %>% filter(!is.na(categoryValue))

#mutate


# results2$result = round(as.numeric(results2$result)*100,digits=0)
# results=NULL
# results = cbind()


# 
# results1$categoryValue = round(as.numeric(results1$categoryValue)*100,digits=0)
# results2$result = round(as.numeric(results2$result)*100,digits=0)
# results=NULL
# results = cbind()


results = results %>% dplyr::mutate(category = dplyr::case_when(category== "trans" ~ "Transparencia",
                                                      category == "temp" ~ "Temporalidad",
                                                      category == "comp" ~ "Competitividad",
                                                      category == "traz" ~ "Trazabilidad",
                                                      category== "network" ~ "Confiabilidad",
                                                      category== "contract-count" ~ "Cantidad de contratos",
                                                      category== "total_score" ~ "total_score",
                                                      TRUE ~ category))




summaries = summaries %>% dplyr::mutate(category = dplyr::case_when(category== "trans" ~ "Transparencia",
                                                                    category == "temp" ~ "Temporalidad",
                                                                    category == "comp" ~ "Competitividad",
                                                                    category == "traz" ~ "Trazabilidad",
                                                                    category== "network" ~ "Confiabilidad",
                                                                    category== "contract-count" ~ "Cantidad de contratos",
                                                                    category== "total_score" ~ "total_score",
                                                                    TRUE ~ category))



results = results %>% dplyr::mutate(flag = dplyr::case_when(flag =="above-average-number-contracts" ~ "Número de contratos por encima del promedio",
                                                                flag =="contract-bidders-who-bid-and-dont-win" ~ "Oferentes perdedores perennes",
                                                                flag =="contract-bids-exact-percentage-apart" ~ "Ofertas con un porcentaje exacto de diferencia",
                                                                flag =="contract-companies-domiciled-tax-havens" ~ "Empresas domiciliadas en paraísos fiscales",
                                                                flag =="contract-amount-repeated" ~ "Monto del contrato es reiterativo",
                                                                flag =="duplicated-id" ~ "Número de identificación duplicado",
                                                                flag =="contract-duration-questioning-stage" ~ "Duración anormal de la etapa de preguntas",
                                                                flag =="contract-information-matches" ~ "Coincidencia en la información entre los oferentes",
                                                                flag =="contract-key-fields-temporality" ~ "Campos fundamentales para la temporalidad",
                                                                flag =="contract-key-fields-traceability"  ~ "Campos fundamentales para la trazabilidad",
                                                                flag =="contract-large-difference-between-award-value-final-contract" ~ "Diferencia entre el valor adjudicado y el valor del contrato",
                                                                flag =="contract-late-bidder-winning-bidder" ~ "Último oferente es el ganador",
                                                                flag =="network-above-threshold" | flag=="network" ~ "Análisis de red",
                                                                #flag =="contract-a-few-bidders-win-disproportionate-number-contracts-same-type" ~ "Varios contratos con un mismo oferente",
                                                                flag =="one-few-bidders-win-disproportionate-number-contracts-same-type" ~ "Varios contratos con un mismo oferente",
                                                                flag =="contract_count" ~ "Cantidad de contratos",
                                                                flag =="contract-planning-sections" ~ "Sección de planificación",
                                                                flag =="contract-presence-suspicious-dates" ~ "Fechas sospechosas",
                                                                flag =="contract-price-awarded-bidder-below-reference-budget"  ~ "El valor adjudicado está por debajo del presupuesto",
                                                                flag =="contract-short-awards-procedures" ~ "Tiempo de adjudicación es corto",
                                                                flag =="contract-supplier-address-not-fully-specified" ~ "Información del proveedor está incompleta",
                                                                flag =="contract-supplier-address-same-project-officials" ~ "Información de contacto coincidente",
                                                                flag =="contract-tender-single-bidder-only" ~ "Procedimiento con un solo oferente",
                                                                flag =="title-contract-repeated" ~ "El objeto contractual es reiterativo",
                                                                flag =="contract-unanswered-bidder-questions" ~ "Preguntas del oferente sin respuesta",
                                                                flag =="contract-understanding-title" ~ "El objeto contractual es comprensible",
                                                                flag =="contract-wide-disparity-bid-prices" ~ "Disparidad en los valores de las ofertas",
                                                                flag =="contract-winning-bid-round-numbers" ~ "Oferta ganadora es un número redondo",
                                                                TRUE  ~ flag
))


summary(summaries$categoryValue)
summaries = summaries %>% mutate(categoryValue = case_when(category!="Cantidad de contratos" ~ round(categoryValue*100,digits=0), TRUE ~categoryValue))
results %>% filter(category!="Cantidad de contratos" & year!="all" )  %>% filter(!is.na(categoryValue))
# class(results$categoryValue)
results$categoryValue = as.numeric(results$categoryValue)
results = results  %>% mutate(categoryValue = case_when( category!="Cantidad de contratos" ~  round((categoryValue)*100,digits=0), TRUE ~categoryValue))


# summaries$categoryValue = round(summaries$categoryValue*100,digits=0)
# results$categoryValue = round(as.numeric(results$categoryValue)*100,digits=0)                                               
                    
# summaries %>% distinct(category)
# results %>% distinct(flag)

# results = results %>% dplyr::mutate(category = dplyr::case_when(category== "trans" ~ "Transparencia",
#                                                                     category == "temp" ~ "Temporalidad",
#                                                                     category == "comp" ~ "Competencia",
#                                                                     category == "traz" ~ "Trazabilidad",
#                                                                     category== "network" ~ "Confiabilidad",
#                                                                     category== "total_score" ~ "total_score",
#                                                                     TRUE ~ category))


summaries = summaries %>% dplyr::mutate(description.role = case_when(
  
                                                              description.role == "supplier" ~ "Proveedor",
                                                              description.role == "supplier,tenderer" ~ "Proveedor",
                                                              description.role == "tenderer,supplier" ~ "Proveedor",
                                                              description.role == "buyer,procuringEntity" ~ "Entidad contratante",
                                                              description.role == "null" ~ "",
                                                              description.role == "buyer" ~ "Entidad contratante",
                                                              TRUE ~ description.role
                                                              ))


results = results %>% dplyr::mutate(description.role = case_when(
  
  description.role == "supplier" ~ "Proveedor",
  description.role == "supplier,tenderer" ~ "Proveedor",
  description.role == "tenderer,supplier" ~ "Proveedor",
  description.role == "buyer,procuringEntity" ~ "Entidad contratante",
  description.role == "null" ~ "",
  description.role == "buyer" ~ "Entidad contratante",
  TRUE ~ description.role
))

# colnames(summaries)
# b=summaries %>% head(2)
# b %>% select(category)
# summaries  %>% select(description.party)
# unlist(summaries$description.party)
# summaries %% select(category)
summaries = summaries %>% dplyr::select(description.party,RUC,description.role, category, categoryValue,year, description.locality,description.region)
results = results %>% dplyr::select(description.party,RUC,description.role,category, flag, categoryValue,year, description.locality,description.region)

# colnames(results) = c("RUC","description.party","description.locality","description.region","description.role","flag","category","result", "year", "categoryValue")

#write_csv(head(x2), "data-raw/red-flags.csv")
# a=summaries %>% select(year) %>% group_by(year) %>% unique()
# ecu=readOGR("data-raw/original/Ecuador_porprovincias/ecuador.shp")
# summaries$description.region
# temp= summaries %>% select(description.region) %>% group_by(description.region) %>% unique()
# temp =summaries %>% select(description.region, categoryValue) %>% group_by(description.region) %>%
#   summarise(categoryvalue= mean(categoryValue, na.rm=TRUE) , ds=sd(categoryValue))
# str(temp)
# class(temp$categoryValue)
# temp=data.frame(c("AZUAY","BOLIVAR"),c(0.4,0.5))
# lfltmagic::lflt_choropleth_GnmNum(temp,map_name="ecu_provinces", palette_colors = c("#ce4216", "#fdd783"))
# 
# b
sys_date =  Sys.Date()
usethis::use_data(results, summaries, sys_date, overwrite = TRUE)
# colnames(summaries)
# summaries %>% filter(category == "network")  %>% group_by(category) %>%  select(year) %>%  unique()
# results %>% head(10)

# library(hgchmagic)
# library(lfltmagic)  
# lfltmagic::lflt_choropleth_GnmNum(temp,map_name="ecu_provinces", pallete_colors = c("#ce4216", "#fdd783"))
# 
# 
# data2 <- sample_data("Gnm-Num", 100)
# lfltmagic::lflt_choropleth_GnmNum(temp,map_name="ecu_provinces", palette_colors = c("#ce4216", "#fdd783"))
# library(googlesheets4)
# lflt_
# generalInfo <- read_sheet("https://docs.google.com/spreadsheets/d/1zqipemy_QUw8K1AtLLHVpTfVe07gXbsMM1F7R3D52Cg/edit#gid=41825163", 1)
# usethis::use_data(generalInfo, overwrite = TRUE)
