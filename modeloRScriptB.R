#library(shiny)
library(reticulate)
#library(shinythemes)
library(geosphere)
library(tidyverse)
library(readxl)
library(leaflet)
library(leaflet.extras)
#library(leaflet.extras2)
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
#require(geosphere)
require(measurements)
require(maps)
library(ggplot2)
library(paletteer)


#setwd("D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/humanitariaLogistics/asignacionLogisticaHumanitariaDemandaAplicacionEnLinea")
setwd('D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/humanitariaLogistics/powerBi/modeloReticulate')
#demandaGrupo1 <- read_exce
#cantidad_centros = 20
demandaGrupo2 <- read_excel("demandaGrupoAsignacion3.xlsx",sheet = "Hoja1")
demandaGrupo2 = demandaGrupo2[1:140,]
dataLocales<-read_excel("geolocalizacionAsignacion7Centros.xlsx",sheet = "GeolocalizacionNuevo")
#dataLocales = dataLocales[1:cantidad_centros,]
valorDemanda<-demandaGrupo2$requerimiento
#create a distance matrix between the demand points and DCs
customer_dc_distmat <- geosphere::distm(
  x=cbind(demandaGrupo2$Longitud,demandaGrupo2$Latitud),
  y=cbind(dataLocales$lng,dataLocales$lat)) %>% 
  #convert from meters (default) to kilometers
  measurements::conv_unit('m','km')
row.names(customer_dc_distmat) = paste0('Customer_',demandaGrupo2$Address)
colnames(customer_dc_distmat) = paste0('DC_',dataLocales$Address)

#create a matrix that is the metric you wish to minimize or maximize.
#in this example, the metric is unit-miles
#just multiply the demand by city, into the distance from that city to each DC
unitmiles_customer_dc_matrix <- 
  valorDemanda * customer_dc_distmat

customer_count <- nrow(demandaGrupo2)
dc_option_count <- nrow(dataLocales)
cantidad_centros<-3
#now make optimization model
dc_location_model <- ompr::MIPModel() %>%
  #binary decision variables: for each customer, which DC to align to?  Yes/no decisions, align Customer A to DC B yes, or no?
  add_variable(customer_dc_align[customerindex,dcindex],
               customerindex=1:customer_count,
               dcindex=1:dc_option_count,type='binary') %>%
  #binary decision variable: open a DC or no?
  add_variable(open_dc_binary[dcindex],dcindex=1:dc_option_count,type='binary') %>%
  #first constraint: each customer aligned to 1 and only 1 DC
  add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                          dcindex=1:dc_option_count)==1,
                 customerindex=1:customer_count) %>%
  #add in "Big M" constraints that activate open_dc_binary when
  #any customers are aligned to a DC
  add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                          customerindex=1:customer_count)<=
                   99999*open_dc_binary[dcindex],dcindex=1:dc_option_count) %>%
  
  #limit the number of opened DCs to EXACTLY 2
  add_constraint(sum_expr(open_dc_binary[dcindex],dcindex=1:dc_option_count)==cantidad_centros) %>%
  #set objective function, the sumproduct
  #of the customer/DC alignment integer variables,
  #and the matrix of the unit-miles for each customer/DC pair
  #sense is either "min" or "max", minimize or maximize the values?
  set_objective(sum_expr(customer_dc_align[customerindex,dcindex]*
                           unitmiles_customer_dc_matrix[customerindex,dcindex],
                         customerindex=1:customer_count,
                         dcindex=1:dc_option_count),sense='min')

solution <- ompr::solve_model(dc_location_model,with_ROI(solver = "glpk"))

customer_dc_alignment_df <- get_solution(solution,customer_dc_align[customerindex,dcindex]) 

customer_dc_alignment_df <- get_solution(solution,customer_dc_align[customerindex,dcindex]) %>%
  dplyr::filter(value==1) %>%
  dplyr::select(customerindex,dcindex) %>%
  #add in customer and DC names and lat/long
  dplyr::mutate(Customer_City = demandaGrupo2$Address[customerindex],
                Customer_Lat = demandaGrupo2$Latitud[customerindex],
                Customer_Lng = demandaGrupo2$Longitud[customerindex],
                DC_City = dataLocales$Address[dcindex],
                DC_Lat = dataLocales$lat[dcindex],
                DC_Lng = dataLocales$lng[dcindex]) %>%
  dplyr::select(Customer_City,Customer_Lat,Customer_Lng,
                DC_City,DC_Lat,DC_Lng)

#verify each Customer City is only present once - meaning that it's only aligned to a single DC
#table(customer_dc_alignment_df$Customer_City)

#verify only two DCs selected, should be Dallas and Houston
dc_cities_selected <- unique(customer_dc_alignment_df$DC_City)
dataLocales2<-dataLocales %>% filter(dataLocales$Address%in%dc_cities_selected )

######################################
use_virtualenv("optimization")
source_python("D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/humanitariaLogistics/powerBi/modeloReticulate/NcentrosNsectoresExportarPBI.py")
#source_python("modeloInventarioCompartido.py")
#crea_demanda(20L,4L)
kitsNecesarios<-py$crea_demanda(20L,4L)

#### este paso es clave para que aparezca en Power Bi
datos<-as.data.frame(kitsNecesarios)




  