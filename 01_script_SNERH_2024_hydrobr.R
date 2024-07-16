require(hydrobr)
require(dplyr)
require(sf)

#1. área de estudo----

#1.1.download subbacia PNRH nivel 1 ----
sub = downloadSubBasinsPNRH(subBasinLevel = 1, outputDir = "./dados")


#1.2. selecionar subbacias que tem mais que 10% da área do PB----
baciaPB = sf::st_intersection(sub,
                              geobr::read_state(code_state = "PB") %>%
                                st_transform(4326)) %>%
  st_transform("ESRI:102033") %>%
  dplyr::mutate(area_km2 = sf::st_area(.)/10000/100,
                porc = as.numeric(area_km2/sum(area_km2)*100))

baciaPB = baciaPB %>%
  dplyr::filter(porc >=10); baciaPB

sum(baciaPB$porc)

nomes =  baciaPB%>%
  pull(PS1_NM)

#area de estudo final

areaEstudo = sub %>%
  filter(PS1_NM %in% nomes) %>%
  sf::st_combine() %>%
  sf::st_as_sf()

plot(areaEstudo)


#exportar

sf::st_write(areaEstudo, "./resultados/bacias_paraiba.shp", delete_layer = T)


#3 Estações fluviométricas----

##3.1inventario----

tempInv = Sys.time()

inv = inventory(stationType = "flu",
                as_sf = T,
                aoi = areaEstudo)

tempInvF = Sys.time() - tempInv; tempInvF

sf::write_sf(inv, "./resultados/invFlu.shp")


## 3.2download stationData----

tempData = Sys.time()

sData = stationsData(inv, deleteNAstations = T)

tempDataF = Sys.time() - tempData; tempDataF

sf::write_sf(inv %>%
               dplyr::filter(station_code %in% names(sData)), "./resultados/invFluComDados.shp", delete_layer = T)

sData$'37080000' %>%
  openxlsx::write.xlsx("./resultados/dadosHidroweb37080000.xlsx")

##3.3 organize----

tempDataO = Sys.time()

sDataO = organize(sData)

tempDataOF = Sys.time() - tempDataO; tempDataOF

sDataO$'37080000' %>%
  openxlsx::write.xlsx("./resultados/dadosHydrobr37080000.xlsx")

##3.4 selectStatioms----

tempDataSC = Sys.time()

estSelec = selectStations(organizeResult = sDataO,
                          mode = "yearly",
                          maxMissing = 5,
                          minYears = 30,
                          month = 1,
                          iniYear = 1980,
                          consistedOnly = F)

tempDataSCF = Sys.time() - tempDataSC; tempDataSCF


sf::write_sf(inv %>%
               dplyr::filter(station_code %in% names(estSelec$series)), "./resultados/invFluComDadosSelecionadas.shp")


estSelec$failureMatrix %>%
  xlsx::write.xlsx("./resultados/flu_matrix_falhas.xlsx")

estSelec$missingMatrix %>%
  openxlsx::write.xlsx("./resultados/flu_matrix_missing.xlsx")

estSelec$series[1] %>%
  openxlsx::write.xlsx("./resultados/flu_estacao1.xlsx")

#4 Estações pluviométricas----

##4.1inventario----

tempInvP = Sys.time()

invP = inventory(stationType = "plu",
                 as_sf = T,
                 aoi = baciaPB %>%
                   sf::st_combine() %>%
                   sf::st_as_sf())

tempInvPF = Sys.time() - tempInvP; tempInvPF


## 4.2 download stationData----

tempDataP = Sys.time()

sDataP = stationsData(invP, deleteNAstations = T)

tempDataPF = Sys.time() - tempDataP; tempDataPF

sData$'37080000' %>%
  openxlsx::write.xlsx("./resultados/dadosHidroweb37080000.xlsx")
##4.3 organize----

tempDataO = Sys.time()

sDataO = organize(sData)

tempDataOF = Sys.time() - tempDataO; tempDataOF

sDataO$'37080000' %>%
  openxlsx::write.xlsx("./resultados/dadosHydrobr37080000.xlsx")

##4.4 selectStatioms----

tempDataSC = Sys.time()

estSelec = selectStations(organizeResult = sDataO,
                          mode = "yearly",
                          maxMissing = 5,
                          minYears = 30,
                          month = 1,
                          iniYear = 1980,
                          consistedOnly = T)

tempDataSCF = Sys.time() - tempDataSC; tempDataSCF


estSelec$failureMatrix %>%
  xlsx::write.xlsx("./resultados/flu_matrix_falhas.xlsx")

estSelec$missingMatrix %>%
  openxlsx::write.xlsx("./resultados/flu_matrix_missing.xlsx")

estSelec$series[1] %>%
  openxlsx::write.xlsx("./resultados/flu_estacao1.xlsx")
