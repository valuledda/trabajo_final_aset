#Preparación datos para tablero en Shiny

library(tidyverse)
library(eph)

base <- readRDS("eph_individual_2016_2025.rds")

#31 aglomerados

tasas_totalpais <- base %>% 
  group_by(ANO4) %>% 
  summarise(poblacion = sum(PONDERA),
            ocupados              = sum(PONDERA[ESTADO == 1]),
            desocupados           = sum(PONDERA[ESTADO == 2]),
            PEA                   = ocupados + desocupados,
            ocupados_demandantes  = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            subocup_demandante    = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            subocup_no_demandante = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            subocupados           = subocup_demandante + subocup_no_demandante,
            asalariados           = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3]),
            asal_sindescuento     = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2], na.rm = T),
            tasa_actividad                  = round(PEA/poblacion*100, 1),
            tasa_empleo                     = round(ocupados/poblacion*100, 1),
            tasa_desocupacion               = round(desocupados/PEA*100, 1),
            tasa_ocup_demandante            = round(ocupados_demandantes/PEA*100, 1),
            tasa_subocupacion               = round(subocupados/PEA*100, 1),
            tasa_subocup_demandante         = round(subocup_demandante/PEA*100, 1),
            tasa_subocup_no_demandante      = round(subocup_no_demandante/PEA*100, 1),
            tasa_asalarizacion              = round(asalariados/ocupados*100,1),
            tasa_no_registro_asal           = round(asal_sindescuento/asalariados*100,1)) %>% 
  select(1,12:20)


tasas_sexo <- base %>% 
  mutate(CH04 = case_when(CH04 == 1 ~ "Varones", 
                          CH04 == 2 ~ "Mujeres")) %>%
  mutate(CH04 = factor(CH04, levels = c("Mujeres", "Varones"))) %>% 
  filter(CH06 >= 14) %>%
  group_by(ANO4, CH04) %>% 
  summarise(poblacion = sum(PONDERA),
            ocupados              = sum(PONDERA[ESTADO == 1]),
            desocupados           = sum(PONDERA[ESTADO == 2]),
            PEA                   = ocupados + desocupados,
            ocupados_demandantes  = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            subocup_demandante    = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            subocup_no_demandante = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            subocupados           = subocup_demandante + subocup_no_demandante,
            asalariados           = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3]),
            asal_sindescuento     = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2], na.rm = T),
            tasa_actividad                  = round(PEA/poblacion*100, 1),
            tasa_empleo                     = round(ocupados/poblacion*100, 1),
            tasa_desocupacion               = round(desocupados/PEA*100, 1),
            tasa_ocup_demandante            = round(ocupados_demandantes/PEA*100, 1),
            tasa_subocupacion               = round(subocupados/PEA*100, 1),
            tasa_subocup_demandante         = round(subocup_demandante/PEA*100, 1),
            tasa_subocup_no_demandante      = round(subocup_no_demandante/PEA*100, 1),
            tasa_asalarizacion              = round(asalariados/ocupados*100,1),
            tasa_no_registro_asal           = round(asal_sindescuento/asalariados*100,1)) %>% 
  select(1,2, 13:21)


tasas_edad <- base %>%
  mutate(GRUPO_EDAD = case_when(CH06 >= 14 & CH06 <= 29 ~ "14 a 29 años",
                                CH06 >= 30 & CH06 <= 64 ~ "30 a 64 años")) %>% 
  filter(CH06 >= 14,
         !is.na(GRUPO_EDAD)) %>%
  group_by(ANO4, GRUPO_EDAD) %>% 
  summarise(poblacion = sum(PONDERA),
            ocupados              = sum(PONDERA[ESTADO == 1]),
            desocupados           = sum(PONDERA[ESTADO == 2]),
            PEA                   = ocupados + desocupados,
            ocupados_demandantes  = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            subocup_demandante    = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            subocup_no_demandante = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            subocupados           = subocup_demandante + subocup_no_demandante,
            asalariados           = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3]),
            asal_sindescuento     = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2], na.rm = T),
            tasa_actividad                  = round(PEA/poblacion*100, 1),
            tasa_empleo                     = round(ocupados/poblacion*100, 1),
            tasa_desocupacion               = round(desocupados/PEA*100, 1),
            tasa_ocup_demandante            = round(ocupados_demandantes/PEA*100, 1),
            tasa_subocupacion               = round(subocupados/PEA*100, 1),
            tasa_subocup_demandante         = round(subocup_demandante/PEA*100, 1),
            tasa_subocup_no_demandante      = round(subocup_no_demandante/PEA*100, 1),
            tasa_asalarizacion              = round(asalariados/ocupados*100,1),
            tasa_no_registro_asal           = round(asal_sindescuento/asalariados*100,1)) %>% 
  select(1,2, 13:21) 


tasas_totalpais <- tasas_totalpais %>% 
  mutate(dominio = "31 aglomerados urbanos",
         desagregacion = "Total",
         categoria = "Total")

tasas_sexo <- tasas_sexo %>% 
  mutate(dominio = "31 aglomerados urbanos",
         desagregacion = "Sexo",
         categoria = CH04) %>% 
  select(-CH04)

tasas_edad <- tasas_edad %>% 
  mutate(dominio = "31 aglomerados urbanos",
         desagregacion = "Edad",
         categoria = GRUPO_EDAD) %>% 
  select(-GRUPO_EDAD)



#Gran Mendoza


tasas_mza <- base %>% 
  filter(AGLOMERADO == 10) %>%  
  group_by(ANO4) %>% 
  summarise(poblacion = sum(PONDERA),
            ocupados              = sum(PONDERA[ESTADO == 1]),
            desocupados           = sum(PONDERA[ESTADO == 2]),
            PEA                   = ocupados + desocupados,
            ocupados_demandantes  = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            subocup_demandante    = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            subocup_no_demandante = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            subocupados           = subocup_demandante + subocup_no_demandante,
            asalariados           = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3]),
            asal_sindescuento     = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2], na.rm = T),
            tasa_actividad                  = round(PEA/poblacion*100, 1),
            tasa_empleo                     = round(ocupados/poblacion*100, 1),
            tasa_desocupacion               = round(desocupados/PEA*100, 1),
            tasa_ocup_demandante            = round(ocupados_demandantes/PEA*100, 1),
            tasa_subocupacion               = round(subocupados/PEA*100, 1),
            tasa_subocup_demandante         = round(subocup_demandante/PEA*100, 1),
            tasa_subocup_no_demandante      = round(subocup_no_demandante/PEA*100, 1),
            tasa_asalarizacion              = round(asalariados/ocupados*100,1),
            tasa_no_registro_asal           = round(asal_sindescuento/asalariados*100,1)) %>% 
  select(1,12:20)


tasas_sexo_mza <- base %>% 
  mutate(CH04 = case_when(CH04 == 1 ~ "Varones", 
                          CH04 == 2 ~ "Mujeres")) %>%
  mutate(CH04 = factor(CH04, levels = c("Mujeres", "Varones"))) %>% 
  filter(AGLOMERADO == 10,
         CH06 >= 14) %>%
  group_by(ANO4, CH04) %>% 
  summarise(poblacion = sum(PONDERA),
            ocupados              = sum(PONDERA[ESTADO == 1]),
            desocupados           = sum(PONDERA[ESTADO == 2]),
            PEA                   = ocupados + desocupados,
            ocupados_demandantes  = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            subocup_demandante    = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            subocup_no_demandante = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            subocupados           = subocup_demandante + subocup_no_demandante,
            asalariados           = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3]),
            asal_sindescuento     = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2], na.rm = T),
            tasa_actividad                  = round(PEA/poblacion*100, 1),
            tasa_empleo                     = round(ocupados/poblacion*100, 1),
            tasa_desocupacion               = round(desocupados/PEA*100, 1),
            tasa_ocup_demandante            = round(ocupados_demandantes/PEA*100, 1),
            tasa_subocupacion               = round(subocupados/PEA*100, 1),
            tasa_subocup_demandante         = round(subocup_demandante/PEA*100, 1),
            tasa_subocup_no_demandante      = round(subocup_no_demandante/PEA*100, 1),
            tasa_asalarizacion              = round(asalariados/ocupados*100,1),
            tasa_no_registro_asal           = round(asal_sindescuento/asalariados*100,1)) %>% 
  select(1,2, 13:21)

tasas_edad_mza <- base %>%
  mutate(GRUPO_EDAD = case_when(CH06 >= 14 & CH06 <= 29 ~ "14 a 29 años",
                                CH06 >= 30 & CH06 <= 64 ~ "30 a 64 años")) %>% 
  filter(AGLOMERADO == 10,
         CH06 >= 14,
         !is.na(GRUPO_EDAD)) %>%
  group_by(ANO4, GRUPO_EDAD) %>% 
  summarise(poblacion = sum(PONDERA),
            ocupados              = sum(PONDERA[ESTADO == 1]),
            desocupados           = sum(PONDERA[ESTADO == 2]),
            PEA                   = ocupados + desocupados,
            ocupados_demandantes  = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            subocup_demandante    = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            subocup_no_demandante = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            subocupados           = subocup_demandante + subocup_no_demandante,
            asalariados           = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3]),
            asal_sindescuento     = sum(PONDERA[ESTADO == 1 & CAT_OCUP == 3 & PP07H == 2], na.rm = T),
            tasa_actividad                  = round(PEA/poblacion*100, 1),
            tasa_empleo                     = round(ocupados/poblacion*100, 1),
            tasa_desocupacion               = round(desocupados/PEA*100, 1),
            tasa_ocup_demandante            = round(ocupados_demandantes/PEA*100, 1),
            tasa_subocupacion               = round(subocupados/PEA*100, 1),
            tasa_subocup_demandante         = round(subocup_demandante/PEA*100, 1),
            tasa_subocup_no_demandante      = round(subocup_no_demandante/PEA*100, 1),
            tasa_asalarizacion              = round(asalariados/ocupados*100,1),
            tasa_no_registro_asal           = round(asal_sindescuento/asalariados*100,1)) %>% 
  select(1,2, 13:21)



tasas_mza <- tasas_mza %>% 
  mutate(dominio = "Gran Mendoza",
         desagregacion = "Total",
         categoria = "Total")

tasas_sexo_mza <- tasas_sexo_mza %>% 
  mutate(dominio = "Gran Mendoza",
         desagregacion = "Sexo",
         categoria = CH04) %>% 
  select(-CH04)

tasas_edad_mza <- tasas_edad_mza %>% 
  mutate(dominio = "Gran Mendoza",
         desagregacion = "Edad",
         categoria = GRUPO_EDAD) %>% 
  select(-GRUPO_EDAD)


#Unión de todas las tablas

indicadores_laborales <- bind_rows(tasas_totalpais,
                                   tasas_sexo,
                                   tasas_edad,
                                   tasas_mza,
                                   tasas_sexo_mza,
                                   tasas_edad_mza) %>%
  relocate(ANO4, dominio, desagregacion, categoria) %>%
  arrange(ANO4, dominio, desagregacion, categoria) %>% 
  select(1:8, 10, 12, 13)

#Cambio de estructura de la tabla

indicadores_laborales <- indicadores_laborales %>%
  pivot_longer(cols = starts_with("tasa_"),
               names_to = "indicador",
               values_to = "valor") 

indicadores_laborales <- indicadores_laborales %>% 
  mutate(indicador = recode(indicador,
                            tasa_actividad = "Tasa de actividad",
                            tasa_empleo = "Tasa de empleo",
                            tasa_desocupacion = "Tasa de desocupación",
                            tasa_ocup_demandante = "Tasa de ocupación demandante",
                            tasa_subocupacion = "Tasa de subocupación",
                            tasa_subocup_demandante = "Tasa de subocupación demandante",
                            tasa_asalarizacion = "Tasa de asalarización",
                            tasa_no_registro_asal = "Tasa de no registro de asalariados" )) %>% 
  rename(año = ANO4) %>%
  select(año,
         dominio,
         desagregacion,
         categoria,
         indicador,
         valor) %>%
  arrange(dominio,
          desagregacion,
          categoria,
          indicador,
          año)

dir.create("data", showWarnings = FALSE)

saveRDS(indicadores_laborales, "data/indicadores_laborales.rds")


