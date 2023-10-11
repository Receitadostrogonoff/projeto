# importação da datatable

library(readr)
lfeminicidio_2022 <- read_delim("C:/Users/14086606798/Downloads/feminicidio_2022.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

lfeminicidio_2022$qtde_vitimas = as.double(lfeminicidio_2022$qtde_vitimas) #as.double ?? o mais parecido com o as numeric

View(lfeminicidio_2022)

# alterações da tabela

library(dplyr)
?case_when

# -------------------------------------------------------------------

#alterar o nome do mês ---- fase fechada

lfeminicidio_2022 = lfeminicidio_2022 %>%
  mutate(
    mes_cat = case_when(
      mes==1 ~ "Janeiro",
      mes==2 ~ "Fevereiro",
      mes==3 ~ "Marco",
      mes==4 ~ "Abril",
      mes==5 ~ "Maio",
      mes==6 ~ "junho",
      mes==7 ~ "julho",
      mes==8 ~ "Agosto",
      mes==9 ~ "Setembro",
      mes==10 ~ "Outubro",
      mes==11 ~ "Novembro",
      mes==12 ~ "Dezembro",
      .default = "Outros"
    )
  )

# --------------------------------------------------------------------------

#alterar o nome da regiao # refazer

lfeminicidio_2022 = lfeminicidio_2022 %>%
  mutate(
    Cidades = case_when(
      municipio_fato=="ALEM PARAIBA" ~ "Alem paraiba",
      municipio_fato=="ALFENAS" ~ "Afenas",
      municipio_fato=="ALMENARA" ~"Almenar",
      municipio_fato=="ANDRADAS" ~ "Andradas",
      municipio_fato=="ANGELANDIA" ~ "Angelandia",
      municipio_fato=="ARACUAI" ~ "Aracuai",
      municipio_fato=="ARAXA" ~ "Araxa",
      municipio_fato=="ANGELANDIA" ~ "Angelandia",
      municipio_fato=="BANDEIRA DO SUL" ~ "Bandeira do Sul",
      municipio_fato=="BELO HORIZONTE" ~ "Belo horizonte",
      municipio_fato=="BETIM" ~ "Betim",
      municipio_fato=="BOM JESUS DO GALHO" ~ "Bom Jesus do Galho",
      municipio_fato=="BONFIM" ~ " Bofim",
      municipio_fato=="BRASILANDIA DE MINAS" ~ "Brasilandia de Minas",
      municipio_fato=="BUENOPOLIS" ~ "Buenopolis",
      municipio_fato=="CAETE" ~ "Caete",
      municipio_fato=="CAMPESTRE" ~ "Campestre",
      municipio_fato=="CAMPO AZUL" ~ "Campo Azul",
      municipio_fato=="CAPELINHA" ~ "Capelinha",
      municipio_fato=="CARANGOLA" ~ "Carongola",
      municipio_fato=="CARATINGA" ~ "Caratinga",
      municipio_fato=="CATAGUASES" ~ "Cataguases",
      municipio_fato=="CHAPADA GAUCHA" ~ "Chapada Gaucha",
      municipio_fato=="CONCEICAO DAS ALAGOAS" ~ "Conceicao de alagoas",
      
      
      .default = "outros"
    )
  )


#criar os graficos e estatisticas - mapa criação do mapa 

library(ggplot2)
library(geobr)
library(dplyr)
library(RColorBrewer)
library(sf)
library(plotly)


#para ler os municipos do MG

desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)


class(lfeminicidio_2022)
class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(lfeminicidio_2022$municipio_cod)

lfeminicidio_2022$municipio_cod = as.numeric(lfeminicidio_2022$municipio_cod)

base_tentado = lfeminicidio_2022 %>% filter(tentado_consumado=='TENTADO') %>% group_by(municipio_fato) %>%
  summarise(vitimas = sum(qtde_vitimas, na.rm = TRUE))

base_consumado = lfeminicidio_2022 %>% filter(tentado_consumado=='CONSUMADO') %>% group_by(municipio_fato) %>%
  summarise(vitimas = sum(qtde_vitimas, na.rm = TRUE))

desenho_municipios$name_muni = toupper(desenho_municipios$name_muni)

#aq parou -  resolver semana q vem 
tentado <-desenho_municipios %>% left_join(base_tentado, by = c( "name_muni"="municipio_fato"))
consumado <-desenho_municipios %>% left_join(base_consumado, by = c( "code_muni"="municipio_cod"))


summary(tentado$vitimas)

mapa1 = ggplot() + 
  geom_sf(data=tentado, aes(fill=vitimas)) +
  scale_fill_distiller(palette = "Reds", direction = 1, name="vitimas",
                       limits = c(0,27))
ggplotly(mapa1)

# -------------------------------------------------------------

#criar  mapa do treemap - criar siglas dos municipios

library(treemap)

treemap(lfeminicidio_2022,
        index=c("municipio_fato"),
        vSize="qtde_vitimas",
        vColor="qtde_vitimas",
        type="value",
        palette = "Set3")

#-----------------------------------------------------------------

#histograma 



lfeminicidio_2022$tentado_consumado = ifelse(lfeminicidio_2022$tentado_consumado==TENTADO, "1")

hist(lfeminicidio_2022$tentado_consumado)



barplot()


par(cex=0.5) # para diminuir a fonte
pie(lfeminicidio_2022$qtde_vitimas, col=c("#D400FF","blue","wheat1","violetred4","slategrey",
                             "pink4","palegreen4","aliceblue", "beige", "beige",
                             "bisque" ,"cadetblue1","chocolate1","orangered","oldlace",
                             "palegoldenrod","lavenderblush","honeydew","ivory2","ivory1",
                             "powderblue","purple" ,"purple1","purple2","purple3","red1",
                             "tan3","firebrick3","gray1","gold4","dodgerblue4","floralwhite",
                             "gray21","gray12","olivedrab4", "gold2"),
                             main= "Numero de vitimas")

