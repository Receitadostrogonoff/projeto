#versão 2 da nosso codigo e da nossa base de dados
#o que foi alterado 
#1 - limpeza das tabelas
#2 - conversão das letras maiusculas para minusculas
#3 - exclusão das colunas que não seriam utilizadas priorizando as uteis
#4 - troca dos codigos municipais anteriores para os codigos do IBGE


#cor que sera utilizada na estetica - #FF61c9 - Rosa

#________________________________________________________________

library(readxl)
vs2fem <- read_excel("C:/Users/14086606798/Downloads/vs2fem.xlsx")

View(vs2fem)

#_____________________________________________________________________

#relaçao ao numeros de letras

vs2fem$tentado_consumado <- iconv(vs2fem$tentado_consumado, "latin1", "UTF-8")

#________________________________________________


#group by

library(dplyr)

#para cidades

df_grupado = vs2fem %>% group_by(municipio_ibge, cidades, qtde_vitimas) %>%
  summarise(total_vitimas = sum(qtde_vitimas),
            .groups = 'drop')

View(df_grupado)


#_____________________________________________________________

#diferenciar tentado pra consumado


#para regioes - deu certo

df_grupado_reg = vs2fem %>% group_by(tipo_regiao, qtde_vitimas) %>%
  summarise(vitimas = sum(qtde_vitimas),
            .groups = 'drop')

View(df_grupado_reg)



#_____________________________________________________________

# transformação da data( para mes)

#as.numeric(vs2fem$data_fato)

vs2fem$data_fato <- format(as.Date(vs2fem$data_fato, format="%d/%m/%Y"),"%m")


#group by


#### observaçoes o mes precisa ser numerico em diversas montagens de graficos

#________________________________________________

# grafico de linha

boxplot(df_grupado$cidades, df_grupado$total_vitimas,
        col = c ("tan"),
        main = "Gráfico 1 - boxplot por idade de quem tem filhos")


#________________________________________________

# grafico de pizza

par(cex=0.5) # para diminuir a fonte
pie(df_grupado_reg$vitimas, col=c("#D400FF","blue","wheat1","violetred4","slategrey"),
    main= "crimes ocorridos por regiao")

#________________________________________________________

# mapa

library(ggplot2)
library(geobr)
library(dplyr)
library(RColorBrewer)
library(sf)
library(plotly)

#para ler os municipos do MG

desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)

class(vs2fem) # para saber ac classe
class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(vs2fem$municipio_ibge)

class(df_grupado$total_vitimas)


vs2fem$municipio_ibge = as.numeric(vs2fem$municipio_ibge)


fem_mg1 <-desenho_municipios %>% left_join(vs2fem, by = c( "code_muni"="municipio_ibge"))




df_grupado$total_vitimas = as.numeric(df_grupado$total_vitimas)

summary(df_grupado$total_vitimas)

mapa1 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=total_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Quantidade de vitimas",
                       limits = c(1,40))
ggplotly(mapa1)



#summary(vs2fem$qtde_vitimas)

#usaremos a cor rosa

mapa2 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=qtde_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Quantidade de vitimas",
                       limits = c(1,40))
ggplotly(mapa2)


#____________________________________________________


mapa3 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=qtde_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Quantidade de vitimas",
                       limits = c(1,40))




##### problema encontrado - ele não esta agrupando como o treemap 

#_________________________________

# treemap

library(treemap)

treemap(vs2fem,
        index=c("cidades"),
        vSize="qtde_vitimas",
        vColor="qtde_vitimas",
        type="value",
        palette = "Set3")
#______________________________________________________________

# dispersão


#criado para o histograma

df_grupado_hist = vs2fem %>% group_by(municipio_ibge, cidades, qtde_vitimas,data_fato) %>%
  summarise(total_vitimas = sum(qtde_vitimas),
            .groups = 'drop')

#df_grupado_hist$data_fato <- format(as.Date(df_grupado_hist$data_fato, format="%d/%m/%Y"),"%m")

View(df_grupado_hist)


#por cidade

plot(df_grupado_hist$data_fato, df_grupado$total_vitimas,
     main = "Diagrama de dispersao entre o as vitimas e suas datas ocorridas",
     xlab = "meses ", ylab = "Numero de vitimas",pch=2, col ="#FF61c9")
abline(lsfit(vs2fem$data_fato, vs2fem$qtde_vitimas), col="black")


#_________________________________________________________________

# correlação



#histograma - duas cores

barplot(df_grupado_reg$vitimas, main = "vitimas por região",
        col=c("#FF61c9","pink"), beside = TRUE, 
        legend.text = rownames(df_grupado_reg$tipo_regiao),
        args.legend = list(x = "topleft"))



#________________________________________________________________

#dot chart - diferenciar tentado para consumado

?dotchart


dotchart(df_grupado_reg$vitimas, labels = row.names(df_grupado_reg$tipo_regiao),
         cex = 0.9, ylab = "Cidades", xlab = "regiao")


#________________________________________________________________________________

barplot()
