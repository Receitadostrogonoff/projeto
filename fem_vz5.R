#    versão 2 da nosso codigo e da nossa base de dados
#    o que foi alterado 
#1 - limpeza das tabelas
#2 - conversão das letras maiusculas para minusculas
#3 - exclusão das colunas que não seriam utilizadas priorizando as uteis
#4 - troca dos codigos municipais anteriores para os codigos do IBGE


# versão 4 - 23/10/23

# 1 - buscar os outliers de cada regiao para apresentar os nomes em sala de aula
# 2 - agrupação das linhas na tabela grupo_reg


# versão 5 - 31/10/23
#1 - criação de uma nova tabela para os mapas (mapa 1 e mapa 2) será usadoo dado consumado e o tentado
#2 - limpeza da tabela 
#



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

#para cidades - pode ser usado para os mapas entretanto nao esta separando o tentado para o consumado

df_grupado = vs2fem %>% group_by(municipio_ibge, cidades, qtde_vitimas) %>%
  summarise(total_vitimas = sum(qtde_vitimas),
            .groups = 'drop')

View(df_grupado)

#____________________________________________________________

# transformação da data( para mes)

vs2fem$data_fato <- format(as.Date(vs2fem$data_fato, format="%d/%m/%Y"),"%m")

#_____________________________________________________________


#diferenciar tentado pra consumado

names(vs2fem)

#tentado
vs2fem_tentado = vs2fem %>% filter(tentado_consumado=="tentado")

#consumado
vs2fem_consumado = vs2fem %>% filter(tentado_consumado=="consumado")


# ______________________________________________________________________________

# para mapas 

#vitimas total 

vs2fem_total = vs2fem %>% group_by(municipio_ibge) %>% summarise(total_de_vitimas=sum(qtde_vitimas))

vs2fem_total_mapa3 = vs2fem %>% filter(tentado_consumado=="consumado") %>%  group_by(municipio_ibge) %>% summarise(total_consumado_de_vitimas=sum(qtde_vitimas))
vs2fem_total_mapa2 = vs2fem %>% filter(tentado_consumado=="tentado") %>%  group_by(municipio_ibge) %>% summarise(total_tentado_de_vitimas=sum(qtde_vitimas))





#### observaçoes o mes precisa ser numerico em diversas montagens de graficos

#________________________________________________

# grafico de linha - ggplot 

#boxplot









#________________________________________________

# grafico de pizza por regiao - refazer

#pie 1 - total de regiao
#pie 2 - tentado por regiao
#pie 3 - consumado por regiao 


#pizza 
library(ggplot2)
library(dplyr)

ggplot(minha_tabela2, aes(x = tipo_regiao, y = vitimas, fill = factor(tipo_regiao))) +
  geom_col(stat = "identity")+scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                                                           "interior de minas" = "#d7008d",
                                                           "regiao metropolitana" = "#9c0067")) +
  coord_polar(theta = "y")

ggplot(data = minha_tabela2, mapping = aes(x = tipo_regiao, y=..prop.., fill = tipo_regiao)) + 
  geom_bar(width = 1) + coord_polar(theta = "x") + theme_minimal()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067"))+
  labs(title = "pizza", subtitle = "uma nova análise",
       x = " ", y = " ",caption = "fonte dos dados: secretaria de seg de mg")+
  guides(fill=guide_legend(title="Regiões de MG"))







#o que falta - legenda, porcentagem, 


#________________________________________________________

# mapa - deu certo 

# mapa 1 - total
# mapa 2 - tentado 
# mapa 3 - consumado 

library(ggplot2)
library(geobr)
library(dplyr)
library(RColorBrewer)
library(sf)
library(plotly)

#para ler os municipos do MG

desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)

class(vs2fem_total) # para saber ac classe
class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(vs2fem_total$municipio_ibge)

class(df_grupado$total_vitimas)


vs2fem_total$municipio_ibge = as.numeric(vs2fem_total$municipio_ibge)


fem_mg1 <-desenho_municipios %>% left_join(vs2fem_total, by = c( "code_muni"="municipio_ibge"))


#___________________________________________
#total de vitimas

mapa1 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=total_de_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Total de vitimas",
                       limits = c(1,40))+
  labs(title = "total de vitimas em mg",caption = "area cinza é sem informação")

mapa1

#_________________________________


#mapa 2 - 
desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)

class(vs2fem_total) # para saber ac classe
class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(vs2fem_total_mapa2$municipio_ibge)



vs2fem_total_mapa2$municipio_ibge = as.numeric(vs2fem_total_mapa2$municipio_ibge)


fem_mg1 <-desenho_municipios %>% left_join(vs2fem_total, by = c( "code_muni"="municipio_ibge"))


#___________________________________________
#total de vitimas

mapa1 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=total_de_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Total de vitimas",
                       limits = c(1,40))+
  labs(title = "total de vitimas em mg",caption = "area cinza é sem informação")

mapa1

















# treemap - deu certo 

library(treemap)


treemap(df_grupado_reg,
        index=c("tipo_regiao"),
        vSize="vitimas",
        vColor="vitimas",
        type="value",
        palette = "PuRd")



#______________________________________________________________

# dispersão

#criado para o histograma

df_grupado_hist_con = vs2fem %>% group_by(municipio_ibge, cidades, qtde_vitimas,data_fato) %>%
  summarise(total_vitimas==sum(qtde_vitimas))


# #tentado - funcionou

plot(vs2fem_tentado$data_fato, vs2fem_tentado$qtde_vitimas,
     main = "Diagrama de dispersao entre o as vitimas tentadas por mes no estado",
     xlab = "meses ", ylab = "Numero de vitimas",pch=2, col ="#FF61c9")
abline(lsfit(vs2fem_tentado$data_fato, vs2fem_tentado$qtde_vitimas), col="black")




#consumado 

plot(vs2fem_tentado$data_fato, vs2fem_tentado$qtde_vitimas,
     main = "Diagrama de dispersao entre o as vitimas consumadas e suas datas ocorridas no estado",
     xlab = "meses ", ylab = "Numero de vitimas",pch=2, col ="#FF61c9")
abline(lsfit(vs2fem_tentado$data_fato, vs2fem_tentado$qtde_vitimas), col="black")





df_grupado_hist = vs2fem %>% group_by(municipio_ibge, cidades, qtde_vitimas,data_fato) %>%
  summarise(total_vitimas = sum(qtde_vitimas),
            .groups = 'drop')

View(df_grupado_hist)


# total - funcionou

plot(df_grupado_hist2$data_fato, df_grupado_hist2$total_vitimas,
     main = "Diagrama de dispersao entre o as vitimas e suas datas ocorridas no estado",
     xlab = "meses ", ylab = "Numero de vitimas",pch=2, col ="#FF61c9")
abline(lsfit(df_grupado_hist2$data_fato, df_grupado_hist2$total_vitimas), col="black")


#por regiao

# criar uma tabela para ele

df_grupado_reg_disp = vs2fem %>% group_by(tipo_regiao, qtde_vitimas, data_fato) %>% filter(tentado_consumado=="tentado")
            .groups = 'drop')

View(df_grupado_reg_disp)

#total de cidades_ pedir para chamar 

plot(df_grupado_reg_disp$data_fato, df_grupado_reg_disp$vitimas,
     main = "Diagrama de dispersao entre as vitimas e os meses ocorridos ppr região",
     xlab = "meses ", ylab = "Numero de vitimas",pch=2, col ="#FF61c9")
abline(lsfit(df_grupado_reg_disp$data_fato, df_grupado_reg_disp$vitimas), col="black")

#_________________________________________________________________

#histograma 

# ggplot - funcionou 


minha_tabela = df_grupado_reg  %>% select(tipo_regiao,vitimas)

library(ggplot2)

#total de vitimas

minha_tabela %>% ggplot(aes(x=tipo_regiao,y=vitimas,fill=tipo_regiao))+ geom_col()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067")) +
  theme_minimal()

#__________________________________________________________________________


# total de vitimas consumada 

#por região

# total de vitimas tentadas 


minha_tabela = df_grupado_reg  %>% select(tipo_regiao,vitimas)

library(ggplot2)

#total de vitimas

minha_tabela %>% ggplot(aes(x=tipo_regiao,y=vitimas,fill=tipo_regiao))+ geom_col()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067")) +
  theme_minimal()

# tentado por regiao

#consumado por regiao 



#________________________________________________________________

minha_tabela2 =  minha_tabela %>% group_by(tipo_regiao) %>% summarise(vitimas=sum(vitimas))



#dot chart - diferenciar tentado para consumado




#________________________________________________________________________________



