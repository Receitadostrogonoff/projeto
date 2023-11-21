#    versão 2 da nosso codigo e da nossa base de dados
#    o que foi alterado?
#1 - limpeza das tabelas
#2 - conversão das letras maiusculas para minusculas
#3 - exclusão das colunas que não seriam utilizadas priorizando as uteis
#4 - troca dos codigos municipais anteriores para os codigos do IBGE


# versão 4 - 23/10/23

# 1 - buscar os outliers de cada regiao para apresentar os nomes em sala de aula
# 2 - agrupação das linhas na tabela grupo_reg


# versão 5 - 31/10/23
#1 - criação de uma nova tabela para os mapas (mapa 1 e mapa 2) será usadoo dado consumado e o tentado
#2 - limpeza da tabela para os mapas
#3 - exclusão da tabela de dispersão ( devidoaa série temporal)
#4 - definição do ggplot como criador fundamental dos gráficos que serão realizados 
#5 - preparaçao para o início da versao 6 (versão final)
#6 - definição de 3 tipos (total, tentado e consumado) de metodos de observações.

#versão 6 - versão final- 01/11/23
#nao sera criado um grafico de dispersão devido ao fato de que sera utilizado uma serie.
#criação dos ultimos graficos e aperfeiçoementodo outros que ja existem.

#versão 7 - versão final parte 2 - 14/11/2023
#1 - grafico de linha mudar cores
#2 - tirar o histograma, variavel quantitativa discreta.
#3 - modelo de previsão comparar com o modelo de 2023 

# ler https://otexts.com/fpp3/intro.html

#versão 8 - versão final parte 3 - 21/11/2023
#1 - definir os teste de hipoteses


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



#### observaçoes o mes precisa ser numerico em diversas montagens de graficos

#________________________________________________

# grafico de linha - ggplot 

#tabelas 

vs2fem_linha_1 = vs2fem %>% group_by(tipo_regiao, qtde_vitimas, data_fato) %>%
  summarise(total_vitimas = sum(qtde_vitimas),
            .groups = 'drop')

View(vs2fem_linha_1)

vs2fem_linha_2 = vs2fem %>% filter(tentado_consumado=="tentado") %>% group_by(tipo_regiao, qtde_vitimas, data_fato) %>%
  summarise(vitimas_tentadas = sum(qtde_vitimas),
            .groups = 'drop')

View(vs2fem_linha_2)


vs2fem_linha_3 = vs2fem %>% filter(tentado_consumado=="consumado") %>% group_by(tipo_regiao, qtde_vitimas, data_fato) %>%
  summarise(vitimas_consumadas = sum(qtde_vitimas),
            .groups = 'drop')

View(vs2fem_linha_3)

#total 

ggplot(vs2fem_linha_1, aes(x=data_fato, y=total_vitimas, group = tipo_regiao)) + 
  geom_line(aes(color=tipo_regiao)) +
  scale_color_manual(name='Regiões de MG', labels=c('belo horizonte','interior de minas','regiao metropolitana'),
                     values=c('blue', '#d7008d', 'green')) +labs(title = "Ocorrências de feminicidio no estado de MG",
                                                                      x = "Meses ", y = "Número de vitimas ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG")) +geom_point( size=2, shape=20, fill="white") +
  theme_light()


#tentado
ggplot(vs2fem_linha_2, aes(x=data_fato, y=vitimas_tentadas, group = tipo_regiao)) + 
  geom_line(aes(color=tipo_regiao)) +
  scale_color_manual(name='Regiões de MG', labels=c('belo horizonte','interior de minas','regiao metropolitana'),
                     values=c('blue', '#d7008d', 'green')) +labs(title = "Ocorrências de tentativas de feminicidio no estado de MG",
                                                                      x = "Meses ", y = "Número de vitimas ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG")) +geom_point( size=2, shape=20, fill="white") +
  theme_light()

#consumado
ggplot(vs2fem_linha_3, aes(x=data_fato, y=vitimas_consumadas, group = tipo_regiao)) + 
  geom_line(aes(color=tipo_regiao)) +
  scale_color_manual(name='Regiões de MG', labels=c('belo horizonte','interior de minas','regiao metropolitana'),
                     values=c('blue', '#d7008d', 'green')) +labs(title = "Ocorrências de feminicidio consumado no estado de MG",
                                                                      x = "Meses ", y = "Número de vitimas ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG")) +geom_point( size=2, shape=20, fill="white")+
  theme_light()


#_________________________________________________________

#grafico de barra

#sera usado as tabelas  que foram usadas para o grafico de linhas

View(vs2fem_linha_1)

library(ggplot2)

#total
ggplot(vs2fem_linha_1, aes(fill=tipo_regiao,x=data_fato, y=total_vitimas )) + 
  geom_col(position = "dodge") +
  theme(legend.position = "bottom")+
  scale_fill_manual(name='Regiões de MG', labels=c('belo horizonte','interior de minas','regiao metropolitana'),
                     values=c('#FF61c9', '#d7008d', '#9c0067')) +labs(title = "Ocorrências de feminicidio no estado de MG",
                              x = "Meses ", y = "Número de vitimas ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG"))+
  theme_light()


#tentado

library(ggplot2)

ggplot(vs2fem_linha_2, aes(fill=tipo_regiao, x=data_fato, y=vitimas_tentadas )) + 
  geom_col(position = "dodge") +
  theme(legend.position = "bottom")+
  scale_fill_manual(name='Regiões de MG', labels=c('belo horizonte','interior de minas','regiao metropolitana'),
                    values=c('#FF61c9', '#d7008d', '#9c0067')) +labs(title = "Ocorrências de  tentativas de feminicidio no estado de MG",
                                                                     x = "Meses ", y = "Número de vitimas ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG")) +
  theme_light()


#consumado

ggplot(vs2fem_linha_3, aes(fill=tipo_regiao,x=data_fato, y=vitimas_consumadas )) + 
  geom_col(position = "dodge") +
  theme(legend.position = "bottom")+
  scale_fill_manual(name='Regiões de MG', labels=c('belo horizonte','interior de minas','regiao metropolitana'),
                    values=c('#FF61c9', '#d7008d', '#9c0067')) +labs(title = "Ocorrências de feminicidio consumado no estado de MG",
                                                                     x = "Meses ", y = "Número de vitimas ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG")) +
  theme_light()


#________________________________________________

# grafico de pizza por regiao 

#pie 1 - total de regiao
#pie 2 - tentado por regiao
#pie 3 - consumado por regiao 

library(ggplot2)
library(dplyr)

#total

View(minha_tabela2)

ggplot(data = minha_tabela2, mapping = aes(x = tipo_regiao, y=..prop.., fill = tipo_regiao)) + 
  geom_bar(width = 1) + coord_polar(theta = "x") + theme_minimal()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067"))+
  labs(title = "Ocorrências de feminicidio no estado de MG",
       x = " ", y = " ",caption = "fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG"))


#tentado
ggplot(data = vs2fem_treemap_2, mapping = aes(x = tipo_regiao, y=..prop.., fill = tipo_regiao)) + 
  geom_bar(width = 1) + coord_polar(theta = "x") + theme_minimal()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067"))+
  labs(title = "Ocorrências de tentativa de feminicidio no estado de MG",
       x = " ", y = " ",caption = "fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG"))


#consumado

ggplot(data = vs2fem_treemap_3, mapping = aes(x = tipo_regiao, y=..prop.., fill = tipo_regiao)) + 
  geom_bar(width = 1) + coord_polar(theta = "x") + theme_minimal()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067"))+
  labs(title = "Ocorrências de feminicidio consumado no estado de MG",
       x = " ", y = " ",caption = "fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)")+
  guides(fill=guide_legend(title="Regiões de MG"))

# ______________________________________________________________________________

# para mapas 

#vitimas total 

vs2fem_total = vs2fem %>% group_by(municipio_ibge) %>% summarise(total_de_vitimas=sum(qtde_vitimas))

vs2fem_total_mapa3 = vs2fem %>% filter(tentado_consumado=="consumado") %>%  group_by(municipio_ibge) %>% summarise(total_consumado_de_vitimas=sum(qtde_vitimas))
vs2fem_total_mapa2 = vs2fem %>% filter(tentado_consumado=="tentado") %>%  group_by(municipio_ibge) %>% summarise(total_tentado_de_vitimas=sum(qtde_vitimas))


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

#mapa 1 - total de vitimas
View(vs2fem_total)

desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)

class(vs2fem_total) # para saber ac classe
class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(vs2fem_total$municipio_ibge)

class(df_grupado$total_vitimas)


vs2fem_total$municipio_ibge = as.numeric(vs2fem_total$municipio_ibge)


fem_mg1 <-desenho_municipios %>% left_join(vs2fem_total, by = c( "code_muni"="municipio_ibge"))

mapa1 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=total_de_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Total de vitimas",
                       limits = c(1,40))+
  labs(title = "Total de ocorrências de feminicidio em MG",caption = "area cinza nao possui informação")

mapa1

#_________________________________


#mapa 2 - tentado
desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)

class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(vs2fem_total_mapa2$municipio_ibge)



vs2fem_total_mapa2$municipio_ibge = as.numeric(vs2fem_total_mapa2$municipio_ibge)


fem_mg1 <-desenho_municipios %>% left_join(vs2fem_total_mapa2, by = c( "code_muni"="municipio_ibge"))


mapa2 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=total_tentado_de_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Total de vitimas",
                       limits = c(1,40))+
  labs(title = "Total de ocorrências de tentativas de feminicidio em MG",caption = "area cinza não possui informação")

mapa2


#___________________________________________
#mapa 3 - total de vitimas consumadas

desenho_municipios = read_municipality(code_muni = "MG", showProgress = FALSE)

class(desenho_municipios)
plot(desenho_municipios)

class(desenho_municipios$code_muni)
class(vs2fem_total_mapa3$municipio_ibge)



vs2fem_total_mapa2$municipio_ibge = as.numeric(vs2fem_total_mapa3$municipio_ibge)


fem_mg1 <-desenho_municipios %>% left_join(vs2fem_total_mapa3, by = c( "code_muni"="municipio_ibge"))


mapa3 = ggplot() + 
  geom_sf(data=fem_mg1, aes(fill=total_consumadas_de_vitimas)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, name="Total de vitimas",
                       limits = c(1,40))+
  labs(title = "Total de ocorrências de feminicidio consumados em MG",caption = "area cinza não possui sem informação")

mapa3

view(vs2fem_total_mapa3)

View(vs2fem_total_mapa3)
#________________________________________________________________________________________
#tabelas para o treemap

View(df_grupado_reg)

vs2fem_treemap_1 = vs2fem  %>%  group_by(tipo_regiao) %>% summarise(total_vitimas=sum(qtde_vitimas))

vs2fem_treemap_2 = vs2fem %>% filter(tentado_consumado=="tentado") %>%  group_by(tipo_regiao) %>% summarise(total_tentado=sum(qtde_vitimas))

vs2fem_treemap_3 = vs2fem %>% filter(tentado_consumado=="consumado") %>%  group_by(tipo_regiao) %>% summarise(total_consumado=sum(qtde_vitimas))

view(vs2fem_treemap_3)

# treemap - deu certo 

library(treemap)

#total por regiao

treemap(vs2fem_treemap_1,
        index=c("tipo_regiao"),
        vSize="total_vitimas",
        fontsize.labels=c(14, 8), 
        vColor="total_vitimas",
        title ="Total de ocorrências de feminicidio em MG por região",
        title.legend="Número de vitimas",
        type="value",
        palette = "PuRd")

#tentado

treemap(vs2fem_treemap_2,
        index=c("tipo_regiao"),
        vSize="total_tentado",
        fontsize.labels=c(14, 8), 
        vColor="total_tentado",
        title ="Total de ocorrências de tentativas de feminicidio em MG por região",
        title.legend="Número de vitimas",
        type="value",
        palette = "PuRd")


#consumado

treemap(vs2fem_treemap_3,
        index=c("tipo_regiao"),
        vSize="total_consumado",
        fontsize.labels=c(14, 8),
        vColor="total_consumado",
        title ="Total de ocorrências de feminicidio consumados em MG por região",
        title.legend="Número de vitimas",
        type="value",
        palette = "PuRd")

_


# ggplot - funcionou 

library(ggplot2)

#total de vitimas

vs2fem_treemap_1 %>% ggplot(aes(x=tipo_regiao,y=total_vitimas,fill=tipo_regiao))+ geom_col()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067")) + 
  labs(title = "Ocorrências de tentativa de feminicidio no estado de MG",
       x = " ", y = " ",caption = "fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)") +
  guides(fill=guide_legend(title="Regiões de MG"))

  
  
#tentativa

vs2fem_treemap_2 %>% ggplot(aes(x=tipo_regiao,y=total_tentado,fill=tipo_regiao))+ geom_col()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067")) +
  labs(title = "Ocorrências de feminicidio no estado de MG",
       x = " ", y = " ",caption = "fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)") +
  guides(fill=guide_legend(title="Regiões de MG"))


#consumado

vs2fem_treemap_3 %>% ggplot(aes(x=tipo_regiao,y=total_consumado,fill=tipo_regiao))+ geom_col()+
  scale_fill_manual(values = c("belo horizonte" = "#FF61c9",
                               "interior de minas" = "#d7008d",
                               "regiao metropolitana" = "#9c0067")) +
  labs(title = "Ocorrências de feminicidio consumado no estado de MG",
       x = " ", y = " ",caption = "Fonte dos dados: Sistema de Informação de Agravos de Notificação (Sinan)") +
  guides(fill=guide_legend(title="Regiões de MG"))

#__________________________________________________________________________

#teste de hipoteses 

#não rejeitar a hipotese ou rejeitar

#nunca concordar com a hipotese

#hipotese

tabela = table(vs2fem$tipo_regiao,vs2fem$tentado_consumado)
tabela # é de natureza categorica
chisq.test(tabela)

#alpha: 0.05
#se pvalor<= alpha REJ H0
#se pvalor > alpha não rej H0

#hipote se existe a associação entre as duas variaveis( qtde de fem(tipo de feminicidio) e tipo de região)
#esta acinma  acima do alpha

#hipotese : nossa hipotese será que a capital possui um numereo maior de feminicidios consumados mais do que o  interior 

#conclusão : não existe a associação das variáveis, não se verifica. não rejeita H0. 


prop.table(tabela,1)*100
prop.table(tabela,2)*100

library(janitor)
tabyl(tabela) # criar tabela 


#______________________________________________________________________

#proporção populacional

#criar uma tabela per capita para comparar com a população 

