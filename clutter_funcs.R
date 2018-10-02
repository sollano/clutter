## # Carregar pacote e dados ####
library(forestr)
library(dplyr)
library(ggplot2)
dados <- read.csv2("dados_clutter.csv")
head(dados, 10)

## # Ajustar Clutter e estimar B & V ####
## # Estimar o Site (Schummacher e Chapman & Richards) ####
Idade_I <- 64
dados <-  dados %>% 
  #Schummacher
  lm_table(log(HD) ~ inv(Idade), output = "merge") %>% 
  mutate(Site_sch = exp(log(HD) - b1 * (1/Idade - 1/Idade_I))  ) %>% 
  select(-b0, -b1, -Rsqr, -Rsqr_adj, -Std.Error) %>% 
  #Chapman & Richards
  nls_table(HD ~ b0 * (1 - exp( -b1 * Idade )  )^b2, mod_start = c( b0=23, b1=0.03, b2 = 1.3   ), output = "merge" ) %>% 
  mutate(Site_chap = HD *( (  (1- exp( -b1/Idade ))^b2   ) / (( 1 - exp(-b1/Idade_I))^b2 ))  ) %>% 
  select(-b0,-b1,-b2)
dados

## # Rodar Clutter ####
coefs_clutter <- fit_clutter(dados, "Idade", "HD", "B", "V", "Site_sch", "Parcela")
coefs_clutter

## # Classificar os dados ####
dados_class <- class_data(dados, "Site_sch", 3, "Parcela")
head(dados_class ,15)

## # Estimar LN(B2) ####

# Metodo da equacao em funcao de B para estimar B1:
dados_est_ <- est_LN_B2(dados_class, "Site_medio", "B", 20:125, coefs_clutter$a0, coefs_clutter$a1, "Categoria_", method = "modelo") 

# Metodo da Media como B1:
dados_est_ <- est_LN_B2(dados_class, "Site_medio", "B", 20:125, coefs_clutter$a0, coefs_clutter$a1, "Categoria_", method = "media")

# dados_est_ <- est_LN_B2(dados_class, Site_chp, B, 20:125, coefs_clutter$a0, coefs_clutter$a1, Categoria_, method = "equacao")
# Site ou site medio??

head(dados_est_ ,15)

## # Estimar B2, Volume, ICM & IMM & ITC ####
dados_est <- dados_est_ %>% 
  group_by(Categoria_) %>% 
  mutate(
    B2_EST = exp(LN_B2_EST),
    V2_EST = exp(coefs_clutter$b0 + 
                   (coefs_clutter$b1 * 1 / Idade) + 
                    coefs_clutter$b2 * Site + 
                    coefs_clutter$b3 * LN_B2_EST  ),
    ICM = abs(V2_EST - lag(V2_EST) ),
    IMM = V2_EST/ Idade,
    ICM_IMM = ICM - IMM) 

  #Estimar ITC
dados_est <- left_join(dados_est,
                       dados_est  %>% 
                         group_by(Categoria_) %>% 
                         filter(round(ICM,1) == round(IMM,1) ) %>% 
                         summarise_at( vars(ITC = Idade, ITC_Y = IMM), mean ) )        
dados_est

# Tab final
tab_final <- dados_est %>% 
  group_by(Categoria_) %>% 
  summarise(
    B2_Inicial = B2_EST[row_number()==1],
    ITC  = mean(Idade[round(ICM,1) == round(IMM,1)], na.rm=T) ,
    Site = mean(Site[round(ICM,1) == round(IMM,1)], na.rm=T) ,
    V_total = sum(V2_EST, na.rm=T)  )
tab_final

## # Graficos de Incremento Corrente Mensal e Incremento Medio Mensal ####

## Com os valores de ICM e IMM obtidos,
## e necessario te-los na mesma coluna, sendo identificados
## por um fator, para que se possa fazer os graficos de linha.
## Para isso utiliza-se a funcao gather;
##
## Em seguida, plota-se o grafico com ggplot, utilizando a variavel Indice como cor,
## para que se diferencie os indices. 
## Pode-se adicionar a idade tecnica de corte ao grafico, com geom_text.
## Alem disso, utiliza-se facet_wrap para gerar um grafico para cada classe:
#+ fig.width = 14, fig.height = 8
graph <- dados_est %>% 
  na.omit() %>% 
  select(Categoria_, ICM, IMM, Idade, ITC, ITC_Y) %>% 
  tidyr::gather(Indice,Valor, ICM, IMM) %>% 
  ggplot(aes( x = Idade, color = Indice ) ) + 
  facet_wrap(~Categoria_) + 
  geom_line(aes(y = Valor), size = 1.5) +
  geom_point(aes(x = ITC, y = ITC_Y), color = "black")+
  geom_text(aes(x = ITC, y = ITC_Y, label = ITC, family = "serif"),
            vjust = 0, 
            nudge_y = 0.2, 
            color = "black", size = 5)+
  labs(x = "Idade (meses)",
       y = "Indice",
       color = "Indice") +
  ggthemes::theme_igray(base_family = "serif") +
    theme(      legend.position = "bottom",
                legend.title = element_text(size=12,face="bold"),
                legend.text = element_text(size=12),
                panel.grid.major = ggplot2::element_blank(), 
                panel.grid.minor = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                axis.title   = ggplot2::element_text(size = 17), 
                axis.text    = ggplot2::element_text(size = 15),
                axis.line.x = ggplot2::element_line(color="black"),
                axis.line.y = ggplot2::element_line(color="black"),
          strip.text.x = element_text(size = 19) )
graph

## Exporta-se o grafico com ggsave:
#ggsave("graph_itc.png",  graph,  width = 14, height = 8)
