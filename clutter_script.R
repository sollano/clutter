## ---

## title: Script para ajuste do modelo de Clutter pelo metodo dos Minimos Quarados em 2 Estagios utilizando o R

## author:
## - Sollano Rabelo Braga 
## - Ana Carolina Araujo

## date: Outubro, 2016

## output:

##    pdf_document:

##      toc: true

##      toc_depth: 3

##      highlight: tango

##    word_document:

##      toc: true

##      toc_depth: 3

##      highlight: tango

## ---
## \pagebreak
##
#+ include=FALSE
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=55),tidy=TRUE)

## # 1) Carregar os pacotes e dados ####

## Primeiro carrega-se as bibliotecas;
## Systemfit para o ajuste do modelo utilizando o metodo
## dos Minimos Quadrados em 2 Estagios, e tidyverse para
## manipulacao de dados e graficos.
library(systemfit)
library(tidyverse)

## Carrega-se os dados em csv:
dados <- read.csv2("dados_clutter.csv")
head(dados, 10)

## # 2) Estimar site ####

## Primeiro ajusta-se o modelo em funcao do inverso da idade;
## Para isso calcula-se o inverso da idade e ln(HD) separadamente:
dados$LN_HD <- log(dados$HD)
dados$I_IDADE <- 1/dados$Idade

## Em seguida visualiza-se o ajuste, e salva-se
## os coeficientes em objetos separados:
summary(lm(LN_HD ~ I_IDADE, dados))

b0 <- lm(LN_HD ~ I_IDADE, dados)[[1]][[1]]
b1 <- lm(LN_HD ~ I_IDADE, dados)[[1]][[2]]
b0
b1

## Remover variaveis criadas:
dados[c("LN_HD", "I_IDADE")] <- NULL

## Utilizando uma idade indice de 64, calcula-se o site:

Idade_I <- 64

dados$Site <- exp( log(dados$HD) - b1 * (1/dados$Idade - 1/Idade_I)  )
head(dados)

## # 3) Preparacao dos dados ####

## Para se fazer o ajuste e necessario organizar os dados de uma
## certa forma. Separando os dados em idade 1 e 2, 
## altura dominante 1 e 2, area basal 1 e 2, 
## e volume 1 e 2. 
## Para isso pode-se utilizar um loop for, ou o pacote dplyr.
##

## ## 3.1) Preparacao dos dados - rbase ####

list <- vector("list", nrow(dados))

for(i in 1: nrow(dados) ){
  
  if(dados$Parcela[i] == dados$Parcela[i+1] && !is.na(dados$Idade[i+1]) )
  {
    list[[i]] <-data.frame(
      Parcela = dados$Parcela[i],
      I1      = dados$Idade[i]  , I2  = dados$Idade[i+1],
      HD1     = dados$HD[i]     , HD2 = dados$HD[i+1]   ,
      B1      = dados$B[i]      , B2  = dados$B[i+1]    ,
      V1      = dados$V[i]      , V2  = dados$V[i+1]    ,
      Site    = dados$Site[i] )
  }
  
}
dados_prep <- do.call(rbind, list)
head(dados_prep, 10)

## ## 3.2) Preparacao dos dados - dplyr ####

## Utilizando o dplyr, agrupa-se os dados por parcela,
## e utilizando a funcao lead, cria-se variaveis novas
## que chama o dados seguinte da linha. Lead funciona
## de forma similar a "i+1", utilizando no topico 3.1.
## obs: na omit remove as linhas com NA.

dados_prep <- dados %>% 
  group_by(Parcela) %>% 
  transmute(
    I1   = Idade, I2  = lead(Idade),
    HD1  = HD,    HD2 = lead(HD)   ,
    B1   = B,     B2  = lead(B)    ,
    V1   = V,     V2  = lead(V)    ,
    Site = Site
  ) %>% 
  na.omit
dados_prep 




## # 4) Converter dados para forma estrutural ####

## Para realizar o ajuste e necessario que os dados sejam convertidos
## para a forma estrutural do modelo:
## Agora sera ofeito o ajuste do modelo de Clutter:
##
## Na sua forma estrutural, ele pode ser representado da seguinte forma:
##
## $$ Y_2 = \beta_0 + \beta_1 X_4 + \beta_2 X_5 + \beta_3 Y_1 + Ln(\varepsilon_1) $$
##
## $$ Y_1 = X_1 +  \alpha_0 X_2 + \alpha_1 X_3 + ln(\varepsilon_2) $$
##

## ## 4.1) Converter dados para forma estrutural - rbase ####

dados_form_est <- dados_prep
dados_form_est$Y1 <-  log(dados_prep$B2)                                  
dados_form_est$Y2 <-  log(dados_prep$V2)                                 
dados_form_est$X1 <-  log(dados_prep$B1) * (dados_prep$I1/dados_prep$I2)  
dados_form_est$X2 <-  1 - dados_prep$I1/dados_prep$I2                     
dados_form_est$X3 <-  (1 - dados_prep$I1/dados_prep$I2) * dados_prep$Site 
dados_form_est$X4 <-  1 / dados_prep$I2                                   
dados_form_est$X5 <- dados_prep$Site

head(dados_form_est, 10)

## ## 4.2) Converter dados para forma estrutural - dplyr ####

dados_form_est2 <- dados_prep %>% 
  mutate(
    Y1 = log(B2)            ,
    Y2 = log(V2)            ,
    X1 = log(B1) * (I1/I2)  ,
    X2 = 1 - I1/I2          ,
    X3 = (1 - I1/I2) * Site ,
    X4 = 1 / I2             ,
    X5 = Site
  )
dados_form_est2



## # 5) Ajuste do modelo de Clutter pelo metodo MQ2E (2SLS) ####

## Agora sera feito o ajuste do modelo de Clutter:
##
## $$ Ln(B_2) = LnB_1\begin{pmatrix}  \frac{I_1}{I_2} \end{pmatrix} +  \alpha_0\begin{pmatrix} 1 - \frac{I_1}{I_2} \end{pmatrix} + \alpha_1\begin{pmatrix} 1 - \frac{I_1}{I_2} \end{pmatrix} S + ln(\varepsilon_2) $$
##
## $$ Ln(V_2) = \beta_0 + \beta_1 \begin{pmatrix} \frac{1}{I_2}\end{pmatrix} + \beta_2 S + \beta_3 Ln(B_2) + Ln(\varepsilon_1) $$
##
## Na sua forma estrutural, ele pode ser representado da seguinte forma:
##
## $$ Y_1 = X_1 +  \alpha_0 X_2 + \alpha_1 X_3 + ln(\varepsilon_2) $$
##
## $$ Y_2 = \beta_0 + \beta_1 X_4 + \beta_2 X_5 + \beta_3 Y_1 + Ln(\varepsilon_1) $$
##
## O ajuste pode ser feito tanto utilizando o pacote systemfit, tanto
## quanto utilizando o rbase, porem, utilizando o pacote,
## o ajuste e mais preciso.
##
## ## 5.1) Ajuste do modelo pelo metodo MQ2E (2SLS) - rbase ####

## O modelo de area basal nao possui intercepto, e o coeficiente
## de X1 possui valor 1. Para gerar este resultado,
## deve-se seguir os seguintes passos:
##
## Primeiro gera-se uma copia dos dados:
dados_form_est_aux <- dados_form_est 
head(dados_form_est_aux)

## Calcula-se o inverso da idade:
dados_form_est_aux$INV_I1 <- 1/dados_form_est_aux$I1

## Ajusta-se o sistema na sua forma reduzida, de forma linear:
reg_B2_linear <- lm(Y1 ~ INV_I1 + Site + X1 + X2 + X3, dados_form_est_aux)

summary(reg_B2_linear)

tab_coef_B2_linear <- data.frame("b0" = coef(reg_B2_linear)[1],
                                 "b1" = coef(reg_B2_linear)[2],
                                 "b2" = coef(reg_B2_linear)[3],
                                 "b3" = coef(reg_B2_linear)[4],  
                                 "b4" = coef(reg_B2_linear)[5],  
                                 "b5" = coef(reg_B2_linear)[6],  
                                 row.names = NULL )
tab_coef_B2_linear

## Estima-se a area basal com os coeficientes:
dados_form_est_aux$Y1_EST <- tab_coef_B2_linear$b0 + 
  tab_coef_B2_linear$b1*dados_form_est_aux$INV_I1 + 
  tab_coef_B2_linear$b2*dados_form_est_aux$Site + 
  tab_coef_B2_linear$b3*dados_form_est_aux$X1 + 
  tab_coef_B2_linear$b4*dados_form_est_aux$X2 + 
  tab_coef_B2_linear$b5*dados_form_est_aux$X3
head(dados_form_est_aux)

## Agora ajusta-se o modelo de volume, utilizando Y1 estimado:
reg_V2 <- lm(Y2 ~ X4 + X5 + Y1_EST, dados_form_est_aux)

summary(reg_V2)

tab_coef_V2 <- data.frame("b0" = coef(reg_V2)[1],
                          "b1" = coef(reg_V2)[2],
                          "b2" = coef(reg_V2)[3],
                          "b3" = coef(reg_V2)[4],  
                          row.names = NULL )
tab_coef_V2

## Agora ja e possivel ajustar o modelo de area basal;
## faz-se o ajuste de forma nao linear, para que se possa
## restringir os coeficientes do intercepto e do X1:
reg_B2_n_linear <- nls(Y1 ~ X1 + a0*X2 + a1*X3, dados_form_est_aux)

summary(reg_B2_n_linear)

tab_coef_B2_n_linear <- data.frame(
  "a0" = coef(reg_B2_n_linear)[1],
  "a1" = coef(reg_B2_n_linear)[2],
  row.names = NULL )
tab_coef_B2_n_linear

## Agora une-se as duas tabelas de coeficientes,
## obtendo-se a tabela de coeficientes do modelo de Clutter:
tab_coef_clut_rbase <- cbind(tab_coef_V2, tab_coef_B2_n_linear)
tab_coef_clut_rbase

## Obs: Caso o objetivo seja ajustar o modelo modificado de Clutter,
## basta realziar as alterações na hora de realizar o ajuste.
##

## ## 5.2) Ajuste do modelo pelo metodo MQ2E (2SLS) - systemfit ####

## Primeiro visualiza-se os dados que serao utilizados:
head(dados_form_est)

## Para se realizar este ajuste, sera utilizada a funcao systemfit,
## do pacote de mesmo nome.
## Nela deve-se definir o sistema de equacoes que sera utilizado no ajuste,
## e os instrumentos.
## Para definir o sistema, cria-se dois objetos representando as duas equacoes,
## e salva-se em uma lista:
eq1 <- Y2 ~ X4 + X5 + Y1
eq2 <- Y1 ~ X1 + X2 + X3
system <- list(Volume = eq1, AreaBasal = eq2)

## Da mesma forma, os instrumentos sao salvos em um objeto separado:
inst <- ~ X4 + X5 + X1 + X2 + X3

## Com estes objetos definidos, e possivel se ajustar o modelo:
systemfit(system, "2SLS", inst = inst, data = dados_form_est)
## Porem, observa-se que foram gerados 8 coeficientes, quando o objetivo
## seriam apenas 6. Isto porque e preciso especificar que deseja-se zerar
## o coeficiente do intercepto, e fazer com que o coeficiente de X1 seja 1.
##
## Para isso, esta funcao possui a entrada de matrizes de restricao linear,
## que permitem ao usuario criar restricoes ao ajuste.
## A matriz deve ser composta de j x k, onde
## j representa o numero de restricoes, e k o numero de coeficientes.
## Neste caso, iremos realizar 2 restricoes, e temos 8 coeficientes, 
## portanto, cria-se uma matriz preenchida por zeros, de 2x8:
restrict <- matrix(0, nrow=2, ncol=8)

## Zeros significam que o coeficiente nao sera alterado.
## Portanto altera-se as posicoes 1x5 e 2x6 para 1, para que
## sejam impostas restricoes nos coeficientes 5 e 6, como desejado:
restrict[1,5] <- 1
restrict[2,6] <- 1

restrict
## Deve ser definido tambem um vetor que contem j elementos;
## por padrao ele e consistido por zeros. Neste caso,
## ele deve ser composto por 0 e 1, pois deseja-se que o 
## segundo coeficiente seja 1. (nao tenho certeza)
restrict.rhs <- c(0, 1)
restrict.rhs

## Agora ja e possivel realizar o ajuste:
reg_clutter <- systemfit(system, "2SLS", inst = inst, data = dados_form_est, 
                 restrict.matrix = restrict, 
                 restrict.rhs = restrict.rhs)

## Observa-se que o intercepto da equacao de area basal tem seu valor
## bem proximo de zero, e X1 tem valor 1, assim como desejado:
summary(reg_clutter)

## E interessante se realizar o teste de Durbin-Watson
## ele testa a hiptose H0 de que nao ha autocorrelacao entre os erros:
dwtest(eq1, data = dados_form_est)
dwtest(eq2, data = dados_form_est)

## Agora basta criar uma tabela de coeficientes, denomimando-os de acordo;
## serao selecionados todos os coeficientes, menos 5 e 6, ja que foram restringidos
## no ajuste:

tab_coef_clut <- data.frame("b0" = coef(reg_clutter)[1],
                            "b1" = coef(reg_clutter)[2],
                            "b2" = coef(reg_clutter)[3],
                            "b3" = coef(reg_clutter)[4],
                            "a0" = coef(reg_clutter)[7],
                            "a1" = coef(reg_clutter)[8], 
                       row.names = NULL )

tab_coef_clut

## Obs: Caso o objetivo seja ajustar o modelo modificado de Clutter,
## além de se realizar as alterações nos modelos, deve-se alterar o 
## número de colunas da matriz de restrição.
##

## # 6) Definicao das classes ####

## O primeiro passo e calcular o site medio,
## e anexa-lo aos dados originais;
## feito isso, arredonda-se os dados para 4 casas decimais,
## e organiza-se os dados de menor para maior com base no site medio.
dados_class <- dados %>% 
  group_by(Parcela) %>% 
  summarise(Site_medio = mean(Site) ) %>% 
  left_join(dados,., by = "Parcela") %>%
  round(4) %>% 
  arrange(Site_medio)
head(dados)

## Serao utilizadas 3 classes, inferior media e superior (1, 2 e 3).
## Entao define-se o numero de classes que sera utilizado:
nc <- 3

## cria-se uma lista com o numero de elementos correspondente ao numero de classes
list <- vector("list", nc)

## define-se o intervalo que sera utilizado no calculo
intervalo <- (max(dados_class$Site_medio) - min(dados_class$Site_medio) ) / nc  

## cria-se o primeiro intervalo da tabela de classe;

## a tabela de classe possui 3 colunas:
## primeiro o intervalo inferior,
## segundo o intervalo inferior,
## terceiro o centro de classe.

list[[1]] <- c(min(dados_class$Site_medio), 
               min(dados_class$Site_medio) + intervalo , 
               mean( c(min(dados_class$Site_medio), 
                       min(dados_class$Site_medio) + intervalo  ) ),
               1)       

## com o loop for cria-se os demais intervalos;
## o loop vai da segunda linha da lista, ate a ultima linha,
## que corresponde ao numero de classes utilizado.

## o loop cria as 3 colunas citadas anteriormente, seguindo o padrao:

## primeiro seleciona-se o intervalo superior da classe anterior, 
## ou seja, linha anterior [i-1] posicao 2 [2];
## segundo seleciona-se o mesmo item selecionado anteriormente, e adiciona-se
## o intervalo de classe;
## terceiro cria-se o centro de classe.
for(i in 2:nc){
  
  list[[i]] <- c(list[[i-1]] [[2]],  
                 list[[i-1]] [[2]] + intervalo , 
                 mean( c(list[[i-1]][[2]],  
                         list[[i-1]][[2]] + intervalo ) ),
                 i )
  
}

## transformacao da lista em matriz e em seguida em data frame
classe <- data.frame(do.call(rbind, list))

## renomear
names(classe) <- c("inf", "sup", "cc", "categoria")

## Criar as classes com funcao composta:
classe <- class_table(dados_class, "Site_medio", 3)

## Cria-se uma coluna adicional, com a classificacao em forma de caractere:
classe$categoria_ <- c("Inferior", "Media", "Superior")
classe

## Para se anexar as classes aos dados originais, compara-se o
## site medio com os intervalos superiores das classes.
## Para isso, sera utilizado o loop while, em conjunto com o loop for.
##
## Primeiro, cria-se duas listas com o tamanho do dataframe:

list1 <- vector("list", nrow(dados_class)  )
list2 <- vector("list", nrow(dados_class)  )

## Serao utilizadas duas listas, pois serao gerados dados de classes diferentes,
## como eles serao gerados em vetores, nao podem ser misturados.
## Se numeros e caracteres sao utilizados no mesmo vetor, todos viram caracteres.

## Em seguida inicia-se o vetor do loop while em 1:
i <- 1

## Agora, os loops:
# Para (loop for) cada classe (1, 2 e 3), faz-se o loop while.
for(j in 1: nrow(classe)){
  
  # enquanto a classe j for maior ou igual que o site medio i,
  while(classe$sup[j] >= dados_class$Site_medio[i]){
    
    # insere-se o intervalo j e a categoria j em um elementos i das listas
    
    list1[[i]] <- classe$sup[j];
    
    list2[[i]] <- c(classe$categoria[j],
                    classe$categoria_[j] ) ; 
    
    # aumentar i, ou seja, passar para o proximo site medio
    i <- i+1;
    
    # parar quando acabar o dataframe
    if (is.na(dados_class$Site_medio[i] ) ) break
    
  }
  
}

## Agora converte-se as listas em data frames:
aux1 <- data.frame(do.call(rbind, list1))
aux2 <- data.frame(do.call(rbind, list2))

## O ultimo passo e unir os dois dataframes gerados em um novo,
## dar nome as variaveis, e adicionar os resultados aos dados:

aux3 <- cbind(aux1, aux2)
names(aux3) <- c("Intervalo", "Categoria", "Categoria_")

dados_class <- cbind(dados_class, aux3)

head(dados_class)

## # 7) Estimar B1, B2, V2, ICM, IMM e ITC ####

## O primeiro passo e estimar o site medio e a area basal media
## para cada categoria (caso seja utilizado o passo 7.2): 
tab_site_medio <- dados_class %>%
  group_by(Categoria_) %>% 
  summarise(Site = mean(Site_medio), B_MEDIO = mean(B) )
tab_site_medio

## Utilizando merge, adiciona um vetor que vai de 20:125,
## representando a idade, para cada site medio;
## Em seguida, organiza-se os dados de acordo com a categora:
dados_est_ <- merge(tab_site_medio, data.frame(Idade = 20:125) ) %>% 
  arrange(Categoria_)
head(dados_est_)

## ## 7.1) Definicao da area basal 1 e 2 - b1 modelo ####

## Para se estimar a area basal utilizando a equacao do sistema
## de Clutter, precisa-se de um valor de area basal inicial, ja que na equacao
## utiliza-se de B1 e B2 nos calculos. Entao primeiro estima-se uma area basal
## com um modelo baseado apenas no site, e dai pra frente se estima utilizando
## a equacao de Clutter.

## Primeiro cria-se uma tabela com os coeficientes 
## para se estimar o primeiro valor de B2. Utiliza-se os dados originais:

dados$Site_quad <- dados$Site^2
reg_B2_inicial <- lm(B ~ Site + Site_quad, dados)
summary(reg_B2_inicial)

tab_coef_B2 <- data.frame(b0 = coef(reg_B2_inicial)[[1]],
                          b1 = coef(reg_B2_inicial)[[2]],
                          b2 = coef(reg_B2_inicial)[[3]] )


## O processo de estimacao sera feito utilizando um loop for.
## Sendo assim, o primeiro passo e criar uma lista fora do loop:
list2 <- vector("list", length = nrow(dados_est_))

## Em seguida, estima-se o primeiro valor de area basal,
## utilizando a equcao que se baseia no site:
list2[[1]] <- log(tab_coef_B2$b0 + 
                   tab_coef_B2$b1*dados_est_$Site[1] + 
                   tab_coef_B2$b2*(dados_est_$Site[1]^2) )

## e feito um log do resultado, pois no modelo de Clutter
## a variavel utilizada e Ln(B1).
##
## Agora ja e possivel estimar utilizando o modelo Clutter, dentro do loop.
## Existem 3 condicionais no loop: 
##
## 1. Primeiro: Inserir NA caso o proximo dado nao exista, ou quando se trocar de classe.
## Isso evita que os dados de classes diferentes nao sejam misturados na hora do calculo.
##
## 2. A primeira area basal de cada classe deve ser calculada utilizando o modelo baseado no site,
## para isso utiliza-se a segunda condicao. Isso e verificado checando se a elemento anterior e NA;
## Se ele for NA, quer dizer que houve troca de Categoria, ou seja, este e o primeiro dado da Categoria, portanto,
## deve-se fazer o calculo com esse modelo.
## Caso contrario, ou seja, se os dados forem da mesma categoria, realizar o calculo
## utilizando o modelo de Clutter para Area Basal.

for(i in 2:nrow(dados_est_)){
  
  
  if(is.na(dados_est_$Categoria_[i+1]) | dados_est_$Categoria_[i] != dados_est_$Categoria_[i+1] ){
    
    list2[[i]]  <- NA
    
    
  }else if(is.na(list2[[i-1]]) ){
    
list2[[i]] <-     log(tab_coef_B2$b0 + 
                        tab_coef_B2$b1*dados_est_$Site[i] + 
                        tab_coef_B2$b2*(dados_est_$Site[i]^2) )

  } else{
    
    list2[[i]]  <- list2[[i-1]] * (dados_est_$Idade[i] / dados_est_$Idade[i+1] )  + 
      tab_coef_clut$a0 * (1 - (dados_est_$Idade[i] / dados_est_$Idade[i+1]  ) ) + 
      tab_coef_clut$a1 * (1 - (dados_est_$Idade[i] / dados_est_$Idade[i+1]  ) ) * dados_est_$Site[i]  
    
    
    
  }

}

## Agora converte-se a lista em um vetor, e salva-se o vetor como
## uma variavel no dataframe:
dados_est_$LN_B2_EST <- as.vector(do.call(rbind, list2))
head(dados_est_)

## ## 7.2) Definicao da area basal 1 e 2 - B1 media ####

## Para se estimar a area basal utilizando a equacao do sistema
## de Clutter, precisa-se de um valor de area basal inicial, ja que na equacao
## utiliza-se de B1 e B2 nos calculos. Neste caso sera utilizada
## a area basal media calculada no passo 7:

## O processo de classificacao sera feito utilizando um loop for.
## Sendo assim, o primeiro passo e criar uma lista fora do loop:
list2 <- vector("list", length = nrow(dados_est_))

## Em seguida, insere-se o primeiro valor de area basal,
## utilizando a area basal media. como ela se repete ao longo de toda a classe,
## qualquer linha (desde que seja daquela classe) tera o seu valor.
## No caso incia-se com a classe baixa, entao a primeira linha ira conter o seu valor:
head(dados_est_)

list2[[1]] <- log( dados_est_$B_MEDIO[1] )

## e feito um log do resultado, pois no modelo de Clutter
## a variavel utilizada e Ln(B1).
##
## Agora ja e possivel estimar utilizando o modelo Clutter, dentro do loop.
## Existem 3 condicionais no loop: 
##
## 1. Primeiro: Inserir NA caso o proximo dado nao exista, ou quando se trocar de classe.
## Isso evita que os dados de classes diferentes nao sejam misturados na hora do calculo.
##
## 2. A primeira area basal de cada classe deve ser calculada utilizando o modelo baseado no site,
## para isso utiliza-se a segunda condicao. Isso e verificado checando se a elemento anterior e NA;
## Se ele for NA, quer dizer que houve troca de Categoria, ou seja, este e o primeiro dado da Categoria, portanto,
## deve-se utilizar a area basal media daquela classe.
## Caso contrario, ou seja, se os dados forem da mesma categoria, realizar o calculo
## utilizando o modelo de Clutter para Area Basal.

for(i in 2:nrow(dados_est_)){
  
  
  if(is.na(dados_est_$Categoria_[i+1]) | dados_est_$Categoria_[i] != dados_est_$Categoria_[i+1] ){
    
    list2[[i]]  <- NA
    
    
  }else if(is.na(list2[[i-1]]) ){
    
    list2[[i]] <-     log( dados_est_$B_MEDIO[i] )
    
  } else{
    
    list2[[i]]  <- list2[[i-1]] * (dados_est_$Idade[i] / dados_est_$Idade[i+1] )  + 
      tab_coef_clut$a0 * (1 - (dados_est_$Idade[i] / dados_est_$Idade[i+1]  ) ) + 
      tab_coef_clut$a1 * (1 - (dados_est_$Idade[i] / dados_est_$Idade[i+1]  ) ) * dados_est_$Site[i]  
    
  }
  
}

## Agora converte-se a lista em um vetor, e salva-se o vetor como
## uma variavel no dataframe:
dados_est_$LN_B2_EST <- as.vector(do.call(rbind, list2))
head(dados_est_)


## ## 7.3) Estimacao de B2, V2, ICM e IMM ####

## No proximo passo remover-se os NAs, adiciona-se os coeficientes
## das equacoes de Clutter, e estima-se o volume, seguido do
## incremento corrente mensal e incremento medio mensal:
dados_est <- dados_est_ %>% 
  cbind(tab_coef_clut) %>% 
  group_by(Categoria_) %>% 
  mutate(
    B2_EST = exp(LN_B2_EST),
    V2_EST = exp(b0 + 
                (b1 * 1 / Idade) + 
                 b2 * Site + 
                 b3 * LN_B2_EST  ),
    ICM = abs(V2_EST - lag(V2_EST) ),
    IMM = V2_EST/ Idade,
    ICM_IMM = ICM - IMM) 
dados_est

## ## 7.4) Idade Tecnica de Corte e tabela de resultados ####

## Para se encontrar a idade tecnica de corte, basta fazer o seguinte:
dados_est %>% 
  group_by(Categoria_) %>% 
  filter(round(ICM,1) == round(IMM,1) ) %>% 
  select(ITC = Idade, ITC_Y = IMM) %>% 
  summarise_all(mean)

## Adiciona-se a ITC aos dados:
dados_est <- dados_est %>% 
  filter(round(ICM,1) == round(IMM,1) ) %>% 
  select(Categoria_, ITC = Idade, ITC_Y = IMM) %>% 
  summarise_all(mean) %>% 
  left_join(dados_est, by = "Categoria_")
dados_est

## Agora gera-se tabela final com informacoes por classe de
## area basal, idade tecnica de corte, site,
## e volume total:

B2_Inicial <- dados_est %>% 
  group_by(Categoria_) %>% 
  filter(row_number()==1 ) %>%
  select(Categoria_, B2_Inicial = B2_EST) 

ITC <-  dados_est %>% 
  group_by(Categoria_) %>% 
  filter(round(ICM,1) == round(IMM,1) ) %>% 
  summarise(ITC  = mean(Idade),
            Site = mean(Site) )

sum_V <- dados_est %>% 
  group_by(Categoria_) %>% 
  summarise(V_total = sum(V2_EST, na.rm=T))

## Apos gerar-se as variaveis, basta junta-las com left_join:
tab_final <- left_join(B2_Inicial, ITC) %>% 
  left_join(sum_V)
tab_final
  
## # 8) Graficos de Incremento Corrente Mensal e Incremento Medio Mensal ####

## Com os valores de ICM e IMM obtidos,
## e necessario te-los na mesma coluna, sendo identificados
## por um fator, para que se possa fazer os graficos de linha.
## Para isso utiliza-se a funcao gather:
dados_graph <- dados_est %>% 
  na.omit() %>% 
  select(Categoria_, ICM, IMM, Idade, ITC, ITC_Y) %>% 
  gather(Indice,Valor, ICM, IMM)
dados_graph


## Agora, plota-se o grafico com ggplot, utilizando a variavel Indice como cor,
## para que se diferencie os indices. 
## Alem disso, utiliza-se facet_wrap para gerar um grafico para cada classe:
#+ fig.width = 14, fig.height = 8
graph <- ggplot(dados_graph, aes( x = Idade, y = Valor, color = Indice ) ) + 
  facet_wrap(~Categoria_) + 
  geom_line(size = 1.5) +
  labs(x = "Idade (meses)",
       y = "Indice",
       color = "Indice") +
  theme( legend.position = "bottom" ,
         legend.title = element_text(size=10, 
                                     face="bold"),
         legend.text = element_text(size=10),
    axis.title   = element_text(size = 14), 
    axis.text    = element_text(size = 12),
    strip.text.x = element_text(size = 16) )
graph

## Pode-se adicionar a idade tecnica de corte ao grafico:
#+ fig.width = 14, fig.height = 8
graph2 <- ggplot(dados_graph, aes( x = Idade, color = Indice ) ) + 
  facet_wrap(~Categoria_) + 
  geom_line(aes(y = Valor), size = 1.5) +
  geom_point(aes(x = ITC, y = ITC_Y), color = "black")+
  geom_text(aes(x = ITC, y = ITC_Y, label = ITC),
            vjust = 0, 
            nudge_y = 0.2, 
            color = "black", size = 5)+
  labs(x = "Idade (meses)",
       y = "Indice",
       color = "Indice") +
  theme( legend.position = "bottom" ,
         legend.title = element_text(size=10, 
                                     face="bold"),
         legend.text = element_text(size=10),
         axis.title   = element_text(size = 14), 
         axis.text    = element_text(size = 12),
         strip.text.x = element_text(size = 16) )
graph2

## Exporta-se o grafico com ggsave:
ggsave("graph_itc.png",  graph,  width = 14, height = 8)
ggsave("graph_itc2.png", graph2, width = 14, height = 8)
