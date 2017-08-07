
library(forestr)
data("ex16_mfr")
data("ex17_mfr")

# Primeiro iremos estimar o Site utilizando para a idade indice de 64 meses:
Idade_I <- 64

dados <- ex16_mfr %>% 
  lm_table(log(HD) ~ inv(Idade), output = "merge") %>% 
  mutate(S = exp(log(HD) - b1 * (1/Idade - 1/Idade_I))  ) %>% 
  select(-b0, -b1, -Rsqr, -Rsqr_adj, -Std.Error)
dados 

# Agora para rodar o modelo de Clutter basta inserir os nomes das variáveis
# Idade, altura dominante, area basal, volume, site e parcela:
coefs_clutter <- fit_clutter(dados, "Idade", "HD", "B", "V", "S", "Parcela")
coefs_clutter

coefs_clutter <- fit_clutter(dados, Idade, HD, B, V, S, Parcela)
coefs_clutter


library(forestr)
fit_clutter(dados, Idade, HD, B, V, S, Parcela)
struct_form(dados, Idade, HD, B, V, S, Parcela)

# Caso os seus dados ja estejam no formato estrutural, basta utilizar
# o argumento struct_form_df para realizar o ajuste:
ex17_mfr

coefs_clutter <- fit_clutter(ex17_mfr, struct_form_df = T)
coefs_clutter
#obs: Neste caso, a nomemclatura deve ser respeitada
