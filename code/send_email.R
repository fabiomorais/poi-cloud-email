library(gmailr)
library(readr)
library(stringr)
library(dplyr)

args = commandArgs(trailingOnly=TRUE)

cloud_addr <- "https://cloud6.lsd.ufcg.edu.br"
cloud_domain <- "lsd"
aws_secret_file <- "data/client_secret_385936253850-lm98muiqpglpgtot43f39v6i9cavs12e.apps.googleusercontent.com.json"
users_data_file <- "data/user_data.csv"

generate_body <- function(name, username, password, alternative=FALSE) {
  
  text_body <- ""
  
  if(alternative) {
  
    text_body <- paste(c("Olá ", name, ", 
  
Para a disciplina Provisionamento e Operação de Infraestrutura você deve usar a sua conta ativa no LSD. Por favor teste o acesso em: 

  - Endereço: ", cloud_addr, "
  - Domínio: ", cloud_domain, "
  
Nos avise em caso de problemas. 

Att. 
                       
Fabio Morais"), collapse ="")
  
  } else {
 
    text_body <- paste(c("Olá ", name, ", 
  
Seguem as informações de acesso à Cloud do LSD no contexto da disciplina Provisionamento e Operação de Infraestruturas:

  - Endereço: ", cloud_addr, "
  - Domínio: ", cloud_domain, "
  - Usuário: ", username, "
  - Senha: ", password, " 
               
Por favor teste o acesso e entre em contato em caso de problemas.
                       
Att. 
                       
Fabio Morais"), collapse ="")
}

return(text_body)

}

generate_email <- function(body, email) {
  
  test_email <-
    gm_mime() |>
    gm_to(email) |>
    gm_from("fabio@computacao.ufcg.edu.br") |>
    gm_subject("POI - Acesso Cloud LSD") |>
    gm_text_body(body)
  return(test_email)
}

gm_auth_configure(path = aws_secret_file)
gm_oauth_client()

df_alunos <- read_csv(users_data_file) %>% 
  select(nome_completo = nome,
         matricula = `matrícula`,
         email = email,
         username = username, 
         passwd = password) %>% 
  rowwise() %>% 
  mutate(nome_completo = str_to_title(nome_completo)) %>% 
  mutate(nome = paste(strsplit(nome_completo, split=" ")[[1]][1:2], collapse = " ")) %>%
  filter(email != "---")

# Teste
# df_alunos <- data.frame(nome="Fabio", username="fabio", passwd=NA, email="fabio.jorge@gmail.com")

for(i in 1:nrow(df_alunos)) {
  aluno <- df_alunos[i,]
  body <- generate_body(aluno$nome, aluno$username, aluno$passwd, is.na(df_alunos[i,]$passwd))
  email <- generate_email(body, aluno$email)
  gm_send_message(email)
}
