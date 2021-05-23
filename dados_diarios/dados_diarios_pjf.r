##### JF EM DADOS - Coronavírus Casos e Óbitos ######
#Dados prefeitura Metadados - https://covid19.pjf.mg.gov.br/arquivos/obitos_covid19_jf.pdf
#  Autor: Marcello Filgueiras  

library(tidyverse)

#Trata-se da base de "metadados" dos óbitos de covid em Juiz de Fora.
# Só há divulgação total de mortos em XLS. Os dados permenoraizados estão nesse pdf.

# A estrutura parece de um csv, mas está longe disso.
# O separador de cada linha as vezes é um ";" "." ou até casos de nada.
# Dentro de cada linha, o separador de colunas as vezes é ".", "," ou apenas um espaço.

# Importação ----------------------------------------------------
library(pdftools)

#No documento inicial, cada linha deveria ser um paciente. Foi feito apenas uma separação com "\\n" no início.
#Mas linhas muito grandes que passam da margem eram divididas em duas, do mesmo caso.
#Assim optei pela separação de cada linha pelos números iniciais, que são "1.", "2.", "3.", "100."
#Mas  "d+\\." isso pegava também datas que estavam separadas por pontos nas frases
#por isso, optei pela divisão de cada caso por "\\r\\n\\d+\\."
#Ela sempre pega o Enter de uma nova linha, que se for um novo óbito, tera números e um ponto.



obitos_diarios_pjf_raw <- tibble(
  texto= pdftools::pdf_text("municipal/dados_diarios/data_raw/obitos_covid19_jf.pdf"))

obitos_diarios_pjf_raw %>% slice(1) %>% pull(texto)                                    


#Separando Cada Linha do PDF que corresponde a um caso
obitos_diarios_pjf_separado<- obitos_diarios_pjf_raw %>%
  mutate(texto= str_split(texto, "\\r\\n\\d+\\. "))%>%
    unnest(texto)

obitos_diarios_pjf_separado %>%# slice(1000:1578) %>%
  pull(texto) 

# Tidying ----------------------------------------------------

# Em tese teríamos 4 colunas, c( "genero", "idade", "data_do_obito" e "comorbidades")
#Temos problemas em todas essas colunas:
#gênero: Não só dois, temos 6 gêneros: "idoso", "idosa", "masculino", "feminino", "homem" e "mulher"
#idade: "dd anos"
#data: temos dd/mm/aaaa, dd/mm/aa, dd/mm, "dd de mes" e incríveis "ultimo domingo, 22"
#comorbidade: Se não tem nehuma, está "Sem Comorbidade Relatada". Se não, começa com "Comorbidade: e proximas doenças..."

# mas simplesmente separar essas colunas pelo delimitador não é possível.
# Ora estamos com a ordem de c( "genero", "idade", "data_do_obito" e "comorbidades")
#ora estamos com a ordem de c( "genero", "idade", "data_do_obito" e "comorbidades")

#Temos um caso de 5 colunas:  se_é_idoso, genero, idade, data_do_obito e comorbidades
#como "idoso, masculino, 88 anos. óbito em 05/07/2020. comorbidades:dcc, etc."


obitos_diarios_pjf_clean<- obitos_diarios_pjf_separado %>%
  #Retirando alguns residuos do PDF que restaram na linha
  mutate(texto=str_remove(texto, "^\\d+\\. |1304") %>%
                                   str_to_lower(),
         texto= str_remove_all(texto, "\\r\\n")%>%
           str_trim()) %>%
  #o gênero está na ordem, então aqui um separate funciona.
  tidyr::separate(col=texto,
                  into= c("genero", "texto"),
                  sep = ",|\\.| ",
                  extra= "merge")%>%
  # seprando idade. Vai dar problema com uma coluna a mais e casos de natimortos.
  #mutate(texto= str_trim(texto)) %>%
  #tidyr::separate(col=texto,
                  #into= c("idade", "texto"),
                  #sep = ",|\\.| ",
                  #extra= "merge")
  #Buscando idade com str_extract.
  mutate(idade = case_when(
                 genero== "natimorto" | str_detect(texto,"natimorto") ~ "0",
                 str_detect(texto,"01 dia") ~ "0",
                 TRUE ~ str_extract(texto, "\\d+"))%>%
                as.numeric(),
         #buscando data do obito em varios formatos
         data_obito = str_extract(texto, paste("\\d+/\\d+(/\\d+)*", 
                                               "\\d{4,4}",
                                               "\\d{1,2} de \\w*",
                                               "morreu.*\\d+",
                                               sep = "|")),
         comorbidade= case_when(
           str_detect(texto,"sem comorbidade") ~ "sem comorbidade",
                                          TRUE ~ str_extract(texto, "comorbidade(s)*.+")),
         comorbidade = str_remove(comorbidade, paste("óbito.+" ,
                                                      "obito.+" ,
                                                      "óbito$",
                                                      sep= "|")),
         comorbidade = str_remove(comorbidade, "comorbidade(s)*(:|,)") %>%
                                                str_squish(),
         comorbidade = str_remove(comorbidade, "(\\.|,|;)$"))
  
  
  
  
  
           
          

# filtros e Classificações  ----------------------------------------------------



# Modelos  ----------------------------------------------------



# Visualização ----------------------------------------------------



# Exportação ----------------------------------------------------
