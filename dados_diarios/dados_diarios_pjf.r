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


# Separando Linhas --------------------------------------------------------

#Separando Cada Linha do PDF que corresponde a um caso
obitos_diarios_pjf_separado<- obitos_diarios_pjf_raw %>%
  mutate(texto= str_split(texto, "\\r\\n\\d+\\. "))%>%
    unnest(texto)

obitos_diarios_pjf_separado %>%# slice(1000:1578) %>%
  pull(texto) 

# -------------- TIDYING ----------------------------------------------------

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


#Portanto, como primeiro passo para transformar isso em uma tabela tidy:
# Vamos separar uma variável por coluna.


# Separando Colunas -------------------------------------------------------



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
         comorbidade = str_remove(comorbidade, "comorbidade(s)*(:|,)|comorbidade(s)*/fator de risco:") %>%
                                                str_squish(),
         comorbidade = str_remove(comorbidade, "(\\.|,|;)$"))
  
  
  

# Padronizando Colunas ----------------------------------------------------


# De posse das mesmas variáveis, o próximo passo é padronizar a forma com que elas aparecem ao longo da tabela.

# Nas DOENÇAS, temos várias abreviações e vários nomes completos

#Sigla DNC é Doença Neural Crônica, DRC é Doença Renal Crônica etc.
# As siglas foram todas passadas  para seus nomes completos para serem mais facilmente entendidos.

# No campo das DATAS, temos vários padrões:

# Em números, temos "dd/mm/yy", "dd/mm/yyyy", "dd/mm", "d/mm", "dd/m" e "ddmm".
# Por extenso, "dd de mes por extenso" e incríveis "morreu no último domingo, dd".
# A meta é passar tudo para yyyy-mm-dd como objeto de data.
# nos números faltantes sem meses e sem anos, como a base é organizad por dia,
# a linha de cima provavelmente é o mesmo mês do registro. Nesse caso, farei um fill("down")
# para lidar com as faltas

#Tentei fazer essa função para lidar com números por extenso, infelizmente não funcionou.
#terá que ser por vez no meio do código

nomeMes_to_nMes <- function(x){
  
  mutate(
    x=stringr::str_replace(x, "janeiro", "01"),
    x=stringr::str_replace(x, "fevereiro", "02"),
    x=stringr::str_replace(x, "março", "03"),
    x=stringr::str_replace(x, "abril", "04"),
    x=stringr::str_replace(x, "maio", "05"),
    x=stringr::str_replace(x, "junho", "06"),
    x=stringr::str_replace(x, "julho", "07"),
    x=stringr::str_replace(x, "agosto", "08"),
    x=stringr::str_replace(x, "setembro", "09"),
    x=stringr::str_replace(x, "outubro", "10"),
    x=stringr::str_replace(x, "novembro", "11"),
    x=stringr::str_replace(x, "dezembro", "12"))
  
}

# no Gênero, temos dois gêneros com várias caligrafias diferentes para masculino e feminino, que você pode ver pela função abaixo.

table(obitos_diarios_pjf_clean$genero)

# esses dois vetores servem para usar na hora de padronizar a coluna do gênero.

genero_feminino <- c( "feminino", "feminina", "idosa", "isosa", "mulher")
genero_masculino <- c( "homem", "idoso", "isoso", "masculino", "natimorto")

# Começando os trabalhos de padronização 

obitos_diarios_pjf_tidy <- obitos_diarios_pjf_clean %>%
  #Com as variáveis já separadas, podemos retirar a coluna original do texto.
  select(!texto) %>%
  #Padronizando GÊNERO
  mutate(genero= case_when(
    genero %in% genero_feminino ~ "feminino",
    genero %in% genero_masculino ~ "masculino",
    TRUE ~ as.character(genero)),
  #padronizando COMORBIDADES. Criei coluna comorbidade e comorbidades para ter controle do que fazia
        comorbidades= str_replace_all(comorbidade, "drc\\b", "doença renal crônica"),
        comorbidades= str_replace_all(comorbidades, "dcc", "doença cardiovascular crônica"),
        comorbidades= str_replace_all(comorbidades, "has|hás|hs", "hipertensão arterial sistêmica"),
        comorbidades= str_replace_all(comorbidades, "dm|diabetes mel{1,2}itus", "diabetes"),
        comorbidades= str_replace_all(comorbidades, "dpoc", "doença pulmonar obstrutiva crônica"),
        comorbidades= str_replace_all(comorbidades, "\\bca\\b", "câncer"),
        comorbidades= str_replace_all(comorbidades, "dnc", "doença neurológica crônica"),
        comorbidades= str_replace_all(comorbidades, "outra pneumopatia", "pneumopatia"),
        comorbidades= str_replace_all(comorbidades, "iam", "infarto agudo do miocárdio"),
        comorbidades= str_replace_all(comorbidades, "ave", "acidente vascular encefálico"),
        comorbidades= str_replace_all(comorbidades, "\\bfa\\b", "fibrilação atrial"),
        comorbidades= str_replace_all(comorbidades, "\\btu\\b", "tumor"),
        comorbidades= str_replace_all(comorbidades, "ave", "acidente vascular encefálico"),
        comorbidades= str_replace_all(comorbidades, "dvc", "doença venosa crônica"),
        comorbidades= str_replace_all(comorbidades, "tep", "trombo embolismo pulmonar"),
  #arrumando delimitadores
        comorbidades= str_replace_all(comorbidades, " e |/", ","),
  #retirando siglas dentro de parentêses
        comorbidades= str_remove_all(comorbidades, "\\(.+\\)"),
  #nesting
        comorbidades = str_split(comorbidades,","),
  #padronizado DATAS, primeiro as datas que estão escritas por extenso para texto
  #tentei usar função mas não funcionou
      data_obito= stringr::str_replace(data_obito, "janeiro", "01"),
      data_obito=stringr::str_replace(data_obito , "fevereiro", "02"),
      data_obito=stringr::str_replace(data_obito , "março", "03"),
      data_obito=stringr::str_replace(data_obito , "abril", "04"),
      data_obito=stringr::str_replace(data_obito , "maio", "05"),
      data_obito=stringr::str_replace(data_obito , "junho", "06"),
      data_obito=stringr::str_replace(data_obito , "julho", "07"),
      data_obito=stringr::str_replace(data_obito , "agosto", "08"),
      data_obito=stringr::str_replace(data_obito , "setembro", "09"),
      data_obito=stringr::str_replace(data_obito , "outubro", "10"),
      data_obito=stringr::str_replace(data_obito , "novembro", "11"),
      data_obito=stringr::str_replace(data_obito , "dezembro", "12"),
      data_obito=stringr::str_replace(data_obito , " de ", "/"),
  # retirando o ultimo caso por extenso, de "morreu no ultimo domingo, e lidando com "dd/mm/yy" para "dd/mm/yyyy"
      data_obito= case_when(
          str_detect(data_obito, "/21\\b") ~ str_replace(data_obito,"/21\\b","/2021"),
          str_detect(data_obito, "/21\\b") ~ str_replace(data_obito,"/20\\b","/2020"),
          str_detect(data_obito, "morreu") ~ str_extract(data_obito, "\\d+"),
          TRUE ~ data_obito),
      tamanho_data= str_length(data_obito),
  #Preparando a coluna ano para lidar com números faltantes
        ano= str_extract(data_obito,"(?=/\\d{4,4})"))

          #str_length(data_obito) == 4 ~ paste(str_extract(data_obito,"^\\d{2,2}"),
           #                                   str_extract(data_obito,"\\d{2,2}$"),
            #                                  sep= "/"))
        #ano = str_extract(data_obito, "")
  

                              "|/20\\b"
          "\\d+/\\d+(/\\d+)*"
          "\\d{4,4}"
          "\\d{1,2} de \\w*"
          "morreu.*\\d+"
        

table(obitos_diarios_pjf_tidy$genero)




# filtros e Classificações  ----------------------------------------------------



# Modelos  ----------------------------------------------------



# Visualização ----------------------------------------------------

#Mortes Diárias




# Exportação ----------------------------------------------------
