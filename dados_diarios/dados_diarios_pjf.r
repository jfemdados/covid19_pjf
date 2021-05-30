##### JF EM DADOS - Coronavírus Casos e Óbitos ######
#Metadados prefeitura de Juiz de Fora - https://covid19.pjf.mg.gov.br/arquivos/obitos_covid19_jf.pdf
#  Autor: Marcello Filgueiras  

library(tidyverse)

#Trata-se da base de "metadados" dos óbitos de covid em Juiz de Fora.
# Só há divulgação total de mortos em XLS, e não mostram a evolução deles, apenas o último número. Os dados permenorizados estão nesse pdf.

# Não se trata de uma base tidy. Como será demonstrado ao longo do código.
#A primeira questão é o formato em PDF, que dificulta o trabalho.
# Cada linha  é um caso.
# A estrutura parece de um csv, mas está longe disso.
# O separador de cada linha as vezes é um ";" "." ou até em alguns casos casos, nada "".
# Dentro de cada linha, o separador de colunas as vezes é ".", "," ou apenas um espaço.

# Importação ----------------------------------------------------
library(pdftools)


obitos_diarios_pjf_raw <- tibble(
  texto= pdftools::pdf_text("municipal/dados_diarios/data_raw/obitos_covid19_jf.pdf"),
  #criando divisão por páginas para ajudar com datas mais a frente
  pag= 1:43) # como automatizar a colocação desse número de páginas? Não quero ter que mudar toda vez que aumente o nº de pág.

obitos_diarios_pjf_raw %>% slice(1) %>% pull(texto)                                    


# Separando Linhas --------------------------------------------------------

#No documento inicial, cada linha deveria ser um paciente. Foi feito apenas uma separação com "\\n" no início.
#Mas linhas muito grandes que passam da margem eram divididas em duas linhas, representando o mesmo caso.
#Assim optei pela separação de cada linha pelos números iniciais, que são "1.", "2.", "3.", "100." ...
#Mas  "d+\\."  também pegava também datas que estavam separadas por pontos nas frases
#por isso, optei pela divisão de cada caso por "\\r\\n\\d+\\."
#Ela sempre pega o Enter de uma nova linha, que se for um novo óbito e terá logo após números e um ponto.

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
#data: temos dd/mm/aaaa, dd/mm/aa, dd/mm, "dd do mes" e incríveis "ultimo domingo, 22"
#comorbidade: Se não tem nenhuma, está "Sem Comorbidade Relatada" ou NA. Se tem, começa com "Comorbidade: nome da doença..."

# mas simplesmente separar essas colunas pelo delimitador não é possível.
# Ora estamos com a ordem de c( "genero", "idade", "data_do_obito" e "comorbidades")
#ora estamos com a ordem de c( "genero", "idade", "data_do_obito" e "comorbidades")
#Temos um caso de 5 colunas:  se_é_idoso, genero, idade, data_do_obito e comorbidades
#como "idoso, masculino, 88 anos. óbito em 05/07/2020. comorbidades:dcc, etc."


#Portanto, como primeiro passo para transformar isso em uma tabela tidy:
# Vamos separar uma variável por coluna, buscando cada uma dessas informações com str_extract.


# Separando Colunas -------------------------------------------------------



obitos_diarios_pjf_clean<- obitos_diarios_pjf_separado %>%
  #Retirando alguns resíduos do PDF que restaram na linha separada
  mutate(texto=str_remove(texto, "^\\d+\\.|1304") %>% #Limpando algarismos que começam no inicio do texto e são seguidos por "." . "1304 não tinha ponto e ficou para trás; 
           str_to_lower(),
         texto= str_remove_all(texto, "\\r\\n") %>%
           str_trim()) %>%
  #o gênero está na ordem, então aqui um separate funciona.
  tidyr::separate(col=texto,
                  into= c("genero", "texto"),
                  sep = ",|\\.| ",
                  extra= "merge")%>%
  # Separando idade. Vai dar problema com uma coluna a mais e casos de natimortos.
  #mutate(texto= str_trim(texto)) %>%
  #tidyr::separate(col=texto,
  #into= c("idade", "texto"),
  #sep = ",|\\.| ",
  #extra= "merge")
  #Buscando idade com str_extract e  padronizando a idade de bebês;
  mutate(idade = case_when(
    genero== "natimorto" | str_detect(texto,"natimorto") ~ "0", # ( idade de bebês = 0 ) =/
    str_detect(texto,"01 dia") ~ "0",  # (idade de bebês = 0)   =/ Triste
    TRUE ~ str_extract(texto, "\\d+"))%>%  #Extrai os primeiros algarismos. Como idade está na frente, não há problema
      as.numeric(),
    #buscando data do obito em varios formatos
    data_obito = str_extract(texto, paste("\\d+/\\d+(/\\d+)*", #dd/mm e dd/mm/yy e dd/mm/yyyy
                                          "\\d{4,4}",          #ddmm (sem separador)
                                          "\\d{1,2} de \\w*",  #dia em número e mês por extenso
                                          "morreu.*\\d+",      #"morreu no último domingo,dd"
                                          sep = "|")),
    #Buscando comorbidades
    comorbidade= case_when(
      str_detect(texto,"sem comorbidade") ~ "sem comorbidade", #buscando o que nao tem comorbidade
      TRUE ~ str_extract(texto, "comorbidade(s)*.+|fator de risco(s)*.+")), #buscando tudo que vem depois de comorbidades ou fator de risco
    comorbidade = str_remove(comorbidade, paste("óbito.+" ,    #Em algumas linhqs, comorbidades é a última coluna, como nas linhas acima
                                                "obito.+" ,    #nessas linhas vem "comorbidades", depois "óbito". 
                                                "óbito$",      #nesses casos, mandei tirar tudo que vem depois de óbito.
                                                sep= "|")),    #ai fica só as comorbidades
    comorbidade = str_remove(comorbidade, "comorbidade(s)*(:|,)|comorbidade(s)*/fator de risco:|fator de risco:") %>% #tendo certeza que tudo vem depois é comorbidade, mandei tirar as inciais de comorbidade
      str_squish(),
    #Falsos NA. Não indicavam a separação "comorbidade:" antes  e por isso vieram nulos, mas não eram NAs
    #Fiz na Mão
    comorbidade = ifelse(is.na(comorbidade),
                         str_extract(texto, "has, dnc, ca|dcc, dm|imunodeficiência, outra pneumopatia crônica"),
                         as.character(comorbidade)),
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


# no Gênero, temos dois gêneros com várias caligrafias diferentes para masculino e feminino, que você pode ver pela função abaixo.

table(obitos_diarios_pjf_clean$genero)

# esses dois vetores servem para usar na hora de padronizar a coluna do gênero.

genero_feminino <- c( "feminino", "feminina", "idosa", "isosa", "mulher")
genero_masculino <- c( "homem", "idoso", "isoso", "masculino", "natimorto")

# Começando os trabalhos de padronização 


#Padronizando Gênero e Comorbidades
obitos_diarios_pjf_tidy_gen_com <- obitos_diarios_pjf_clean %>%
  #Com as variáveis já separadas, podemos retirar a coluna original do texto.
  select(!texto) %>%
  #Padronizando GÊNERO
  mutate(genero= case_when(
    genero %in% genero_feminino ~ "feminino",
    genero %in% genero_masculino ~ "masculino",
    TRUE ~ as.character(genero)),
    #padronizando COMORBIDADES. Criei coluna comorbidade (original) e comorbidades (modificada) para ter controle do que fazia
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
    comorbidades= str_replace_all(comorbidades, "tb pulmonar", "tuberculose pulmonar"),
    #arrumando delimitadores
    comorbidades= str_replace_all(comorbidades, " e |/", ","),
    #retirando tudo que estava dentro de parentêses geralmente a sigla(explocação da sigla)
    comorbidades= str_remove_all(comorbidades, "\\(.+\\)"),
    #Todas as Comorbidades estão separadas por "," prontas para sofrerem unnest. Finalizando, retirando espaços.
    comorbidades = str_squish(comorbidades),
    #Arrumando NA e Sem Comorbidades
    comorbidades = str_replace(comorbidades, "comorbidades", "sem comorbidade"),
    comorbidades = tidyr::replace_na(comorbidades, "sem comorbidade"))

table(obitos_diarios_pjf_clean_gen_com$genero)


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

#Padronizado DATAS - 1º Passo: Passando as datas que estão escritas por extenso para número.
#tentei usar função mas não funcionou

obitos_diarios_pjf_tidy_datas_extenso<- obitos_diarios_pjf_tidy_gen_com %>%
  mutate(
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
    # retirando o ultimo caso por extenso, de "morreu no ultimo domingo" e anos yy para yyyy
    data_obito= case_when(
      str_detect(data_obito, "/21\\b") ~ str_replace(data_obito,"/21\\b","/2021"),
      str_detect(data_obito, "/20\\b") ~ str_replace(data_obito,"/20\\b","/2020"),
      str_detect(data_obito, "morreu") ~ str_extract(data_obito, "\\d+"),
      # Padronizando casos sem parentêses
      str_detect(data_obito, "^\\d{4,4}") ~ paste(str_extract(data_obito, "\\d{2,2}"),"/",str_extract(data_obito, "\\d{2,2}\\b")),
      TRUE ~ data_obito))



# 2º passo data - Lidando com Datas com 1º Algarismo faltante d/mm/aaaa ou dd/m/aaaa
#Separei as Datas em três colunas difrentes "dia", "mes", "ano" para facilitar na faxina.
#Com todos os numeros arrumados, juntei tudo. com unite.

#OBS: O Rstudio está com problema na hora de rodar. Ctrl+Enter só vai até metade desse parágrafo. Tem que selecionar tudo para rodar.

obitos_diarios_pjf_tidy_datas_numero<- obitos_diarios_pjf_tidy_datas_extenso %>%
  mutate( data_obito = str_split(data_obito, "/")) %>%
  unnest_wider(data_obito) %>%
  rename("dia"= "...1", "mes"= "...2", "ano" = "...3" ) %>%
  #arrumando meses e dias com apenas um algarismo (dd/m/yyyy, d/mm/yyyy e d/m/yyyy) #Tentei com across não funcionou
  mutate(dia = case_when(
    str_length(dia) == 1 ~ paste(0, dia, sep= ""),
    # dia == "00" ~ "01",
    TRUE ~ as.character(dia)),
    mes = case_when(
      str_length(mes) == 1 ~ paste(0, mes, sep= ""),
      is.na(mes) & pag == 9 ~ "11",
      TRUE ~ as.character(mes)),
    mes= str_replace(mes,"00","01"),
    ano = case_when(#arrumando anos faltantes com auxílio da pagina do documento e o mês que ela se encontra
      pag == 31 & dia == 23 ~ str_replace(ano, "2021","2020"), #um caso em que a data está  23/12/2021.
      ano == "2020" ~ as.character(ano), #Como a ordem importa no case_when, apenas para não acionar nos casos que já estão completos
      ano == "2021" ~ as.character(ano), #Como a ordem importa no case_when, apenas para não acionar nos casos que já estão completos
      pag <= 13 ~ "2020", #todas as linhas acima da página 13 são 2020
      pag >= 24 ~ "2021", #todas as linhas abaixo da página 24 são 2021
      pag >= 14 & pag <= 23 & str_detect(mes, "12") ~ "2020", #o que estiver entre 14 e 23 e for mês 12 é 2020
      pag >= 14 & pag <= 23 & str_detect(mes, "1|2|3") ~ "2020" #o que estiver entre 14 e 23 e for mês 1,2 ou 3 é 2021
    ))



#Ultimos passos antes do tidy final. Unindo para transformar datas character em dates, com lubridate
obitos_diarios_pjf_tidy <- obitos_diarios_pjf_tidy_datas_numero %>%
  mutate(across(c(dia:ano), str_squish)) %>% #Ultima Limpada antes da unida, mas across não funcionou muito bem, ora funcionava ora não
  unite("data_obito", dia:ano, sep="-") %>% #Unindo as colunas dia, mes e ano em uma só
  mutate(data_obito=lubridate::dmy(data_obito)) %>% #Transformando em objeto data
  #mutate(data_obito = readr::parse_date(data_obito, format = "%d/%m/%Y")) %>%
  select(!comorbidade) %>% #retirando a coluna controle de comorbidades
  as_tibble()

#Tidy!
# Temos uma base em rds ou xlsx pronta para ser utilizada, não em pdf, que dificulta o trabalho.
# Temos uma variável por coluna "genero", "idade", "data_obito" e "comorbidades".
# Cada valor dessa variável específica está padronizado de uma só forma:
# Temos dois gêneros padronizados em masculino e feminino, não 6 palavras que diziam a mesma coisa.
# Temos todas as comorbidades em seu nome completo e não em siglas que só são reconhecíveis por quem é da área, separadas em "," para operações com unnest.
# Temos idade e páginas em numéricos. Datas em formato date como Mr Hadley pede.
# Temos portanto uma base tidy!



# filtros e Classificações  ----------------------------------------------------

#Obtendo Faixa Etária
obitos_diarios_pjf_fx_etaria <- obitos_diarios_pjf_tidy %>%
  mutate(faixa_etaria = cut(idade,
                            breaks =  c(0,20,40,60,80,101), 
                            labels = c("Menos de 20 anos", "20 a 40", "40 a 60",
                                       "De 60 a 80", "Mais de 80")
  ))


# "Modelos"  ----------------------------------------------------


#Média Móvel

obitos_diarios_pjf_total<- obitos_diarios_pjf_fx_etaria %>%
  # group_by(data_obito) %>%
  count(data_obito)%>%
  tsibble::as_tsibble(index=data_obito)%>%
  mutate(media_movel_mortes_7dias= slider::slide_index_dbl(.i = data_obito,
                                                           .x = n,
                                                           .f = mean,
                                                           .before = 6),
         media_movel_mortes_14dias = slider::slide_index_dbl(.i = data_obito,
                                                             .x = n,
                                                             .f = mean,
                                                             .before = 13))%>%
  arrange(data_obito)



# Visualização ----------------------------------------------------

#Mortes Diárias com Média Móvel - GGPLOT

obitos_diarios_pjf_total%>%
  ggplot(aes(x=data_obito, y=n)) +geom_col() +
  geom_line(aes(x=data_obito, y= media_movel_mortes_7dias), 
            color= "red", show.legend = TRUE, size= 1.2, alpha= 0.8) +
  geom_line(aes(x=data_obito, y= media_movel_mortes_14dias), 
            color= "blue", show.legend = TRUE, size= 1.2, alpha= 0.8)+
  labs(title = "Nº Diário de Mortes por Coronavírus em Juiz de Fora - PJF",
       subtitle = "Em Vermelho, média movel dos últimos 7 dias. Em Azul, média móvel dos últimos 14 dias.",
       caption= "Fonte: Prefeitura de Juiz de Fora - Elaboração do Gráfico: JF em Dados")+
  scale_y_continuous(name = "Nº de Mortes") + xlab(label= "Data da Ocorrência do Óbito") +
  theme_classic() + theme(text =  element_text(family="Times New Roman"), plot.title = element_text(size=18, face="bold" )) 



# Exportação ----------------------------------------------------


writexl::write_xlsx(obitos_diarios_pjf_total,
                    path= "municipal/dados_diarios/obitos_diarios_pjf_total.xlsx")
