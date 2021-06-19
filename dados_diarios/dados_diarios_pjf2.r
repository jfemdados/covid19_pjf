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

# --------------------   IMPORTING ----------------------------------------------------
library(pdftools)


obitos_diarios_pjf_raw <- tibble(
  texto= pdftools::pdf_text("municipal/dados_diarios/data_raw/obitos_covid19_jf.pdf"),
  #criando divisão por páginas para ajudar com datas mais a frente
  pag= 1:45) # como automatizar a colocação desse número de páginas? Não quero ter que mudar toda vez que aumente o nº de pág.


#Se quiser verificar como estava cada página, é possível verificar por esse table.
# colquei o # para possiblitar o Ctrl + Shift + Enter

obitos_diarios_pjf_raw %>% #slice(1) %>%
  pull(texto)                                    


# Separando Linhas --------------------------------------------------------

#No documento inicial, cada linha deveria ser um paciente. Foi feito apenas uma separação com "\\n" no início.
#Mas linhas muito grandes que passam da margem eram divididas em duas linhas, representando duas linhas do mesmo caso.
#Assim optei pela separação de cada linha pelos números iniciais, que são "1.", "2.", "3.", "100." ...
#Mas  "d+\\."  também pegava também datas que estavam separadas por pontos nas frases
#por isso, optei pela divisão de cada caso por "\\r\\n\\d+\\."
#Ela sempre pega o Enter de uma nova linha, que se for um novo óbito e terá logo após números e um ponto.

#Separando Cada Linha do PDF que corresponde a um caso
obitos_diarios_pjf_separado<- obitos_diarios_pjf_raw %>%
  mutate(texto= str_split(texto, "\\n\\d+\\. "))%>%
  unnest(texto)

#Aqui printei todo o DF para conferir se não havia duplicidade de linhas
# Novamente, para verificar rodar o código abaixo.
# colquei o # para possiblitar o Ctrl + Shift + Enter

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
# ora estamos com a ordem de c( "genero", "idade", "data_do_obito" e "comorbidades")
# Temos inclusive um caso de 5 colunas:  se_é_idoso, genero, idade, data_do_obito e comorbidades
# como "idoso, masculino, 88 anos. óbito em 05/07/2020. comorbidades:dcc, etc."


# Portanto, como primeiro passo para transformar isso em uma tabela tidy:
# Vamos separar uma variável por coluna, buscando cada uma dessas informações com str_extract.


# Separando Colunas -------------------------------------------------------



obitos_diarios_pjf_clean<- obitos_diarios_pjf_separado %>%
  #Limpando algarismos que começam no inicio do texto e são seguidos por "." . "1304 não tinha ponto e ficou para trás;
  mutate(texto=str_remove(texto, "^\\d+\\.|1304") %>% 
           str_to_lower(),
  #Retirando alguns resíduos do PDF que restaram na linha separada
         texto= str_remove_all(texto, "\\r\\n") %>%
           str_squish()) %>%
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
  # Portanto,Buscando Idade com str_extract e padronizando a idade de bebês;
  mutate(idade = case_when(
    genero== "natimorto" | str_detect(texto,"natimorto") ~ "0", # ( idade de bebês = 0 ) Triste =/
    str_detect(texto,"01 dia") ~ "0",  # (idade de bebês = 0)   =/ Triste
    TRUE ~ str_extract(texto, "\\d+"))%>%  #Extraí os primeiros algarismos. Como idade está na frente, não há problema
      as.numeric(),
    #buscando data do obito em varios formatos
    data_obito = str_extract(texto, paste("\\d+/\\d+(/\\d+)*", #dd/mm e dd/mm/yy e dd/mm/yyyy
                                          "\\d{4,4}",          #ddmm (sem separador)
                                          "\\d{1,2} de \\w*",  #dia em número e mês por extenso
                                          "morreu.*\\d+",      #"morreu no último domingo,dd"
                                          sep = "|")),
    #Buscando comorbidades, buscando tudo que vem depois de "comorbidades" ou "fator de risco"
    comorbidade= case_when(
      str_detect(texto,"sem comorbidade") ~ "sem comorbidade", #buscando o que nao tem comorbidade
      TRUE ~ str_extract(texto, "comorbidade(s)*.+|fator de risco(s)*.+")),
      #Em algumas linhas, a ordem é "data_do_obito","comorbidades". Essa regex acima, nessas linhasm pega somente as comorbidades portanto.
      #Entretanto, algumas linhas estão na ordem "comorbidades" e depois "óbito".
    comorbidade = str_remove(comorbidade, paste("[oó]bito.+" ,    #nesses casos, mandei tirar tudo que vem depois de óbito,
                                                "óbito$",       #para ficar somente comoribdadescomorbidades
                                                sep= "|")),    
    comorbidade = str_remove(comorbidade, "comorbidade(s)*(:|,)|comorbidade(s)*/fator de risco:|fator de risco:") %>% #tendo certeza que tudo vem depois é comorbidade, mandei tirar as iniciais de comorbidade
      str_squish(),
    #Lidandos Falsos NA. Não indicavam o separador "comorbidade:". Vieram somente a doença.
    # Por isso vieram nulos, mas não eram NAs.
    #Fiz na Mão
    comorbidade = ifelse(is.na(comorbidade),
                         str_extract(texto, "has, dnc, ca|dcc, dm|imunodeficiência, outra pneumopatia crônica"),
                         as.character(comorbidade)),
    #Retirando finais de linhas que ainda continham separadores
    comorbidade = str_remove(comorbidade, "(\\.|,|;)$"))




# Padronizando Colunas ----------------------------------------------------


# De posse de uma variável por coluna, o próximo passo é padronizar a forma com que elas aparecem ao longo das coluna

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

# Assim, esses dois vetores foram criados para usar na hora de padronizar as colunas de genero

genero_feminino <- c( "feminino", "feminina", "idosa", "isosa", "mulher")
genero_masculino <- c( "homem", "idoso", "isoso", "masculino", "natimorto")


# Começando os trabalhos de padronização de variáveis


#Padronizando Gênero e Comorbidades

obitos_diarios_pjf_tidy_gen_com <- obitos_diarios_pjf_clean %>%
  #Com as variáveis já separadas, podemos retirar a coluna original do texto.
  select(!texto) %>%
  #1º Passo - Padronizando GÊNERO
  mutate(genero= case_when(
    genero %in% genero_feminino ~ "feminino",
    genero %in% genero_masculino ~ "masculino",
    TRUE ~ as.character(genero)),
  #2º Passo - Padronizando COMORBIDADES.
  #Criei coluna comorbidade (original) e comorbidades (modificada) para ter controle do que fazia
  #Acho que as colunas são autoexplicativas de qual conteúdo da mesma variável doença, despadronizada, passada doença, padronizada
    comorbidades= str_remove(comorbidade, "severa descompensada e não tratada|;l"),
    comorbidades= str_replace_all(comorbidades, "drc\\b|irc", "doença renal crônica"),
    comorbidades= str_replace_all(comorbidades, "dialítico|dialitica", "dialitico"),
    comorbidades= str_replace_all(comorbidades, "dc(c)*", "doença cardiovascular crônica"),
    comorbidades= str_replace_all(comorbidades, "dm|diabetes mel{1,2}itus", "diabetes"),
    comorbidades= str_replace_all(comorbidades, "has|hás|hs|had\\b|hipertensão arterial$", "hipertensão arterial sistêmica"),
    comorbidades= str_replace_all(comorbidades, "dpoc", "doença pulmonar obstrutiva crônica"),
    comorbidades= str_replace_all(comorbidades, "\\b[cç]a\\b( de)*|paciente oncológico|doença oncológica", "câncer"),
    comorbidades= str_replace_all(comorbidades, "câncer útero|útero","câncer útero"),
    comorbidades= str_replace_all(comorbidades, "dnc", "doença neurológica crônica"),
    comorbidades= str_replace_all(comorbidades, "dhc", "doença hepática crônica"),
    comorbidades= str_replace_all(comorbidades, "outra pneumopatia", "pneumopatia"),
    comorbidades= str_replace_all(comorbidades, "iam", "infarto agudo do miocárdio"),
    comorbidades= str_replace_all(comorbidades, "\\bave", "acidente vascular encefálico"),
    comorbidades= str_replace_all(comorbidades, "marcapasso","marca-passo"),
    comorbidades= str_replace_all(comorbidades, "imunosupressão","imunossupressão"),
    comorbidades= str_replace_all(comorbidades, "\\bfa\\b", "fibrilação atrial"),
    comorbidades= str_replace_all(comorbidades, "\\btu\\b", "tumor"),
    comorbidades= str_replace_all(comorbidades, "\\bave\\b", "acidente vascular encefálico"),
    comorbidades= str_replace_all(comorbidades, "avc", "acidente vascular cerebral"),
    comorbidades= str_replace_all(comorbidades, "histórico de avc", "acidente vascular cerebral prévio"),
    comorbidades= str_replace_all(comorbidades, "dvc", "doença venosa crônica"),
    comorbidades= str_replace_all(comorbidades, "tep", "tromboembolismo pulmonar"),
    comorbidades= str_replace_all(comorbidades, "tb( pulmonar)*", "tuberculose pulmonar"),
    comorbidades= str_replace_all(comorbidades, "etilismo", "etilista"),
    comorbidades= str_replace_all(comorbidades, "hpb", "hiperplasia prostática benigna"),
    comorbidades= str_replace_all(comorbidades, "usuário de droga", "drogadição"),
    comorbidades= str_replace_all(comorbidades, "puérpera","puerpério"),
    comorbidades= str_replace_all(comorbidades, "tce","traumatismo cranioencefálico"),
    comorbidades= str_replace_all(comorbidades, "imunosupressão","imunossupressão"),
    comorbidades= str_replace_all(comorbidades, "pti","púrpura trombocitopênica idiopática"),
    comorbidades= str_replace_all(comorbidades, "drge","doença do refluxo esofágico"),
    comorbidades= str_replace_all(comorbidades, "eplepsia|convulsão|crise convulsiva","epilepsia"),
    comorbidades= str_replace_all(comorbidades, "transtorno bipolar","bipolar"),
    comorbidades= str_replace_all(comorbidades, "transtorno depressivo","depressão"),
    comorbidades= str_replace_all(comorbidades, "síndrome demência","demência"),
    comorbidades= str_replace_all(comorbidades, "transplantado","transplante"),
    comorbidades= str_replace_all(comorbidades, "\\bic(c)*\\b","insuficiência cardiáca"),
    comorbidades= str_replace_all(comorbidades, "dac|coronariopata|coranopata","doença arterial coronariana"),
    comorbidades= str_replace_all(comorbidades, "da,","doença arterial coronariana,"),#se colocasse da(c)* na linha acima, quando fosse pegar dac, só pegaria dac, por isso optei por uma nova linha
    comorbidades= str_replace_all(comorbidades, "deficicência","deficiência"),
    comorbidades= str_replace_all(comorbidades, "transplantado","transplante"),
    comorbidades= str_replace_all(comorbidades, "dsp","intoxicação diarreica por molusco"),
    comorbidades= str_replace_all(comorbidades, "dlp|dislepidemia","dislipidemia"),
    comorbidades= str_replace_all(comorbidades,
                                  paste("hiperplasia benigna próstata",
                                        "hiperplasia prostática benigna",
                                        sep= "|"),
                                  "hiperplasia de próstata"),
    #arrumando delimitadores dentro da coluna comorbidades " e " e "," por "/"
    comorbidades= str_replace_all(comorbidades, " e |,", "/"),
    #a intenção era retirar tudo que estava dentro de parentêses geralmente a "sigla(explicação da sigla)"
      # mas comorbidades= str_remove_all(comorbidades, "\\(.+\\)"), quando havia mais de dois parenteses por linhas, removia tudo entre elas.
    # ex: "doença cardiovascular crônica (dcc), diabetes melitus (dm)" retivara não só cada grupo desse, mas tudo como uma regex "(dcc.*dm)"
    # Assim, fiz na mão, retirando cada um dos casos entre parênteses que apareceram:
    comorbidades= str_remove_all(comorbidades, paste("\\(diabetes\\)",
                                                     "\\(doença cardiovascular crônica\\)",
                                                     "\\(doença pulmonar obstrutiva crônica\\)", 
                                                     "\\(hipertensão arterial sistêmica\\)",
                                                     "\\(alzheimer\\)",
                                                     "\\(doença renal crônica\\)",
                                                     "\\(doença neurológica crônica\\)",
                                                     sep= "|")),
    #Todas as Comorbidades estão separadas por "," prontas para sofrerem unnest. Finalizando, retirando espaços.
    comorbidades = str_squish(comorbidades),
  #um caso em que não havia separado nenhum, foi necessário colocar na mão
    comorbidades = str_replace(comorbidades,
                             pattern = "doença cardiovascular crônica marca-passo",
                             replacement = "doença cardiovascular crônica/marca-passo"),
  #retirando espaços depois de barras, str squish nao pegou esse caso
    comorbidades= str_replace_all(comorbidades, " / | /|/ ", "/"),
    #Arrumando NA e Sem Comorbidades
    comorbidades = str_replace(comorbidades, "comorbidades", "sem comorbidade"),
    comorbidades = tidyr::replace_na(comorbidades, "sem comorbidade"))

#Coluna Gênero Tidy! Mesma variável de uma só forma

table(obitos_diarios_pjf_clean$genero)
table(obitos_diarios_pjf_tidy_gen_com$genero)

#Dando Unnest nas comorbidades para verificar padronização dessa coluna

obitos_diarios_pjf_tidy_comorbidade_separado <- obitos_diarios_pjf_tidy_gen_com %>%
  mutate(comorbidades= str_split(comorbidades, "/")) %>%
  unnest(comorbidades)

# Coluna Comorbidades Tidy!

#Pode se verificar abaixo no table()
# Dentro da possibilidades, busquei colocar tudo da mesma doença com nomes diferente com nome da mesma variável.
# Porém falta padronização quanto ao nível de especialização.
# Algumas doenças estão em grandes grupos, como doença cardiovascular crônica,
# mas algumas, que poderiam estar nesses grandes grupos, estão muito especificadas como insuficiência cardíaca e doença arterial coronariana, etc...
# Como a necessidade de mais especialização ou menos varia com o objetivo, optei por não colocar todos casos mais especializados em gênero.
  
table(obitos_diarios_pjf_tidy_comorbidade_separado$comorbidades)


#Padronizado DATAS - 1º Passo: Passando as datas que estão escritas por extenso para número.

#Tentei fazer essa função para lidar com números por extenso, infelizmente não funcionou.

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

#terá que ser por vez no meio do código. Passando as datas que estão escritas por extenso para número.

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
      # Padronizando casos de datas ddmm sem parentes ou qualquer delimitandor entre.
      #Se encontrar quatro números, contínuos, no início da coluna, sem delimitador entre eles,
      # faça um paste colocando um delimitardor entre os dois primeiros e os dois últimos
      str_detect(data_obito, "^\\d{4,4}") ~ paste(str_extract(data_obito, "\\d{2,2}"),"/",str_extract(data_obito, "\\d{2,2}\\b")),
      TRUE ~ data_obito))



# 2º passo data - Lidando com Datas com algum algarismo faltante (d/mm/aaaa ou dd/m/aaaa)
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
      pag >= 14 & pag <= 23 & str_detect(mes, "01|02|03") ~ "2021" #o que estiver entre 14 e 23 e for mês 1,2 ou 3 é 2021
    ))




#Ultimos passos antes do tidy final. Unindo para transformar datas character em dates, com lubridate
obitos_diarios_pjf_tidy <- obitos_diarios_pjf_tidy_datas_numero %>%
  mutate(across(c(dia:ano), str_squish)) %>% #Ultima Limpada antes da unida, mas across não funcionou muito bem, ora funcionava ora não
  unite("data_obito", dia:ano, sep="-") %>% #Unindo as colunas dia, mes e ano em uma só
  mutate(data_obito=lubridate::dmy(data_obito)) %>% #Transformando em objeto data
  #mutate(data_obito = readr::parse_date(data_obito, format = "%d/%m/%Y")) %>%
  select(!comorbidade) %>% #retirando a coluna controle de comorbidades
  as_tibble()

# Temos uma tabela Tidy!


# Temos uma base em rds ou xlsx pronta para ser utilizada, não em pdf, que dificulta o trabalho.
# Temos uma variável por coluna "genero", "idade", "data_obito" e "comorbidades".
# Cada valor dessa variável específica está padronizado de uma só forma:
# Temos dois gêneros padronizados em masculino e feminino, não 6 palavras que diziam a mesma coisa.
# Temos todas as comorbidades em seu nome completo e não em siglas que só são reconhecíveis por quem é da área, separadas em "," para operações com unnest.
# Temos idade e páginas em numéricos,datas em formato date como Mr Hadley pede.
# Temos portanto uma base tidy!



# filtros e Classificações  ----------------------------------------------------

#Obtendo Faixa Etária
obitos_diarios_pjf_fx_etaria <- obitos_diarios_pjf_tidy %>%
  mutate(faixa_etaria = cut(idade,
                            breaks =  c(-1,20,40,60,80,101), 
                            labels = c("Menos de 20", "20 a 40", "40 a 60",
                                       "60 a 80", "Mais de 80")))

#,
  #obtendo nº de comorbidades
 #         n_comorbidades = case_when(
  #          comor
   #       )

# "Modelos"  ----------------------------------------------------


# Mortes por Mês agrupado por Faixa Etária

obitos_diarios_pjf_total_por_mes<- obitos_diarios_pjf_tidy_datas_numero %>%
  mutate(faixa_etaria = cut(idade,
                            breaks =  c(-1,20,40,60,80,101), 
                            labels = c("Menos de 20", "20 a 40", "40 a 60",
                                       "60 a 80", "Mais de 80")))%>%
  mutate(across(c(dia:ano), str_squish)) %>%
   group_by(ano, faixa_etaria) %>%
  count(mes)%>%
  mutate(mes_ano= paste(mes,ano,sep= "-") %>%
           lubridate::my())




# Numero de mortes Por dia com Média Móvel

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

#Mortes por Mês Por Faixa Etária

obitos_diarios_pjf_total_por_mes %>%
  ggplot(aes(x=mes_ano, y= n, fill=faixa_etaria)) + geom_col(position = "fill")+
  labs(title = "Mortes por Covid em Juiz de Fora por Faixa Etária - PJF",
       subtitle = "Percentual de mortes por Cada Faixa Etária em cada mês",
       caption= "Fonte: Prefeitura de Juiz de Fora - Elaboração do Gráfico e Faxina de Dados: JF em Dados") +
  scale_y_continuous(name = "Nº de Mortes") + xlab(label= "Data da Ocorrência do Óbito") +
  theme_classic() + theme( plot.title = element_text(size=18, face="bold" )) 



#Mortes Diárias com Média Móvel 

obitos_diarios_pjf_total%>%
  ggplot(aes(x=data_obito, y=n)) +geom_col() +
  geom_line(aes(x=data_obito, y= media_movel_mortes_7dias), 
            color= "red", show.legend = TRUE, size= 1.2, alpha= 0.8) +
  geom_line(aes(x=data_obito, y= media_movel_mortes_14dias), 
            color= "blue", show.legend = TRUE, size= 1.2, alpha= 0.8)+
  labs(title = "Nº Diário de Mortes por Coronavírus em Juiz de Fora - PJF",
       subtitle = "Em Vermelho, média movel dos últimos 7 dias. Em Azul, média móvel dos últimos 14 dias.",
       caption= "Fonte: Prefeitura de Juiz de Fora - Elaboração do Gráfico e Faxina de Dados: JF em Dados")+
  scale_y_continuous(name = "Nº de Mortes") + xlab(label= "Data da Ocorrência do Óbito") +
  theme_classic() + theme( plot.title = element_text(size=18, face="bold" )) 




# Exportação ----------------------------------------------------

rio::export(obitos_diarios_pjf_fx_etaria,
            file= "municipal/dados_diarios/obitos_diarios_pjf_fx_etaria.csv")

writexl::write_xlsx(obitos_diarios_pjf_fx_etaria,
                    path= "municipal/dados_diarios/obitos_diarios_fx_etaria.xlsx")

