#JF em Dados: casos de coronavirus em Juiz de Fora
#Autor: M. Valentim - Help: Marcello F.
library(rvest)
library(tidyverse)
library(stringr)
library(glue)
#parte do scrapper

#definindo url
url <- "https://covid19.pjf.mg.gov.br/boletim.php#gsc.tab=0"

#gera parte do link
gera_link <- function(posicao_tr){
  x <- read_html(url) %>% 
    html_elements(xpath = glue('//*[@id="Conteudo"]/div[2]/div[2]/table/tbody/tr[{posicao_tr}]/td[2]/a')) %>%
    as.character() %>% 
    str_extract("boletim_\\d+")  
  paste0("https://covid19.pjf.mg.gov.br/arquivos/",x,".pdf")
}


#Já gerando um tibble reduzido com os conteudos do PDF

#423-2 - essas são todas as posições dos links na página scrappada, coloquei de 10 a 3 
#só para testar antes de gerar tudo

base_previa <- tibble(link = map_chr(10:3,gera_link),
                      conteudo_pdf = map_chr(link, pdftools::pdf_text))


 #mortes = str_extract_all(conteudo_pdf, '//d')
  #unnest_longer(mortes) %>%
#Aqui em cima já ta gerando uma base com um conteudo_pdf, mas precisa ser extraído via regex
#as coisas


# @marcello, resolve isso pra mim? já ta gerando um tibble com o conteudo TODO do pdf,
#precisa só de separar esse conteúdo de jeito que faça sentido



# Done - =D

 base_previa_tidy<- base_previa %>%
   tidyr::separate(conteudo_pdf,
                  into = c("erro", "data", "casos_suspeitos", "casos_confirmados",
                           "obitos", "obitos_em_investigacao"),
                  sep= "\\n+",
                  extra = "merge") %>%
  mutate(across(.cols= c(data:obitos_em_investigacao),
                .fns = str_squish)) %>%
  mutate(across(.cols = c(casos_suspeitos:obitos_em_investigacao),
               ~str_remove_all(.x, "\\D+")%>%
                 as.numeric()),
         data= lubridate::dmy(data))
 
 
 base_previa_tidy %>%
   ggplot(aes(x=data,y=casos_suspeitos)) +geom_col()
 
 







#-----------------------------------------------------------
#Daqui pra baixo eu tentei fazer lendo o pdf como data table ao invés de como text,
#da errado porque o lugar onde os casos ficam varia



#tentando pegar como dataframes
#criei essa funcao para ja pegar casos confirmados, casos nao confirmados e outras coisas,
#seria só substituir em "algo"-
extrai_algo <- function(posicao, algo){
  pluck(a,2,posicao) %>% 
    select(text) %>% 
    slice(algo) %>% 
    as.double()
}

extrai_numeros <- function(posicao_tr){
  conteudo_pdf[[posicao_tr]] %>% 
    select(text) %>% 
    slice(3) %>% 
    as.double()
}

#primeira_base <-tibble(link = map_chr(199:2,gera_link)) %>% 
 # mutate(conteudo_pdf = purrr::map(link, pdftools::pdf_data)) %>% 
  #unnest(conteudo_pdf)%>% 
  #mutate(casos_suspeitos = 
   #        map_dbl(1:234, extrai_algo, algo = 3),
    #     casos_confirmados = 
     #      map_dbl(1:234, extrai_algo, algo = 5),
      #   data= 
       #    map_dbl(1:234, extrai_algo, algo = 2))


seq(from = 'x1', to = 'x12')

#gerando varios link
#Deu certo
tibble(link= map(seq(423:2), gera_link))

#mas essa aqui buga , dá um erro 404, algum link está errado

base_varios_link_raw <- tibble(link= map(seq(423:2), gera_link),
                               conteudo_pdf = map_chr(link, pdftools::pdf_text)) 
       
#parte do pdf
pdftools::pdf_text("https://covid19.pjf.mg.gov.br/arquivos/boletim_230621.pdf")

