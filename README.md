# covid19_pjf

semelhante à "covid19jf", mas focado somente em dados fornecidos pela prefeitura
Trata-se da base de "metadados" dos óbitos de covid em Juiz de Fora.
 Só há divulgação total de mortos em XLS, e não mostram a evolução deles, apenas o último número. Os dados permenorizados estão nesse pdf.
É o meu TCC do curso de faxina de dados da curso-R.


 Não se trata de uma base tidy no pdf. Como será demonstrado ao longo do código.
 A primeira questão é o formato em PDF, que dificulta o trabalho.
 Cada linha  é uma pessoa que morreu em Juiz de Fora de COVID.
 Existem colunas de gênero, idade, data_do_obito (nao data de notificação), comorbidades.
 A estrutura parece de um csv, mas está longe disso.
 O separador de cada linha as vezes é um ";" "." ou até em alguns casos casos, nada "".
 Dentro de cada linha, o separador de colunas as vezes é ".", "," ou apenas um espaço.

# Assim, eu faxinei e transformei em um CSV e XL que estão disponível no final como *obitos_diarios_pjf_fx_etaria* em csv e xls.
# Codigo no *dados_diarios_pjf.r*
As comorbidades estão separadas dentro da coluna comorbidades por "/".

Rendeu esse post:

![image](https://user-images.githubusercontent.com/53457944/120711466-ca694580-c495-11eb-87fc-806038c76cc3.png)



