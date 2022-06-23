# Painel de Inteligência de Dados do SIA (Sistema de Informações Ambulatoriais do SUS) e SIH (Sistema de Informações Hospitalares) 

Este pacote tem o objetivo de disseminar um painel para acesso aos dados disponibilizados pelo DATASUS, de forma fácil e rápida. 

O painel possui três grandes fontes de dados:

1) Informações do SIA: são dados referentes ao quantitativo e valor financeiro dos procedimentos aprovados e produzidos no âmbito ambulatorial (documentos BPA e APAC). Para saber mais acesse aqui.
2) Informações do SIH: são dados provenientes dos registros de todos os atendimentos provenientes de internações hospitalares que foram financiadas pelo SUS. Os dados são disponibilizados em relação às AIH's (Autorização de Internação Hospitalar) aprovadas e rejeitadas. São fornecidas as seguintes informações: valor realizado das AIH's, perda financeira e motivos de rejeições. Neste seção analisa-se os dados em relação ao Procedimento Principal da AIH. Para mais detalhes acesse aqui.
3) Informações conjuntamente SIA e SIH: são dados de produção obtidos da união das informações coletadas do SIA e dos procedimentos secundários e principais produzidos na componente hospitalar.

## Instalação

```r
install.packages("devtools")
devtools::install_github("tivorn/faturamento.sus")
```

## Instruções de uso

A utilização do pacote consiste no uso da função `create_output_SIA` que realiza o download e preprocessamento das bases de dados SIA e SIH do DATASUS. Além disso, a função mencionada retorna o arquivo `outputSIA.csv` na pasta em que o usuário executou o arquivo `.R`.

1) Executar função `create_output_SIA`
```r
create_output_SIA(year_start=2021,
                  month_start=1,
                  year_end=2021,
                  month_end=1,
                  uf="CE",
                  CNES=c("2561492","2481286")
                  )
```

2) Obter painel Power BI

Uma vez que a função foi executada, é necessário baixar o modelo do painel Power BI presente neste repositório no diretório `inst/extdata`.

3) Carregar base de dados 

No painel, na barra superior clique em "Página Inicial" > "Obter dados" > "Texto/CSV" e selecione o arquivo `outputSIA.csv`

