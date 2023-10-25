<img src='App/www/minho.png' width='150'>

# Regressão não linear em curvas de crescimento para parâmetros placentares em R

Este repositório contém o código e os recursos relacionados à dissertação de mestrado em Bioinformática, intitulada "Regressão não linear em curvas de crescimento para parâmetros placentares em R". Neste estudo, foi explorada a aplicabilidade de modelos de regressão não linear em curvas de crescimento de parâmetros da placenta e do feto, usando um conjunto de dados de parturientes portuguesas fornecido pelo CGC Genetics, Porto.

## Resumo

Nos últimos anos, tem havido um crescente interesse na avaliação dos parâmetros biométricos da placenta e a sua relação com resultados obstétricos. Evidências sugerem que as medidas da placenta e a sua evolução podem refletir alterações no desenvolvimento do feto e até mesmo doenças do recém-nascido e do adulto. Neste contexto, este estudo teve como principal objetivo a construção de curvas de crescimento de referência para parâmetros como o Diâmetro 1 (D1) e 2 (D2), Espessura placentar (EP), Peso placentar (PP) e Peso fetal (PF) usando uma abordagem de regressão semiparamétrica.

## Conteúdo do Repositório

O repositório está organizado da seguinte forma:

- **App**: Contém os ficheiros relacionados à aplicação web desenvolvida para analisar e interpretar as curvas de crescimento construídas.

    - Ficheiros `global.r`, `ui.r`, `server.r` contêm o código da aplicação web.
    - Ficheiros CSV contêm os dados para traçar as curvas: `diametro1.csv`, `diametro2.csv`, `fetalweight.csv`, `placentalweight.csv`, `placentalthickness.csv`.
    - Ficheiros HTML para a interface da aplicação: `home.html`, `intro_text.html`, `ratios.html`.
    - Ficheiro `help.csv` contém informações do menu de ajuda.
    - Ficheiro `report.rmd` contém o template do relatório produzido pela aplicação com as análises.
    - Pasta `www` contém recursos adicionais, como folhas de estilo CSS personalizadas, um guia de utilização em PDF e imagens em PNG.

- **Curvas_crescimento**: Contém o código e as figuras relacionadas à construção das curvas de crescimento para diferentes parâmetros.

    - `Diametro1`, `Diametro2`, `Espessura`, `Peso`, `Peso_fetal`: Subdiretórios para cada parâmetro.

        - `Codigo`: Contém ficheiros com o código utilizado.
        - `Figuras`: Imagens resultantes do código.

- **Dados**: Contém os ficheiros dos dados utilizados neste estudo e o código relacionado ao seu tratamento e exploração.

    - `Codigo`: Contém ficheiros com o código para o tratamento e exploração dos dados.
    - `Figuras`: Imagens resultantes do código.
    - `dataset_placenta.csv`: Conjunto de dados original.
    - `dataset_placenta_tratado.csv`: Conjunto de dados tratado.
 
## Aplicação Web   

 <img src='App/www/logo_app.png' width='250'>
 
Foi desenvolvida uma aplicação web "PlacentalGrowth" que permite que profissionais de saúde e investigadores analisem e interpretem as curvas de crescimento construídas. 

A aplicação pode ser acedida em [placentalgrowth.shinyapps.io/uminho_pt/](https://placentalgrowth.shinyapps.io/uminho_pt/).

## Resultados e Implicações

Os resultados deste estudo fornecem informações importantes sobre o desenvolvimento da placenta e têm implicações significativas para a prática clínica em obstetrícia, permitindo o avanço e acompanhamento da saúde materno-fetal.

## Autor

- Daniela Lemos
- Contacto: pg45469@alunos.uminho.pt

Dissertação sob orientação de: 
- Professora Doutora Ana Cristina de Silva Braga
- Professora Doutora Rosete Maria Amorim Novais Nogueira Cardoso

