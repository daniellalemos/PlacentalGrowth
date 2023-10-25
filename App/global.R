####################################
# Global                          #
####################################

# BEM-VINDO a PlacentalGrowth


# Carrega as bibliotecas necessarias para a aplicacao
# Certifique-se de que todas estas bibliotecas estejam instaladas no ambiente de execucao

library(shiny) # Biblioteca para criar aplicacoes interativas em R
library(ggplot2) # Biblioteca para visualizacao de dados e graficos
library(plotly) # Biblioteca para criar graficos interativos
library(shinydashboard) # Biblioteca para criar paineis de controlo interativos
library(prompter) # Biblioteca para apresentar mensagens interativas
library(shinyjs)  # Biblioteca que fornece funcoes JavaScript personalizadas em aplicacoes Shiny
library(DT) # Biblioteca para criar tabelas interativas
library(dplyr)  # Biblioteca para manipulacao de dados
library(readr)  # Biblioteca para leitura de dados em diferentes formatos


# Configuracao para lidar com erros sanitarios
options(shiny.sanitize.errors = FALSE)

# Carrega o texto do menu suspenso
# Le o ficheiro "help.csv" e cria um objeto "steps" que contem as opcoes do menu suspenso
steps <- read_delim("help.csv", delim = ";", show_col_types = FALSE)