####################################
# User Interface                   #
####################################


# Define a interface do usuario usando dashboardPage
ui <- dashboardPage(
  title = "PlacentalGrowth", # Titulo da pagina
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = tags$h1(
      img(
        src = "logo2.png", # logo 
        style = "height: auto; max-height: 28px; width: auto; max-width: 100%;",
        alt = "PlacentalGrowth Logo"
      ),
      "PlacentalGrowth", # titulo da aplicacao
      style = "color: white; font-size: 18px; font-weight: bold; margin-left: 10px;"
    ),
    # Menu de ajuda
    dropdownMenu( 
      type = "notifications", 
      headerText = strong("Ajuda"), # Titulo do menu de ajuda
      icon = icon("question"), # Icone de ponto de interrogação
      badgeStatus = NULL,
      notificationItem(
        text = (steps$text[1]), # Texto do primeiro item de notificacao
        icon = icon("home") # Icone para a primeira notificacao 
      ),
      notificationItem(
        text = steps$text[2],
        icon = icon("chart-line") 
      ),
      notificationItem(
        text = steps$text[3],
        icon =icon("table")
      ),
      notificationItem(
        text = steps$text[4],
        icon = icon("calculator")
      ),
      notificationItem(
        text = steps$text[5],
        icon = icon("question-circle")
      )
    ),
    #Link para Github
    tags$li(
      a(
        img(src = "github.png", height = 20), # Icone do Github
        href = "https://github.com/daniellalemos/Tese", # URL do link do Github
        title = "",
        target = "_blank" # Abre o link numa nova pagina
      ),
      class = "dropdown"
    )
  ),
  
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Página Inicial", tabName = "home", icon = icon("home")),  # Item do menu para a pagina inicial
      menuItem("Gráficos", tabName = "app", icon = icon("chart-line")), # Item do menu para graficos
      menuItem("Informações", tabName = "point_info", icon = icon("table")),  # Item do menu para informacoes
      menuItem("Rácios", tabName = "ratios", icon = icon("calculator")),  # Item do menu para racios
      menuItem("Ajuda", tabName = "help", icon = icon("question-circle")) # Item do menu de ajuda
    )
  ),
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # Inclui um ficheiro css de estilo personalizado
    ),
    useShinyjs(), # Ativa o pacote Shinyjs para funcionalidades JavaScript personalizadas
    use_prompt(),  # Ativa o pacote prompter Para adicionar icones de ajuda em inputs
    br(), # Insere uma quebra de linha
    tabItems(
      
      # PAGINA INICIAL  ---------------------------------------------------------------
      tabItem(
        tabName = "home",
        includeHTML("home.html") # Inclui o conteudo da pagina inicial a partir de um ficheiro HTML externo
      ),
      
      # GRAFICOS ---------------------------------------------------------------
      tabItem(
        tabName = "app",
        fluidPage(
          sidebarLayout(
            # Painel com os inputs para os graficos e informacoes
            sidebarPanel(
              tabsetPanel(
                tabPanel(
                  # Inputs das informações do processo
                  "Informações",
                  textInput("process_number", "Número de processo:"), # Input para numero do processo
                  textInput("pacient_name", "Nome do paciente:"),  # Input para nome do paciente
                  numericInput("age", "Idade do paciente:", value = ""), # Input para idade do paciente
                  dateInput("date_input", "Data:", format = "dd-mm-yyyy", language = "pt"),  # Input para data
                  textAreaInput("infos", 
                                label = tags$span("Informações adicionais:", 
                                                  tags$span(icon(name = "question-circle"))
                                                  |> add_prompt(message = "Se tiver alguma informação adicional que queira adiconar no relatório escreva aqui.", 
                                                                position = "right")),
                                rows = 4), # Área de texto para informações adicionais
                  actionButton("reset_info_btn", "Reiniciar", icon = icon("refresh"))
                ),
                # Inputs do diametro1
                tabPanel(
                  "Diâmetro 1",
                  selectInput("load_csv_d1", 
                              label = tags$span("Deseja carregar pontos de um ficheiro CSV?", 
                                                tags$span(icon(name = "question-circle"))
                                                |> add_prompt(message = "Selecione 'Sim' se desejar carregar pontos de um ficheiro CSV. Apenas é aceite ficheiros neste formato e com 2 colunas (IG,Diâmetro1).", 
                                                              position = "right")),
                              choices = c("Não", "Sim")), # Opcao para carregar um ficheiro CSV
                  conditionalPanel(
                    condition = "input.load_csv_d1 == 'Sim'",
                    fileInput("csv_file_d1", "Escolha o ficheiro CSV para carregar os pontos:", accept = ".csv") # Input para escolher um ficheiro CSV
                  ),
                  numericInput("ga_input_d1", label = tags$span("Idade gestacional (semanas):", tags$span(icon(name = "question-circle"))  
                                                                |> add_prompt(message = "Idade gestacional em semanas. Deve ser um número inteiro entre 12 e 41 semanas.", position = "right")), 
                               value = NULL, min = 12, max = 41),  # Input para idade gestacional
                  numericInput("diameter1_input", label = tags$span("Diâmetro 1 (cm):", tags$span(icon(name = "question-circle"))  
                                                                    |> add_prompt(message = "Comprimento - maior diâmetro da placenta em centímetros. Digite um valor válido.", position = "right")), 
                               value = NULL), # Input para diametro 1
                  actionButton("submit_btn_d1", "Submeter", style = "background-color: #3498db; color: #fff;"), # Botao para submeter
                  conditionalPanel(
                    condition = "input.submit_btn_d1 > 0",
                    checkboxInput("save_csv_d1", 
                                  label = tags$span("Deseja salvar os pontos num ficheiro CSV?", 
                                                    tags$span(icon(name = "question-circle"))
                                                    |> add_prompt(message = "Marque esta opção apenas se desejar salvar os pontos adicionados no gráfico num ficheiro CSV.", 
                                                                  position = "right")))
                  ), # Checkbox para salvar em CSV
                  conditionalPanel(
                    condition = "input.save_csv_d1 == true && input.submit_btn_d1 > 0",
                    downloadButton("download_csv_link_d1", "Download CSV", style = "background-color: #299929; color: #fff;")  # Botao para fazer o download do CSV
                  ),
                  actionButton("reset_btn_d1", "Reiniciar", icon = icon("refresh"))  # Botao para reiniciar
                ),
                # Inputs do diametro2
                tabPanel(
                  "Diâmetro 2",
                  selectInput("load_csv_d2", 
                              label = tags$span("Deseja carregar pontos de um ficheiro CSV?", 
                                                tags$span(icon(name = "question-circle"))
                                                |> add_prompt(message = "Selecione 'Sim' se desejar carregar pontos de um ficheiro CSV. Apenas é aceite ficheiros neste formato e com 2 colunas (IG,Diâmetro2).", 
                                                              position = "right")),
                              choices = c("Não", "Sim")),
                  conditionalPanel(
                    condition = "input.load_csv_d2 == 'Sim'", # Opcao para carregar um ficheiro CSV
                    fileInput("csv_file_d2", "Escolha o ficheiro CSV para carregar os pontos:", accept = ".csv") # Input para escolher um ficheiro CSV
                  ),
                  numericInput("ga_input_d2", label = tags$span("Idade gestacional (semanas):", tags$span(icon(name = "question-circle"))  
                                                                |> add_prompt(message = "Idade gestacional em semanas. Deve ser um número inteiro entre 12 e 41 semanas.", position = "right")), 
                               value = NULL, min = 12, max = 41),   # Input para idade gestacional
                  numericInput("diameter2_input", label = tags$span("Diâmetro 2 (cm):", tags$span(icon(name = "question-circle"))  
                                                                    |> add_prompt(message = "Largura - menor diâmetro da placenta em centímetros. Digite um valor válido.", position = "right")), 
                               value = NULL), # Input para diametro 2
                  actionButton("submit_btn_d2", "Submeter", style = "background-color: #3498db; color: #fff;"), # Botao para submeter
                  conditionalPanel(
                    condition = "input.submit_btn_d2 > 0",
                    checkboxInput("save_csv_d2", 
                                  label = tags$span("Deseja salvar os pontos num ficheiro CSV?", 
                                                    tags$span(icon(name = "question-circle"))
                                                    |> add_prompt(message = "Marque esta opção apenas se desejar salvar os pontos adicionados no gráfico num ficheiro CSV.", 
                                                                  position = "right")))
                  ), # Checkbox para salvar em CSV
                  conditionalPanel(
                    condition = "input.save_csv_d2 == true && input.submit_btn_d2 > 0",
                    downloadButton("download_csv_link_d2", "Download CSV", style = "background-color: #299929; color: #fff;")
                  ),# Botão para fazer o download do CSV
                  actionButton("reset_btn_d2", "Reiniciar", icon = icon("refresh"))  # Botão para reiniciar
                ),
                # Inputs da espessura
                tabPanel(
                  "Espessura",
                  selectInput("load_csv_pt", 
                              label = tags$span("Deseja carregar pontos de um ficheiro CSV?", 
                                                tags$span(icon(name = "question-circle"))
                                                |> add_prompt(message = "Selecione 'Sim' se desejar carregar pontos de um ficheiro CSV. Apenas é aceite ficheiros neste formato e com 2 colunas (IG,Espessura).", 
                                                              position = "right")),
                              choices = c("Não", "Sim")),
                  conditionalPanel(
                    condition = "input.load_csv_pt == 'Sim'", # Opcao para carregar um ficheiro CSV
                    fileInput("csv_file_pt", "Escolha o ficheiro CSV para carregar os pontos:", accept = ".csv")  # Input para escolher um ficheiro CSV
                  ),
                  numericInput("ga_input_pt", label = tags$span("Idade gestacional (semanas):", tags$span(icon(name = "question-circle"))  
                                                                |> add_prompt(message = "Idade gestacional em semanas. Deve ser um número inteiro entre 12 e 41 semanas.", position = "right")), 
                               value = NULL, min = 12, max = 41),  # Input para idade gestacional
                  numericInput("pt_input", label = tags$span("Espessura (cm):", tags$span(icon(name = "question-circle"))  
                                                             |> add_prompt(message = "Espessura em centímetros. Digite um valor válido.", position = "right")), 
                               value = NULL),  # Input para espessura
                  actionButton("submit_btn_pt", "Submeter", style = "background-color: #3498db; color: #fff;"), # Botao para submeter
                  conditionalPanel(
                    condition = "input.submit_btn_pt > 0",
                    checkboxInput("save_csv_pt", 
                                  label = tags$span("Deseja salvar os pontos num ficheiro CSV?", 
                                                    tags$span(icon(name = "question-circle"))
                                                    |> add_prompt(message = "Marque esta opção apenas se desejar salvar os pontos adicionados no gráfico num ficheiro CSV.", 
                                                                  position = "right")))
                  ), # Checkbox para salvar em CSV
                  conditionalPanel(
                    condition = "input.save_csv_pt == true && input.submit_btn_pt > 0",
                    downloadButton("download_csv_link_pt", "Download CSV", style = "background-color: #299929; color: #fff;")
                  ),# Botão para fazer o download do CSV
                  actionButton("reset_btn_pt", "Reiniciar", icon = icon("refresh"))  # Botão para reiniciar
                ),
                # Inputs do peso
                tabPanel(
                  "Peso",
                  selectInput("load_csv_pw", 
                              label = tags$span("Deseja carregar pontos de um ficheiro CSV?", 
                                                tags$span(icon(name = "question-circle"))
                                                |> add_prompt(message = "Selecione 'Sim' se desejar carregar pontos de um ficheiro CSV. Apenas é aceite ficheiros neste formato e com 2 colunas (IG,Peso).", 
                                                              position = "right")),
                              choices = c("Não", "Sim")),
                  conditionalPanel(
                    condition = "input.load_csv_pw == 'Sim'", # Opcao para carregar um ficheiro CSV
                    fileInput("csv_file_pw", "Escolha o ficheiro CSV para carregar os pontos:", accept = ".csv")  # Input para escolher um ficheiro CSV
                  ),
                  numericInput("ga_input_pw", label = tags$span("Idade gestacional (semanas):", tags$span(icon(name = "question-circle"))  
                                                                |> add_prompt(message = "Idade gestacional em semanas. Deve ser um número inteiro entre 12 e 41 semanas.", position = "right")), 
                               value = NULL, min = 12, max = 41),  # Input para idade gestacional
                  numericInput("pw_input", label = tags$span("Peso (g):", tags$span(icon(name = "question-circle"))  
                                                             |> add_prompt(message = "Peso em gramas. Digite um valor válido.", position = "right")), 
                               value = NULL),  # Input para peso
                  actionButton("submit_btn_pw", "Submeter", style = "background-color: #3498db; color: #fff;"), # Botao para submeter
                  conditionalPanel(
                    condition = "input.submit_btn_pw > 0",
                    checkboxInput("save_csv_pw", 
                                  label = tags$span("Deseja salvar os pontos num ficheiro CSV?", 
                                                    tags$span(icon(name = "question-circle"))
                                                    |> add_prompt(message = "Marque esta opção apenas se desejar salvar os pontos adicionados no gráfico num ficheiro CSV.", 
                                                                  position = "right")))
                  ), # Checkbox para salvar em CSV
                  conditionalPanel(
                    condition = "input.save_csv_pw == true && input.submit_btn_pw > 0",
                    downloadButton("download_csv_link_pw", "Download CSV", style = "background-color: #299929; color: #fff;")
                  ),# Botão para fazer o download do CSV
                  actionButton("reset_btn_pw", "Reiniciar", icon = icon("refresh"))  # Botao para reiniciar
                ),
                # Inputs do peso fetal
                tabPanel(
                  "Peso fetal",
                  selectInput("load_csv_fw", 
                              label = tags$span("Deseja carregar pontos de um ficheiro CSV?", 
                                                tags$span(icon(name = "question-circle"))
                                                |> add_prompt(message = "Selecione 'Sim' se desejar carregar pontos de um ficheiro CSV. Apenas é aceite ficheiros neste formato e com 2 colunas (IG,Peso fetal).", 
                                                              position = "right")),
                              choices = c("Não", "Sim")),
                  conditionalPanel(
                    condition = "input.load_csv_fw == 'Sim'", # Opcao para carregar um ficheiro CSV
                    fileInput("csv_file_fw", "Escolha o ficheiro CSV para carregar os pontos:", accept = ".csv")  # Input para escolher um ficheiro CSV
                  ),
                  numericInput("ga_input_fw", label = tags$span("Idade gestacional (semanas):", tags$span(icon(name = "question-circle"))  
                                                                |> add_prompt(message = "Idade gestacional em semanas. Deve ser um número inteiro entre 12 e 41 semanas.", position = "right")), 
                               value = NULL, min = 12, max = 41),  # Input para idade gestacional
                  numericInput("fw_input", label = tags$span("Peso fetal (g):", tags$span(icon(name = "question-circle"))  
                                                             |> add_prompt(message = "Peso em gramas. Digite um valor válido.", position = "right")), 
                               value = NULL),  # Input para peso_fetal
                  actionButton("submit_btn_fw", "Submeter", style = "background-color: #3498db; color: #fff;"), # Botao para submeter
                  conditionalPanel(
                    condition = "input.submit_btn_fw > 0",
                    checkboxInput("save_csv_fw", 
                                  label = tags$span("Deseja salvar os pontos num ficheiro CSV?", 
                                                    tags$span(icon(name = "question-circle"))
                                                    |> add_prompt(message = "Marque esta opção apenas se desejar salvar os pontos adicionados no gráfico num ficheiro CSV.", 
                                                                  position = "right")))
                  ), # Checkbox para salvar em CSV
                  conditionalPanel(
                    condition = "input.save_csv_fw == true && input.submit_btn_fw > 0",
                    downloadButton("download_csv_link_fw", "Download CSV", style = "background-color: #299929; color: #fff;")
                  ),# Botão para fazer o download do CSV
                  actionButton("reset_btn_fw", "Reiniciar", icon = icon("refresh"))  # Botao para reiniciar
                ),
              )
            ),
            # Painel com os graficos
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Diâmetro 1",
                  plotlyOutput("curve_plot_d1", width = "100%", height = "500px", inline = TRUE), # Grafico de Diametro 1
                  verbatimTextOutput("centile_output_d1"), # Saida de texto para Diametro 1
                  actionButton("remove_point_btn_d1", "Remover ponto selecionado", icon = icon("eraser"), style = "background-color: #ff4d4d; color: #fff;") # Botao para remover ponto em Diametro 1
                ),
                tabPanel(
                  "Diâmetro 2",
                  plotlyOutput("curve_plot_d2", width = "100%", height = "500px", inline = TRUE), # Grafico de Diametro 2
                  verbatimTextOutput("centile_output_d2"),
                  actionButton("remove_point_btn_d2", "Remover ponto selecionado", icon = icon("eraser"), style = "background-color: #ff4d4d; color: #fff;")
                ),
                tabPanel(
                  "Espessura",
                  plotlyOutput("curve_plot_pt", width = "100%", height = "500px", inline = TRUE),  # Grafico de Espessura
                  verbatimTextOutput("centile_output_pt"), # Saida de texto para Espessura
                  actionButton("remove_point_btn_pt", "Remover ponto selecionado", icon = icon("eraser"), style = "background-color: #ff4d4d; color: #fff;") # Botao para remover ponto em Espessura
                ),
                tabPanel(
                  "Peso",
                  plotlyOutput("curve_plot_pw", width = "100%", height = "500px", inline = TRUE), # Grafico de Peso
                  verbatimTextOutput("centile_output_pw"),  # Saida de texto para Peso
                  actionButton("remove_point_btn_pw", "Remover ponto selecionado", icon = icon("eraser"), style = "background-color: #ff4d4d; color: #fff;")   # Botao para remover ponto em Peso
                ),
                tabPanel(
                  "Peso fetal",
                  plotlyOutput("curve_plot_fw", width = "100%", height = "500px", inline = TRUE), # Grafico de Peso fetal
                  verbatimTextOutput("centile_output_fw"), # Saida de texto para Peso fetal
                  actionButton("remove_point_btn_fw", "Remover ponto selecionado", icon = icon("eraser"), style = "background-color: #ff4d4d; color: #fff;") # Botao para remover ponto em Peso fetal
                ),
                footer = fluidRow(
                  column(12,
                         downloadButton("download_pdf_link", "Download relatório PDF", style = "background-color: #299929; color: #fff;") # Botao para download de relatorio em PDF
                  )
                ),
              )
            )
          )
        )
      ),
      # INFORMACOES ---------------------------------------------------------------
      tabItem(
        tabName = "point_info",
        h2("Tabelas de informações sobre os pontos adicionados nos gráficos", style = "color: black; font-size: 22px; font-weight: bold;"),  # Titulo informativo
        tabsetPanel(
          tabPanel(
            "Diâmetro 1",
            DT::dataTableOutput("point_info_table_d1") # Tabela de informacoes para Diametro 1
          ),
          tabPanel(
            "Diâmetro 2",
            DT::dataTableOutput("point_info_table_d2") # Tabela de informacoes para Diametro 2
          ),
          tabPanel(
            "Espessura",
            DT::dataTableOutput("point_info_table_pt") # Tabela de informacoes para Espessura
          ),
          tabPanel(
            "Peso",
            DT::dataTableOutput("point_info_table_pw") # Tabela de informacoes para Peso
          ),
          tabPanel(
            "Peso fetal", 
            DT::dataTableOutput("point_info_table_fw") # Tabela de informacoes para Peso fetal
          )
        )
      ),
      # RACIOS ---------------------------------------------------------------
      tabItem(
        tabName = "ratios",
        h2("Cálculo dos rácios",style = "color: black; font-size: 22px; font-weight: bold;"), # Titulo informativo
        includeHTML("ratios.html"),  # Inclui o conteudo HTML externo 
        tabsetPanel(
          tabPanel(
            "Peso",
            DT::dataTableOutput("point_info_table_pw_no_percentil"), # Tabela de informacoes para Peso (sem percentis)
            class = "datatable-margin"
          ),
          tabPanel(
            "Peso Fetal",
            DT::dataTableOutput("point_info_table_fw_no_percentil"), # Tabela de informacoes para Peso Fetal (sem percentis)
            class = "datatable-margin"
          )
        ),
        fluidRow(
          column(12, 
                 actionButton("calculate_ratio1", "Calcular Rácios",  # Botao para calcular racios
                              style = "background-color: #3498db; color: #fff; margin-top: 0px; margin-bottom: 60px;")
          )
        ),
        fluidRow(
          column(12,
                 DT::dataTableOutput("calculated_ratios_table") # Tabela de racios calculados
          )
        )
      ),
      # AJUDA ---------------------------------------------------------------
      tabItem(
        tabName = "help",
        tags$iframe(
          src = "Guia.pdf", # Ficheiro PDF com o guia de utilizacao
          width = "100%",
          height = "1100px"
        )
      )
    )
  )
)