####################################
# Server                           #
####################################


# Define o server
server <- function(input, output, session) {
  
  #Modal de introducao --------------------------------------------------------------
  
  #mostrar o modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"), # Inclui o conteudo HTML no modal
      easyClose = TRUE, # Permite fechar o modal facilmente
      footer = tagList(
        actionButton(inputId = "intro", label = "Fechar", icon = icon("circle-xmark"), style = "background-color: #ff4d4d; color: #fff;") # Botão "Fechar" com ícone e estilo de cor personalizado
      )
    ))
  })
  # Fechar o modal de introducaoo quando o botao "Fechar" e pressionado
  observeEvent(input$intro,{
    removeModal() # Remove o modal
  })
  
  
  
  #Carregar os dados e iniciar variáveis --------------------------------------------------------------
  
  #diametro1
  
  # Carregar os dados do ficheiro "diametro1.csv" com cabecalho
  data_cent_d1 <- read.csv("diametro1.csv", header = TRUE)
  
  # Inicializar uma variavel reativa para os pontos de dados de Diametro1
  points_data_d1 <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Inicializar uma variavel reativa para sinalizar se um ficheiro CSV foi carregado
  load_csv_flag_d1 <- reactiveVal(FALSE)
  
  # Inicializar uma variavel reativa para a saida do grafico de Diametro1
  plot_output_d1 <- reactiveVal()
  
  # Inicializar uma variavel reativa para informacoes sobre pontos de Diametro1
  point_info_d1 <- reactiveVal(data.frame(
    Processo = character(0),
    Data = as.Date(character(0)),
    IG = numeric(0),
    Diâmetro1 = numeric(0),
    Percentil = character(0)
  ))
  
  #diametro2
  
  # Carregar os dados do ficheiro "diametro2.csv" com cabeçalho
  data_cent_d2 <- read.csv("diametro2.csv", header = TRUE)
  
  # Inicializar uma variavel reativa para os pontos de dados de Diametro2
  points_data_d2 <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Inicializar uma variavel reativa para sinalizar se um ficheiro CSV foi carregado
  load_csv_flag_d2 <- reactiveVal(FALSE)
  
  # Inicializar uma variavel reativa para a saida do grafico de Diametro2
  plot_output_d2 <- reactiveVal()
  
  # Inicializar uma variavel reativa para informacoes sobre pontos de Diametro2
  point_info_d2 <- reactiveVal(data.frame(
    Processo = character(0),
    Data = as.Date(character(0)),
    IG = numeric(0),
    Diâmetro2 = numeric(0),
    Percentil = character(0)
  ))
  
  #espessura
  
  # Carregar os dados do ficheiro "placentalthickness.csv" com cabecalho
  data_cent_pt <- read.csv("placentalthickness.csv", header = TRUE)
  
  # Inicializar uma variavel reativa para os pontos de dados de Espessura
  points_data_pt <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Inicializar uma variavel reativa para sinalizar se um ficheiro CSV foi carregado
  load_csv_flag_pt <- reactiveVal(FALSE)
  
  # Inicializar uma variavel reativa para a saida do grafico de Espessura
  plot_output_pt <- reactiveVal()
  
  # Inicializar uma variavel reativa para informacoes sobre pontos de Espessura
  point_info_pt <- reactiveVal(data.frame(
    Processo = character(0),
    Data = as.Date(character(0)),
    IG = numeric(0),
    Espessura = numeric(0),
    Percentil = character(0)
  ))
  
  #peso
  
  # Carregar os dados do ficheiro "placentalweight.csv" com cabecalho
  data_cent_pw <- read.csv("placentalweight.csv", header = TRUE)
  
  # Inicializar uma variavel reativa para os pontos de dados de Peso
  points_data_pw <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Inicializar uma variavel reativa para sinalizar se um ficheiro CSV foi carregado
  load_csv_flag_pw <- reactiveVal(FALSE)
  
  # Inicializar variável reativa para a saida do grafico de Peso
  plot_output_pw <- reactiveVal()
  
  # Inicializar uma variável reativa para informacoes sobre pontos de Peso
  point_info_pw <- reactiveVal(data.frame(
    Processo = character(0),
    Data = as.Date(character(0)),
    IG = numeric(0),
    Peso = numeric(0),
    Percentil = character(0)
  ))
  
  
  #peso_fetal
  
  # Carregar os dados doficheiro "fetalweight.csv" com cabecalho
  data_cent_fw <- read.csv("fetalweight.csv", header = TRUE)
  
  # Inicializar uma variavel reativa para os pontos de dados de Peso Fetal
  points_data_fw <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  
  # Inicializar uma variavel reativa para sinalizar se um ficheiro CSV foi carregado
  load_csv_flag_fw <- reactiveVal(FALSE)
  
  # Inicializar uma variavel reativa para a saida do grafico de Peso Fetal
  plot_output_fw <- reactiveVal()
  
  # Inicializar uma variavel reativa para informacoes sobre pontos de Peso Fetal
  point_info_fw <- reactiveVal(data.frame(
    Processo = character(0),
    Data = as.Date(character(0)),
    IG = numeric(0),
    Peso_fetal = numeric(0),
    Percentil = character(0)
  ))
  
  
  
  
  
  #Funções auxiliares -------------------------------------------------------------- 
  
  
  
  # Funcao para adicionar um novo ponto a um dataframe
  addPoint <- function(x_input, y_input, points_data) {
    # Obter os valores de entrada x e y a partir dos inputs
    x <- input[[x_input]]
    y <- input[[y_input]]
    max_value <- NULL
    error_message <- NULL
    
    if (is.null(x) || is.null(y)) {
      # Mostrar mensagem de erro se x ou y não estiver preenchido
      showNotification("Por favor, preencha ambos os campos.", type = "error")
      return(NULL)
    } else if (!is.integer(x) || x < 12 || x > 41) {
      # Verificar se a idade gestacional (x) e um numero inteiro no intervalo de 12 a 41 semanas
      showNotification("A idade gestacional deve ser um número inteiro no intervalo de 12 a 41 semanas.", type = "error")
      return(NULL)
    } else if (y_input == "diameter1_input" || y_input == "diameter2_input") {
      # Definir o valor maximo e a mensagem de erro com base no tipo de entrada (diametro 1 ou 2)
      max_value <- 30
      error_message <- "O valor do diâmetro 1 deve ser um número acima de 0 e abaixo de 30 cm."
    } else if (y_input == "pt_input") {
      # Definir o valor maximo e a mensagem de erro para espessura
      max_value <- 7
      error_message <- "O valor da espessura deve ser um número acima de 0 e abaixo de 7 cm."
    } else if (y_input == "pw_input") {
      # Definir o valor maximo e a mensagem de erro para peso
      max_value <- 900
      error_message <- "O valor do peso deve ser um número acima de 0 e abaixo de 900 g."
    } else if (y_input == "fw_input") {
      # Definir o valor maximo e a mensagem de erro para peso fetal
      max_value <- 6000
      error_message <- "O valor do peso fetal deve ser um número acima de 0 e abaixo de 6000 g."
    }
    
    # Verificar se y nao e numerico ou esta fora do intervalo maximo e mostrar mensagem de erro
    if (!is.numeric(y) || y <= 0 || y > max_value) {
      # Mostrar a mensagem de erro base em y_input
      showNotification(error_message, type = "error")
      return(NULL)
    } else {
      # Obter o dataframe atual de pontos e criar um novo ponto
      df <- points_data()
      new_point <- data.frame(x = x, y = y)
      
      # Verificar se o numero de colunas do novo ponto corresponde ao dataframe existente
      if (ncol(df) != ncol(new_point)) {
        #  Se nao corresponder, cria um ponto vazio com a mesma estrutura
        empty_point <- data.frame(matrix(ncol = ncol(df)))
        colnames(empty_point) <- colnames(df)
        new_point <- empty_point
      }
      # Adicionar o novo ponto ao dataframe de pontos
      df <- rbind(df, new_point)
      points_data(df)
      
      # Mostrar mensagem de sucesso
      showNotification(
        "Ponto adicionado com sucesso!",
        type = "message"
      )
      return(TRUE)
    }
  }
  
  
  
  # Função para encontrar o percentil para um determinado ponto
  findCentile <- function(x, y, df) {
    # Encontra a linha no dataframe onde x está mais próximo do valor de x fornecido
    nearest_row <- which.min(abs(df$x - x))
    
    if (y == df$cent3[nearest_row]) {
      # Se o valor de y for igual ao valor do percentil 3, atribui o percentil como "no 3º"
      percentile <- "no 3º"
    } else if (y == df$cent10[nearest_row]) {
      percentile <- "no 10º"
    } else if (y == df$cent25[nearest_row]) {
      percentile <- "no 25º"
    } else if (y == df$cent50[nearest_row]) {
      percentile <- "no 50º"
    } else if (y == df$cent75[nearest_row]) {
      percentile <- "no 75º"
    } else if (y == df$cent90[nearest_row]) {
      percentile <- "no 90º"
    } else if (y == df$cent97[nearest_row]) {
      percentile <- "no 97º"
    } else {
      # Determine o percentil com base no valor y
      if (y < df$cent3[nearest_row]) {
        # Se o valor de y for abaixo do valor do percentil 3, atribui o percentil como "abaixo do 3º"
        percentile <- "abaixo do 3º"
      } else if (y >= df$cent3[nearest_row] && y < df$cent10[nearest_row]) {
        percentile <- "entre o 3º e o 10º"
      } else if (y >= df$cent10[nearest_row] && y < df$cent25[nearest_row]) {
        percentile <- "entre o 10º e o 25º"
      } else if (y >= df$cent25[nearest_row] && y < df$cent50[nearest_row]) {
        percentile <- "entre o 25º e 50º"
      } else if (y >= df$cent50[nearest_row] && y < df$cent75[nearest_row]) {
        percentile <- "entre o 50º e o 75º"
      } else if (y >= df$cent75[nearest_row] && y < df$cent90[nearest_row]) {
        percentile <- "entre o 75º e o 90º"
      } else if (y >= df$cent90[nearest_row] && y < df$cent97[nearest_row]) {
        percentile <- "entre o 90º e o 97º"
      } else if (y >= df$cent97[nearest_row]) {
        percentile <- "acima do 97º"
      } else {
        percentile <- "desconhecido"
      }
    }
    
    return(percentile)
  }
  
  
  
  # Funcao para atualizar todo o grafico
  update_plot <- function(plot_name, points_data ,plot_output, data_cent) {
    df_points <- points_data()
    
    if (nrow(df_points) == 0) {
      # Limpa o gráfico se nenhum ponto estiver presente
      p <- ggplot() +
        NULL 
    } else {
      # Cria um novo gráfico com os pontos fornecidos
      plot_name <- ggplot() +
        geom_point(data = df_points, aes(x = x, y = y), color = "red")
      
      # Define informações percentuais para cada ponto
      centile_info <- sapply(1:nrow(df_points), function(i) {
        x <- df_points$x[i]
        y <- df_points$y[i]
        centile <- findCentile(x, y, data_cent)
        paste("Coordenadas (x, y):", x, ",", y, "\nO ponto encontra-se", centile, "percentil.")
      })
      
      # Adiciona anotacoes de texto para as informacoes percentuais de cada ponto
      for (i in 1:nrow(df_points)) {
        p <- p + geom_text(
          x = df_points$x[i],
          y = df_points$y[i],
          label = centile_info[i],
          show.legend = FALSE,
          hjust = 0,  
          vjust = 0.5,
          color = "black"
        )
      }
    }
    
    
    plot_output(plot_name)  # Atualiza o valor reativo com o novo grafico
  }
  
  
  # Observacoes para atualizar os graficos com base nos dados inseridos
  observe({
    update_plot("d1",points_data_d1, plot_output_d1, data_cent_d1)
  })
  
  observe({
    update_plot("d2",points_data_d2, plot_output_d2, data_cent_d2)
  })
  
  observe({
    update_plot("pt",points_data_pt, plot_output_pt, data_cent_pt)
  })
  
  observe({
    update_plot("pw",points_data_pw, plot_output_pw, data_cent_pw)
  })
  
  observe({
    update_plot("fw",points_data_fw, plot_output_fw, data_cent_fw)
  })
  
  
  
  # Função para verificar se um valor é numérico e está dentro de um intervalo especificado
  checkNumericRange <- function(value, min_value, max_value) {
    # Verifica se o valor e numerico
    is_numeric <- is.numeric(value)
    # Verifica se o valor esta dentro do intervalo especificado
    is_in_range <- value >= min_value && value < max_value
    # Retorna TRUE se ambas as condicoes forem atendidas
    return(is_numeric && is_in_range)
  }
  
  
  
  # Carregar csv com pontos  --------------------------------------------------------------
  
  # Carregamento do CSV
  
  #diametro1
  
  
  observeEvent(input$load_csv_d1, {
    if (input$load_csv_d1 == "Sim") {
      # Define a flag de carregamento do CSV como verdadeira
      load_csv_flag_d1(TRUE)
    } else {
      load_csv_flag_d1(FALSE)
      points_data_d1(data.frame(x = numeric(0), y = numeric(0)))
    }
  })
  
  #diametro2
  
  
  observeEvent(input$load_csv_d2, {
    if (input$load_csv_d2 == "Sim") {
      # Define a flag de carregamento do CSV como verdadeira
      load_csv_flag_d2(TRUE)
    } else {
      # Define a flag de carregamento do CSV como falsa e reinicia os pontos de dados
      load_csv_flag_d2(FALSE)
      points_data_d2(data.frame(x = numeric(0), y = numeric(0)))
    }
  })
  
  #espessura
  
  
  observeEvent(input$load_csv_pt, {
    if (input$load_csv_pt == "Sim") {
      # Define a flag de carregamento do CSV como verdadeira
      load_csv_flag_pt(TRUE)
    } else {
      # Define a flag de carregamento do CSV como falsa e reinicia os pontos de dados
      load_csv_flag_pt(FALSE)
      points_data_pt(data.frame(x = numeric(0), y = numeric(0)))
    }
  })
  
  #peso 
  
  
  observeEvent(input$load_csv_pw, {
    if (input$load_csv_pw == "Sim") {
      # Define a flag de carregamento do CSV como verdadeira
      load_csv_flag_pw(TRUE)
    } else {
      # Define a flag de carregamento do CSV como falsa e reinicia os pontos de dados
      load_csv_flag_pw(FALSE)
      points_data_pw(data.frame(x = numeric(0), y = numeric(0)))
    }
  })
  
  #peso_fetal
  
  
  observeEvent(input$load_csv_fw, {
    if (input$load_csv_fw == "Sim") {
      # Define a flag de carregamento do CSV como verdadeira
      load_csv_flag_fw(TRUE)
    } else {
      # Define a flag de carregamento do CSV como falsa e reinicia os pontos de dados
      load_csv_flag_fw(FALSE)
      points_data_fw(data.frame(x = numeric(0), y = numeric(0)))
    }
  })
  
  
  # Input do csv
  
  #diametro1
  
  
  observeEvent(input$csv_file_d1, {
    if (!is.null(input$csv_file_d1)) {
      df <- read.csv(input$csv_file_d1$datapath)
      
      # Verifica se os dados CSV possuem duas colunas numéricas (x e y)
      if (ncol(df) != 2 || !all(sapply(df, is.numeric))) {
        showNotification(
          "O ficheiro CSV deve ter duas colunas numéricas (x e y).",
          type = "error"
        )
        return()
      }
      
      # Inicializa uma flag de erro
      has_errors <- FALSE
      # Loop por cada linha do CSV
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        # Verifica se 'x' é um número inteiro entre 12 e 41
        if (!is.integer(row$x) || row$x < 12 || row$x > 41) {
          showNotification(
            "Os valores de IG (x) no ficheiro CSV devem ser números inteiros entre 12 e 41 semanas.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
        
        # Verifica o intervalo de y para d1
        max_value <- 30
        if (row$y < 0 || row$y > max_value) {
          showNotification(
            "Os valores de Diâmetro 1 (y) no ficheiro CSV devem estar entre 0 e 30 cm.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
      }
      
      if (has_errors) {
        return()
      }
      # Se nenhum erro for encontrado, combina os dados e atualiza o point_info
      combined_df <- rbind(points_data_d1(), df)
      points_data_d1(combined_df)
      # Atualiza point_info com informações dos percentis para ambos os tipos de pontos
      centile_info <- sapply(1:nrow(combined_df), function(i) {
        x <- combined_df$x[i]
        y <- combined_df$y[i]
        centile <- findCentile(x, y, data_cent_d1)
        paste(centile)
      })
      # Combina processo, data e outras informações
      updated_point_info <- data.frame(
        Processo = input$process_number,
        Data = input$date_input,
        IG = combined_df$x,
        Diâmetro1 = combined_df$y,
        Percentil = centile_info
      )
      point_info_d1(updated_point_info)
    }
  })
  
  
  
  
  #diametro2
  
  
  observeEvent(input$csv_file_d2, {
    if (!is.null(input$csv_file_d2)) {
      df <- read.csv(input$csv_file_d2$datapath)
      
      # Verifica se os dados CSV possuem duas colunas numéricas (x e y)
      if (ncol(df) != 2 || !all(sapply(df, is.numeric))) {
        showNotification(
          "O ficheiro CSV deve ter duas colunas numéricas (x e y).",
          type = "error"
        )
        return()
      }
      
      # Inicializa uma flag de erro
      has_errors <- FALSE
      # Loop por cada linha do CSV
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        # Verifica se 'x' é um número inteiro entre 12 e 41
        if (!is.integer(row$x) || row$x < 12 || row$x > 41) {
          showNotification(
            "Os valores de IG (x) no ficheiro CSV devem ser números inteiros entre 12 e 41 semanas.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
        
        # Verifica o intervalo de y para d2
        max_value <- 30
        if (row$y < 0 || row$y > max_value) {
          showNotification(
            "Os valores de Diâmetro 2 (y) no ficheiro CSV devem estar entre 0 e 30 cm.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
      }
      
      if (has_errors) {
        return()
      }
      # Se nenhum erro for encontrado, combina os dados e atualiza o point_info
      combined_df <- rbind(points_data_d2(), df)
      points_data_d2(combined_df)
      # Atualiza point_info com informações dos percentis para ambos os tipos de pontos
      centile_info <- sapply(1:nrow(combined_df), function(i) {
        x <- combined_df$x[i]
        y <- combined_df$y[i]
        centile <- findCentile(x, y, data_cent_d2)
        paste(centile)
      })
      # Combina processo, data e outras informações
      updated_point_info <- data.frame(
        Processo = input$process_number,
        Data = input$date_input,
        IG = combined_df$x,
        Diâmetro2 = combined_df$y,
        Percentil = centile_info
      )
      point_info_d2(updated_point_info)
    }
  })
  
  
  
  #espessura
  
  
  observeEvent(input$csv_file_pt, {
    if (!is.null(input$csv_file_pt)) {
      df <- read.csv(input$csv_file_pt$datapath)
      
      # Verifica se os dados CSV possuem duas colunas numéricas (x e y)
      if (ncol(df) != 2 || !all(sapply(df, is.numeric))) {
        showNotification(
          "O ficheiro CSV deve ter duas colunas numéricas (x e y).",
          type = "error"
        )
        return()
      }
      
      # Inicializa uma flag de erro
      has_errors <- FALSE
      # Loop por cada linha do CSV
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        # Verifica se 'x' é um número inteiro entre 12 e 41
        if (!is.integer(row$x) || row$x < 12 || row$x > 41) {
          showNotification(
            "Os valores de IG (x) no ficheiro CSV devem ser números inteiros entre 12 e 41 semanas.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
        
        # Verifica o intervalo de y para pt
        max_value <- 7
        if (row$y < 0 || row$y > max_value) {
          showNotification(
            "Os valores de Espessura (y) no ficheiro CSV devem estar entre 0 e 7 cm.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
      }
      
      if (has_errors) {
        return()
      }
      # Se nenhum erro for encontrado, combina os dados e atualiza o point_info
      combined_df <- rbind(points_data_pt(), df)
      points_data_pt(combined_df)
      # Atualiza point_info com informações dos percentis para ambos os tipos de pontos
      centile_info <- sapply(1:nrow(combined_df), function(i) {
        x <- combined_df$x[i]
        y <- combined_df$y[i]
        centile <- findCentile(x, y, data_cent_pt)
        paste(centile)
      })
      # Combina processo, data e outras informações
      updated_point_info <- data.frame(
        Processo = input$process_number,
        Data = input$date_input,
        IG = combined_df$x,
        Espessura = combined_df$y,
        Percentil = centile_info
      )
      point_info_pt(updated_point_info)
    }
  })
  
  
  
  #peso
  
  
  observeEvent(input$csv_file_pw, {
    if (!is.null(input$csv_file_pw)) {
      df <- read.csv(input$csv_file_pw$datapath)
      
      # Verifica se os dados CSV possuem duas colunas numéricas (x e y)
      if (ncol(df) != 2 || !all(sapply(df, is.numeric))) {
        showNotification(
          "O ficheiro CSV deve ter duas colunas numéricas (x e y).",
          type = "error"
        )
        return()
      }
      
      # Inicializa uma flag de erro
      has_errors <- FALSE
      
      # Loop por cada linha do CSV
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        # Verifica se 'x' é um número inteiro entre 12 e 41
        if (!is.integer(row$x) || row$x < 12 || row$x > 41) {
          showNotification(
            "Os valores de IG (x) no ficheiro CSV devem ser números inteiros entre 12 e 41 semanas.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
        
        # Verifica o intervalo de y para pw
        max_value <- 900
        if (row$y < 0 || row$y > max_value) {
          showNotification(
            "Os valores de Peso (y) no ficheiro CSV devem estar entre 0 e 900 g.",
            type = "error"
          )
          has_errors <- TRUE
          break  # Sai do loop no primeiro erro
        }
      }
      
      if (has_errors) {
        return()
      }
      # Se nenhum erro for encontrado, combina os dados e atualiza o point_info
      combined_df <- rbind(points_data_pw(), df)
      points_data_pw(combined_df)
      # Atualiza point_info com informações dos percentis para ambos os tipos de pontos
      centile_info <- sapply(1:nrow(combined_df), function(i) {
        x <- combined_df$x[i]
        y <- combined_df$y[i]
        centile <- findCentile(x, y, data_cent_pw)
        paste(centile)
      })
      # Combina processo, data e outras informações
      updated_point_info <- data.frame(
        Processo = input$process_number,
        Data = input$date_input,
        IG = combined_df$x,
        Peso = combined_df$y,
        Percentil = centile_info
      )
      point_info_pw(updated_point_info)
    }
  })
  
  
  #peso_fetal
  
  
  observeEvent(input$csv_file_fw, {
    if (!is.null(input$csv_file_fw)) {
      df <- read.csv(input$csv_file_fw$datapath)
      
      # Verifica se os dados CSV possuem duas colunas numéricas (x e y)
      if (ncol(df) != 2 || !all(sapply(df, is.numeric))) {
        showNotification(
          "O ficheiro CSV deve ter duas colunas numéricas (x e y).",
          type = "error"
        )
        return()
      }
      
      # Inicializa uma flag de erro
      has_errors <- FALSE
      
      # Loop por cada linha do CSV
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        
        # Verifica se 'x' é um número inteiro entre 12 e 41
        if (!is.integer(row$x) || row$x < 12 || row$x > 41) {
          showNotification(
            "Os valores de IG (x) no ficheiro CSV devem ser números inteiros entre 12 e 41 semanas.",
            type = "error"
          )
          has_errors <- TRUE
          break # Sai do loop no primeiro erro
        }
        
        # Verifica o intervalo de y para pf
        max_value <- 6000
        if (row$y < 0 || row$y > max_value) {
          showNotification(
            "Os valores de Peso fetal (y) no ficheiro CSV devem estar entre 0 e 6000 g.",
            type = "error"
          )
          has_errors <- TRUE
          break   # Sai do loop no primeiro erro
        }
      }
      
      if (has_errors) {
        return()
      }
      
      # Se nenhum erro for encontrado, combina os dados e atualiza o point_info
      combined_df <- rbind(points_data_fw(), df)
      points_data_fw(combined_df)
      
      # Atualiza point_info com informações dos percentis para ambos os tipos de pontos
      centile_info <- sapply(1:nrow(combined_df), function(i) {
        x <- combined_df$x[i]
        y <- combined_df$y[i]
        centile <- findCentile(x, y, data_cent_fw)
        paste(centile)
      })
      
      # Combina processo, data e outras informações
      updated_point_info <- data.frame(
        Processo = input$process_number,
        Data = input$date_input,
        IG = combined_df$x,
        Peso_fetal = combined_df$y,
        Percentil = centile_info
      )
      point_info_fw(updated_point_info)
    }
  })
  
  
  #GRÁFICOS --------------------------------------------------------------
  
  
  #diametro1
  
  # Legenda do percentil 97% no gráfico
  legend_data_d1 <- data.frame(x = 41.5, y = data_cent_d1$cent97[nrow(data_cent_d1)], label = "97%")
  
  
  # Render plot
  output$curve_plot_d1 <- renderPlotly({
    # Traçar as curvas de referência
    d1 <- ggplot(data_cent_d1, aes(x = x)) +
      geom_ribbon(aes(ymin = cent3, ymax = cent10), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent10, ymax = cent25), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent25, ymax = cent50), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent50, ymax = cent75), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent75, ymax = cent90), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent90, ymax = cent97), fill = "red", alpha = 0.2) +
      geom_line(aes(y = cent50), color = "darkgreen") +
      geom_text(data = subset(data_cent_d1, x == max(x)),
                aes(x = 42, y = cent3, label = "3%"), vjust = -0.5, color = "red", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d1, x == max(x)),
                aes(x = 42, y = cent10, label = "10%"), vjust = -0.5, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d1, x == max(x)),
                aes(x = 42, y = cent25, label = "25%"), vjust = -0.5, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d1, x == max(x)),
                aes(x = 42, y = cent50, label = "50%"), vjust = -0.5, color = "darkgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d1, x == max(x)),
                aes(x = 42, y = cent75, label = "75%"), vjust = -0.5, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d1, x == max(x)),
                aes(x = 42, y = cent90, label = "90%"), vjust = -0.5, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = legend_data_d1, aes(x = 42, y = y, label = label), vjust = -0.5, color = "red") +
      geom_blank(aes(fill = "97%")) +
      xlab("Idade gestacional (semanas)") +
      ylab("Diâmetro 1 (cm)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(12, 41, 1), limits = c(12, max(data_cent_d1$x) + 2), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, max(data_cent_d1$cent97), 2)) +
      guides(fill = guide_legend(title = "Percentil", override.aes = list(vjust = 1.5, hjust = 0.5)))
    
    # Desenha os pontos definidos pelo utilizador no gráfico
    df_d1 <- points_data_d1()
    if (nrow(df_d1) > 0) {
      d1 <- d1 + geom_point(data = df_d1, aes(x = x, y = y), color = "red")
      
    }
    
    
    # Converte o objeto ggplot em plotly para interatividade
    d1 <- ggplotly(d1, source = "source_d1")
    
    
    
    # Regista o evento plotly_click
    event_register(d1, "plotly_click")
    
    
    
    # Retorna o plot interativo
    d1
  })
  
  
  
  #diametro2
  
  
  # Legenda do percentil 97% no gráfico
  legend_data_d2 <- data.frame(x = 41.5, y = data_cent_d2$cent97[nrow(data_cent_d2)], label = "97%")
  
  
  # Render plot
  output$curve_plot_d2 <- renderPlotly({
    # Traçar as curvas de referência
    d2 <- ggplot(data_cent_d2, aes(x = x)) +
      geom_ribbon(aes(ymin = cent3, ymax = cent10), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent10, ymax = cent25), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent25, ymax = cent50), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent50, ymax = cent75), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent75, ymax = cent90), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent90, ymax = cent97), fill = "red", alpha = 0.2) +
      geom_line(aes(y = cent50), color = "darkgreen") +
      geom_text(data = subset(data_cent_d2, x == max(x)),
                aes(x = 42, y = cent3, label = "3%"), vjust = -0.5, color = "red", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d2, x == max(x)),
                aes(x = 42, y = cent10, label = "10%"), vjust = -0.5, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d2, x == max(x)),
                aes(x = 42, y = cent25, label = "25%"), vjust = -0.5, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d2, x == max(x)),
                aes(x = 42, y = cent50, label = "50%"), vjust = 0.2, color = "darkgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d2, x == max(x)),
                aes(x = 42, y = cent75, label = "75%"), vjust = 1.2, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_d2, x == max(x)),
                aes(x = 42, y = cent90, label = "90%"), vjust = 2, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = legend_data_d2, aes(x = 42, y = y, label = label), vjust = 3, color = "red") +
      geom_blank(aes(fill = "97%")) +
      xlab("Idade gestacional (semanas)") +
      ylab("Diâmetro 2 (cm)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(12, 41, 1), limits = c(12, max(data_cent_d2$x) + 2), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, max(data_cent_d2$cent97), 2)) +
      guides(fill = guide_legend(title = "Percentil", override.aes = list(vjust = 1.5, hjust = 0.5)))
    
    # Desenha os pontos definidos pelo utilizador no gráfico
    df_d2 <- points_data_d2()
    if (nrow(df_d2) > 0) {
      d2 <- d2 + geom_point(data = df_d2, aes(x = x, y = y), color = "red")
    }
    
    
    # Converte o objeto ggplot em plotly para interatividade
    d2 <- ggplotly(d2, source = "source_d2")
    
    
    # Regista o evento plotly_click
    event_register(d2, "plotly_click")
    
    
    # Retorna o plot interativo
    d2
  })
  
  
  
  #espessura
  
  
  # Legenda do percentil 97% no gráfico
  legend_data_pt <- data.frame(x = 41.5, y = data_cent_pt$cent97[nrow(data_cent_pt)], label = "97%")
  
  
  # Render plot
  output$curve_plot_pt <- renderPlotly({
    # Traçar as curvas de referência
    pt <- ggplot(data_cent_pt, aes(x = x)) +
      geom_ribbon(aes(ymin = cent3, ymax = cent10), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent10, ymax = cent25), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent25, ymax = cent50), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent50, ymax = cent75), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent75, ymax = cent90), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent90, ymax = cent97), fill = "red", alpha = 0.2) +
      geom_line(aes(y = cent50), color = "darkgreen") +
      geom_text(data = subset(data_cent_pt, x == max(x)),
                aes(x = 42, y = cent3, label = "3%"), vjust = -0.5, color = "red", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pt, x == max(x)),
                aes(x = 42, y = cent10, label = "10%"), vjust = -0.5, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pt, x == max(x)),
                aes(x = 42, y = cent25, label = "25%"), vjust = -0.5, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pt, x == max(x)),
                aes(x = 42, y = cent50, label = "50%"), vjust = 0.2, color = "darkgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pt, x == max(x)),
                aes(x = 42, y = cent75, label = "75%"), vjust = 1.2, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pt, x == max(x)),
                aes(x = 42, y = cent90, label = "90%"), vjust = 2, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = legend_data_pt, aes(x = 42, y = y, label = label), vjust = 3, color = "red") +
      geom_blank(aes(fill = "97%")) +
      xlab("Idade gestacional (semanas)") +
      ylab("Espessura da placenta (cm)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(12, 41, 1), limits = c(12, max(data_cent_pt$x) + 2), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, max(data_cent_pt$cent97), 0.5)) +
      guides(fill = guide_legend(title = "Percentil", override.aes = list(vjust = 1.5, hjust = 0.5)))
    
    # Desenha os pontos definidos pelo utilizador no gráfico
    df_pt <- points_data_pt()
    if (nrow(df_pt) > 0) {
      pt <- pt + geom_point(data = df_pt, aes(x = x, y = y), color = "red")
    }
    
    
    # Converte o objeto ggplot em plotly para interatividade
    pt <- ggplotly(pt, source = "source_pt")
    
    
    # Regista o evento plotly_click
    event_register(pt, "plotly_click")
    
    
    # Retorna o plot interativo
    pt
  })
  
  
  
  #peso
  
  
  # Legenda do percentil 97% no gráfico
  legend_data_pw <- data.frame(x = 41.5, y = data_cent_pw$cent97[nrow(data_cent_pw)], label = "97%")
  
  
  # Render plot
  output$curve_plot_pw <- renderPlotly({
    # Traçar as curvas de referência
    pw <- ggplot(data_cent_pw, aes(x = x)) +
      geom_ribbon(aes(ymin = cent3, ymax = cent10), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent10, ymax = cent25), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent25, ymax = cent50), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent50, ymax = cent75), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent75, ymax = cent90), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent90, ymax = cent97), fill = "red", alpha = 0.2) +
      geom_line(aes(y = cent50), color = "darkgreen") +
      geom_text(data = subset(data_cent_pw, x == max(x)),
                aes(x = 42, y = cent3, label = "3%"), vjust = -0.5, color = "red", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pw, x == max(x)),
                aes(x = 42, y = cent10, label = "10%"), vjust = -0.5, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pw, x == max(x)),
                aes(x = 42, y = cent25, label = "25%"), vjust = -0.5, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pw, x == max(x)),
                aes(x = 42, y = cent50, label = "50%"), vjust = 0.2, color = "darkgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pw, x == max(x)),
                aes(x = 42, y = cent75, label = "75%"), vjust = 1.2, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_pw, x == max(x)),
                aes(x = 42, y = cent90, label = "90%"), vjust = 2, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = legend_data_pw, aes(x = 42, y = y, label = label), vjust = 3, color = "red") +
      geom_blank(aes(fill = "97%")) +
      xlab("Idade gestacional (semanas)") +
      ylab("Peso placentar (g)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(12, 41, 1), limits = c(12, max(data_cent_pw$x) + 2), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, max(data_cent_pw$cent97), 100)) +
      guides(fill = guide_legend(title = "Percentil", override.aes = list(vjust = 1.5, hjust = 0.5)))
    
    
    # Desenha os pontos definidos pelo utilizador no gráfico
    df_pw <- points_data_pw()
    if (nrow(df_pw) > 0) {
      pw <- pw + geom_point(data = df_pw, aes(x = x, y = y), color = "red")
    }
    
    
    # Converte o objeto ggplot em plotly para interatividade
    pw <- ggplotly(pw, source = "source_pw")
    
    
    # Regista o evento plotly_click
    event_register(pw, "plotly_click")
    
    
    # Retorna o plot interativo
    pw
  })
  
  
  
  #peso_fetal
  
  
  # Legenda do percentil 97% no gráfico
  legend_data_fw <- data.frame(x = 41.5, y = data_cent_fw$cent97[nrow(data_cent_fw)], label = "97%")
  
  
  # Render plot
  output$curve_plot_fw <- renderPlotly({
    # Traçar as curvas de referência
    fw <- ggplot(data_cent_fw, aes(x = x)) +
      geom_ribbon(aes(ymin = cent3, ymax = cent10), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent10, ymax = cent25), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent25, ymax = cent50), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent50, ymax = cent75), fill = "green4", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent75, ymax = cent90), fill = "lightgreen", alpha = 0.2) +
      geom_ribbon(aes(ymin = cent90, ymax = cent97), fill = "red", alpha = 0.2) +
      geom_line(aes(y = cent50), color = "darkgreen") +
      geom_text(data = subset(data_cent_fw, x == max(x)),
                aes(x = 42, y = cent3, label = "3%"), vjust = -0.5, color = "red", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_fw, x == max(x)),
                aes(x = 42, y = cent10, label = "10%"), vjust = -0.5, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_fw, x == max(x)),
                aes(x = 42, y = cent25, label = "25%"), vjust = -0.5, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_fw, x == max(x)),
                aes(x = 42, y = cent50, label = "50%"), vjust = 0.2, color = "darkgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_fw, x == max(x)),
                aes(x = 42, y = cent75, label = "75%"), vjust = 1.2, color = "green4", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = subset(data_cent_fw, x == max(x)),
                aes(x = 42, y = cent90, label = "90%"), vjust = 2, color = "lightgreen", show.legend = FALSE, na.rm = TRUE) +
      geom_text(data = legend_data_fw, aes(x = 42, y = y, label = label), vjust = 3, color = "red") +
      geom_blank(aes(fill = "97%")) +
      xlab("Idade gestacional (semanas)") +
      ylab("Peso fetal (g)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(12, 41, 1), limits = c(12, max(data_cent_fw$x) + 2), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, max(data_cent_fw$cent97), 500)) +
      guides(fill = guide_legend(title = "Percentil", override.aes = list(vjust = 1.5, hjust = 0.5)))
    
    # Desenha os pontos definidos pelo utilizador no gráfico
    df_fw <- points_data_fw()
    if (nrow(df_fw) > 0) {
      fw <- fw + geom_point(data = df_fw, aes(x = x, y = y), color = "red")
    }
    
    
    # Converte o objeto ggplot em plotly para interatividade
    fw <- ggplotly(fw, source = "source_fw")
    
    
    # Regista o evento plotly_click
    event_register(fw, "plotly_click")
    
    
    # Retorna o plot interativo
    fw
  })
  
  
  
  
  # BOTAO SUBMISSAO --------------------------------------------------------------
  
  #diametro1
  
  # Observar o evento de submissão
  observeEvent(input$submit_btn_d1, {
    req(input$ga_input_d1, input$diameter1_input)
    add_result <- addPoint("ga_input_d1","diameter1_input",points_data_d1) 
    
    if (!is.null(add_result)) {
      # Obter valores de entrada
      x <- input$ga_input_d1
      y <- input$diameter1_input
      
      # Encontrar o percentil correspondente para o ponto
      centile <- findCentile(x, y, data_cent_d1)
      
      # Criar um novo conjunto de dados com informacoes do ponto
      new_data <- data.frame(
        IG = x,
        Diâmetro1 = y,
        Percentil = centile,
        Processo = input$process_number,  
        Data = input$date_input  
      )
      
      # Atualizar a tabela de informacoes de pontos
      df <- bind_rows(point_info_d1(), new_data)
      point_info_d1(df)
      
      # Atualizar a saida de texto para exibir informacoes do ponto
      output$centile_output_d1 <- renderText({
        paste("Coordenadas (IG, Diâmetro 1):", x, ",", y, "\nO ponto encontra-se", centile, "percentil.")
      })
      
      # Limpar os campos de entrada apos a submissso
      updateNumericInput(session, "ga_input_d1", value = "")  # Apaga input
      updateNumericInput(session, "diameter1_input", value = "")  # Apaga input
    }
  })
  
  
  #diametro2
  
  # Observar o evento de submissão
  observeEvent(input$submit_btn_d2, {
    req(input$ga_input_d2, input$diameter2_input)
    add_result <- addPoint("ga_input_d2","diameter2_input", points_data_d2)  
    
    if (!is.null(add_result)) {
      # Obter valores de entrada
      x <- input$ga_input_d2
      y <- input$diameter2_input
      
      # Encontrar o percentil correspondente para o ponto
      centile <- findCentile(x, y, data_cent_d2)
      
      # Criar um novo conjunto de dados com informacoes do ponto
      new_data <- data.frame(
        IG = x,
        Diâmetro2 = y,
        Percentil = centile,
        Processo = input$process_number,  
        Data = input$date_input  
      )
      
      # Atualizar a tabela de informacoes de pontos
      df <- bind_rows(point_info_d2(), new_data)
      point_info_d2(df)
      
      # Atualizar a saida de texto para exibir informacoes do ponto
      output$centile_output_d2 <- renderText({
        paste("Coordenadas (IG, Diâmetro 2):", x, ",", y, "\nO ponto encontra-se", centile, "percentil.")
      })
      
      # Limpar os campos de entrada apos a submissso
      updateNumericInput(session, "ga_input_d2", value = "")  # Apaga input
      updateNumericInput(session, "diameter2_input", value = "")  # Apaga input
    }
  })
  
  
  #espessura
  
  # Observar o evento de submissão
  observeEvent(input$submit_btn_pt, {
    req(input$ga_input_pt, input$pt_input)
    add_result <- addPoint("ga_input_pt","pt_input", points_data_pt)  
    
    if (!is.null(add_result)) {
      # Obter valores de entrada
      x <- input$ga_input_pt
      y <- input$pt_input
      
      # Encontrar o percentil correspondente para o ponto
      centile <- findCentile(x, y, data_cent_pt)
      
      # Criar um novo conjunto de dados com informacoes do ponto
      new_data <- data.frame(
        IG = x,
        Espessura = y,
        Percentil = centile,
        Processo = input$process_number,  
        Data = input$date_input  
      )
      
      # Atualizar a tabela de informacoes de pontos
      df <- bind_rows(point_info_pt(), new_data)
      point_info_pt(df)
      
      # Atualizar a saida de texto para exibir informacoes do ponto
      output$centile_output_pt <- renderText({
        paste("Coordenadas (IG, Espessura):", x, ",", y, "\nO ponto encontra-se", centile, "percentil.")
      })
      
      # Limpar os campos de entrada apos a submissso
      updateNumericInput(session, "ga_input_pt", value = "")  # Apaga input
      updateNumericInput(session, "pt_input", value = "")  # Apaga input
    }
  })
  
  #peso
  
  # Observar o evento de submissão
  observeEvent(input$submit_btn_pw, {
    req(input$ga_input_pw, input$pw_input)
    add_result <- addPoint("ga_input_pw","pw_input", points_data_pw) 
    
    if (!is.null(add_result)) {
      # Obter valores de entrada
      x <- input$ga_input_pw
      y <- input$pw_input
      
      # Encontrar o percentil correspondente para o ponto
      centile <- findCentile(x, y, data_cent_pw)
      
      # Criar um novo conjunto de dados com informacoes do ponto
      new_data <- data.frame(
        IG = x,
        Peso = y,
        Percentil = centile,
        Processo = input$process_number, 
        Data = input$date_input 
      )
      
      # Atualizar a tabela de informacoes de pontos
      df <- bind_rows(point_info_pw(), new_data)
      point_info_pw(df)
      
      # Atualizar a saida de texto para exibir informacoes do ponto
      output$centile_output_pw <- renderText({
        paste("Coordenadas (IG, Peso):", x, ",", y, "\nO ponto encontra-se", centile, "percentil.")
      })
      
      # Limpar os campos de entrada apos a submissso
      updateNumericInput(session, "ga_input_pw", value = "")  # Apaga input
      updateNumericInput(session, "pw_input", value = "")  # Apaga input
    }
  })
  
  
  #peso_fetal
  
  # Observar o evento de submissão
  observeEvent(input$submit_btn_fw, {
    req(input$ga_input_fw, input$fw_input)
    add_result <- addPoint("ga_input_fw","fw_input", points_data_fw) 
    if (!is.null(add_result)) {
      # Obter valores de entrada
      x <- input$ga_input_fw
      y <- input$fw_input
      
      # Encontrar o percentil correspondente para o ponto
      centile <- findCentile(x, y, data_cent_fw)
      
      # Criar um novo conjunto de dados com informacoes do ponto
      new_data <- data.frame(
        IG = x,
        Peso_fetal = y,
        Percentil = centile,
        Processo = input$process_number, 
        Data = input$date_input 
      )
      
      # Atualizar a tabela de informacoes de pontos
      df <- bind_rows(point_info_fw(), new_data)
      point_info_fw(df)
      
      # Atualizar a saida de texto para exibir informacoes do ponto
      output$centile_output_fw <- renderText({
        paste("Coordenadas (IG, Peso fetal):", x, ",", y, "\nO ponto encontra-se", centile, "percentil.")
      })
      
      # Limpar os campos de entrada apos a submissso
      updateNumericInput(session, "ga_input_fw", value = "")  # Apaga input
      updateNumericInput(session, "fw_input", value = "")  # Apaga input
    }
  })
  
  
  
  #BOTAO REMOVER PONTOS --------------------------------------------------------------
  
  #diametro1
  
  
  # Observar o evento de clique no botao de remocao
  observeEvent(input$remove_point_btn_d1, {
    clicked_point <- event_data("plotly_click", source = "source_d1")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      
      # Mostrar um modal de confirmacao antes de remover o ponto
      showModal(modalDialog(
        title = "Remover Ponto",
        "Tem a certeza que deseja remover este ponto?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_remove_btn_d1", "Remover", class = "btn-danger")
        )
      ))
    }
  })
  
  # Lidar com a  remocao de ponto confirmado
  observeEvent(input$confirm_remove_btn_d1, {
    clicked_point <- event_data("plotly_click", source = "source_d1")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      # Obter a dataframe de pontos
      df <- points_data_d1()
      # Remover o ponto selecionado da dataframe
      df <- df[!(df$x == clicked_point$x & df$y == clicked_point$y), ]
      points_data_d1(df)
      
      # Atualiza a dataframe point_info para excluir o ponto removido
      point_info_d1(point_info_d1()[!(point_info_d1()$IG == clicked_point$x & point_info_d1()$Diâmetro1 == clicked_point$y), ])
      
      # Atualiza o grafico para refletir o ponto removido
      update_plot("d1",points_data_d1,plot_output_d1,data_cent_d1)
    }
    removeModal()  # Fechar o modal de confirmacao
  })
  
  
  
  #diametro2
  
  
  # Observar o evento de clique no botao de remocao
  observeEvent(input$remove_point_btn_d2, {
    clicked_point <- event_data("plotly_click", source = "source_d2")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      
      # Mostrar um modal de confirmacao antes de remover o ponto
      showModal(modalDialog(
        title = "Remover Ponto",
        "Tem a certeza que deseja remover este ponto?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_remove_btn_d2", "Remover", class = "btn-danger")
        )
      ))
    }
  })
  
  # Lidar com a  remocao de ponto confirmado
  observeEvent(input$confirm_remove_btn_d2, {
    clicked_point <- event_data("plotly_click", source = "source_d2")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      # Obter a dataframe de pontos
      df <- points_data_d2()
      df <- df[!(df$x == clicked_point$x & df$y == clicked_point$y), ]
      points_data_d2(df)
      
      # Atualiza a dataframe point_info para excluir o ponto removido
      point_info_d2(point_info_d2()[!(point_info_d2()$IG == clicked_point$x & point_info_d2()$Diâmetro2 == clicked_point$y), ])
      
      # Atualiza o gráfico para refletir o ponto removido
      update_plot("d2",points_data_d2,plot_output_d2,data_cent_d2)
    }
    removeModal() # Fechar o modal de confirmacao
  })
  
  
  #espessura
  
  
  # Observar o evento de clique no botao de remocao
  observeEvent(input$remove_point_btn_pt, {
    clicked_point <- event_data("plotly_click", source = "source_pt")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      
      # Mostrar um modal de confirmacao antes de remover o ponto
      showModal(modalDialog(
        title = "Remover Ponto",
        "Tem a certeza que deseja remover este ponto?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_remove_btn_pt", "Remover", class = "btn-danger")
        )
      ))
    }
  })
  
  
  # Lidar com a  remocao de ponto confirmado
  observeEvent(input$confirm_remove_btn_pt, {
    clicked_point <- event_data("plotly_click", source = "source_pt")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      # Obter a dataframe de pontos
      df <- points_data_pt()
      df <- df[!(df$x == clicked_point$x & df$y == clicked_point$y), ]
      points_data_pt(df)
      
      # Atualiza a dataframe point_info para excluir o ponto removido
      point_info_pt(point_info_pt()[!(point_info_pt()$IG == clicked_point$x & point_info_pt()$Espessura == clicked_point$y), ])
      
      # Atualiza o gráfico para refletir o ponto removido
      update_plot("pt",points_data_pt,plot_output_pt,data_cent_pt)
    }
    removeModal() # Fechar o modal de confirmacao
  })
  
  
  #peso
  
  
  # Observar o evento de clique no botao de remocao
  observeEvent(input$remove_point_btn_pw, {
    clicked_point <- event_data("plotly_click", source = "source_pw")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      
      # Mostrar um modal de confirmacao antes de remover o ponto
      showModal(modalDialog(
        title = "Remover Ponto",
        "Tem a certeza que deseja remover este ponto?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_remove_btn_pw", "Remover", class = "btn-danger")
        )
      ))
    }
  })  
  
  # Lidar com a  remocao de ponto confirmado
  observeEvent(input$confirm_remove_btn_pw, {
    clicked_point <- event_data("plotly_click", source = "source_pw")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      # Obter a dataframe de pontos
      df <- points_data_pw()
      df <- df[!(df$x == clicked_point$x & df$y == clicked_point$y), ]
      points_data_pw(df)
      
      # Atualiza a dataframe point_info para excluir o ponto removido
      point_info_pw(point_info_pw()[!(point_info_pw()$IG == clicked_point$x & point_info_pw()$Peso == clicked_point$y), ])
      
      # Atualiza o gráfico para refletir o ponto removido
      update_plot("pw",points_data_pw,plot_output_pw,data_cent_pw)
    }
    removeModal() # Fechar o modal de confirmacao
  })
  
  
  #peso_fetal
  
  
  # Observar o evento de clique no botao de remocao
  observeEvent(input$remove_point_btn_fw, {
    clicked_point <- event_data("plotly_click", source = "source_fw")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      
      # Mostrar um modal de confirmacao antes de remover o ponto
      showModal(modalDialog(
        title = "Remover Ponto",
        "Tem a certeza que deseja remover este ponto?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_remove_btn_fw", "Remover", class = "btn-danger")
        )
      ))
    }
  })
  
  # Lidar com a  remocao de ponto confirmado
  observeEvent(input$confirm_remove_btn_fw, {
    clicked_point <- event_data("plotly_click", source = "source_fw")
    if (!is.null(clicked_point) && !is.null(clicked_point$x) && !is.null(clicked_point$y)) {
      # Obter a dataframe de pontos
      df <- points_data_fw()
      df <- df[!(df$x == clicked_point$x & df$y == clicked_point$y), ]
      points_data_fw(df)
      
      # Atualiza a dataframe point_info para excluir o ponto removido
      point_info_fw(point_info_fw()[!(point_info_fw()$IG == clicked_point$x & point_info_fw()$Peso_fetal == clicked_point$y), ])
      
      # Atualiza o gráfico para refletir o ponto removido
      update_plot("fw",points_data_fw,plot_output_fw,data_cent_fw)
    }
    removeModal() # Fechar o modal de confirmacao
  })
  
  
  
  
  #BOTAO REINICIAR --------------------------------------------------------------
  #Reinicia todas as entradas
  
  #Informacoes
  
  
  observeEvent(input$reset_info_btn, {
    updateTextInput(session, "process_number", value = "")
    updateTextInput(session, "pacient_name", value = "")
    updateNumericInput(session, "age", value = "")
    updateDateInput(session, "date_input", value = NULL)
    updateTextAreaInput(session, "infos", value = "")
    
  })
  
  
  #diametro1
  
  
  observeEvent(input$reset_btn_d1, {
    updateSelectInput(session, "load_csv_d1", selected = "Não")
    updateNumericInput(session, "ga_input_d1", value = NULL)
    updateNumericInput(session, "diameter1_input", value = NULL)
    updateActionButton(session, "submit_btn_d1", label = "Submeter")
    updateCheckboxInput(session, "save_csv_d1", value = FALSE)
    points_data_d1(data.frame(x = numeric(0), y = numeric(0)))
    update_plot("d1",points_data_d1, plot_output_d1, data_cent_d1)  # Reiniciar grafico
  })
  
  
  #diametro2
  
  
  observeEvent(input$reset_btn_d2, {
    updateSelectInput(session, "load_csv_d2", selected = "Não")
    updateNumericInput(session, "ga_input_d2", value = NULL)
    updateNumericInput(session, "diameter2_input", value = NULL)
    updateActionButton(session, "submit_btn_d2", label = "Submeter")
    updateCheckboxInput(session, "save_csv_d2", value = FALSE)
    points_data_d2(data.frame(x = numeric(0), y = numeric(0)))
    update_plot("d2",points_data_d2, plot_output_d2, data_cent_d2)  # Reiniciar grafico
  })
  
  
  #espessura
  
  
  observeEvent(input$reset_btn_pt, {
    updateSelectInput(session, "load_csv_pt", selected = "Não")
    updateNumericInput(session, "ga_input_pt", value = NULL)
    updateNumericInput(session, "pt_input", value = NULL)
    updateActionButton(session, "submit_btn_pt", label = "Submeter")
    updateCheckboxInput(session, "save_csv_pt", value = FALSE)
    points_data_pt(data.frame(x = numeric(0), y = numeric(0)))
    update_plot("pt",points_data_pt, plot_output_pt, data_cent_pt)  # Reiniciar grafico
  })
  
  #peso
  
  
  observeEvent(input$reset_btn_pw, {
    updateSelectInput(session, "load_csv_pw", selected = "Não")
    updateNumericInput(session, "ga_input_pw", value = NULL)
    updateNumericInput(session, "pw_input", value = NULL)
    updateActionButton(session, "submit_btn_pw", label = "Submeter")
    updateCheckboxInput(session, "save_csv_pw", value = FALSE)
    points_data_pw(data.frame(x = numeric(0), y = numeric(0)))
    update_plot("pw",points_data_pw, plot_output_pw, data_cent_pw)  # Reiniciar grafico
  })
  
  #peso_fetal
  
  
  observeEvent(input$reset_btn_fw, {
    updateSelectInput(session, "load_csv_fw", selected = "Não")
    updateNumericInput(session, "ga_input_fw", value = NULL)
    updateNumericInput(session, "fw_input", value = NULL)
    updateActionButton(session, "submit_btn_fw", label = "Submeter")
    updateCheckboxInput(session, "save_csv_fw", value = FALSE)
    points_data_fw(data.frame(x = numeric(0), y = numeric(0)))
    update_plot("fw",points_data_fw, plot_output_fw,data_cent_fw)  # Reiniciar grafico
  })
  
  
  #TABELA DA PAGINA INFORMACOES  --------------------------------------------------------------
  
  #dimaetro1
  
  
  # Cria um reactiveVal para armazenar os dados da tabela
  table_data_d1  <- reactiveVal()
  
  # Atualiza table_data sempre que point_info muda
  observe({
    table_data_d1(point_info_d1())
  })
  
  # Render a tabela de informações do ponto
  output$point_info_table_d1 <- renderDataTable({
    df <- point_info_d1()
    datatable(
      df,
      options = list(
        pageLength = 10,  
        columnDefs = list(
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Diâmetro1"),
          list(targets = 5, title = "Percentil")
        )
      )
    )
  })
  
  
  #diametro2
  
  
  # Cria um reactiveVal para armazenar os dados da tabela
  table_data_d2  <- reactiveVal()
  
  # Atualiza table_data sempre que point_info muda
  observe({
    table_data_d2(point_info_d2())
  })
  
  # Render a tabela de informações do ponto
  output$point_info_table_d2 <- renderDataTable({
    df <- point_info_d2()
    datatable(
      df,
      options = list(
        pageLength = 10, 
        columnDefs = list(
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Diâmetro2"),
          list(targets = 5, title = "Percentil")
        )
      )
    )
  })
  
  
  #espessura
  
  
  # Cria um reactiveVal para armazenar os dados da tabela
  table_data_pt  <- reactiveVal()
  
  # Atualiza table_data sempre que point_info muda
  observe({
    table_data_pt(point_info_pt())
  })
  
  # Render a tabela de informações do ponto
  output$point_info_table_pt <- renderDataTable({
    df <- point_info_pt()
    datatable(
      df,
      options = list(
        pageLength = 10,  
        columnDefs = list(
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Espessura"),
          list(targets = 5, title = "Percentil")
        )
      )
    )
  })
  
  
  #peso
  
  
  # Cria um reactiveVal para armazenar os dados da tabela
  table_data_pw  <- reactiveVal()
  
  # Atualiza table_data sempre que point_info muda
  observe({
    table_data_pw(point_info_pw())
  })
  
  # Render a tabela de informações do ponto
  output$point_info_table_pw <- renderDataTable({
    df <- point_info_pw()
    datatable(
      df,
      options = list(
        pageLength = 10,  
        columnDefs = list(
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Peso"),
          list(targets = 5, title = "Percentil")
        )
      )
    )
  })
  
  
  #peso_fetal
  
  
  # Cria um reactiveVal para armazenar os dados da tabela
  table_data_fw  <- reactiveVal()
  
  # Atualiza table_data sempre que point_info muda
  observe({
    table_data_fw(point_info_fw())
  })
  
  # Render a tabela de informações do ponto
  output$point_info_table_fw <- renderDataTable({
    df <- point_info_fw()
    datatable(
      df,
      options = list(
        pageLength = 10,  # Adjust options as needed
        columnDefs = list(
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Peso_fetal"),
          list(targets = 5, title = "Percentil")
        )
      )
    )
  })
  
  
  #Racios  --------------------------------------------------------------
  
  # Cria um dataframe reativo vazio para armazenar racios calculadas
  calculated_ratios <- reactiveVal(data.frame(Processo = character(0), IG = numeric(0), rácio1 = numeric(0), rácio2 = numeric(0)))
  
  # Adiciona valores reativos para armazenar linhas selecionadas
  selected_rows_pw <- reactiveVal(NULL)
  selected_rows_fw <- reactiveVal(NULL)
  
  # Atualiza os valores reativos das linhas selecionadas ao interagir com as tabelas
  observe({
    selected_rows_pw(input$point_info_table_pw_no_percentil_rows_selected)
    selected_rows_fw(input$point_info_table_fw_no_percentil_rows_selected)
  })
  
  # Render a tabela de informacoes de pontos para Peso sem "Percentil" com checkboxes
  output$point_info_table_pw_no_percentil <- DT::renderDataTable({
    df <- point_info_pw()
    df_no_percentil <- df[, -which(names(df) == "Percentil")]
    
    datatable(
      df_no_percentil,
      options = list(
        pageLength = 10,
        columnDefs = list(
          list(targets = 0, searchable = FALSE, orderable = FALSE, checkboxes = TRUE),
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Peso")
        )
      )
    )
  })
  
  # Render a tabela de informações de pontos para Peso fetal sem "Percentil" com checkboxes
  output$point_info_table_fw_no_percentil <- DT::renderDataTable({
    df <- point_info_fw()
    df_no_percentil <- df[, -which(names(df) == "Percentil")]
    
    datatable(
      df_no_percentil,
      options = list(
        pageLength = 10,
        columnDefs = list(
          list(targets = 0, searchable = FALSE, orderable = FALSE, checkboxes = TRUE),
          list(targets = 1, title = "Processo"),
          list(targets = 2, title = "Data"),
          list(targets = 3, title = "IG"),
          list(targets = 4, title = "Peso_fetal")
        )
      )
    )
  })
  
  # Calcula e atualiza a dataframe de racios calculadas
  observeEvent(input$calculate_ratio1, {
    
    # Obtém as linhas selecionadas nas tabelas de Peso  e Peso Fetal 
    selected_pw <- selected_rows_pw()
    selected_fw <- selected_rows_fw()
    
    if (length(selected_pw) == 1 && length(selected_fw) == 1) {
      df_pw <- point_info_pw()
      df_fw <- point_info_fw()
      
      ig_pw <- df_pw[selected_pw, "IG"]
      ig_fw <- df_fw[selected_fw, "IG"]
      
      # Verifica se os valores de IG são iguais
      if (ig_pw == ig_fw) {
        
        # Calcula os racios
        rácio1 <- df_fw[selected_fw, "Peso_fetal"] / df_pw[selected_pw, "Peso"]
        rácio2 <- df_pw[selected_pw, "Peso"] / df_fw[selected_fw, "Peso_fetal"]
        
        # Cria uma nova linha com os racios calculados
        new_row <- data.frame(Processo = df_pw[selected_pw, "Processo"],
                              IG = df_pw[selected_pw, "IG"],
                              rácio1 = round(rácio1, 3),
                              rácio2 = round(rácio2, 3))
        
        # Atualiza a dataframe de racios calculados
        calculated_ratios(union(calculated_ratios(), new_row))
        
        # Exibe mensagem de sucesso
        showNotification(
          "Valores dos rácios adicionados com sucesso!",
          type = "message"
        )
      } else {
        # Exibe mensagem de erro se os valores de IG não forem iguais
        showNotification(
          "Os valores selecionados devem ter IG iguais para calcular os rácios.",
          type = "error"
        )
      }
    } else {
      # Exibe mensagem de erro se não for selecionada exatamente uma linha de cada tabela
      showNotification(
        "Selecione apenas uma linha de cada tabela.",
        type = "error"
      )
    }
  })
  
  
  # Render a tabela de racios calculadas
  output$calculated_ratios_table <- DT::renderDataTable({
    datatable(calculated_ratios())
  })
  
  
  
  #DOWNLOAD CSV COM PONTOS --------------------------------------------------------------
  
  #diametro1
  
  
  output$download_csv_link_d1 <- downloadHandler(
    filename = function() {
      "pontos_salvos_diametro1.csv" # Nome do ficheiro que sera feito download
    },
    content = function(file) {
      
      # Obtem a dataframe com os pontos guardados
      df <- points_data_d1() 
      if (nrow(df) > 0) {
        
        # Escreve a dataframe num ficheiro CSV, excluindo nemeros de linha
        write.csv(df, file, row.names = FALSE)
        
        # Mensagem de sucesso
        showNotification(
          "Pontos salvos em pontos_salvos_diametro1.csv",
          type = "message"
        )
      } else {
        showNotification(
          "Não há pontos para salvar.",
          type = "warning"
        )
      }
    }
  )
  
  
  #diametro2
  
  
  output$download_csv_link_d2 <- downloadHandler(
    filename = function() {
      "pontos_salvos_diametro2.csv" # Nome do ficheiro que sera feito download
    },
    content = function(file) {
      
      # Obtem a dataframe com os pontos guardados
      df <- points_data_d2() 
      if (nrow(df) > 0) {
        
        # Escreve a dataframe num ficheiro CSV, excluindo nemeros de linha
        write.csv(df, file, row.names = FALSE)
        
        # Mensagem de sucesso
        showNotification(
          "Pontos salvos em pontos_salvos_diametro2.csv",
          type = "message"
        )
      } else {
        showNotification(
          "Não há pontos para salvar.",
          type = "warning"
        )
      }
    }
  )
  
  
  #espessura
  
  
  output$download_csv_link_pt <- downloadHandler(
    filename = function() {
      "pontos_salvos_espessura.csv" # Nome do ficheiro que sera feito download
    },
    content = function(file) {
      
      # Obtem a dataframe com os pontos guardados
      df <- points_data_pt()
      if (nrow(df) > 0) {
        
        # Escreve a dataframe num ficheiro CSV, excluindo nemeros de linha
        write.csv(df, file, row.names = FALSE)
        
        # Mensagem de sucesso
        showNotification(
          "Pontos salvos em pontos_salvos_espessura.csv",
          type = "message"
        )
      } else {
        showNotification(
          "Não há pontos para salvar.",
          type = "warning"
        )
      }
    }
  )
  
  
  #peso
  
  
  output$download_csv_link_pw <- downloadHandler(
    filename = function() {
      "pontos_salvos_peso.csv" # Nome do ficheiro que sera feito download
    },
    content = function(file) {
      
      # Obtem a dataframe com os pontos guardados
      df <- points_data_pw()
      if (nrow(df) > 0) {
        
        # Escreve a dataframe num ficheiro CSV, excluindo nemeros de linha
        write.csv(df, file, row.names = FALSE)
        
        # Mensagem de sucesso
        showNotification(
          "Pontos salvos em pontos_salvos_peso.csv",
          type = "message"
        )
      } else {
        showNotification(
          "Não há pontos para salvar.",
          type = "warning"
        )
      }
    }
  )
  
  
  #peso_fetal
  
  
  output$download_csv_link_fw <- downloadHandler(
    filename = function() {
      "pontos_salvos_pesofetal.csv" # Nome do ficheiro que sera feito download
    },
    content = function(file) {
      
      # Obtem a dataframe com os pontos guardados
      df <- points_data_fw()
      if (nrow(df) > 0) {
        
        # Escreve a dataframe num ficheiro CSV, excluindo nemeros de linha
        write.csv(df, file, row.names = FALSE)
        
        # Mensagem de sucesso
        showNotification(
          "Pontos salvos em pontos_salvos_pesofetal.csv",
          type = "message"
        )
      } else {
        showNotification(
          "Não há pontos para salvar.",
          type = "warning"
        )
      }
    }
  )
  
  
  #RELATORIO COM GRAFICOS E INFORMACOES  --------------------------------------------------------------
  
  
  # Gera relatorio em PDF e inicia o download
  output$download_pdf_link <- downloadHandler(
    filename = function() {
      "relatorio_placentalgrowth.pdf" # Nome do ficheiro do relatório em PDF
    },
    content = function(file) {
      
      # Parametros para passar para o template
      report_params <- list(
        process_number = input$process_number,
        pacient_name = input$pacient_name,
        age = input$age,
        date_input = input$date_input,
        infos = input$infos,
        points_data_d1 = points_data_d1(),
        points_data_d2 = points_data_d2(),
        points_data_pt = points_data_pt(),
        points_data_pw = points_data_pw(),
        points_data_fw = points_data_fw(),
        point_info_d1 = point_info_d1(),
        point_info_d2 = point_info_d2(),
        point_info_pt = point_info_pt(),
        point_info_pw = point_info_pw(),
        point_info_fw = point_info_fw(),
        calculated_ratios = calculated_ratios()
      )
      rmarkdown::render(
        "report.Rmd",  # Nome do ficheiro R Markdown que será usado como template do relatorio
        output_file = file,
        params = report_params
      )
      
      # Mensagem de sucesso
      showNotification("Relatório PDF gerado com sucesso!", type = "message")
    }
  )
  
}