

function(input, output, session) {
  #################  on start -----------------------------------------------------------------------------------------
  
  
  path <- reactiveVal()
  path_ok <- reactiveVal(0)
  path2_ok <- reactiveVal(0)
  section_first <- reactiveVal(0)
  cd <- reactiveVal(0)
  
  
  
  observeEvent(input$section_first_xlsx,
               {
                 section_first(input$section_first_xlsx)
               })
  
  observeEvent(input$section_first_txt,
               {
                 section_first(input$section_first_txt)
               })
  
  
  observeEvent(input$section_first_proj,
               {
                 section_first(input$section_first_proj)
               })
  
  
  
  
  output$dir_select <- renderUI({
    if (miss_input() == 0)
      return(shinyDirButton(
        "dir",
        "Chose directory",
        title = "",
        class = "btn-primary"
      ))
    
    else
      return(shinyDirButton(
        "dir",
        "Chose directory",
        title = "",
        class = "btn-warning"
      ))
  })
  
  observeEvent(input$dir, {
    updateActionButton(session, "dir", label = "Directory selected")
  })
  
  volumes = getVolumes()()
  
  shinyDirChoose(input, 'dir', roots = volumes)
  
  dir <- reactive(input$dir)
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 c(input$dir, input$name_out_file)
               },
               handlerExpr = {
                 req(is.list(input$dir))
                 path_ok(1)
                 roots = getVolumes()
                 p <- parseDirPath(roots, dir())
                 p <- paste0(p, "/", input$name_out_file)
                 path(p)
               })
  
  data <- reactiveVal()
  
  observeEvent(input$new_project, {
    path2_ok(0)
    new_or_load(0)
    options(shiny.maxRequestSize = 9 * 1024 ^ 2)
    
    output$table.output <- renderTable({
      return(NULL)
    })
    
    output$graph1 <- renderPlot({
      return(NULL)
    })
    
    output$graph2 <- renderPlot({
      return(NULL)
    })
    
    output$table.output2 <- DT::renderDataTable({
      return(NULL)
    })
    
    output$max_data_txt <- renderUI({
      return(NULL)
    })
    
    output$max_data_xlsx <- renderUI({
      return(NULL)
    })
    
    output$max_data_proj <- renderUI({
      return(NULL)
    })
    
    output$file1_ui <- renderUI({
      fileInput(
        inputId  = 'file1',
        label = NULL,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        ),
        buttonLabel = "New project",
        placeholder = NULL
      )
    })
  })
  
  
  observeEvent(input$load_project, {
    new_or_load(1)
    path2_ok(0)
    data(NULL)
    data_ok(NULL)
    time_col(NULL)
    drawdown_col(NULL)
    min_val(NULL)
    max_val(NULL)
    format_file1(0)
    
    output$table.output <- renderTable({
      return(NULL)
    })
    
    output$graph1 <- renderPlot({
      return(NULL)
    })
    
    output$graph2 <- renderPlot({
      return(NULL)
    })
    
    output$table.output2 <- DT::renderDataTable({
      return(NULL)
    })
    
    output$max_data_txt <- renderUI({
      return(NULL)
    })
    
    output$max_data_xlsx <- renderUI({
      return(NULL)
    })
    
    output$max_data_proj <- renderUI({
      return(NULL)
    })
    
    output$file_project <- renderUI({
      fileInput(
        inputId  = 'file_project',
        label = NULL,
        buttonLabel = "Project",
        placeholder = NULL,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    })
  })
  
  data_to_report <- reactiveVal()
  setupWT_main <- reactiveValues()
  
  observeEvent(input$file1, {
    if (exists("setupWT"))
      remove(setupWT, pos = 1, inherits = TRUE)
    
    data(NULL)
    data_ok(NULL)
    time_col(NULL)
    drawdown_col(NULL)
    min_val(NULL)
    max_val(NULL)
    data_to_report <- reactiveVal()
    setupWT_main <- reactiveValues()
    
    setupWT_main$hydraulic_cond = NULL
    setupWT_main$cone_of_depr = NULL
    setupWT_main$depth = NULL
    setupWT_main$Q = NULL
    setupWT_main$R = NULL
    setupWT_main$Stor = NULL
    setupWT_main$Trans = NULL
    setupWT_main$iter = NULL
    setupWT_main$skin_bot = NULL
    setupWT_main$skin_top = NULL
    setupWT_main$stor_bot = NULL
    setupWT_main$stor_top = NULL
    setupWT_main$cores_to_use = NULL
    setupWT_main$name_out_file = NULL
    setupWT_main$file_t = NULL
    setupWT_main$file2_t = NULL
    setupWT_main$well_location = NULL
    setupWT_main$min = NULL
    setupWT_main$max = NULL
    setupWT_main$data = NULL
    setupWT_main$data_ok = NULL
    setupWT_main$time_col = NULL
    setupWT_main$drawdown_col = NULL
    data_to_report(setupWT_main)
  })
  
  ################## file type ------------------------------------------------------------------------------------------------
  
  file_t <- reactiveVal()
  file_t2 <- reactiveVal()
  report_cond <- reactiveVal()
  new_or_load <- reactiveVal()
  
  #### file 1 ----------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$file1, {
    setupWT_main = data_to_report()
    file1 <- input$file1
    
    if (is.null(file1))
      return()
    
    new_or_load(0)
    data_path <- file1$datapath
    end_string <- nchar(data_path)
    start_string <- end_string
    letters <-
      substring(data_path, 1:end_string , 1:end_string)
    type <- ("")
    
    for (n in 1:6) {
      if (letters[start_string] == ".")
        type <-
          substr(data_path, start_string + 1, end_string)
      
      start_string <- start_string - 1
    }
    
    if (type == "txt" || type == "xlsx")  {
      format_file1(0)
      file_t(type)
      setupWT_main$file_t = type
    }
    
    else {
      format_file1(1)
      file_t(NA)
      setupWT_main$file_t = NA
    }
    data_to_report(setupWT_main)
  })
  
  observeEvent(c(
    input$file1,
    input$sep,
    input$dec,
    input$length_data1x,
    input$length_data1t
  ),
  {
    if (is.null(input$file1) ||
        is.na(data_to_report()$file_t) ||
        is.null(input$sep) ||
        is.null(input$dec) || format_file1() == 1)
      return ()
    
    file1 = input$file1
    
    if (file_t() == "txt") {
      showModal(modalDialog("Loading", footer = NULL))
      dat =  read.csv(
        input$file1$datapath,
        header = TRUE,
        sep = input$sep,
        dec = input$dec
      )
      
      setupWT_main = data_to_report()
      setupWT_main$data = dat
      data_to_report(setupWT_main)
      data(dat)
      
      
      removeModal()
    }
  })
  
  observeEvent(input$load_sheet, {
    if (is.null(input$file1) ||
        is.null(data_to_report()$file_t) ||
        is.null(input$sep) ||
        is.null(input$dec) || is.null(input$sheet))
      return ()
    
    if (file_t() == "xlsx") {
      showModal(modalDialog("Loading", footer = NULL))
      file1 = input$file1
      dat = read.xlsx(file1$datapath, sheetName = input$sheet)
      setupWT_main = data_to_report()
      setupWT_main$data = dat
      data_to_report(setupWT_main)
      data(dat)
      
      
      
      
      removeModal()
    }
  })
  
  #### file 2 ---------------------------------------------------------------------------------------------------------------------------------
  
  format_file2 <- reactiveVal(NA)
  
  observeEvent(input$file2, {
    file2 <- input$file2
    
    if (is.null(file2))
      return()
    
    data_path <- file2$datapath
    end_string <- nchar(data_path)
    start_string <- end_string
    letters <-
      substring(data_path, 1:end_string , 1:end_string)
    type <- ("")
    
    for (n in 1:6) {
      if (letters[start_string] == ".")
        type <-  substr(data_path, start_string + 1, end_string)
      
      start_string <- start_string - 1
    }
    
    if (type == "txt")  {
      format_file2(0)
      file_t2(type)
      stage2(223)
    }
    
    else if (type == "xlsx")  {
      format_file2(0)
      file_t2(type)
      stage2(222)
    }
    
    else {
      format_file2(1)
      file_t2(NA)
    }
  })
  
  output$format_file2 <- reactive ({
    format_file2()
  })
  
  outputOptions(output, "format_file2", suspendWhenHidden = FALSE)
  
  #### file project ---------------------------------------------------------------------------------------------------------------------------------
  
  file_t_proj <- reactiveVal (0)
  end_for_report <- reactiveVal()
  length_slider_proj <- reactiveVal()
  
  observeEvent(input$file_project, {
    new_or_load(1)
    file_proj <- input$file_project
    
    
    if (is.null(file_proj))
      return()
    
    data_path = file_proj$datapath
    end_string = nchar(data_path)
    
    if (end_string < 4)
      return ()
    
    start_string = end_string - 1
    type = substr(data_path, start_string , end_string)
    
    if (type == "wt") {
      file_t_proj(0)
      report_cond(1)
      inFile <- isolate({
        input$file_project
      })
      
      
      
      file <- inFile$datapath
      load(file, envir = .GlobalEnv)
      
      cd(setupWT$Cbar)
      setupWT_main = setupWT
      data(setupWT$data)
      data_ok(setupWT$data_ok)
      cone(setupWT_main$cone_of_depr)
      cond(setupWT_main$hydraulic_cond)
      time_col("time")
      drawdown_col("drawdown")
      drawdown <- data_ok()$drawdown
      end_t(which.max(drawdown))
      end_for_report(which.max(drawdown))
      end_t_lvl(as.numeric(drawdown[end_t()]))
      result_graph(setupWT$fitted_data)
      Sys.setlocale("LC_TIME", "English")
      setupWT_main$time = format(Sys.time(), "%d %B %Y")
      file_t(setupWT$file_type1)
      data_to_report(setupWT_main)
      length_slider_proj(setupWT$max)
      section_first(setupWT$first_section)
      
    }
    
    else {
      file_t_proj(1)
      return ()
    }
  })
  
  output$file_t_proj <- reactive ({
    file_t_proj()
  })
  
  outputOptions(output, "file_t_proj", suspendWhenHidden = FALSE)
  
  #### out ------------------------------------------------------------------------------------------------------------------------------------
  
  output$file_type <- reactive ({
    file_t()
  })
  
  output$file_type2 <- reactive ({
    file_t2()
  })
  
  outputOptions(output, "file_type", suspendWhenHidden = FALSE)
  outputOptions(output, "file_type2", suspendWhenHidden = FALSE)
  
  #### file 1 data ok ----------------------------------------------------------------------------------------------------------
  
  dat_slope <- reactiveVal()
  data_ok <- reactiveVal()
  
  observeEvent(c(time_col(), drawdown_col(), data()), {
    if (is.null(data_to_report()$time_col) ||
        is.null(data_to_report()$drawdown_col) ||
        is.null(data_to_report()$data) ||
        format_file1() == 1 ||
        is.null(time_col()) ||
        is.null(drawdown_col()) ||
        is.null(data())) {
      return()
      
    }
    
    else {
      if (new_or_load() == 0) {
        dat <-
          data.frame(time = data()[, time_col()], drawdown = data()[, drawdown_col()])
        
        if (sum(is.na(dat[, 1])) > 0 ||
            sum(is.na(dat[, 2])) > 0) {
          dat <- dat[-which(is.na(dat[, c(1, 2)])),]
        }
        data_ok(dat)
      }
      
      else {
        dat <- data.frame(time = data_to_report()$data[, 1],
                          drawdown = data_to_report()$data[, 2])
        
        if (sum(is.na(dat[, 1])) > 0 ||
            sum(is.na(dat[, 2])) > 0) {
          dat <- dat[-which(is.na(dat[, c(1, 2)])),]
        }
      }
      data_ok(dat)
      setupWT_main <- data_to_report()
      setupWT_main$data_ok = dat
      data_to_report(setupWT_main)
    }
  })
  
  ##############################  eval param ------------------------------------------------------------------------------------------------------
  cone_of_d <- reactiveVal ("no")
  conduct <- reactiveVal ("no")
  eval_p <- reactiveVal(0)
  slider_tr <- reactiveVal("no")
  thickness <- reactiveVal("no")
  
  observeEvent(input$previous_eval_p, {
    eval_p(0)
  })
  
  observeEvent(input$eval_param_x, {
    eval_p(1)
  })
  
  observeEvent(input$eval_param_t, {
    eval_p(1)
  })
  
  observeEvent(input$eval_param_p, {
    eval_p(1)
  })
  
  output$eval_par <- reactive({
    eval_p()
  })
  
  output$slider_trans <- reactive({
    slider_tr()
  })
  
  output$thick <- reactive({
    thickness()
  })
  
  outputOptions(output, "eval_par", suspendWhenHidden = FALSE)
  outputOptions(output, "slider_trans", suspendWhenHidden = FALSE)
  outputOptions(output, "thick", suspendWhenHidden = FALSE)
  
  ############################## render ui slider trans  ------------------------------------------------------------------------------------------------------
  
  output$trans_limits <- renderUI({
    delka <- end_t()
    
    sliderInput(
      "trans_lim",
      "Select section for transmissivity evaluation",
      min = 0,
      max = delka,
      value = c(round(delka * 0.6, 0), round(delka * 0.95, 0))
    )
  })
  
  ############################## min max value ------------------------------------------------------------------------------------------------------
  
  min_val <- reactiveVal()
  max_val <- reactiveVal()
  
  observeEvent(file_t(), {
    if (format_file1() == 1)
      return()
    
    if (file_t_proj() == 0 || new_or_load() == 1) {
      min_val(data_to_report()$min)
      max_val(data_to_report()$max)
    }
  })
  
  observeEvent(
    c(
      input$min_data_txt,
      input$max_data_txt,
      input$min_data_xlsx,
      input$max_data_xlsx,
      input$min_data_proj,
      input$max_data_proj,
      data()
    ),
    {
      if (exists("setupWT") && new_or_load() == 1) {
        min_val(as.numeric(input$min_data_proj))
        max_val(input$max_data_proj)
      }
      
      else {
        if (file_t() == "txt") {
          min_val(as.numeric(input$min_data_txt))
          max_val(input$max_data_txt)
        }
        
        if (file_t() == "xlsx") {
          min_val(as.numeric(input$min_data_xlsx))
          max_val(input$max_data_xlsx)
        }
        
        else
          return ()
      }
    }
  )
  
  observeEvent(c(min_val(), max_val()), {
    setupWT_main <- data_to_report()
    setupWT_main$min <- min_val()
    setupWT_main$max <- max_val()
    data_to_report(setupWT_main)
  })
  
  ############################## columns selected ------------------------------------------------------------------------------------------------------
  
  time_col <- reactiveVal()
  drawdown_col <- reactiveVal()
  
  observeEvent(
    c(
      input$time_col_txt,
      input$drawdown_col_txt,
      input$time_col_xlsx,
      input$drawdown_col_xlsx
    ),
    {
      if (new_or_load() == 1)  {
        time_col("time")
        drawdown_col("drawdown")
      }
      
      else {
        if (file_t() == "txt") {
          time_col(input$time_col_txt)
          drawdown_col(input$drawdown_col_txt)
        }
        
        if (file_t() == "xlsx") {
          time_col(input$time_col_xlsx)
          drawdown_col(input$drawdown_col_xlsx)
        }
        setupWT_main <- data_to_report()
        setupWT_main$time_col <- time_col()
        setupWT_main$drawdown_col <- drawdown_col()
        data_to_report(setupWT_main)
      }
    }
  )
  
  ########### data 1 loaded y or no --------------------------------------------------------------------------------------------------------------------------
  
  datt1 <- reactiveVal(0)
  
  observeEvent(c(input$file1, input$file_project), {
    if (format_file1() == 1 || file_t_proj() == 1)
      return()
    
    else
      datt1(1)
  })
  
  observeEvent(c(input$load_project, input$new_project), {
    datt1(0)
  })
  
  output$dat1 <- reactive({
    datt1()
  })
  
  outputOptions(output, "dat1", suspendWhenHidden = FALSE)
  
  ########### data 2 loaded y or no --------------------------------------------------------------------------------------------------------------------------
  
  datt2 <- reactiveVal(0)
  
  observeEvent(input$file2, {
    if (is.null(file_t2()))
      return()
    
    datt2(1)
  })
  
  output$dat2 <- reactive({
    datt2()
  })
  
  outputOptions(output, "dat2", suspendWhenHidden = FALSE)
  
  ############################# file 1 sheets -----------------------------------------------------------------------------------------------------------------
  
  observeEvent(file_t(), {
    if (format_file1() == 1)
      return()
    
    if (file_t() == "xlsx") {
      output$sheet_names <- renderUI({
        infile <- input$file1
        names <- excel_sheets(infile$datapath)
        leng <- length(names)
        selectInput(
          inputId = "sheet",
          choices = names,
          label = NULL,
          multiple = FALSE,
          selected = NULL,
          selectize = FALSE,
          size = leng
        )
      })
    }
    
    else
      return()
  })
  
  
  output$report_con <- renderUI({
    checkboxGroupInput(
      "report_contain",
      label = NULL,
      c(
        "Include",
        "Graph - input data",
        "Graph - data used",
        "Graph - fitted values"
      ),
      selected = "Include"
    )
  })
  
  observeEvent(input$select_all_graphs_report, {
    updateCheckboxGroupInput(
      session,
      inputId = "report_contain",
      selected = c(
        "Include",
        "Graph - input data",
        "Graph - data used",
        "Graph - fitted values"
      )
    )
  })
  
  
  ####################### render ui 1 page  colnames  ----------------------------------------------------------------------------------------------
  
  
  observeEvent(data(), {
    if (is.null(file_t()))
      return ()
    
    if (file_t() == "txt") {
      var_names <- variable.names(data())
      rd = c()
      len = length(var_names)
      
      for (i in 1:len)
      {
        if (sum(!is.na(data()[, i])) != 0)
          rd = c(rd, i)
      }
      
      if (is.null(rd))
      {
        var_n = NULL
        var_n2 = NULL
      }
      
      else
      {
        var_n = var_names[rd[1]]
        var_n2 = var_names[rd[2]]
      }
      
      output$time_col_txt <- renderUI({
        selectInput(
          inputId = "time_col_txt",
          choices = var_names[rd],
          label = NULL,
          selected = var_n
        )
      })
      
      output$drawdown_col_txt <- renderUI({
        selectInput(
          inputId = "drawdown_col_txt",
          choices = var_names[rd],
          label = NULL,
          selected = var_n2
        )
      })
      
      
      output$section_first_txt <- renderUI({
        sliderInput(
          "section_first_txt",
          label = NULL,
          min = 5,
          max = end_t() * 0.6,
          value = round(end_t() * 0.06, 0)
        )
      })
    }
    
    if (file_t() == "xlsx") {
      var_names <- variable.names(data())
      rd = c()
      len = length(var_names)
      
      for (i in 1:len)
      {
        if (sum(!is.na(data()[, i])) != 0)
          rd = c(rd, i)
      }
      
      if (is.null(rd))
      {
        var_n = NULL
        var_n2 = NULL
      }
      
      else
      {
        var_n = var_names[rd[1]]
        var_n2 = var_names[rd[2]]
      }
      
      output$time_col_xlsx <- renderUI({
        selectInput(
          inputId = "time_col_xlsx",
          choices = var_names[rd],
          label = NULL,
          selected = var_n
        )
      })
      
      output$drawdown_col_xlsx <- renderUI({
        selectInput(
          inputId = "drawdown_col_xlsx",
          choices = var_names[rd],
          label = NULL,
          selected = var_n2
        )
      })
      
      output$section_first_xlsx <- renderUI({
        sliderInput(
          "section_first_xlsx",
          label = NULL,
          min = 5,
          max = end_t() * 0.3,
          value = round(end_t() * 0.03, 0)
        )
      })
      
    }
  })
  
  ####################### render ui 1 page  min max  ----------------------------------------------------------------------------------------------
  
  observeEvent(input$file_project, {
    if (!exists("setupWT") ||
        file_t_proj() == 1 ||
        format_file1() == 1 ||
        is.null(data_ok()))
      return ()
    delka = length(data_ok()[1:end_t(), 1])
    
    output$min_data_proj <- renderUI({
      textInput(
        "min_data_proj",
        label = NULL,
        width = "200",
        value = data_to_report()$min
      )
    })
    
    output$max_data_projj <- renderUI({
      sliderInput(
        "max_data_proj",
        label = NULL ,
        min = 2,
        max = delka,
        value = length_slider_proj()
      )
    })
    
    
    output$drawdown_col_txt <- renderUI({
      selectInput(
        inputId = "drawdown_col_txt",
        choices = var_names[rd],
        label = NULL,
        selected = var_n2
      )
    })
    
    
    
    output$section_first_proj <- renderUI({
      sliderInput(
        "section_first_proj",
        label = NULL,
        min = 5,
        max = end_t() * 0.3,
        value = section_first()
      )
    })
    
    
  })
  
  
  
  
  observeEvent(data_ok(), {
    if (exists("setupWT") || is.null(file_t()) || is.null(data_ok()))
      return ()
    
    else {
      drawdown <- data_ok()$drawdown
      end_t(which.max(drawdown))
      end_for_report(which.max(drawdown))
      end_t_lvl(as.numeric(drawdown[end_t()]))
      delka = length(data_ok()[, 1])
      
      if (delka > end_t())
        delka = end_t()
      
    }
    
    if (file_t() == "txt") {
      output$min_data_txt <- renderUI({
        textInput(
          "min_data_txt",
          label = NULL,
          width = "200",
          value = 1
        )
      })
      
      output$max_data_txt <- renderUI({
        sliderInput(
          "max_data_txt",
          label = NULL,
          min = 2,
          max = end_t(),
          value = round(delka * 0.85, 0)
        )
      })
      
      
      output$section_first_txt <- renderUI({
        sliderInput(
          "section_first_txt",
          label = NULL,
          min = 5,
          max = end_t() * 0.3,
          value = round(end_t() * 0.03, 0)
        )
      })
      
    }
    
    if (file_t() == "xlsx") {
      output$min_data_xlsx <- renderUI({
        textInput(
          "min_data_xlsx",
          label = NULL,
          width = "200",
          value = 1
        )
      })
      
      output$max_data_xlsx <- renderUI({
        sliderInput(
          "max_data_xlsx",
          label = NULL,
          min = 5,
          max = end_t(),
          value = round(delka * 0.85, 0)
        )
      })
      
      
      output$section_first_xlsx <- renderUI({
        sliderInput(
          "section_first_xlsx",
          label = NULL,
          min = 5,
          max = end_t() * 0.6,
          value = round(end_t() * 0.06, 0)
        )
      })
      
      
    }
  })
  
  
  
  
  ####################### render ui 2 page  parameters  -----------------------------------------------------------------------------------------------
  
  observeEvent(c(input$file1, input$file_project), {
    output$Q <- renderUI({
      numericInput(
        inputId = "Q",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$Q
        
        else
          value =  NA
        ,
        min = 0,
        max = 1000
      )
    })
    
    output$R <- renderUI({
      numericInput(
        inputId = "R",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$R
        
        else
          value =  NA
        ,
        min = 0,
        max = 1000
      )
    })
    
    output$Storr <- renderUI({
      numericInput(
        inputId = "Stor",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$Stor
        
        else
          value =  NA
        ,
        min = 0,
        max = 1000
      )
    })
    
    output$Stor <- renderUI({
      numericInput(
        "aqua_thick",
        label = NULL,
        min = 0,
        max = 1000,
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$aqua_thick
        
        else
          value = NA
      )
    })
    
    output$aq_thick <- renderUI({
      numericInput(
        "aqua_thick",
        label = NULL,
        min = 0,
        max = 1000,
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$aqua_thick
        
        else
          value = NA
      )
    })
    
    output$well_locat <- renderUI({
      textInput(
        inputId = "well_location",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = data_to_report()$well_location
        
        else
          value = NA
      )
    })
    
    output$well_depthh <- renderUI({
      numericInput(
        inputId = "well_depth",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = data_to_report()$well_depth
        
        else
          value = NULL
      )
    })
  })
  
  observeEvent(c(input$file1, input$file_project, input$param_to_eval), {
    output$Trans <- renderUI({
      numericInput(
        inputId = "Trans",
        label = NULL,
        if (exists("setupWT") &&
            new_or_load() == 1 || slider_tr() == "yes") {
          if (slider_tr() == "yes") {
            if (is.na(input$Q) || is.null(input$Q))
              return ()
            
            tran_eval = (lm(data_ok()[input$trans_lim[1]:input$trans_lim[2], 2] ~ log10(data_ok()[input$trans_lim[1]:input$trans_lim[2], 1])))
            val = round((0.183 * (input$Q) / tran_eval$coefficients[2]), 6)
            
            if (val > 0)
              value = val
            
            else
              value = NA
          }
          
          else
            value = setupWT$Transmiss
        }
        
        else
          value =  NA,
        min = 0,
        max = 1000
      )
    })
  })
  
  ####################### render ui 2 page  limits  ----------------------------------------------------------------------------------------------
  
  observeEvent(c(input$file1, input$file_project), {
    output$out_name <- renderUI({
      textInput(
        inputId = "name_out_file",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$name
        
        else
          value =  NA
      )
    })
    
    output$iter_ui <- renderUI({
      numericInput(
        inputId = "iter",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$iter
        
        else
          value =  25
        ,
        min = 0,
        max = 10000
      )
    })
    
    output$skin_bot_ui <- renderUI({
      numericInput(
        inputId = "skin_bot",
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$skin_bot
        
        else
          value =  0
        ,
        label = NULL,
        min = 0,
        max = 10000
      )
    })
    
    output$skin_top_ui <- renderUI({
      numericInput(
        inputId = "skin_top",
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$skin_top
        
        else
          value =  NA
        ,
        label = NULL,
        min = 0,
        max = 100000
      )
    })
    
    output$stor_bot_ui <- renderUI({
      numericInput(
        inputId = "stor_bot",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$stor_bot
        
        else
          value =  0
        ,
        min = 0,
        max = 100000
      )
    })
    
    output$stor_top_ui <- renderUI({
      numericInput(
        inputId = "stor_top",
        label = NULL,
        
        if (exists("setupWT") && new_or_load() == 1)
          value = setupWT$stor_top
        
        else
          value =  NA
        ,
        min = 0,
        max = 100000
      )
    })
    
    output$num_cores <- renderUI({
      radioButtons(
        "cores_to_use",
        label = NULL,
        set_cores,
        
        if (exists("setupWT") && new_or_load() == 1)
          selected  = setupWT$cores
        
        else
          selected =  1
      )
    })
    
    output$num_cores <- renderUI({
      radioButtons(
        "cores_to_use",
        label = NULL,
        set_cores,
        
        if (exists("setupWT") && new_or_load() == 1)
          selected  = setupWT$cores
        
        else
          selected =  1
      )
    })
  })
  
  ####################  2.page  - sheets2 ---------------------------------------------------------------------------------------------------------
  
  
  observeEvent(file_t2(), {
    output$sheet_names2 <- renderUI({
      if (file_t2() == "xlsx") {
        infile = input$file2
        names = excel_sheets(infile$datapath)
        leng <- length(names)
        
        selectInput(
          inputId = "sheet2",
          choices = names,
          label = NULL,
          selected = NULL,
          selectize = FALSE,
          size = leng
        )
      }
      
      else
        return()
    })
  })
  
  
  ########################## data load  ------------------------------------------------------------------------------------------------------------
  
  
  #### file 1 --------------------------------------------------------------------------------------------------------------------------------------
  
  data <- reactiveVal()
  
  #### file 2 -----------------------------------------------------------------------------------------------------------------------------------
  
  data2 <- reactiveVal()
  
  observeEvent(c(input$sep2, input$dec2, input$file2), {
    if (is.null(input$file2) || format_file2() == 1 || is.null(file_t2))
    {
      return ()
    }
    
    showModal(modalDialog("Loading", footer = NULL))
    file2 = input$file2
    
    if (file_t2()  == "txt") {
      dat =  read.csv(
        input$file2$datapath,
        header = TRUE,
        sep = input$sep2,
        dec = input$dec2
      )
      data2(dat)
    }
    removeModal()
  })
  
  observeEvent(input$load_sheet2, {
    if (is.null(input$file2))
      return()
    
    showModal(modalDialog("Loading", footer = NULL))
    
    if (file_t2() == "xlsx") {
      file2 = input$file2
      dat = read.xlsx(file2$datapath, sheetName = input$sheet2)
      data2(dat)
    }
    removeModal()
  })
  
  observeEvent(input$no22, {
    data2(data())
  })
  
  
  ######################### table 1 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      data(),
      input$file1,
      input$sep,
      input$dec,
      input$length_data1x,
      input$length_data1t
    ),
    {
      if (format_file1() == 1)
        return()
      
      showModal(modalDialog("Loading", footer = NULL))
      
      if (input$length_data1t == "no" ||
          input$length_data1x == "no")
      {
        output$table.output <- renderTable({
          data()[1:20,]
        })
      }
      
      if (input$length_data1t == "yes" ||
          input$length_data1x == "yes")
      {
        output$table.output <- renderTable({
          data()
        })
      }
      
      else {
        output$table.output <- renderTable({
          data()[1:20,]
        })
      }
      removeModal()
    }
  )
  
  ########################## table 2 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(data2(), {
    showModal(modalDialog("Loading", footer = NULL))
    
    if (format_file2() == 1)
      return()
    output$table.output2 <- DT::renderDataTable({
      data2()
    })
    removeModal()
  })
  
  ########################## graph 1 out  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      min_val(),
      max_val(),
      input$previous_eval_p,
      input$section_first_xlsx,
      input$section_first_txt
    ),
    {
      if (is.null(data_ok()) ||
          is.null(time_col()) &&
          is.null(drawdown_col) &&
          new_or_load() == 0 ||
          format_file1() == 1 ||
          is.null(min_val()) ||
          is.null(max_val()))
      {
        output$graph1 <- renderPlot({
          return(NULL)
        })
      }
      
      else {
        output$graph1 <- renderPlot({
          dat_slope(first_straight_section(data_ok(), section_first()))
          
          return(
            ggplot(data = data_ok()[1:end_t(),], aes(x = time,
                                                     y = drawdown)) +
              geom_point() + ggtitle("Data range for fit") + scale_x_log10() +
              labs(x = "Time", y = "Drawdown") +
              geom_abline(
                aes(
                  slope = as.numeric(dat_slope()$coefficients[2]),
                  intercept = as.numeric(dat_slope()$coefficients[1]),
                  color = "slope"
                )
              ) +
              geom_point(data = data_ok()[min_val():max_val(), ], aes(
                x = time,
                y = drawdown,
                col = "Selected - fit"
              )) +
              labs(color = "")
            
            + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black")
            ) +
              theme(plot.title = element_text(size = 18)) +
              theme(text = element_text(size = 16))
          )
          
        })
      }
    }
  )
  
  
  ########################## graph transmissivity  ------------------------------------------------------------------------------------------------
  
  observeEvent(
    c(
      input$trans_lim,
      input$eval_param_x,
      input$eval_param_t,
      input$eval_param_p,
      input$go_to_eval
    ),
    {
      
      if (eval_p() == 0 || slider_tr() == "no")
        return ()
      
      output$graph1 <- renderPlot({
        return(
          ggplot(data_ok()[1:end_t(),], aes(x = time,
                                            y = drawdown)) +
            geom_point() + ggtitle("Data range for evaluate transmissivity") + scale_x_log10() +
            labs(x = "Time [s]", y = "Drawdown [m]") +
            geom_point(
              data = data_ok()[input$trans_lim[1]:input$trans_lim[2], ],
              aes(x = time,
                  y = drawdown,
                  color = "Selected - transmissivity")
            ) +
            scale_color_manual(values = "blue") +
            
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black")
            ) +
            theme(plot.title = element_text(size = 18)) +
            theme(text = element_text(size = 16))
        )
      },height = 588, width = 1090
      )
    }
  )
  
  ########################## graph 2 out  ------------------------------------------------------------------------------------------------
  
  result_graph <- reactiveVal()
  
  observeEvent(c(result_graph(), input$run_process), {
    if (is.null(data_to_report()$final_data))
      return()
    
    output$graph2 <- renderPlot({
      return(
        ggplot(data_to_report()$final_data[1:end_t(),], aes(x = time,
                                                            y = drawdown)) +
          geom_point() + ggtitle("Restults - fit") + scale_x_log10() +
          labs(x = "Time [s]", y = "Drawdown [m]") +
          geom_point(
            data = data_to_report()$fitted_data,
            aes(x = time,
                y = drawdown,
                col = "Fitted values")
          ) +
          labs(color = "")
        + theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) +
          theme(plot.title = element_text(size = 18)) +
          theme(text = element_text(size = 16))
      )
      
      
    },
    height = 590, width = 1000
    )
  })
  
  ########################## setup loaded  ------------------------------------------------------------------------------------------------
  
  load_set <- reactiveVal (0)
  
  observeEvent(input$file1, {
    load_set(0)
  })
  
  observeEvent(input$file_project, {
    load_set(1)
  })
  
  output$loaded_setup <- reactive ({
    load_set()
  })
  
  outputOptions(output, "loaded_setup", suspendWhenHidden = FALSE)
  
  
  
  ########################## Main process   ------------------------------------------------------------------------------------------------
  
  
  report_data <- reactiveValues()
  work_dir <- reactiveValues()
  end_t <- reactiveVal(0)
  end_t_lvl <- reactiveVal(0)
  miss_input <- reactiveVal(0)
  final_data <- reactiveVal()
  save_dir <- reactiveVal()
  path2 <- reactiveVal()
  wel_depth <- reactiveVal(0)
  wel_loc <- reactiveVal(0)
  aqua_thick <- reactiveVal(0)
  
  
  observeEvent(input$run_process , {
    if (path_ok() == 0 && input$setup_out == "yes")
    {
      miss_input(5)
      return()
    }
    
    if (is.null(time_col()) ||
        is.null(min_val()))
    {
      miss_input(4)
      return()
    }
    
    if ((
      is.na(data_to_report()$stor_bot) ||
      is.na(data_to_report()$stor_top) ||
      is.na(data_to_report()$skin_bot) ||
      is.na(data_to_report()$skin_top)
    )
    &&
    (
      is.na(data_to_report()$Q) ||
      is.na(data_to_report()$R) ||
      is.na(data_to_report()$Stor) ||
      is.na(data_to_report()$Trans)
    ))
    {
      miss_input(3)
      return()
    }
    
    if (is.na(data_to_report()$Q) ||
        is.na(data_to_report()$R) ||
        is.na(data_to_report()$Stor) ||
        is.na(data_to_report()$Trans))
    {
      miss_input(2)
      return()
    }
    
    if (is.na(data_to_report()$stor_bot) ||
        is.na(data_to_report()$stor_top) ||
        is.na(data_to_report()$skin_bot) ||
        is.na(data_to_report()$skin_top))
    {
      miss_input(1)
      return()
    }
    report_cond(1)
    miss_input(0)
    
    ########################## main process   ------------------------------------------------------------------------------------------------
    
    showModal(modalDialog("Loading", footer = NULL))
    
    time = data_ok()$time
    drawdown = data_ok()$drawdown
    final_data = data.frame(
      time =  time * data_to_report()$Trans / data_to_report()$R / data_to_report()$R / data_to_report()$Stor,
      drawdown = drawdown * 2 * 3.14 * data_to_report()$Trans / data_to_report()$Q
    )
    final_data = final_data[1:max_val(), ]
    ind = log10(final_data$time) < 5
    final_data <- final_data[ind,]
    final_data(final_data)
    DimlesTime = final_data()$time
    obsSnormDra = final_data()$drawdown
    
    #######  main calculation --------------------------------------------------------------------------------------------------------------------
    
    len <- length(obsSnormDra)
    
    if (len < max_val())
    {
      end_t(len)
      max_val(len)
    }
    
    else
      max_val()
    
    if (input$cores_to_use == 1) {
      result <- GwOptim(
        obsSnormDra[min_val():max_val()],
        DimlesTime[min_val():max_val()],
        "MSE",
        lower = c(data_to_report()$stor_bot, data_to_report()$skin_bot),
        upper = c(data_to_report()$stor_top, data_to_report()$skin_top),
        control = DEoptim.control(itermax = as.numeric(data_to_report()$iter))
      )
    }
    
    if (input$cores_to_use > 1) {
      Gwinput = list(
        Vi = c(
          8.333333e-02,
          -3.208333e+01,
          1.279000e+03,
          -1.562367e+04,
          8.424417e+04,
          -2.369575e+05,
          3.759117e+05,-3.400717e+05,
          1.640625e+05,
          -3.281250e+04
        ),
        Cbar = -0.5,
        Skin = -0.5
      )
      class(Gwinput) = "IsotropicMediumBesselR"
      
      Gwinput$TD = DimlesTime[min_val():max_val()]
      obsSnorm = obsSnormDra[min_val():max_val()]
      paral = TRUE
      ncores = as.numeric(input$cores_to_use)
      cltype <-
        ifelse(.Platform$OS.type != "windows", "FORK", "PSOCK")
      pType = 2
      par_Var = c("obsSnormDra", "DimlesTime", "Gwinput", "obsSnorm")
      foreachArguments <-
        list("obsSnormDra", "DimlesTime", "Gwinput", "obsSnorm")
      clusters <- makeCluster(ncores, type = cltype)
      registerDoParallel(clusters)
      clusterExport(cl = clusters,
                    varlist = foreachArguments,
                    envir = environment())
      
      result <- GwOptim(
        obsSnormDra[min_val():max_val()],
        DimlesTime[min_val():max_val()],
        "MSE",
        lower = c(data_to_report()$stor_bot, data_to_report()$skin_bot),
        upper = c(data_to_report()$stor_top, data_to_report()$skin_top),
        control = DEoptim.control(
          itermax = as.numeric(data_to_report()$iter),
          trace = TRUE,
          parallelType = pType,
          parVar = par_Var,
          packages = list("GTest")
        )
      )
    }
    closeAllConnections()
    
    ########################## save setup   --------------------------------------------------------------------------------------
    
    name <- paste(input$name_out_file, "results")
    path1 <- paste0(path(), "/", input$name_out_file, "-results")
    pathh2 <- paste0(path(), "/", input$name_out_file, "-project.wt")
    setupWT_main <- data_to_report()
    setupWT_main$max_data_used = len
    
    if (sum(is.na(DimlesTime[1:end_t()])) > 0)
    {
      if (which(is.na(DimlesTime[1:end_t()]))[1] < end_t())
      {
        end <- which(is.na(DimlesTime[1:end_t()]))[1] - 1
        end_t(end)
      }
      
      else
        end_t(end_t())
    }
    
    sim_in = SetStehfestData(
      Time = DimlesTime[1:end_t()],
      Cbar = result$Cbar,
      N = 10,
      Skin = result$Skin ,
      WellTestType = "IsotropicMediumBesselR"
    )
    
    sim <- DimensionlessDrawDown(sim_in)
    fitted_data <-
      data.frame(time = DimlesTime[1:end_t()], drawdown = sim)
    resist <-
      (data_to_report()$Q / (2 * pi * data_to_report()$Trans)) * result$Skin
    well_stor <-
      result$Cbar * 2 * pi * data_to_report()$Stor * data_to_report()$R ^ 2
    
    
    result_graph(fitted_data)
    
    
    
    results <- list(
      Cbar = result$Cbar,
      fitted_data = fitted_data,
      Q = data_to_report()$Q,
      R = data_to_report()$R,
      Stor = data_to_report()$Stror,
      Trans = data_to_report()$Trans,
      cone_of_depr = data_to_report()$cone_of_depr,
      hydraulic_cond = data_to_report()$hydraulic_cond,
      aqua_thick = data_to_report()$aqua_thick,
      well_depth = data_to_report()$well_depth ,
      additional_res = resist,
      well_storativity = well_stor
    )
    
    setupWT <<- list(
      Cbar = result$Cbar,
      skin = result$Skin,
      file_type1 = file_t(),
      Q = data_to_report()$Q,
      R = data_to_report()$R,
      Stor = data_to_report()$Stor,
      Transmiss = data_to_report()$Trans,
      data_ok = data_ok(),
      final_data = final_data,
      fitted_data = fitted_data,
      min = min_val(),
      max = max_val(),
      iter = data_to_report()$iter,
      skin_bot = data_to_report()$skin_bot,
      skin_top = data_to_report()$skin_top,
      stor_bot = data_to_report()$stor_bot,
      stor_top = data_to_report()$stor_top,
      cores = data_to_report()$cores_to_use,
      name = data_to_report()$name,
      well_depth = wel_depth(),
      well_location = wel_loc(),
      aqua_thick = aqua_thick(),
      additional_resistance = resist,
      well_stor = well_stor,
      hydraulic_cond = cond(),
      cone_of_depr = cone(),
      max_data_used = len,
      end = end_t(),
      first_section = section_first()
    )
    
    cd(result$Cbar)
    
    Sys.setlocale("LC_TIME", "English")
    
    setupWT_main$well_stor = well_stor
    setupWT_main$skin = result$Skin
    setupWT_main$Cbar = result$Cbar
    setupWT_main$time = format(Sys.time(), "%d %B %Y")
    setupWT_main$well_depth = data_to_report()$well_depth
    setupWT_main$additional_resistance = resist
    setupWT_main$final_data = final_data()
    setupWT_main$fitted_data = fitted_data
    setupWT_main$aqua_thick = data_to_report()$aqua_thick
    data_to_report(setupWT_main)
    setupWT$additional_resistance <<- resist
    
    if (input$setup_out == "yes")
    {
      if (!dir.exists(path()))
      {
        dir.create(path())
      }
      path2_ok(1)
      save(results, file = path1)
      path2(pathh2)
      save(setupWT, file = path2())
    }
    updateTabsetPanel(session, "main2", "panel2")
    removeModal()
  })
  
  output$miss_in <- reactive ({
    miss_input()
  })
  
  outputOptions(output, "miss_in", suspendWhenHidden = FALSE)
  
  ########################## end of main process ------------------------------------------------------------------------------------------
  
  observeEvent(input$file1, {
    report_cond(0)
  })
  
  output$report_condition <- reactive ({
    report_cond()
  })
  
  outputOptions(output, "report_condition", suspendWhenHidden = FALSE)
  
  ####################### render final report  -----------------------------------------------------------------------------------------------
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Pumping test - report', sep = '.', switch(
        input$format,
        HTML = 'html',
        Word = 'docx'
      ))
    },
    
    content = function(file) {
      showConnections(all = TRUE)
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(),
        HTML = html_document(),
        Word = word_document()
      ))
      file.rename(out, file)
      
    }
  )
  
  
  
  additional_from_slope = reactiveVal()
  
  
  observeEvent(input$show_report, {
    showModal(modalDialog("Loading", footer = NULL))
  
    
    
    ad = additional_resistances_from_slope(
      data_to_report()$Trans,
      dat_slope()$coefficients[2],
      data_to_report()$Q,
      data_to_report()$Cbar
    )
    
    additional_from_slope(ad)
    
    
    output$report <- renderUI({
      src2 <- normalizePath('reporrr.Rmd')
      library(knitr)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      knitr::opts_knit$set(root.dir = owd)
      
      tagList(HTML(knitr::knit2html(
        text = readLines(src2), fragment.only = TRUE
      )),
      
      tags$script(HTML(
        'MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'
      )),
      
      tags$script(
        HTML(
          "if (hljs) $('#report pre code').each(function(i, e) {
                       hljs.highlightBlock(e)
                 });"
        )
      ))
    })
    
    
    removeModal()
  })
  
  ############################## btn class ---------------------------------------------------------------------------------------------------------------------------------
  
  output$go_to_evall <- renderUI({
    if (new_proj() == 1)
      cls = "btn-primary"
    
    if (new_proj() == 2)
      cls = "btn-secundary"
    
    actionButton(
      inputId = "input_parameters_from_file",
      label = "Find parameters in file",
      class = "btn-primary",
      width = "180"
    )
  })
  
  ############################## stages ---------------------------------------------------------------------------------------------------------------------------------
  
  ############################### new or project ---------------------------------------------------------------------------------------------------------
  
  new_proj <- reactiveVal (0)
  
  observeEvent(input$new_project, {
    new_proj(1)
    updateTabsetPanel(session, "main1", "panel11")
    updateTabsetPanel(session, "main2", "panel1")
  })
  
  observeEvent(input$load_project, {
    new_proj(2)
    updateTabsetPanel(session, "main1", "panel22")
    updateTabsetPanel(session, "main2", "panel2")
  })
  
  output$new_orproj <- reactive ({
    new_proj()
  })
  
  outputOptions(output, "new_orproj", suspendWhenHidden = FALSE)
  
  ############################### 1.1 ---------------------------------------------------------------------------------------------------------
  
  stage <- reactiveVal(1)
  
  #### previous --------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$previous1.1, {
    new_proj(1)
  })
  
  observeEvent(input$previous1.1.1, {
    stage(1)
  })
  
  observeEvent(input$previous1.1.1x, {
    stage(1)
  })
  
  observeEvent(input$previous1.1.2x, {
    stage(111)
  })
  
  observeEvent(input$previous1.1.2t, {
    stage(111)
  })
  
  #### next --------------------------------------------------------------------
  
  format_file1 <- reactiveVal(0)
  
  observeEvent(input$file1, {
    if (format_file1() == 1)
      return ()
    
    else
      stage(111)
  })
  
  observeEvent(input$next1.1, {
    if (is.null(input$file1) || is.null(file_t()))
      return()
    
    else
      stage(111)
    
  })
  
  observeEvent(input$next1.1.1t, {
    stage(112)
    updateTabsetPanel(session, "main1", "panel22")
  })
  
  observeEvent(input$next1.1.1x, {
    if (is.null(data()))
      return()
    
    stage(112)
    updateTabsetPanel(session, "main1", "panel22")
  })
  
  output$format_file1 <- reactive ({
    format_file1()
  })
  
  outputOptions(output, "format_file1", suspendWhenHidden = FALSE)
  
  ################################# 1.2 -----------------------------------------------------------------------------------------------------
  
  ####  previous ----------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$previous1.2, {
    stage(1)
  })
  
  output$stagee <- reactive ({
    stage()
  })
  
  outputOptions(output, "stagee", suspendWhenHidden = FALSE)
  
  #################################   2. page -----------------------------------------------------------------------------------------------
  
  
  observeEvent(c(
    input$next_to_param_xlsx,
    input$next_to_param_txt,
    input$next_proj1t
  ),
  {
    if (input$next_to_param_xlsx == 0 &&
        input$next_to_param_txt == 0 && input$next_proj1t == 0)
      return()
    
    updateTabsetPanel(session, "navbar", "Input parameters")
    stage2(2)
  })
  
  observeEvent(input$prev_to_file,  {
    if (input$prev_to_file == 0 || is.null(time_col()))
      return()
    
    updateTabsetPanel(session, "navbar", "Input file")
    
    if (new_or_load() == 1)
      proj_stag(1)
    
    else
      stage(112)
  })
  
  observeEvent(input$previous_to_file, {
    stage(112)
  })
  
  stage2 <- reactiveVal(2)
  
  #### previous -----------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$previous21, {
    stage2(2)
  })
  
  observeEvent(input$previous22, {
    stage2(2)
  })
  
  observeEvent(input$previous221, {
    stage2(22)
  })
  
  observeEvent(input$previous23, {
    stage2(21)
  })
  
  observeEvent(input$go_to_eval, {
    updateTabsetPanel(session, "navbar", "Input parameters")
    stage2(23)
  })
  
  observeEvent(c(input$go_to_start, input$go_to_start2), {
    stage(1)
    proj_stag(0)
    updateTabsetPanel(session, "navbar", "Input file")
  })
  
  observeEvent(input$previous222, {
    stage2(221)
  })
  
  observeEvent(input$previous223, {
    stage2(221)
  })
  
  #### next ----------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$next2.1, {
    stage2(21)
  })
  
  observeEvent(input$next21, {
    stage2(23)
  })
  
  observeEvent(input$next_stage2_0, {
    stage2(21)
  })
  
  ##### 22 input file 2 -----------------------------------------------------------------------------------------------------------------------
  
  format_file2 <- reactiveVal(0)
  
  observeEvent(input$input_parameters_from_file, {
    stage2(22)
  })
  
  observeEvent(input$yes22, {
    stage2(221)
  })
  
  observeEvent(input$no22, {
    stage2(2)
  })
  
  observeEvent(input$load_sheet2, {
    stage2(2)
  })
  
  observeEvent(input$next221, {
    if (is.null(input$file2))
      return ()
    
    else if (file_t2() == "xlsx")
      stage2(222)
    
    else
      stage2(223)
  })
  
  observeEvent(input$next223, {
    stage2(2)
  })
  
  observeEvent(input$next222, {
    stage2(2)
  })
  
  ##############
  
  output$stagee2 <- reactive ({
    stage2()
  })
  
  outputOptions(output, "stagee2", suspendWhenHidden = FALSE)
  
  ##############################  set limits yes or no ------------------------------------------------------------------------------------------------------
  
  limits <- reactiveVal(0)
  
  observeEvent(input$limit_set, {
    if (input$limit_set == "yes")
      limits(1)
    
    else
      limits(0)
  })
  
  output$set_limits <- reactive({
    limits()
  })
  
  outputOptions(output, "set_limits", suspendWhenHidden = FALSE)
  
  ########### file project stages --------------------------------------------------------------------------------------------------------------------------
  
  proj_stag <- reactiveVal(0)
  
  observeEvent(input$load_project, {
    proj_stag(0)
  })
  
  observeEvent(input$new_project, {
    proj_stag(0)
  })
  
  observeEvent(input$previous_proj0, {
    stage(1)
    proj_stag()
  })
  
  observeEvent(input$next_proj0, {
    if (is.null(input$file_project))
      return()
    
    else
      proj_stag(1)
  })
  
  observeEvent(input$file_project, {
    if (file_t_proj() == 1)
      return ()
    
    else
      proj_stag(1)
  })
  
  observeEvent(input$previous_proj1t, {
    proj_stag(0)
  })
  
  observeEvent(input$input$next_proj0, {
    proj_stag(1)
  })
  
  output$proj_stage <- reactive({
    proj_stag()
  })
  
  outputOptions(output, "proj_stage", suspendWhenHidden = FALSE)
  
  #### main data to react val -----------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$Q, {
    setupWT_main = data_to_report()
    setupWT_main$Q = input$Q
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$R, {
    setupWT_main = data_to_report()
    setupWT_main$R = input$R
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$Stor, {
    setupWT_main = data_to_report()
    setupWT_main$Stor = input$Stor
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$Trans, {
    setupWT_main = data_to_report()
    setupWT_main$Trans = input$Trans
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$iter,  {
    setupWT_main = data_to_report()
    setupWT_main$iter = input$iter
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$skin_bot, {
    setupWT_main = data_to_report()
    setupWT_main$skin_bot = input$skin_bot
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$skin_top, {
    setupWT_main = data_to_report()
    setupWT_main$skin_top = input$skin_top
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$stor_bot, {
    setupWT_main = data_to_report()
    setupWT_main$stor_bot = input$stor_bot
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$stor_top, {
    setupWT_main = data_to_report()
    setupWT_main$stor_top = input$stor_top
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$cores_to_use, {
    setupWT_main = data_to_report()
    setupWT_main$cores_to_use = input$cores_to_use
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$name_out_file, {
    setupWT_main = data_to_report()
    setupWT_main$name = input$name_out_file
    data_to_report(setupWT_main)
  })
  
  observeEvent(file_t(), {
    setupWT_main = data_to_report()
    setupWT_main$file_type1 = file_t()
    data_to_report(setupWT_main)
  })
  
  ####################### report contain  -----------------------------------------------------------------------------------------------
  
  eval_par_report <- reactiveVal ()
  
  observeEvent(input$report_contain, {
    setupWT_main = data_to_report()
    setupWT_main$report_contain = input$report_contain
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$well_location, {
    setupWT_main = data_to_report()
    setupWT_main$well_location = input$well_location
    
    if (exists("setupWT") && path2_ok() == 1)
    {
      setupWT$well_location <<- input$well_location
      save(setupWT, file = path2())
    }
    wel_loc(input$well_location)
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$well_depth, {
    setupWT_main = data_to_report()
    setupWT_main$well_depth = input$well_depth
    setupWT$well_depth <<- input$well_depth
    
    if (exists("setupWT") && path2_ok() == 1)
      save(setupWT, file = path2())
    
    wel_depth(input$well_depth)
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$aqua_thick, {
    setupWT_main = data_to_report()
    setupWT_main$aqua_thick = input$aqua_thick
    
    if (exists("setupWT") && path2_ok() == 1)
    {
      setupWT$aqua_thick <<- input$aqua_thick
      save(setupWT, file = path2())
    }
    aqua_thick(input$aqua_thick)
    data_to_report(setupWT_main)
  })
  
  observeEvent(input$param_to_eval, {
    if (sum(input$param_to_eval == "Transmissivity") == 1)
      slider_tr("yes")
    
    else
      slider_tr("no")
  })
  
  observeEvent(input$param_to_eval, {
    if (sum(input$param_to_eval == "Hydraulic conductivity") == 1 ||
        sum(input$param_to_eval == "Cone of depression") == 1)
      thickness("yes")
    
    else
      thickness("no")
  })
  
  cone <- reactiveVal(0)
  cond <- reactiveVal(0)
  trans <- reactiveVal (0)
  
  observeEvent(c(
    input$param_to_eval,
    input$aqua_thick,
    thickness(),
    input$show_report
  ),
  {
    if (is.null(input$aqua_thick))
      return()
    
    if (!is.null(input$Trans))
      trans(input$Trans)
    
    else if (exists("setupWT"))
      trans(setupWT$Transmiss)
    
    
    
    
    if (sum(input$param_to_eval == "Hydraulic conductivity") == 1 &&
        sum(input$param_to_eval == "Cone of depression") == 1)
    {
      cond(trans() / input$aqua_thick)
      cone(3000 * end_t_lvl() * cond() ^ (0.5))
    }
    
    else if (sum(input$param_to_eval == "Hydraulic conductivity") == 1 &&
             sum(input$param_to_eval == "Cone of depression") != 1)
    {
      cond(trans() / input$aqua_thick)
    }
    
    else if (sum(input$param_to_eval == "Hydraulic conductivity") != 1 &&
             sum(input$param_to_eval == "Cone of depression") == 1)
    {
      cond(trans() / input$aqua_thick)
      cone(3000 * end_t_lvl() * cond() ^ (0.5))
      cond(0)
    }
    
    if (exists("setupWT") && path2_ok() == 1)
    {
      setupWT$hydraulic_cond <<- cond()
      setupWT$cone_of_depr <<- cone()
    }
    
    setupWT_main = data_to_report()
    setupWT_main$hydraulic_cond = cond()
    setupWT_main$cone_of_depr = cone()
    
    data_to_report(setupWT_main)
    eval_par_report(0)
    
    if (exists("setupWT") && path2_ok() == 1)
      save(setupWT, file = path2())
  })
  
  size_text <- reactiveVal()
  size_title <- reactiveVal()
  graph_size <- reactiveVal()
  mark_format <- reactiveVal()
  nav_change <- reactiveVal(0)
  
  observeEvent(input$format, {
 
    
    if (input$format == "HTML") {
      size_text(12)
      size_title(17)
      mark_format("html")
    }
    
    if (input$format == "Word") {
      size_text(8)
      size_title(11)
      graph_size(11)
      mark_format("docx")
    }
  })
  
  onStop(function() {
    to_del = c("cores",
               "packages",
               "set_cores",
               "ipak",
               "setupWT",
               "obsSnorm",
               "Gwinput")
    
    for (i in 1:5)
    {
      if (exists(to_del[i]))
        remove(list = to_del[i],
               pos = 1,
               inherits = TRUE)
    }
  })
  
  
  output$nav_changee <- reactive ({
    nav_change()
  })
  
  outputOptions(output, "nav_changee", suspendWhenHidden = FALSE)
  
}
