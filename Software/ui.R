

navbarPage( id = "navbar",
  title = 'Pumping test',
  theme = shinytheme("cerulean"),
  tabPanel( 
    'Input file',
 
    sidebarPanel(
      #########################################################  start page ------------------------------------------------------
      
      conditionalPanel(
        condition = "output.nav_changee == '0' ",
        uiOutput('style_tagg')
      ),
      
      conditionalPanel(
        condition = "output.stagee == '1'  && output.proj_stage == '0' ",
        tags$div(class = "header", checked = NA,
                 tags$h3("Wellcome")),
    
        actionButton(
          inputId = "new_project",
          label = "New project",
          class = "btn-primary",
          width = "185"
        ),
        
        hr(),
        
        actionButton(
          inputId = "load_project",
          label = "Load project",
          class = "btn-primary",
          width = "185"
        ),
        
        hr()
      ),
      
      #########################################################  1.1  file input ----------------------------------------------------
      
      conditionalPanel(
        condition = "output.stagee == '1'  && output.new_orproj == '1' && output.proj_stage == '0' && output.format_file1 == '0' ",
                       tags$h4("Select file", style = "color:black")
        ),
      
      conditionalPanel(condition = "output.stagee == '1'  && output.new_orproj == '1' && output.proj_stage == '0' && output.format_file1 == '1' ",
        hr(),
        tags$h4("Accepts only files in format - txt / xlsx", style = "color:black")),
      
      conditionalPanel(condition = "output.stagee == '1'  && output.new_orproj == '1' && output.proj_stage == '0' ",
                       uiOutput("file1_ui")
                       ),
      
      conditionalPanel(
        condition = "output.stagee == '1'  && output.new_orproj == '1' && output.proj_stage == '0' && output.dat1 == '1' ",
       
         actionButton(
          inputId = "next1.1",
          label = "Next",
          width = "145",
          class = "btn-primary")
      ),
      
      #############################################   file input - project ------------------------------------------------------------------------
      
      conditionalPanel(
      condition = "output.stagee == '1'  && output.new_orproj == '2' && output.proj_stage == '0' && output.file_t_proj == '0' ",
                       tags$h4("Select project", style = "color:black")
      ),
      
      conditionalPanel(
        condition = "output.stagee == '1'  && output.new_orproj == '2' && output.proj_stage == '0' && output.file_t_proj == '1' ",
        
        hr(),
        
        tags$h4("Accepts only files WT-project", style = "color:black")
        ),
      
      conditionalPanel(condition = "output.stagee == '1' && output.new_orproj == '2' && output.proj_stage == '0' ",
                       uiOutput("file_project")
                       ),
      
      conditionalPanel(
        condition = "output.stagee == '1' && output.new_orproj == '2' && output.proj_stage == '0' && output.dat1 == '1' ",
        
        actionButton(
          inputId = "next_proj0",
          label = "Next",
          width = "145",
          class = "btn-primary")
      ),
      
      conditionalPanel(
        condition = "output.proj_stage == '1' && output.eval_par == '0' && output.loaded_setup == '1'",
        
        actionButton(
          inputId = "go_to_eval",
          label = "Go to evaluation",
          class = "btn-info",
          width = "190"),
        
        hr()
      ),
      
      conditionalPanel(
        condition = "output.proj_stage == '1' && output.eval_par == '0' ",
        
        tags$h4("Select minimum", style = "color:black"),
        uiOutput("min_data_proj"),
        
        tags$h4("Select maximum", style = "color:black"),
        uiOutput("max_data_projj"),
        
        actionButton(
          inputId = "eval_param_p",
          width = "185",
          class = "btn-primary",
          label = "Evaluate parameters"
        ),
        
        hr (),
        
        actionButton(inputId = "previous_proj1t",
                     label = "Previous",
                     width = "85"
                     ),
        
        actionButton(
          inputId = "next_proj1t",
          label = "Next",
          width = "85",
          class = "btn-primary")
      ),
      
      #########################################################  1.1.1  txt  sep dec ------------------------------------------
      
      conditionalPanel(
        condition = "output.file_type  == 'txt' && output.stagee == '111' ",
        tags$h4("Display data", style = "color:black"),
        radioButtons(
          'length_data1t',
          label = NULL,
          c('head' = "no", 'all' = "yes"),
          selected = "no"
        ),
        
        tags$h4("Separator", style = "color:black"),
        radioButtons(
          "sep",
          label = NULL,
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = "\t"
        ),
        
        tags$h4("Desimal seperator", style = "color:black"),
        radioButtons(
          "dec",
          label = NULL,
          c('Comma' = ",",
            'Punktum' = "."),
          selected = "."
        ),
        
        actionButton(inputId = "previous1.1.1",
                     label = "Previous"
                     ),
        
        actionButton(
          inputId = "next1.1.1t",
          label = "Next",
          width = "85",
          class = "btn-primary")
      ),
      
      ###############################################  1.1.1  xlsx  sheet ------------------------------------------
      
      conditionalPanel(
        condition = "output.file_type  == 'xlsx' && output.stagee == '111' ",
        tags$h4("Display data", style = "color:black"),
        
        radioButtons('length_data1x',
                     label = NULL,
                     c('head' = "no", 'all' = "yes")
                     ),
        
        tags$h4("Select list", style = "color:black"),
        uiOutput("sheet_names"),
        
        actionButton(
          inputId = "load_sheet",
          label = "Load this list",
          class = "btn-primary",
          width = "185"
        ),
        
        hr(),
        
        actionButton(
          inputId = "previous1.1.1x",
          label = "Previous",
          width = "85"
        ),
        
        actionButton(
          inputId = "next1.1.1x",
          label = "Next",
          width = "85",
          class = "btn-primary")
      ),
      
      #########################################################  1.1.2  txt  graph --------------------------------------------------------------------
      
      conditionalPanel(
        condition = "output.file_type  == 'txt' && output.stagee == '112' && output.eval_par == '0' ",
        
        tags$h4("Select time", style = "color:black"),
        uiOutput("time_col_txt"),
        
        tags$h4("Select drawdown", style = "color:black"),
        uiOutput("drawdown_col_txt"),
        
        tags$h4("Select minimum", style = "color:black"),
        uiOutput("min_data_txt"),
        
        tags$h4("Select maximum", style = "color:black"),
        uiOutput("max_data_txt"),
        
        actionButton(
          inputId = "eval_param_t",
          width = "185",
          class = "btn-primary",
          label = "Evaluate parameters"
        ),
        
        hr (),
        
        actionButton(
          inputId = "previous1.1.2t",
          width = "85",
          label = "Previous"
        ),
        
        actionButton(
          inputId = "next_to_param_txt",
          label = "Next",
          class = "btn-primary",
          width = "85")
      ),
      
      #########################################################  1.1.2  xlsx graph ------------------------------------------------------------------
      
      conditionalPanel(
        condition = "output.file_type  == 'xlsx' && output.stagee == '112' && output.eval_par == '0' ",
        
        tags$h4("Select time", style = "color:black"),
        uiOutput("time_col_xlsx"),
        
        tags$h4("Select drawdown", style = "color:black"),
        uiOutput("drawdown_col_xlsx"),
        
        tags$h4("Select minimum", style = "color:black"),
        uiOutput("min_data_xlsx"),
        
        tags$h4("Select maximum", style = "color:black"),
        uiOutput("max_data_xlsx"),
        
        actionButton(
          inputId = "eval_param_x",
          width = "185",
          class = "btn-primary",
          label = "Evaluate parameters"
        ),
        
        hr (),
        
        actionButton(
          inputId = "previous1.1.2x",
          label = "Previous",
          width = "85"
        ),
        
        actionButton(
          inputId = "next_to_param_xlsx",
          label = "Next",
          class = "btn-primary",
          width = "85")
      ),
      
      #######  eval param  --------------------------------------------------------------------------------------------------------------------------
      
      conditionalPanel(
        condition = "output.stagee == '112' && output.eval_par == '1' || output.proj_stage == '1' && output.eval_par == '1'  ",
        tags$h4("Evaluate parameters", style = "color:black"),
        
        checkboxGroupInput(
          "param_to_eval",
          label = NULL,
          c(
            "Selected",
            "Transmissivity",
            "Hydraulic conductivity",
            "Cone of depression"),
          selected = "Selected"),
        
        hr ()
      ),
      
      conditionalPanel(
        condition = "output.stagee == '112' && output.eval_par == '1' && output.slider_trans == 'yes' || output.proj_stage == '1' && output.eval_par == '1' && output.slider_trans == 'yes' ",
                       uiOutput("trans_limits")),
      
      conditionalPanel(
        condition = "output.stagee == '112' && output.eval_par == '1' && output.thick == 'yes' || output.proj_stage == '1' && output.eval_par == '1' && output.thick == 'yes' ",
        
        tags$h4("Aquifer thickness [ m ]", style = "color:black"),
        
      uiOutput("aq_thick")
      ),
      
      conditionalPanel(
        condition = "output.stagee == '112' && output.eval_par == '1' ||  output.proj_stage == '1' && output.eval_par == '1' ",
        actionButton(
          inputId = "previous_eval_p",
          label = "Back",
          width = "85")
      )
    ),
    
    ###################  main panel  ---------------------------------------------------------------------------------------------------------------------------
    
    mainPanel(
      tags$style(HTML("
    .tabbable > .nav > li > a                  {border:thin solid black; background-color: white;  color:black}
    .tabbable > .nav > li[class=active]    > a {border:thin solid black; background-color: orange; color:white}
  ")
          ),
      
      tabsetPanel(
      tabPanel("Preview file", 
               tabPanel("Logs",icon = icon("bell"),mainPanel(htmlOutput("table.output"))),
               tags$style(type="text/css", "#logs th, td {border: medium solid white;text-align:center; background-color: white}"),
               tags$style(type="text/css", "#logs td {border: medium solid white; text-align:center; background-color: white}")),
      
      tabPanel("Graph",
               plotOutput("graph1"))
      )
    )
  ),
  
  
  ######################  2. page ------------------------------------------------------------------------------------------------------------------------------
  
  tabPanel(   
    'Input parameters',
    
    conditionalPanel(
      condition = "output.nav_changee == '0' ",
    uiOutput("style_tag")
    ),
    
    sidebarPanel(
      conditionalPanel(
        condition = " output.stagee2 == '2' ",
        
        uiOutput("go_to_evall")
      ),
   
      conditionalPanel(
        condition = " output.dat2 == '1' &&  output.stagee2 == '2' && output.file_type2 == 'txt'",
          tags$h4("Display data", style = "color:black"),
        
        radioButtons(
          'length_data2t',
          label = NULL,
          c('head' = "no", 'all' = "yes"),
          selected = "no")
      ),
      
      conditionalPanel(
        condition = " output.stagee2 == '2' ",
        
        hr(),
        
        tags$h4("Pumped well discharge rate [m3/s]", style = "color:black"),
        uiOutput("Q"),
        
        tags$h4("Radius [m]", style = "color:black"),
        uiOutput("R"),
        
        tags$h4("Storativity [-]", style = "color:black"),
        uiOutput("Storr"),
        
        tags$h4("Transmissivity [m2/s]", style = "color:black"),
        uiOutput("Trans"),
        
        actionButton(
          inputId = "prev_to_file",
          label = "Previous",
          width = "85"
          ),
        
        actionButton(
          inputId = "next_stage2_0",
          label = "Next",
          class = "btn-primary",
          width = "85")
      ),
      
      conditionalPanel(
        condition = " output.stagee2 == '22' ",
        
        tags$h4("Load new file", style = "color:black"),
        
        actionButton(
          inputId = "yes22",
          label = "Yes",
          class = "btn-primary",
          width = "185"
        ),
        
        actionButton(
          inputId = "no22",
          label = "No",
          width = "185"
        ),
        
        hr(),
        
        actionButton(
          inputId = "previous22",
          label = "Previous",
          width = "85"
        )
      ),
      
      conditionalPanel(
        condition = "output.stagee2 == '221' && output.format_file2 == '1' ",
        hr(),
        tags$h4("Accepts only files in format - txt / xlsx ", style = "color:black")
      ),
      
      conditionalPanel(
        condition = " output.stagee2 == '221' && output.format_file2 == '0' && output.format_file2 != '1'",
                       tags$h4("Select File", style = "color:black")
        ),
      
      conditionalPanel(
        condition = " output.stagee2 == '221' && output.format_file2 == '0' && output.format_file2 == '1' ",
        hr(),
        tags$h4("Accepts only files WT-project", style = "color:black")
        ),
      
      conditionalPanel(
        condition = " output.stagee2 == '221' ",
        fileInput(
          'file2',
          label = NULL,
          accept = c('text/csv', 'text/comma-separated- values,text/plain', '.csv')
        ),
        
        actionButton(
          inputId = "previous221",
          label = "Previous",
          width = "85"
        ),
        
        actionButton(
          inputId = "next221",
          label = "Next",
          class = "btn-primary",
          width = "85")
      ),
      
      ##### sheet names 2 --------------------------------------------------------------------------------------------------------------------
      
      conditionalPanel(
        condition = " output.stagee2 == '222' ",
        
        tags$h4("Select list", style = "color:black"),
        uiOutput("sheet_names2"),
        
        actionButton(
          inputId = "load_sheet2",
          label = "Load this sheet",
          class = "btn-primary",
          width = "185"
        ),
        
        hr(),
        
        actionButton(
          inputId = "previous222",
          label = "Previous",
          width = "85"),
        
        actionButton(
          inputId = "next222",
          label = "Back",
          class = "btn-primary",
          width = "85")
      ),
      
      ##### sep dec - 2 ---------------------------------------------------------------------------------------------------------------------
      
      conditionalPanel(
        condition = " output.stagee2 == '223' ",
        
        tags$h4("Separator", style = "color:black"),
        
        radioButtons(
          "sep2",
          label = NULL,
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = "\t"
        ),
        
        tags$h4("Desimal seperator", style = "color:black"),
        radioButtons(
          "dec2",
          label = NULL,
          c('komma' = ",",
            'punktum' = "."),
          selected = ","
        ),
        
        hr(),
        
        actionButton(
          inputId = "previous223",
          label = "Previous",
          width = "85"
          ),
        
        actionButton(
          inputId = "next223",
          label = "Back",
          class = "btn-primary",
          width = "85")
      ),
      
      ##### 2. page stage 2 -----------------------------------------------------------------------------------------------------------------------------------
      
      conditionalPanel(
        condition = "output.stagee2 == '21'  ",
        
        tags$h4("Number of iterations", style = "color:black"),
        uiOutput("iter_ui"),
        hr()
      ),
      
      conditionalPanel(
        condition = "output.stagee2 == '21'  ",
        
        tags$h4("Skin bottom limit", style = "color:black"),
        uiOutput("skin_bot_ui"),
        
        tags$h4("Skin upper limit ", style = "color:black"),
        uiOutput("skin_top_ui"),
        
        tags$h4("Storativity bottom limit", style = "color:black"),
        uiOutput("stor_bot_ui"),
        
        tags$h4("Storativity upper limit", style = "color:black"),
        uiOutput("stor_top_ui")
      ),
      
      conditionalPanel(
        condition = " output.stagee2 == '21' ",
        actionButton(
          inputId = "previous21",
          label = "Previous",
          width = "85"
        ),
        
        actionButton(
          inputId = "next21",
          label = "Next",
          class = "btn-primary",
          width = "85")
      ),
      
      ######################################### main -----------------------------------------------------------------------------------
      
      conditionalPanel(
        condition = "output.stagee2 == '23' ",
        
        tags$h4("Select the number of processor cores to use", style = "color:black"),
        uiOutput("num_cores"),
        
        tags$h4("Save project", style = "color:black"),
        radioButtons("setup_out", label = NULL,
                     c("Yes" = "yes",
                       "No" = "no")
                     ),
        
        uiOutput("dir_select"),
        
        tags$h4("Enter name of wellbore", style = "color:black"),
        uiOutput("out_name")
        ),
      
      conditionalPanel(
        condition = "output.stagee2 == '23' && output.miss_in == '1' ",
                       tags$h4("Missing limits", style = "color:black")
        ),
      
      conditionalPanel(
        condition = "output.stagee2 == '23' && output.miss_in == '2' ",
        tags$h4("Missing parameters", style = "color:black")
      ),
      
      conditionalPanel(
        condition = "output.stagee2 == '23' && output.miss_in == '3' ",
        tags$h4("Missing parameters and limits", style = "color:black")
      ),
      
      conditionalPanel(
        condition = "output.stagee2 == '23' && output.miss_in == '4' ",
        tags$h4("Missing columns settings ", style = "color:black")
      ),
      
      conditionalPanel(
        condition = "output.stagee2 == '23' && output.miss_in == '5' ",
        tags$h4("Please choose directory", style = "color:black")),
      
      conditionalPanel(
        condition = "output.stagee2 == '23' ",
        
        actionButton(
          inputId = "run_process",
          label = "Run process",
          class = "btn-success",
          width = "185"
        ),
       
        hr(),
        
        actionButton(
          inputId = "previous23",
          label = "Previous",
          width = "85"
        )
      )
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Preview file",
               DT::dataTableOutput("table.output2")),
      tabPanel("Graph - results",
               plotOutput("graph2"))
      )
    )
  ),
  
  tabPanel('Report', 
           uiOutput('style_taggg'),
           
           sidebarLayout(
             sidebarPanel(
               helpText(),
               
               conditionalPanel(
                 "output.report_condition == '1'  ",
                 
                 tags$h4("Report options", style = "color:black"),
                 
                 uiOutput("report_con"), 
                 
                 
                 actionButton('select_all_graphs_report', 'Select all', class = "btn-primary", width = "85"),
                 
                 hr(),
                 
                 tags$h4("Enter well location ", style = "color:black"),
                 uiOutput("well_locat"),
                 
                 tags$h4("Enter well depth", style = "color:black"),
                 uiOutput("well_depthh"),
                 
                 actionButton('show_report', 'Preview report', class = "btn-primary"),
                 
                 hr(),
                 
                 tags$h4(" Output document format", style = "color:black"),
                 radioButtons(
                   'format',
                   label = NULL,
                   c('PDF', 'HTML', 'Word'),
                   inline = TRUE
                 ),
                 
                 downloadButton('downloadReport', label =  "Save report", class = "btn-primary")
               ),
               
               hr(),
               
               actionButton(
                 inputId = "go_to_start2",
                 label = "Start new project",
                 class = "btn-primary",
                 width = "185")
             ),
             mainPanel(uiOutput('report'))
           )
      )
)
