<<<<<<< HEAD
#' BETS search 
#' 
#' An interface for searching time series with possibility 
#' to extract the data in different extensions.
#' 
#' @import miniUI
#' @import rstudioapi
#' @import shiny dplyr
#' @importFrom utils write.csv write.csv2
#' 
#' @export





BETS.addin_en  = function(){
  
  ui<- miniUI::miniPage(
    gadgetTitleBar("BETS Search Addin"),
    miniTabstripPanel(
      
      # Tab 1 - choose any colourweb.
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          
          fluidRow(
            column(4,
                   textInput("description", 
                             "Description:", c("Search")
                   ) 
            ),
            
            
            column(2,
                   selectInput("periodicity",
                               "Periodicity:",
                               c("All","Monthly","Anual","Quarterly","Weekly","Daily")
                               
                   )       
            ),
            column(3,
                   textInput("source",
                             "Source:",
                             c("All")
                   )       
            ),
            # Create a new row for the table.
            
            DT::dataTableOutput("table")
            
            
          )
        )
      ),
      
      # Tab 2 - choose an R colour similar to a colour you choose
      miniTabPanel(
        "Visualization",
        icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput("table2")
        )
      ),
      
      miniTabPanel(
        "Export",
        icon = icon("save"),
        miniContentPanel(
          fluidRow(
            column(2, 
                   textInput("code","TS code")
            ),
            column(3,
                   textInput("name","file name","Dados")
            ), 
            column(7,
                   textInput("local","Save file",c(getwd()))
            )
          ),
          
          br(),
          
          
          fluidRow(
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#3D8B37; color:#FFF;
                        border-color:#3D8B37; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_csv", "Excel - CSV",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#2E8B57; color:#FFF;
                        border-color:#2E8B57; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_csv2", "Excel - CSV2",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#3c97ac; color:#FFF;
                        border-color:#3c97ac; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_rds", "R - RDS",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            )
          ),
          
          br(),
          
          
          fluidRow(
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#1D5F6D; color:#FFF;
                        border-color:#1D5F6D; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_stata", "STATA",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#4782ba; color:#FFF;
                        border-color:#4782ba; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_sas", "SAS",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#e1004b; color:#FFF;
                        border-color:#e1004b; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_spss", "SPSS",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            )
          ),
          fluidRow(
            br(),
            span(verbatimTextOutput('done_export'))
            
          )
          
        )
      ) 
      
      
    )
  )  
  
  
  server <- function(input, output, session) {
    
    
    
    
    output$table <- DT::renderDataTable(DT::datatable({
      
      req(input$description) # tratamento para o input da descricao
      req(input$source)      # tratamento para o input da fonte
      
      
      
      #nomes = c("Code","Description","Unit","Periodicity","Start","Last Value","Source")
      
      data.addin <- BETSsearch(description ="*",view=F)
      #names(data.addin) = nomes
      
      
      
      if(input$description != "Search"){
        print(input$description)
        req(input$description) # tratamento para o input da descricao
        data.addin <- BETSsearch(description = input$description,view=F)
        if(is.character(data.addin)){
          data.addin = BETSsearch(description="*",view=F)
          
        }else{
          data.addin 
        }
        
      }else{
        data.addin <- BETSsearch(description ="*",view=F)
      }
      
      
      if(input$periodicity!= "All"){
        req(input$periodicity)     
        
        if(input$periodicity == "Monthly"){
          data.addin <- data.addin[data.addin$periodicity == "M",]
          
          }
        if(input$periodicity == "Anual"){
          data.addin <- data.addin[data.addin$periodicity == "A",]
          
          }
        if(input$periodicity == "Quarterly"){
          data.addin <- data.addin[data.addin$periodicity == "Q",]
          
          }
        if(input$periodicity == "Weekly"){
          data.addin <- data.addin[data.addin$periodicity == "W",]
          
          }
        if(input$periodicity == "Daily"){
          data.addin <- data.addin[data.addin$periodicity == "D",]
          
          }
        
        
        # tratamento para o input da fonte
        #data.addin <- search(description = input$description,view=F)
        
        if(is.character(data.addin)){
          data.addin = BETSsearch(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      if(input$source!= "All"){
        req(input$source)      # tratamento para o input da fonte
        #data.addin <- search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$source == input$source,]
        if(is.character(data.addin)){
          data.addin = BETSsearch(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      print(data.addin)
      
      if(is.character(data.addin)){
        data.addin = BETSsearch(description="*",view=F)
      }else{
        data.addin 
      }
      
      #names(data.addin) = nomes
      data.addin  
      
      
      
    },options = list(pageLength = 5, dom = 'tip'),selection = 'single'))
    
    
    
    output$table2<- DT::renderDataTable(DT::datatable({
      
      req(input$description) # tratamento para o input da descricao
      req(input$source)      # tratamento para o input da fonte
      
      
      
      #nomes = c("Code","Description","Unit","Periodicity","Start","Last Value","Source")
      
      data.addin <- BETSsearch(description ="*",view=F)
      #names(data.addin) = nomes
      
      
      
      if(input$description != "Search"){
        print(input$description)
        req(input$description) # tratamento para o input da descricao
        data.addin <- BETSsearch(description = input$description,view=F)
        if(is.character(data.addin)){
          data.addin = BETSsearch(description="*",view=F)
          
        }else{
          data.addin 
        }
        
      }else{
        data.addin <- BETSsearch(description ="*",view=F)
      }
      
      
      if(input$periodicity!= "All"){
        req(input$periodicity)      # tratamento para o input da fonte
        #data.addin <- search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
        if(is.character(data.addin)){
          data.addin = BETSsearch(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      if(input$source!= "All"){
        req(input$source)      # tratamento para o input da fonte
        #data.addin <- search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$source == input$source,]
        if(is.character(data.addin)){
          data.addin = BETSsearch(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      
      
      if(is.character(data.addin)){
        data.addin = BETSsearch(description="*",view=F)
      }else{
        data.addin 
      }
      
      #names(data.addin) = nomes
      data.addin  
      
      
      
    },options = list(pageLength = 50),selection = 'single'))
    
    
    
    # observeEvent(input$table_cell_clicked,{
    #   output$code = renderPrint(input$table_cell_clicked$value)
    # 
    # })
    
    # output$code = req(renderPrint(input$table_cell_clicked$value))
    # code_ts = as.numeric(input$table_cell_clicked$value)
    
    
    
    
    
    
    # output$code = renderPrint(input$table2_cell_clicked$value)
    
    
    
    
    observeEvent(input$action_csv2,{
      dados = BETSget(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      write.csv2(dados,file =paste0(local,".csv") )
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    observeEvent(input$action_csv,{
      dados = BETSget(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      write.csv(dados,file =paste0(local,".csv") )
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    observeEvent(input$action_rds,{
      dados = BETSget(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      saveRDS(dados,file = paste0(local,".rds"))
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    observeEvent(input$action_sas,{
      saveSas(code = as.numeric(input$code,file.name = input$name))
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    
    observeEvent(input$action_stata,{
      saveStata(code = as.numeric(input$code),file.name = paste0(input$name,".dta"))
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    
    observeEvent(input$action_spss,{
      saveSpss(code = input$code,file.name = input$name)
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
   
   
    observeEvent(input$done, {
      invisible(stopApp())
    })
    
    
    
  }
  
  
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
=======
#' BETS search 
#' 
#' An interface for searching time series with possibility 
#' to extract the data in different extensions.
#' 
#' @import miniUI
#' @import rstudioapi
#' @import shiny
#' @importFrom utils write.csv write.csv2
#' 
#' @export





BETS.addin_en  = function(){
  
  ui<- miniUI::miniPage(
    gadgetTitleBar("BETS Search Addin"),
    miniTabstripPanel(
      
      # Tab 1 - choose any colourweb.
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          
          fluidRow(
            column(4,
                   textInput("description", 
                             "Description:", c("Search")
                   ) 
            ),
            
            
            column(2,
                   selectInput("periodicity",
                               "Periodicity:",
                               c("All","Monthly","Anual","Quarterly","Weekly","Daily")
                               
                   )       
            ),
            column(3,
                   textInput("source",
                             "Source:",
                             c("All")
                   )       
            ),
            # Create a new row for the table.
            
            DT::dataTableOutput("table")
            
            
          )
        )
      ),
      
      # Tab 2 - choose an R colour similar to a colour you choose
      miniTabPanel(
        "Visualization",
        icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput("table2")
        )
      ),
      
      miniTabPanel(
        "Export",
        icon = icon("save"),
        miniContentPanel(
          fluidRow(
            column(2, 
                   textInput("code","TS code")
            ),
            column(3,
                   textInput("name","file name","Dados")
            ), 
            column(7,
                   textInput("local","Save file",c(getwd()))
            )
          ),
          
          br(),
          
          
          fluidRow(
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#3D8B37; color:#FFF;
                        border-color:#3D8B37; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_csv", "Excel - CSV",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#2E8B57; color:#FFF;
                        border-color:#2E8B57; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_csv2", "Excel - CSV2",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#3c97ac; color:#FFF;
                        border-color:#3c97ac; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_rds", "R - RDS",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            )
          ),
          
          br(),
          
          
          fluidRow(
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#1D5F6D; color:#FFF;
                        border-color:#1D5F6D; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_stata", "STATA",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#4782ba; color:#FFF;
                        border-color:#4782ba; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_sas", "SAS",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#e1004b; color:#FFF;
                        border-color:#e1004b; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_spss", "SPSS",style = "color:#FFF"),
                        div("Click to export", style = "font-size:75%; font-weight:normal"))
            )
          ),
          fluidRow(
            br(),
            span(verbatimTextOutput('done_export'))
            
          )
          
        )
      ) 
      
      
    )
  )  
  
  
  server <- function(input, output, session) {
    
    
    
    
    output$table <- DT::renderDataTable(DT::datatable({
      
      req(input$description) # tratamento para o input da descricao
      req(input$source)      # tratamento para o input da fonte
      
      
      
      #nomes = c("Code","Description","Unit","Periodicity","Start","Last Value","Source")
      
      data.addin <- BETS.search(description ="*",view=F)
      #names(data.addin) = nomes
      
      
      
      if(input$description != "Search"){
        print(input$description)
        req(input$description) # tratamento para o input da descricao
        data.addin <- BETS.search(description = input$description,view=F)
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F)
          
        }else{
          data.addin 
        }
        
      }else{
        data.addin <- BETS.search(description ="*",view=F)
      }
      
      
      if(input$periodicity!= "All"){
        req(input$periodicity)     
        
        if(input$periodicity == "Monthly"){
          data.addin <- data.addin[data.addin$periodicity == "M",]
          
          }
        if(input$periodicity == "Anual"){
          data.addin <- data.addin[data.addin$periodicity == "A",]
          
          }
        if(input$periodicity == "Quarterly"){
          data.addin <- data.addin[data.addin$periodicity == "Q",]
          
          }
        if(input$periodicity == "Weekly"){
          data.addin <- data.addin[data.addin$periodicity == "W",]
          
          }
        if(input$periodicity == "Daily"){
          data.addin <- data.addin[data.addin$periodicity == "D",]
          
          }
        
        
        # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      if(input$source!= "All"){
        req(input$source)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$source == input$source,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      print(data.addin)
      
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
      }else{
        data.addin 
      }
      
      #names(data.addin) = nomes
      data.addin  
      
      
      
    },options = list(pageLength = 5, dom = 'tip'),selection = 'single'))
    
    
    
    output$table2<- DT::renderDataTable(DT::datatable({
      
      req(input$description) # tratamento para o input da descricao
      req(input$source)      # tratamento para o input da fonte
      
      
      
      #nomes = c("Code","Description","Unit","Periodicity","Start","Last Value","Source")
      
      data.addin <- BETS.search(description ="*",view=F)
      #names(data.addin) = nomes
      
      
      
      if(input$description != "Search"){
        print(input$description)
        req(input$description) # tratamento para o input da descricao
        data.addin <- BETS.search(description = input$description,view=F)
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F)
          
        }else{
          data.addin 
        }
        
      }else{
        data.addin <- BETS.search(description ="*",view=F)
      }
      
      
      if(input$periodicity!= "All"){
        req(input$periodicity)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      if(input$source!= "All"){
        req(input$source)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$source == input$source,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F)
        }else{
          data.addin 
        }
      }
      
      
      
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F)
      }else{
        data.addin 
      }
      
      #names(data.addin) = nomes
      data.addin  
      
      
      
    },options = list(pageLength = 50),selection = 'single'))
    
    
    
    # observeEvent(input$table_cell_clicked,{
    #   output$code = renderPrint(input$table_cell_clicked$value)
    # 
    # })
    
    # output$code = req(renderPrint(input$table_cell_clicked$value))
    # code_ts = as.numeric(input$table_cell_clicked$value)
    
    
    
    
    
    
    # output$code = renderPrint(input$table2_cell_clicked$value)
    
    
    
    
    observeEvent(input$action_csv2,{
      dados = BETS.get(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      write.csv2(dados,file =paste0(local,".csv") )
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    observeEvent(input$action_csv,{
      dados = BETS.get(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      write.csv(dados,file =paste0(local,".csv") )
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    observeEvent(input$action_rds,{
      dados = BETS.get(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      saveRDS(dados,file = paste0(local,".rds"))
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    observeEvent(input$action_sas,{
      BETS.save.sas(code = as.numeric(input$code,file.name = input$name))
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    
    observeEvent(input$action_stata,{
      BETS.save.stata(code = as.numeric(input$code),file.name = paste0(input$name,".dta"))
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
    
    observeEvent(input$action_spss,{
      BETS.save.spss(code = input$code,file.name = input$name)
      output$done_export = renderPrint("The file was successfully exported!")
    })
    
   
   
    observeEvent(input$done, {
      invisible(stopApp())
    })
    
    
    
  }
  
  
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
>>>>>>> 71754a35b050d392363ebfd3f352018aae3558c9
}