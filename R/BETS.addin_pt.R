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
#' 
#' @export





BETS.addin_pt  = function(){
  
  ui<- miniUI::miniPage(
    gadgetTitleBar("BETS Search Addin"),
    miniTabstripPanel(
      
      # Tab 1 - choose any colourweb.
      miniTabPanel(
        "Pesquisa",
        icon = icon("search"),
        miniContentPanel(
          
          fluidRow(
            column(4,
                   textInput("description", 
                             "Descri\u00E7\u00E3o:", c("Search")
                   ) 
            ),
            
            
            column(2,
                   selectInput("periodicity",
                               "Periodicidade:",
                               c("All","Mensal","Anual","Semanal","Trimestral","Di\u00E1ria")
                               
                   )       
            ),
            column(3,
                   textInput("source",
                             "Fonte:",
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
        "Visualiza\u00E7\u00E3o",
        icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput("table2")
        )
      ),
      
      miniTabPanel(
        "Exporta\u00E7\u00E3o",
        icon = icon("save"),
        miniContentPanel(
          fluidRow(
            column(2, 
                   textInput("code","C\u00F3digo da s\u00E9rie")
            ),
            column(3,
                   textInput("name","Nome do arquivo","Dados")
            ), 
            column(7,
                   textInput("local","Salvar no diret\u00F3rio",c(getwd()))
            )
          ),
          
          br(),
          
          
          fluidRow(
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#3D8B37; color:#FFF;
                        border-color:#3D8B37; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_csv", "Excel - CSV",style = "color:#FFF"),
                        div("Clique no formato para exportar", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#2E8B57; color:#FFF;
                        border-color:#2E8B57; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_csv2", "Excel - CSV2",style = "color:#FFF"),
                        div("Clique no formato para exportar", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#3c97ac; color:#FFF;
                        border-color:#3c97ac; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_rds", "R - RDS",style = "color:#FFF"),
                        div("Clique no formato para exportar", style = "font-size:75%; font-weight:normal"))
            )
          ),
          
          br(),
          
          
          fluidRow(
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#1D5F6D; color:#FFF;
                        border-color:#1D5F6D; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_stata", "STATA",style = "color:#FFF"),
                        div("Clique no formato para exportar", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#4782ba; color:#FFF;
                        border-color:#4782ba; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_sas", "SAS",style = "color:#FFF"),
                        div("Clique no formato para exportar", style = "font-size:75%; font-weight:normal"))
            ),
            column( 4,
                    div(style = "text-align: center; font-size: 120%; font-weight:bold; background-color:#e1004b; color:#FFF;
                        border-color:#e1004b; border-style:solid; border-width:1px; padding: 30px 0px 30px 0px",
                        actionLink("action_spss", "SPSS",style = "color:#FFF"),
                        div("Clique no formato para exportar", style = "font-size:75%; font-weight:normal"))
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
      
      
      
      #nomes = c("Codigo","Descricao","Unidade","Periodicidade","Inicio","Ultimo Valor","Fonte")
      
      data.addin <- BETS.search(description ="*",lang="pt",view=F)
      #names(data.addin) = nomes
      
      
      
      if(input$description != "Search"){
        req(input$description) # tratamento para o input da descricao
        data.addin <- BETS.search(description = input$description,lang="pt",view=F)
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",lang="pt",view=F)
          
        }else{
          data.addin 
        }
        
      }else{
        data.addin <- BETS.search(description ="*",lang="pt",view=F)
      }
      
      
      if(input$periodicity!= "All"){
        req(input$periodicity)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",lang="pt",view=F)
        }else{
          data.addin 
        }
      }
      
      if(input$source!= "All"){
        req(input$source)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$source == input$source,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",lang="pt",view=F)
        }else{
          data.addin 
        }
      }
      
      
      
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",lang="pt",view=F)
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
        req(input$description) # tratamento para o input da descricao
        data.addin <- BETS.search(description = input$description,view=F,lang="pt")
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F,lang="pt")
          
        }else{
          data.addin 
        }
        
      }else{
        data.addin <- BETS.search(description ="*",view=F,lang="pt")
      }
      
      
      if(input$periodicity!= "All"){
        req(input$periodicity)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F,lang="pt")
        }else{
          data.addin 
        }
      }
      
      if(input$source!= "All"){
        req(input$source)      # tratamento para o input da fonte
        #data.addin <- BETS.search(description = input$description,view=F)
        data.addin <- data.addin[data.addin$source == input$source,]
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F,lang="pt")
        }else{
          data.addin 
        }
      }
      
      
      
      if(is.character(data.addin)){
        data.addin = BETS.search(description="*",view=F,lang="pt")
      }else{
        data.addin 
      }
      
      #names(data.addin) = nomes
      data.addin  
      
      
      
    },options = list(pageLength = 50),selection = 'single'))
    
    
    
    
    
    observeEvent(input$action_csv2,{
      dados = BETS.get(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      write.csv2(dados,file =paste0(local,".csv") )
      output$done_export = renderPrint("O arquivo foi exportado com \u00EAxito!")
    })
    
    observeEvent(input$action_csv,{
      dados = BETS.get(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      write.csv(dados,file =paste0(local,".csv") )
      output$done_export = renderPrint("O arquivo foi exportado com \u00EAxito!")
    })
    
    observeEvent(input$action_rds,{
      dados = BETS.get(code = input$code,data.frame = T)
      local = paste0(input$local,"/",input$name)
      saveRDS(dados,file = paste0(local,".rds"))
      output$done_export = renderPrint("O arquivo foi exportado com \u00EAxito!")
    })
    
    observeEvent(input$action_sas,{
      BETS.save.sas(code = as.numeric(input$code,file.name = input$name))
      output$done_export = renderPrint("O arquivo foi exportado com \u00EAxito!")
    })
    
    
    observeEvent(input$action_stata,{
      BETS.save.stata(code = as.numeric(input$code),file.name = paste0(input$name,".dta"))
      output$done_export = renderPrint("O arquivo foi exportado com \u00EAxito!")
    })
    
    
    observeEvent(input$action_spss,{
      BETS.save.spss(code = input$code,file.name = input$name)
      output$done_export = renderPrint("O arquivo foi exportado com \u00EAxito!")
    })
    
    
    observeEvent(input$done, {
      invisible(stopApp())
    })
    
    
  }
  
  
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}