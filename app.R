source("funcs.R")
options(shiny.maxRequestSize=500*1024^2) 


#--------load data initial
downloaded_data()
da <<- load_data()
pr <<- sql_get(query="select * from bid_ask WHERE bid > 0 and ask > 0 order by time asc;")
pr
#_#----PARAMETERS

P1 <<- unique(da[,pair_start])
P2 <<- unique(da[,pair_end])
ALL_PAIRS <<- sort(unique(c(P1,P2)))
EX1<<- sort(unique(da[,ex1]))
EX2<<- sort(unique(da[,ex2]))
ALL_CUR <<- sort(unique(da[,cur_buy]))
cols_table <<- c("ex1","ex2","pair_start","pair_end","n_pairs","n_forex","forex_rate","logic_str","roi","roi_av","roi_std")
last_da_update <<-max(da[,time])


# renderObject, objectOutput


server = function(input, output, session) {
      
  
      #-------refresh data
      observeEvent(input$refresh_data, {js$refresh();}, once = T)
      
      #-------data prep
      arbis =  reactive({ 
          withProgress(message = 'Calculation in progress', detail = '...', value = 1, {
          
              #------ create da roi
              if (input$i_pairs_filter_buy == "exclude") f1 = "!pair_start %in% input$i_pairs_buy" else f1 = "pair_start %in% input$i_pairs_buy"
              if (input$i_pairs_filter_sell == "exclude") f2 = "!pair_end %in% input$i_pairs_sell" else f2 = "pair_end %in% input$i_pairs_sell"
              
              STRATEGY = 1
              if (input$i_strategy == "S3-FAST-COINS") STRATEGY = 3
              
              d_roi=da[   eval(parse(text=f1)) 
                      & eval(parse(text=f2)) &
                       ex1 %in% input$i_ex1 &
                       ex2 %in% input$i_ex2 &
                       n_pairs <= input$i_npairs &
                       n_forex <= input$i_nforex &
                       cur_buy %in% input$i_cur_buy &
                        cur_sell %in% input$i_cur_sell &
                        strategy == STRATEGY
                  ]
              if (input$i_evalroi == "latest ROI") setorder(d_roi,-roi,-roi_av)
              if (input$i_evalroi == "average ROI (median)") setorder(d_roi,-roi_av,-roi)
              #------ create table
              d_table = d_roi[timstamp == max(timstamp),..cols_table]
              print('......................table data')
              print(d_table %>% head(5))
              #------ best values
              best_logic = gsub("-"," ",d_table[1,logic_str])
              best_roi = d_table[1,roi]
              best_roi_av = d_table[1,roi_av]
              
              return(list(d_roi=d_roi,d_table=d_table,best_logic=best_logic,best_roi=best_roi,best_roi_av=best_roi_av))
      
          })   
      })
      
      prices= reactive({
        
        withProgress(message = 'Calculation in progress', detail = '...', value = 1, {
        
            pri=pr[pair %in% input$i_pricepair & time == pr[nrow(pr),time]  ]
            last_pri_time = pri[1,time]
            setorder(pri,-bid)
            bids=pri[,bid]
            bids = (1-(bids / bids[1])) %>% round(.,2) * -1
            bids[1]=pri[1,bid]
            pri[,bidP :=bids]
            setorder(pri,-ask)
            asks=pri[,ask]
            asks = (1-(asks / asks[1])) %>% round(.,2) * -1
            asks[1]=pri[1,ask]
            pri[,askP :=asks]
            col_names=pri[,exchange]
            pri=pri[,c("ask","bid","askP","bidP")]
            rownames(pri) = col_names
            g_pri=pr[pair %in% input$i_pricepair ]
            list(t_pri=pri,last_pri_time=last_pri_time,g_pri=g_pri)
        })
      })
      
      
      
      
      #-------output Graph ROI
      output$plot_roi <- renderPlotly({
    
            title = "ROI arbitrage logic"
            filt_logic = arbis()$d_table[1:input$i_graphmax,logic_str]
            pl = arbis()$d_roi[logic_str %in% filt_logic ] %>% as.data.frame
            plong=melt(pl[,c("time","logic_str","roi")],id.vars = c("logic_str","time"),value.name = "roi")
            plong=plong[with(plong, order(time)), ]
            #ggplot(dp, aes(x=date)) + geom_line(aes(y = value, colour = variable))
            print('......................graph data')
            print(plong %>% head(10))
            plot_ly(plong,x=~time,y=~roi, type='scatter',mode='lines',split=~logic_str) %>% 
                  layout(xaxis = list(title=""),legend = list(orientation = 'h',font = list(size = 12)))  #%>%  layout(title = title )
      
      }) 
      
      #-------output Graph Prices
      output$plot_price <- renderPlotly({

            title = "ASK prices of " %+% input$i_pricepair
            plot_ly(prices()$g_pri,x=~time,y=~ask, type='scatter',mode='lines',split=~exchange) %>%
              layout(legend = list(orientation = 'h'))  %>%  layout(title = title )

      }) 
      
      #-------output Table and logic
      output$table_roi = DT::renderDataTable({DT::datatable({arbis()$d_table}, options = list(searching = FALSE, pageLength = 5))})
      
      output$best_roi = renderText({
        a=arbis()
        out="<h4>Best strategy (table row 1) <span style='color:red'>" %+% as.character(a$best_roi * 100) %+% "%</span> [mean " %+% as.character(a$best_roi_av * 100) %+%  "%]<br><br>" %+% a$best_logic %+% "<h4>"
        print(out)
        })
     
       output$table_prices = DT::renderDataTable(DT::datatable({prices()$t_pri}, options = list(index=F,searching = FALSE, pageLength = 20)))
      
       output$table_prices_last = renderText({"<h4>Last crypto price on most expensive exchange vs. % difference to all other exchanges " %+% prices()$last_pri_time %>% as.character %+% " (UTC)" })
      
      #-------time
      output$last_da_update <- renderText({
        invalidateLater(5000, session) 
        ago=as.numeric((Sys.time() -last_da_update)) %>% abs %>% round(.,2)
        paste("<p>",ago," minutes ago","</p>")
        
      })
      
      #------variables
      output$pri_nrow = renderText({"<h4>" %+% length(unique(da[,logic_str])) %+%  " strategies sorted by ROI descending (use filters on left)</h4>"})
      
       
      #hide(id = "loading-content", anim = TRUE, animType = "fade") 
}


ui=fluidPage(
  shinyjs::useShinyjs(),
  extendShinyjs(text =  "shinyjs.refresh = function() { history.go(0); }"),
  tags$head(
  tags$style( HTML(".shiny-notification { height: 50px; width: 100%; position:fixed; top: 0; left: 0; } " ) ) ),
  includeCSS("custom.css"),
  div(id = "loading-content",h3(id="loading-p","loading...")),
  navbarPage("Crypto Arbitrage",
             
             
             tabPanel("Strategy Finder",id="tab_strategy",icon = icon("cubes",lib = "font-awesome"),
                
                  fluidRow(
                        
                        column(3,
                        wellPanel(tags$style(type="text/css", '#leftPanel {}'),id = "leftPanel",
                          
                              tags$head(tags$style(type="text/css", ".well { }")),
                              tags$label(class="boxhead","LAST DATA POINT:"),
                              fluidRow(
                                  column(6,htmlOutput("last_da_update")),
                                  column(6,HTML("<p>new data <br>approx. all 20 - 60 minutes, refresh will pull latest available"))
                              ),
                              actionButton(inputId = "refresh_data", label = "Refresh Data",class="btn btn-primary",icon = icon("refresh")),
                              tags$hr(),
                              tags$label(class="boxhead","FILTER PAIRS "),
                              fixedRow(
                                column(12,selectInput("i_strategy",  "Strategy", c("S1-FIAT","S3-FAST-COINS"),select="S1-FIAT", multiple = F, selectize = T))
                              ),
                              fixedRow(
                                column(4,selectInput("i_pairs_filter_buy", "Filter", c("include","exclude"),select="exclude", multiple = F, selectize = T)),
                                column(8,selectInput("i_pairs_buy","Pair for buy-in", ALL_PAIRS,select=NULL, multiple = T, selectize = T))
                              ),
                              fixedRow(
                                column(4,selectInput("i_pairs_filter_sell",  "Filter", c("include","exclude"),select="exclude", multiple = F, selectize = T)),
                                column(8,selectInput("i_pairs_sell","Pair for sell", ALL_PAIRS,select=NULL,multiple = T, selectize = T))
                              ),
                              fixedRow(
                                column(6,sliderInput("i_npairs", "# max pair conversions", min = 0, max = 3, value = 4, step = 1,pre = "#", sep = ",")),
                                column(6,sliderInput("i_nforex", "# max forex conversions", min = 0, max = 3, value = 3, step = 1,pre = "#", sep = ","))
                              ),
                              tags$hr(),
                              tags$label(class="boxhead","SELECT CURRENCIES"),
                              fixedRow(
                                column(6,selectInput("i_cur_buy", "Currencies for buy-in:", ALL_CUR, selected=ALL_CUR,multiple = T, selectize = T)),
                                column(6,selectInput("i_cur_sell","Currencies for sell:", ALL_CUR, selected=ALL_CUR,multiple = T, selectize = T))
                              ),
                              tags$hr(),
                              fixedRow(
                                column(6,selectInput("i_ex1", "Exchange 1", EX1, selected=EX1,multiple = T, selectize = F,size=length(EX1))),
                                column(6,selectInput("i_ex2","Exchange 2", EX2, selected=EX2,multiple = T, selectize = F,size=length(EX1)))
                              )
                              
                        )
                        )
                        
                        ,column(9,
            
                              tags$div(class = "well plot",
                                      HTML("<h4>Strategies ROI over time last hours (0.01 =  1%)</h4>"),
                                      fluidRow(
                                      column(6,selectInput("i_evalroi",  "Evaluate by: ", c("latest ROI","average ROI (median)"),select="roi", multiple = F, selectize = T)),
                                      column(6,sliderInput("i_graphmax", "Show n best strategies from table:", min = 1, max = 5, value = 2, step = 1,pre = "#", sep = ","))
                                      ),
                                      fluidRow(
                                      plotlyOutput("plot_roi") %>% withSpinner()
                                      )
                                       
                              ),
                                       
                              
                              tags$div(class = "well plot",
                                       column(6, 
                                              htmlOutput("pri_nrow"), 
                                              HTML("n_pairs = number different pairs in strategy <br> n_forex = number forex exchange <br> roi_av = median av. roi over time <br> roi = latest ROI ")
                                              
                                       ),
                                       column(6,
                                              wellPanel(tags$style(type="text/css", '#strat {background-color: #F0F0F0}'),id = "strat",htmlOutput("best_roi"))
                                       ),
                                       
                                      tags$hr(),
                                       DT::dataTableOutput("table_roi") %>% withSpinner(),
                                       HTML("<br>")
                                      
                                  
                             )
                             
                             ,tags$div(class = "well plot",
                                      htmlOutput("table_prices_last"),
                                      fluidRow(
                                      column(3,selectInput("i_pricepair","Select pair: ",ALL_PAIRS,select="btcusd", multiple = F, selectize = T)),column(9,"")
                                      ),
                                      fluidRow(
                                      column(3,DT::dataTableOutput("table_prices")),
      
                                      column(9,plotlyOutput("plot_price")) %>% withSpinner()
                                      )
                             )
                      
                        
                        )
                        
                  ) ,
                  
                  fluidRow(
                    
                        
                     column(12,
                            
                            
                            HTML("<iframe frameborder='no' scrolling='no' width='100%' height='800' src='https://docs.google.com/spreadsheets/d/e/2PACX-1vTLTCmdRgmzt-G4-MuHQ_0B9ci2FDrKoqq_724UoGNbL-U6iQDbokocex38Cp3hSBzFu6zRHBhQxwPI/pubhtml?gid=0&amp;single=true&amp;widget=true&amp;headers=false'></iframe>")
                    
                     )
                  )
                  
             )
             
  ),HTML("<script>setTimeout(function() {$('#loading-content').fadeOut(1000); }, 5000);</script>")
)


shinyApp(ui = ui, server = server)