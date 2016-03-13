library(dygraphs)
library(dplyr)
library(stringr)
library(tidyr)
library(xts)

shinyServer(function(input, output) {
        
        fee = reactive({
                
                load("csfee.rda")
                
                csfee = csfee %>% 
                        filter(usps == input$selectState) %>% 
                        filter(cpt %in% input$selectCPT) %>% 
                        tidyr::spread(cpt, fee) 
                
                fee = xts(x = csfee[, input$selectCPT], 
                          order.by = csfee[["yearmon"]])                
                
                names(fee) = stringr::str_c("CPT ", names(fee))
                
                return(fee)
        })
        
        output$dygraph <- renderDygraph({
                
                dygraph(data = fee(), 
                        main = str_c("Medicaid Birth Procedure Fee Schedules, ",
                                     input$selectState),
                        ylab = "Dollars") %>%
                        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
                        dyRangeSelector(dateWindow = c("2003-01-01", "2010-12-31")) %>% 
                        dyLegend(width = 800)
                
        })
        
})