#' @import shiny
#' @import miniUI
#' @import magrittr
launcheRviewer <- function() {
    
    ui <- miniPage(
        gadgetTitleBar("launcheR viewer"),
        miniTabstripPanel(
            miniTabPanel("Running", icon = icon("play"),
                         miniContentPanel(
                             div(style = "color:red; font-weight:bold", "Running batch(s)"),
                             DT::dataTableOutput("DT_wait_batch"),
                             hr(),p(),
                             div(style = "color:red; font-weight:bold", "Running queue(s)"),
                             DT::dataTableOutput("DT_wait_queue")
                         )
            ),
            miniTabPanel("Historized", icon = icon("save"),
                         miniContentPanel(
                             div(style = "color:blue; font-weight:bold", "Historized batch(s)"),
                             DT::dataTableOutput("DT_hist_batch"),
                             hr(),p(),
                             div(style = "color:blue; font-weight:bold", "Historized queue(s)"),
                             DT::dataTableOutput("DT_hist_queue")
                         )
            )
        )
    )
    
    
    # Server code for the gadget.
    server <- function(input, output, session) {
        
        # Timer every 2 sec
        timer <- reactiveTimer(2000)
        
        # All DT
        output$DT_wait_batch <- DT::renderDataTable({
            df <- isolate(getWaitBatch())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', ordering = FALSE)) %>%
                DT::formatStyle("wait", target = "row", backgroundColor = DT::styleEqual(c(0:1000), c("lightgreen", rep("orange", 1000)))) %>%
                DT::formatStyle("progress",
                            background = DT::styleColorBar(c(1,100), 'lightblue'),
                            backgroundSize = '98% 88%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center'
                )
        })
        proxy_WB = DT::dataTableProxy("DT_wait_batch")
        
        output$DT_wait_queue <- DT::renderDataTable({
            df <- isolate(getWaitQueue())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', ordering = FALSE)) %>%
                DT::formatStyle("wait", target = "row", backgroundColor = DT::styleEqual(c(0:1000), c("lightgreen", rep("orange", 1000))))
        })
        proxy_WQ = DT::dataTableProxy("DT_wait_queue")
        
        output$DT_hist_batch <- DT::renderDataTable({
            df <- isolate(getHistorizedBatch())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', order = list(0,"desc")))
        })
        proxy_HB = DT::dataTableProxy("DT_hist_batch")
        
        output$DT_hist_queue <- DT::renderDataTable({
            df <- isolate(getHistorizedQueue())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', order = list(0,"desc")))
        })
        proxy_HQ = DT::dataTableProxy("DT_hist_queue")
        
        # Observe on timer to update all DT
        observeEvent(timer(), {
            # Wait batch
            df <- getWaitBatch()
            DT::dataTableAjax(session, df, rownames = FALSE, outputId = "DT_wait_batch")
            DT::reloadData(proxy_WB, resetPaging = FALSE, clearSelection = "none")
            # Wait queue
            df <- getWaitQueue()
            DT::dataTableAjax(session, df, rownames = FALSE, outputId = "DT_wait_queue")
            DT::reloadData(proxy_WQ, resetPaging = FALSE, clearSelection = "none")
            # Historized batch
            df <- getHistorizedBatch()
            DT::dataTableAjax(session, df, rownames = FALSE, outputId = "DT_hist_batch")
            DT::reloadData(proxy_HB, resetPaging = FALSE, clearSelection = "none")
            # Historized queue
            df <- getHistorizedQueue()
            DT::dataTableAjax(session, df, rownames = FALSE, outputId = "DT_hist_queue")
            DT::reloadData(proxy_HQ, resetPaging = FALSE, clearSelection = "none")
        })
        
        # Listen for 'done'.
        observeEvent(input$done, {
            invisible(stopApp())
        })
    }
    
    # Use a modal dialog as a viewr.
    viewer <- dialogViewer("launcheR", width = 1400, height = 800)
    runGadget(ui, server, viewer = viewer)
    
}
