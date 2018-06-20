#' @import magrittr
launcheRviewer <- function() {
    requireNamespace("DT")
    requireNamespace("shiny")
    requireNamespace("miniUI")
    requireNamespace("magrittr")
    
    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("launcheR viewer"),
        miniUI::miniTabstripPanel(
            miniUI::miniTabPanel("Running", icon = shiny::icon("play"),
                         miniUI::miniContentPanel(
                             shiny::div(style = "color:red; font-weight:bold", "Running batch(s)"),
                             DT::dataTableOutput("DT_wait_batch"),
                             shiny::hr(),shiny::p(),
                             shiny::div(style = "color:red; font-weight:bold", "Running queue(s)"),
                             DT::dataTableOutput("DT_wait_queue")
                         )
            ),
            miniUI::miniTabPanel("Historized", icon = shiny::icon("save"),
                         miniUI::miniContentPanel(
                             shiny::div(style = "color:blue; font-weight:bold", "Historized batch(s)"),
                             DT::dataTableOutput("DT_hist_batch"),
                             shiny::hr(),shiny::p(),
                             shiny::div(style = "color:blue; font-weight:bold", "Historized queue(s)"),
                             DT::dataTableOutput("DT_hist_queue")
                         )
            ),
            miniUI::miniTabPanel("Visualization", icon = shiny::icon("search"),
                         miniUI::miniContentPanel(
                             shiny::fluidRow(
                                 shiny::column(8,shiny::uiOutput("ui_Visu_SZI_queueid"))
                                 ,shiny::column(2, shiny::checkboxInput("Visu_CBI_xlabs", label = "show x labs", value = TRUE))
                                 ,shiny::column(2, shiny::checkboxInput("Visu_CBI_ylabs", label = "show y labs", value = TRUE))
                             )
                             ,shiny::fluidRow(
                                 shiny::uiOutput("ui_PL_vis")
                             )
                         )
            )
            
        )
    )
    
    
    # Server code for the gadget.
    server <- function(input, output, session) {
        
        # Timer every 2 sec
        timer <- shiny::reactiveTimer(2000)
        
        # All DT
        output$DT_wait_batch <- DT::renderDataTable({
            df <- shiny::isolate(getWaitBatch())
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
            df <- shiny::isolate(getWaitQueue())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', ordering = FALSE)) %>%
                DT::formatStyle("wait", target = "row", backgroundColor = DT::styleEqual(c(0:1000), c("lightgreen", rep("orange", 1000))))
        })
        proxy_WQ = DT::dataTableProxy("DT_wait_queue")
        
        output$DT_hist_batch <- DT::renderDataTable({
            df <- shiny::isolate(getHistorizedBatch())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', order = list(0,"desc")))
        })
        proxy_HB = DT::dataTableProxy("DT_hist_batch")
        
        output$DT_hist_queue <- DT::renderDataTable({
            df <- shiny::isolate(getHistorizedQueue())
            DT::datatable(df, rownames = FALSE, options = list(dom = 'tp', order = list(0,"desc")))
        })
        proxy_HQ = DT::dataTableProxy("DT_hist_queue")
        
        # Observe on timer to update all DT
        shiny::observeEvent(timer(), {
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
        
        #########
        ## Vis ##
        #########
        visu <- shiny::reactiveValues(queueid = NULL)
        
        shiny::observeEvent(timer(), {
            df <- getHistorizedQueue()
            visu$queueid <- unique(df$queueid)
        })
        
        shiny::observeEvent(visu$queueid, {
            already_selected <- input$Visu_SZI_queueid
            shiny::updateSelectizeInput(session, "Visu_SZI_queueid", choices = visu$queueid, selected = already_selected)
        })
        
        output$ui_Visu_SZI_queueid <- shiny::renderUI({
            if (is.null(visu$queueid)) return(shiny::div(style = "color:red; font-style:italic;", "No queue historized yet"))
            shiny::selectizeInput("Visu_SZI_queueid", label = "Queue id(s)", choices = shiny::isolate(visu$queueid), multiple = TRUE)
        })
        
        # Reactive containing the plot
        rv_vis <- shiny::eventReactive(c(input$Visu_SZI_queueid, input$Visu_CBI_xlabs, input$Visu_CBI_ylabs), {
            shiny::req(input$Visu_SZI_queueid)
            p <- vis(input$Visu_SZI_queueid, show_xlabs = input$Visu_CBI_xlabs, show_ylabs = input$Visu_CBI_ylabs)
            return(p)
        })
        
        # RenderPlot
        output$PL_vis <- shiny::renderPlot({
            shiny::req(rv_vis())
            rv_vis()
        })
        
        # RenderUI of the RenderPlot
        output$ui_PL_vis <- shiny::renderUI({
            if (is.null(rv_vis())) return(shiny::div(style = "color:red; font-style:italic;", "Plot not run"))
            shiny::plotOutput("PL_vis")
        })
        
        # Listen for 'done'.
        shiny::observeEvent(input$done, {
            invisible(shiny::stopApp())
        })
    }
    
    # Use a modal dialog as a viewr.
    viewer <- shiny::dialogViewer("launcheR", width = 1400, height = 800)
    shiny::runGadget(ui, server, viewer = viewer)
    
}
