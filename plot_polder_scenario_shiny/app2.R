library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(sobekio)
library(tidyverse)
library(data.table)
library(miniUI)
library(leaflet)
library(rstudioapi)
# test data

polders <- rhein_tbl[grepl('Polder_', besonderheit), besonderheit] %>%
    str_match('Polder_([^,;]*)')
polders <- polders[, 2]%>%
    str_replace('Polder_|_Innen|_Vor|_Nach|_Vol|_Einlass.*|_Auslass.*|_Ende|_Beginn',
                '') %>%
    unique()
# case_cmt <- read.table('c:/rhein.lit/caselist.cmt', sep = ' ',
#                        col.names = c('case_number', 'case_name'),
#                        stringsAsFactors = FALSE)
ui <- miniPage(
    gadgetTitleBar("Plotting polder hydrographs"),
    miniTabstripPanel(
        miniTabPanel(
            "Basic Parameters",
            icon = icon("sliders"),
            miniContentPanel(
                textInput("sobek_cmt", 'Path to sobek project: ',
                          "d:\\so21302\\rhein29a.lit", width = '100%')
                ,
                multiInput(
                    inputId = "case_list",
                    label = "Select cases: ",
                    choices = "Please select caselist.cmt first",
                    width = "100%"
                ),
                textAreaInput('case_desc', 'Case formatting: ', width = '100%'),
                selectInput('polder_name', 'Choose Polder Name',
                            choices = polders, width = '100%'),
                selectInput(
                    'parameter',
                    'Choose parameter: ',
                    choices = c('Waterlevel', 'Discharge'), width = '100%'
                ),
                actionButton("read_data", "Read data", width = '100%')
            )
        ),
        miniTabPanel("Graphic options", icon = icon("list-alt"),
                     checkboxGroupInput(
                         'plot_option',
                         'Choose plotting options: ',
                         choices = list(
                             'Einlass Ganglinie(n)' = 'q_in',
                             'Auslass Ganglinie(n)'= 'q_out',
                             # 'delta.measure',
                             # 'delta.pegel',
                             'Delta line'= 'delta_line',
                             'Sort delta parameters' = 'cmp_sort'
                         )
                     ),
                     numericInput('y2_scale', 'y2_scale', value = NA),
                     numericInput('y2_tick1', 'Value of first tick for y2-axis',
                                  value = NA),
                     actionButton("make_plot", "Create graphic...")
        ),
        miniTabPanel("Visualize", icon = icon("area-chart"),
                     miniContentPanel(
                         plotOutput("polder_plot", height = "100%")
                     )
        ),

        miniTabPanel("polder_data", icon = icon("table"),
                     miniContentPanel(
                         textOutput('plot_option'),
                         textOutput('plot_msg')
                     )
        )
    )
)

server <- function(input, output, session){
    #---- create reactive version of the parameters----
    # volumes  <-  getVolumes()
    sobek_cmt <- reactive(input$sobek_cmt)
    plot_option <- reactive(input$plot_option)
    # sobek_cmt_path <- reactive(input$sobek_cmt)
    case_desc <- reactive(input$case_desc)
    # update input$case_list if input file has changed
    # observe({
    #     if(input$sobek_cmt > 0){
    #         output$plot_option <- renderText(function(){
    #             list.files(choose.dir())})
    #     }
    #
    #
    # })
    observeEvent(input$sobek_cmt, {

        # shinyDirChoose(input, 'sobek_cmt', roots = c(home = '~'))
        cmt_path <- paste(sobek_cmt(), '/caselist.cmt', sep = '')
        if (file.exists(cmt_path)){
            cmt_case_list <- read.table(
                cmt_path,
                sep = ' ',
                col.names = c('case_number', 'case_name'),
                stringsAsFactors = FALSE
            )
            updateMultiInput(session, 'case_list',
                             choices = cmt_case_list$case_name)
        }
        # cmt_path
    }
    # ignoreInit = TRUE
    )
    # clickling the read_data button will trigger following parameters
    polder_name <- eventReactive(input$read_data, input$polder_name)
    case_list <- eventReactive(input$read_data, {
        input$case_list
    })
    parameter <- eventReactive(input$read_data, input$parameter)
    case_desc <- eventReactive(
        input$read_data,
        {
            str_trim(str_split(case_list(), ',')[[1]])
        }
        )
    #----init values----
    g_data <- reactiveValues(df = NULL,
                             case_tbl = NULL,
                             id_tbl = NULL,
                             polder_name = NULL,
                             case_list = NULL,
                             case_desc = NULL,
                             sobek_prj = so_prj,
                             master_tbl = rhein_tbl,
                             param = 'discharge',
                             q_in = TRUE,
                             q_out = TRUE,
                             # sum.q.in = FALSE,
                             # sum.q.out = FALSE,
                             w_canal = FALSE,
                             ref_mID = NULL,
                             y2_scale = NULL,
                             y2_tick1 = NULL,
                             h_lines = NULL,
                             peak_nday = NULL,
                             peak_pegel = FALSE,
                             delta_pegel = FALSE,
                             delta_measure = TRUE,
                             delta_line = FALSE,
                             rel_heights = c(2, 0.7),
                             compare_by = 'zustand',
                             cmp_sort = FALSE,
                             plot_title = NULL,
                             lt_name = 'Linienart',
                             color_name = 'Farbe',
                             # v.max = TRUE,
                             text_pos_x = 0,
                             text_pos_y = 1,
                             date_break = '3 days',
                             date_label = '%d.%m.%y',
                             text_size = 12,
                             text_x_angle = 0L,
                             polder_f = NULL,
                             polder_z = NULL,
                             verbose = TRUE
                             )
    #----observeEvent----
    observeEvent(input$read_data,{
        cmt_path <- paste(sobek_cmt(), '/caselist.cmt', sep = '')
        g_data$df <- get_polder_data(
            name = polder_name(),
            case.list = case_list(),
            param = parameter(),
            sobek.project =   sobek_cmt(),
            master.tbl = rhein_tbl
        )
        # g_data$case_tbl <- parse_case(case.desc = case_list(),
                                      # orig.name = case_list())
        # g_data$df <- merge(g_data$df, g_data$case_tbl, by = 'case')
    })
    observeEvent(input$make_plot,{
        if (!is.null(g_data$df)){
            p_options <- paste(plot_option(), collapse = ",")
            q_in <- grepl('q_in', p_options)
            q_out <- grepl('q_out', p_options)
            delta_line <- grepl('delta_line', p_options)
            cmp_sort <- grepl('cmp_sort', p_options)
            output$polder_plot <- renderPlot({
                # g <- ggplot(data = g_data$df,
                #        aes(x = ts, y = Nach,
                #            color = case,
                #            linetype = zustand
                #            )
                #        ) +
                #     scale_x_datetime() + geom_line()+
                #     theme_bw() + theme(legend.position = 'bottom')

                # if (grepl('delta_line', p_options)){
                #     g <- g + geom_line(aes(y = Einlass, color = 'Q_Einlass'))
                # }

                tryCatch(
                    g <- plot_shiny(
                        name = polder_name(),
                        id_data = g_data$df,
                        case.list = case_list(),
                        param = parameter(),
                        sobek.project = sobek_cmt(),
                        q.in = q_in,
                        q.out = q_out,
                        delta.line = delta_line,
                        master.tbl = rhein_tbl
                    ),
                    error = function(error_message) {
                        output$plot_msg <- renderText({
                            print(error_message)
                        })
                        g <- ggplot()
                        # g <- ggplot(data = data.frame(x = 1, y = 1),
                        #             aes(x = 1, y = 1)) +
                        #     annotate('text',
                        #              x = 1 ,
                        #              y = 1,
                        #              label = 'Error while plotting....')
                        g
                    }
                )
                g
            })
        } else{
            output$plot_msg <- renderText({
                paste('No data to plot...')
            }
            )
        }

    })
    output$plot_option <- renderText({
        paste(sobek_cmt(), collapse = ",")
        }
        )
}

shinyApp(ui, server)
