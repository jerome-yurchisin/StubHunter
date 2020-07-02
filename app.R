library(shiny)
library(readr)
library(rjson)
library(DT)

options(width = 500)

chartD2020 = "//Dolphin/d/NEMS_DV/data/chartD/2020"
charts = list.files(chartD2020, pattern = "*.json", full.names = F)
stubs.l = list()

# Define UI for application that draws a histogram
ui <- fluidPage(
    h3("STUBS"),
    wellPanel(
        fluidRow(
            # column(6,
            #        fileInput("chart.pkg", "Select Chart Package:", width = '100%')),
            column(6,
                   fileInput("api.file", "Select api file:", accept = c('text'), width = '100%'))
        ),
        style = "padding: 15px;"),
    fluidRow(column(12, 
                    tabsetPanel(type = "tabs",
                                tabPanel("Missing Stub Table", DT::dataTableOutput("stub_tab")),
                                tabPanel("Chart Info")
                                )
                    )
            )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    missing.stubs = reactive({
        for (ch in charts) {
            json_data <- fromJSON(file = paste0(chartD2020,"/",ch))
            for (s in 1:length(json_data)) {
                titles = json_data[[s]]$title
                stubs = ifelse(is.null(json_data[[s]]$stubs),NA,json_data[[s]]$stubs)
                #n = length(stubs)
                stubs.l[[paste(ch,s)]] = data.frame(title = titles, stubs = stubs, chart = ch)
            }
        }
        stubs.df = do.call(rbind, stubs.l)
        stubs.df$chart = gsub("\\.json","",stubs.df$chart)
        row.names(stubs.df) = NULL
        
        api.file = input$api.file
        api = read_delim(api.file$datapath, delim = "|", escape_double = FALSE, trim_ws = TRUE)
        api.stubs = unique(api$VarName)
        missing.df = stubs.df[which(!stubs.df$stubs %in% api.stubs),]
        
        missing.stubs = setdiff(unique(missing.df$stubs), NA)
        
        x = as.data.frame(adist(api.stubs, missing.stubs, costs = c(1,1,2)))
        x.min = data.frame(stubs = missing.stubs, 
                           closest.match = unlist(apply(x, 2, function(x) {api.stubs[which.min(x)]})), 
                           match.value = unlist(apply(x, 2, min)))
        x.min = x.min[order(x.min$match.value),]
        missing.df = merge(missing.df, x.min, by = "stubs")
        return(missing.df)
    })

    output$stub_tab = DT::renderDataTable({
        if(!is.null(input$api.file)) {
            return(missing.stubs())
            } else return(NULL)
    }, options = list(paging = FALSE), rownames= FALSE) #, caption = input$api.file$name
}

# Run the application 
shinyApp(ui = ui, server = server)
