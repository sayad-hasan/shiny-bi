

life_admin_title <- "Shiny Insurance BI"

life_admin_side=list(
    
    selectInput("selectinput", "Branch", choices = life_select_options, selected = "All"), #select company 
    sidebarMenu( id = "tabs",
      menuItem("Premium Summary", tabName = "premiumsummary", icon = icon("balance-scale-left"), selected = T),
      menuItem("E-Receipt Summary Trend", tabName = "e_receipt_summary", icon = icon("bacon")),
      menuItem("Policy Issue Trend", tabName = "homepage", icon = icon("bacon")),
      menuItem("Policyholders Summary Info", tabName = "policyholders_summary", icon = icon("balance-scale")),
      menuItem("Company-wise Comparison", tabName = "comparison", icon = icon("not-equal")),
      menuItem("Policy Status", tabName = "policysummary", icon = icon("calendar-check"))
      #menuItem("Advanced Analysis", tabName = "life_admin_advanced_analysis", icon = icon("chart-line"))
      #menuItem("Performance Comparison Report", tabName= "performance_comparison", icon = icon("table"))
      #menuItem("Map", tabName = "policy_map", icon = icon("map-marked"))
    )
    
  )

life_admin_main=list(
    #style
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styleLife.css")
    ),
    column(12,
      # column(12, h1("Life Insurance BI for IDRA", align = "center",
      #               style = "text-align:center; margin-top: 5px !important; padding: 5px 0!important; font-weight: bold!important")),
      column(12, uiOutput('companyname_all_tab')),
      column(12, tags$hr(style = "border: 1px solid #e0e0e0; margin-top: 10px; margin-bottom: 10px; text-align:center;"))
    ),
    tabItems(
      
      #premium summary dashboard
      tabItem( 
        tabName = "premiumsummary",
        # h2("Premium Summary Report", align = "center"),
        fluidRow(
          # valueBoxOutput("companyname_p", width = 12),
          box(
            dateRangeInput("daterange_p", "Choose a date range","2021-01-01",today() ),
            width = 12),
          
          
          
          column(6, uiOutput("total_prem")), 
          column(6, uiOutput("fpr_prem")), 
          column(6, uiOutput("deferred_prem")), 
          column(6, uiOutput("renewal_prem")), 
          column(12, 
                 div(
                   style = "display: flex; height: 100px; padding: 10px; justify-content: center; align-items: center;",
                   downloadButton("premiumsummary_download", style = "padding: 10px", label = "Download Premium Summary Status Data")
                 )
          ),
          
          box(
            title = "Total Premium Collection Per Day", solidHeader = TRUE, background = "black",align='center',
            selectInput("linechart_p_ortype", "OR Type", choices = c("Total Premium", "First Premium","Deferred Premium", "Renewal Premium"), 
                        selected = "Total Premium"),
            highchartOutput("linechart_p"),
            width = 12),
          
          box(
            title = "Total Premium Collection Per Month", solidHeader = TRUE, background = "teal", align='center',
            width = 6,
            column(6, selectInput("selectyear_p", "Select a Year", choices = life_or_year_choices, selected = 2021)),
            column(6, selectInput("monthlychart_p_ortype", "OR Type", choices = c("Total Premium", "First Premium","Deferred Premium", "Renewal Premium"), 
                                  selected = "Total Premium")),
            highchartOutput("monthlychart_p")
          ),
          box(
            title = "Total Premium Collection Per Year", status = "primary", solidHeader = TRUE, background = "teal",
            width = 6,
            selectInput("yearlychart_p_ortype", "OR Type", choices = c("Total Premium", "First Premium","Deferred Premium", "Renewal Premium"), 
                        selected = "Total Premium"),   
            highchartOutput("yearlychart_p")
          )
          # ,
          # 
          # box(
          #   title = "Total Premium Collection Per Year", status = "success", solidHeader = TRUE,
          #   width = 6,
          #   sliderTextInput(
          #     inputId = "yearrange_p",
          #     label = "Choose a range:", 
          #     choices = c(2020:2025),    #choices = sort(unique(or$year_r))
          #     selected = c(2020,2025)
          #   ),
          #   plotlyOutput("yearlychart_p")
          # )
          
          # ,box(
          #   title = "SMS Length based on Total Premium Amount", status = "primary", solidHeader = TRUE,
          #   width = 12,
          #   plotlyOutput("sms_lengths")
          # )
          
        )),
      
      #E-Receipt Summary
      tabItem(
        tabName = "e_receipt_summary",
        # h2("E-Receipt Summary", align='center'),
        fluidRow(
          # valueBoxOutput("companyname_r", width = 12),
          box(
            dateRangeInput("daterange_r", "Choose a date range","2021-01-01",today() ),
            width = 12),
          
          column(6, uiOutput("total_e_receipt_issued")), 
          column(6, uiOutput("fpr_e_receipt_issued")), 
          column(6, uiOutput("deferred_e_receipt_issued")), 
          column(6, uiOutput("renewal_e_receipt_issued")), 
          
          column(12, 
                 div(
                   style = "display: flex; height: 100px; padding: 10px; justify-content: center; align-items: center;",
                   downloadButton("e_receipt_download", style = "padding: 10px", label = "Download E-Receipt Summary Data")
                 )
          ),
          
          
          box(
            title = "Total E-Receipt Issued Per Day", status = "primary", solidHeader = TRUE, background = "aqua",
            selectInput("linechart_r_ortype", "OR Type", choices = c("Total Premium", "First Premium","Deferred Premium", "Renewal Premium"), 
                        selected = "Total Premium"),
            highchartOutput("linechart_r"),
            width = 12),
          
          box(
            title = "Total E-Receipt Issued Per Month", status = "primary", solidHeader = TRUE,
            width = 6,
            column(6, selectInput("selectyear_r", "Select a Year", choices = life_or_year_choices, selected = 2021)), 
            column(6,  
                   selectInput("monthlychart_r_ortype", "OR Type", choices = c("Total Premium", "First Premium","Deferred Premium", "Renewal Premium"), 
                                   selected = "Total Premium")),
            
            highchartOutput("monthlychart_r")
          ),
          
          box(
            title = "Total E-Receipt Issued Per Year", status = "primary", solidHeader = TRUE,
            width = 6,  
            selectInput("yearlychart_r_ortype", "OR Type", choices = c("Total Premium", "First Premium","Deferred Premium", "Renewal Premium"), 
                        selected = "Total Premium"),
            highchartOutput("yearlychart_r")
          )
          
          # ,
          # 
          # box(
          #   title = "Total E-Receipt Issued Per Year", status = "success", solidHeader = TRUE,
          #   width = 6,
          #   sliderTextInput(
          #     inputId = "yearrange_r",
          #     label = "Choose a range:", 
          #     choices = c(2020:2025),
          #     selected = c(2020,2025)
          #   ),
          #   plotlyOutput("yearlychart_r")
          # )
          # 
          # box(
          #   title = "SMS Sending Status from UMP", status = "primary", solidHeader = TRUE,
          #   dateRangeInput("daterange_sms", "Choose a date range","2021-01-01",today()),
          #   plotlyOutput("smspiechart"),
          #   width = 12)
          
          
        )
      ),
      
      tabItem(
        tabName = "homepage",
        fluidRow(
          box(
            
            dateRangeInput("policydaterange","Choose a date range", start = "2021-01-01",end = today () ),
            valueBoxOutput("totalpolicyvalue", width = 12),
            width = 12),
          
          box(
            title = "Total Policies Issued Per Day", status = "primary", solidHeader = TRUE,
            
            dateRangeInput("daterange", "Choose a date range","2021-01-01", today () ),
            highchartOutput("linechart"),
            width = 12),
          
          box(
            title = "Total Policies Issued Per Month", status = "primary", solidHeader = TRUE,
            width = 6,
            selectInput("selectyear", "Select a Year", choices = life_policy_year_choices, selected = 2021),
            highchartOutput("monthlychart")
          ),
          
          box(
            title = "Total Policies Issued Per Year", status = "success", solidHeader = TRUE,
            width = 6,
            sliderTextInput(
              inputId = "yearrange",
              label = "Choose a range:", 
              choices = life_policy_year_choices,    #sort(unique(policy$year))
              selected = c(2015,2022)
            ),
            highchartOutput("yearlychart")
          ),
          
          box(
            title = "Risk date does not match commencement date", status = "primary", solidHeader = TRUE,
            plotlyOutput("nomatchpiechart"),
            width = 12
          )
        )),
      
      #Policy Holders Socio-Demographic Information
      tabItem(
        tabName = "policyholders_summary",
        fluidRow(
          
          box(
            
            title = "Policyholder's Age Distribution", status = "primary", solidHeader = TRUE, background = "aqua",
            highchartOutput("age_dist"),
            width = 12),
          
          box(
            title = "Distribution of Gender", status = "primary", solidHeader = TRUE,
            dateRangeInput("daterange_gender", "Choose a date range","2021-01-01", today()), 
            plotlyOutput("genderchart"),
            width = 12)
        )),
      
      
      
      #Interactive Company comparison
      tabItem(
        tabName = "comparison",
        # h2("Interactive Comparison", align='center'),
        fluidRow(
          box(
            background = "navy",
            width = 12,
            pickerInput(
              inputId = "linechart_company_picker",
              label = "Branches", 
              choices = life_company_picker,
              selected = life_company_picker[1:3],
              multiple = TRUE
            ),
            actionButton(
              inputId = "compare_button",
              label = "Compare", 
              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          ),
          box(
            title = "Total Policies Issued Per Day", status = "primary", solidHeader = TRUE,
            
            dateRangeInput("daterange_comparison", "Choose a date range","2021-01-01", today () ),
            
            plotlyOutput("linechart_comparison"),
            
            width = 12),
          
          box(
            title = "Total Policies Issued Per Month", status = "success", solidHeader = TRUE,
            width = 12,
            selectInput("selectyear_policy_comparison", "Select a Year", choices = c(2020:2022), selected = 2021),   #choices = sort(unique(policy$year), T),
            plotlyOutput("monthlychart_policy_comparison")
          ),
          
          box(
            title = "Total Premium Collection Per Day", status = "primary", solidHeader = TRUE,
            dateRangeInput("daterange_p_comparison", "Choose a date range","2021-01-01", today () ),
            plotlyOutput("linechart_p_comparison"),
            width = 12),
          
          box(
            title = "Total Premium Collection Per Month", status = "warning", solidHeader = TRUE,
            width = 12,
            selectInput("selectyear_premium_comparison", "Select a Year", choices = c(2020:2022), selected = 2021),  #choices = sort(unique(or$year_r), T)
            plotlyOutput("monthlychart_premium_comparison")
          )
          
        )
      ),
      
      
      tabItem(
        tabName = "policysummary",
        # h2("Policy Status At a Glance", align='center'),
        
        
        #number boxes in policy summary tab
        fluidRow(
          # valueBoxOutput("companyname", width = 12),
          
          column(6, uiOutput("totalpolicy") ),
          column(6, uiOutput("totalinforce")),
          column(6, uiOutput("totalmatured")),
          column(6, uiOutput("totalpaidup")),
          
          column(6, uiOutput("totalsurrender")),
          column(6, uiOutput("totaldeath")),
          column(6, uiOutput("totallapse")),
          column(6, 
                 div(
                   style = "display: flex; height: 100px; padding: 10px; justify-content: center; align-items: center;",
                   downloadButton("policysummary_download", style = "padding: 10px", label = "Download Policy Status Data")
                 )
                 ), 
          
          # valueBoxOutput("totalinforce", width = 3),
          # valueBoxOutput("totallapse", width = 3),
          # valueBoxOutput("totalpaidup", width = 3),
          # valueBoxOutput("totalpolicy", width = 3),
          # valueBoxOutput("totalsurrender", width = 4),
          # valueBoxOutput("totaldeath", width = 4),
          # valueBoxOutput("totalmatured", width = 4),
          
          
          # box(
          #   title = "Total Policy Issued by Each Insurance Company", status = "primary", solidHeader = TRUE,
          #   plotlyOutput("totalbycompany", height = 800),
          #   width = 6
          # ),
          
          box(
            title = "Policy Status by Each Branches", status = "primary", solidHeader = TRUE,
            plotlyOutput("policystatusbycompany"),
            width = 12
          ),
          
          box(
            title = "Policy Status by Each Branches in Percentage", status = "primary", solidHeader = TRUE,
            plotlyOutput("policystatusbycompanypercent"),
            width = 12),
          
          box(
            title = "Total Policy by Term", status = "primary", solidHeader = TRUE,
            textOutput("ploicyterm_companyname"),
            plotlyOutput("policytermplot"),
            width = 12
          )
        )
      )
      
      # tabItem(
      #   tabName = "life_admin_advanced_analysis",
      #   
      #   tabsetPanel(
      #     tabPanel("Trend",
      #              column(12,
      #                     selectInput("life_admin_trend_type_select", "Trend: ", 
      #                                 c("Policy", "Premium"))
      #              ),
      #              column(12,
      #                     plotlyOutput("life_admin_trend_plot",height = "600px")
      #              )
      #     ),
      #     tabPanel("Forecast",
      #              fluidRow(
      #                
      #                column(width = 6, selectInput("life_admin_select_period", "Period to forecast (Months)", c(6,12,18), 12)),
      #                column(width = 6, selectInput("life_admin_forecast_product_type_select", "Forecast: ", 
      #                                              c("Policy", "Premium")),
      #                       actionButton("life_admin_forecastButton","Run Forecast", class = "btn-warning")
      #                ),
      #                column(12, hr(style = "border-color: #cbcbcb; margin-top: 2px; margin-bottom: 2px;")),
      #                
      #                column(12,
      #                       plotlyOutput("life_admin_forecast_plot",height = "600px") %>% shinycssloaders::withSpinner()
      #                )
      #              )
      #     )
      #   )
      #   
      # )
      
      
      # tabItem(
      #   tabName = "performance_comparison",
      #   fluidRow(
      #     box(
      #       title = "Branchwise Performance", status = "primary", solidHeader = TRUE,
      #       DTOutput("branchdt"),
      #       width = 12),
      #     
      #     box(
      #       title = "Agentwise Performance", status = "primary", solidHeader = TRUE,
      #       DTOutput("agentdt"),
      #       width = 12)
      #   )),
      
      
      
      # tabItem(
      #   tabName = "policy_map",
      #   fluidRow(
      #     
      #     tmapOutput("policy_tmap_output", width = "100%", height = 800)
      #   ))
      
    ))
