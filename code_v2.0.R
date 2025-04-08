library(shiny)
library(readxl)
library(dplyr)
library(bs4Dash)
library(highcharter)
library(shinyWidgets)
library(DT)
library(tidyr)
library(ggplot2)
#library(ggwaterfall)
library(devtools)
library(plotly)
library(shinyBS)
library(rmarkdown)
library(rmarkdown)
library(shinyjs)



createKPIBox <- function(data, metric_name, box_color, output_metric ,icon ,divisor) {
  # Filter for the specified metric
  metric_data <- data %>%
    filter(name == metric_name) %>%
    select(Category, name, y)
  
  # Sum the metric value
  metric_value <- sum(metric_data$y, na.rm = TRUE)
  
  # Convert the metric value to billions
  if(divisor==1)
    {
      display_value <- round(metric_value / 1e9, 2)
      unit <- "Billion euros"
    }
  if(divisor==2)
  {
    display_value <- round(metric_value / 1e6, 3)
    unit <- "Million euros"
  }
  
  if(divisor==3)
  {
    display_value <- round(metric_value / 1e3, 3)
    unit <- "Thousands "
  }
  bs4ValueBox(
    value = tags$b(paste0(display_value, " ", unit)),
    subtitle = output_metric,
    icon = icon(icon),
    color = box_color,
    width = 12
  )
}


ui <- bs4DashPage(
  header = bs4DashNavbar(
    title = "Impact Estimate Dashboard",
    skin = "light",
    fixed = TRUE
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    fixed = TRUE,
    bs4SidebarMenu(
      id = "tabs",
      bs4SidebarMenuItem(
        text = tags$span("Welcome", style = "font-size: 12px;"),
        tabName = "welcome",
        icon = icon("home")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Ibuprofen containing molecules", style = "font-size: 12px;"),
        tabName = "ibuprofen_molecules",
        icon = icon("pills")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Percent use by indication", style = "font-size: 12px;"),
        tabName = "indication_percentage",
        icon = icon("chart-bar")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Percent use by ICD code", style = "font-size: 12px;"),
        tabName = "top_icd_codes",
        icon = icon("list-ol")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Sales of ibuprofen molecules", style = "font-size: 12px;"),
        tabName = "otc_sales",
        icon = icon("shopping-cart")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Assumptions input", style = "font-size: 12px;"),
        tabName = "input_form",
        icon = icon("shopping-cart")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Magnitude of exposure", style = "font-size: 12px;"),
        tabName = "burden_estimate",
        icon = icon("calculator")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Rx prescribed for indications", style = "font-size: 12px;"),
        tabName = "top_rx_molecules",
        icon = icon("prescription-bottle")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Socioeconomic cost", style = "font-size: 12px;"),
        tabName = "rx_cost_estimate",
        icon = icon("dollar-sign")
      ),
      bs4SidebarMenuItem(
        text = tags$span("Country level estimate", style = "font-size: 12px;"),
        tabName = "country_level_estimate",
        icon = icon("flag")
      )
      
    ), minified = TRUE,collapsed = TRUE
  ),
  body = bs4DashBody(
    tags$head(
      tags$style(id = "controlbarSkinStyle",HTML("
        .welcome-text {
          font-size: 3em;
          font-weight: bold;
          color: black;
          opacity: 0;
          animation: fadeIn 3s ease-in forwards;
          position: absolute;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          text-align: center;
        }
        @keyframes fadeIn {
          0% { opacity: 0; }
          100% { opacity: 1; }
        }
        .logo {
          position: fixed;
          top: 20px;
          right: 20px;
          width: 100px;
        }
        body {
          background-color: white;
        }
        .animated-box {
          animation: fadeIn 2s;
        }
        .animated-box1 {
          animation: fadeIn 2s;
        }
      ")),
      
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      tabItem(
        tabName = "welcome",
        tags$div(
          class = "welcome-text",
          "Welcome to Impact Estimate of Reverse Switching Ibuprofen",
          #style = "background-image: url('back.png'); background-size: cover;background-position: center; background-repeat: no-repeat; height: 50vh; width: 50vw; opacity: 0.25;"
        )
      ),
      
      tabItem(
        tabName = "ibuprofen_molecules",
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              status = "primary",
              selectInput("country_mol", "Country:", 
                          choices = c("Germany", "France", "Italy", "Poland"))
            )
          ),
          column(
            width = 9,
            box(
              width = NULL,
              highchartOutput("otc_mol_chart", height = "600px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "indication_percentage",
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              status = "primary",
              selectInput("country", "Country:", 
                          choices = c("Germany", "France", "Italy", "Poland"))
            )
          ),
          column(
            width = 9,
            box(
              width = NULL,
              highchartOutput("pivotChart", height = "600px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "top_icd_codes",
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              status = "primary",
              selectInput("country_diag", "Country:", choices = NULL),
              selectInput("category_diag", "Category:", choices = NULL)
            )
          ),
          column(
            width = 9,
            box(
              width = NULL,
              bs4ValueBoxOutput("kpiICDBox", width = 12),
              highchartOutput("topICDChart", height = "500px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "otc_sales",
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              status = "primary",
              selectInput("country3", "Country:", 
                          choices = c("Germany", "France", "Italy", "Poland"))
            )
          ),
          column(
            width = 9,
            box(
              width = NULL,
              highchartOutput("otcSalesChartMG1", height = "500px"),
              dataTableOutput("temp_table1")
              #highchartOutput("otcSalesChartPercentage", height = "300px")
            )
          )
        )
      ),
      tabItem(
        tabName = "input_form",
        tabItem(
          tabName = "common_cold",
          fluidRow(
            box(
              title = "Common Cold",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(3, numericInput("person1_1", "Person1 (%)", value = 78, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person2_1", "Person2 (%)", value = 12.5, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person3_1", "Person3 (%)", value = 7.5, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person4_1", "Person4 (%)", value = 2.5, min = 0, max = 100, step = 0.1)),
                column(3, selectInput("person1_dosage1", "Person1 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person2_dosage1", "Person2 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person3_dosage1", "Person3 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person4_dosage1", "Person4 Dosage (mg)", choices = seq(600, 2400, by = 600)))
              )
            )
          )
        ),
        tabItem(
          tabName = "headache_migraine",
          fluidRow(
            box(
              title = "Headache or Migraine",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(3, numericInput("person1_2", "Person1 (%)", value = 88, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person2_2", "Person2 (%)", value = 12, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person3_2", "Person3 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person4_2", "Person4 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, selectInput("person1_dosage2", "Person1 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person2_dosage2", "Person2 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person3_dosage2", "Person3 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person4_dosage2", "Person4 Dosage (mg)", choices = seq(600, 2400, by = 600)))
              )
            )
          )
        ),
        tabItem(
          tabName = "menstrual_pain",
          fluidRow(
            box(
              title = "Menstrual or Gynecological Pain",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(3, numericInput("person1_3", "Person1 (%)", value = 90, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person2_3", "Person2 (%)", value = 10, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person3_3", "Person3 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person4_3", "Person4 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, selectInput("person1_dosage3", "Person1 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person2_dosage3", "Person2 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person3_dosage3", "Person3 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person4_dosage3", "Person4 Dosage (mg)", choices = seq(600, 2400, by = 600)))
              )
            )
          )
        ),
        tabItem(
          tabName = "body_pain",
          fluidRow(
            box(
              title = "Body or Muscle Pain",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(3, numericInput("person1_4", "Person1 (%)", value = 77, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person2_4", "Person2 (%)", value = 23, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person3_4", "Person3 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person4_4", "Person4 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, selectInput("person1_dosage4", "Person1 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person2_dosage4", "Person2 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person3_dosage4", "Person3 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person4_dosage4", "Person4 Dosage (mg)", choices = seq(600, 2400, by = 600)))
              )
            )
          )
        ),
        tabItem(
          tabName = "dental_pain",
          fluidRow(
            box(
              title = "Mouth or Dental Pain",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(3, numericInput("person1_5", "Person1 (%)", value = 89, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person2_5", "Person2 (%)", value = 11, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person3_5", "Person3 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, numericInput("person4_5", "Person4 (%)", value = 0, min = 0, max = 100, step = 0.1)),
                column(3, selectInput("person1_dosage5", "Person1 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person2_dosage5", "Person2 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person3_dosage5", "Person3 Dosage (mg)", choices = seq(600, 2400, by = 600))),
                column(3, selectInput("person4_dosage5", "Person4 Dosage (mg)", choices = seq(600, 2400, by = 600)))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "burden_estimate",
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              status = "primary",
              selectInput("country4", "Country:", 
                          choices = c("Germany", "France", "Italy", "Poland")),
              
              
              div(class = "animated-box", valueBoxOutput("cautionNote", width = NULL))
            )
          ),
          column(
            width = 9,
            box(
              width = NULL,
              highchartOutput("otcSalesbuden_point_esitmete", height = "600px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "top_rx_molecules",
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              status = "primary",
              selectInput("country2", "Country:", choices = NULL),
              selectInput("category2", "Category:", choices = NULL)
            )
          ),
          column(
            width = 9,
            box(
              width = NULL,
              bs4ValueBoxOutput("kpiBox", width = 12),
              #uiOutput("kpiBox"),
              highchartOutput("topRxChart", height = "500px")
            )
          )
        )
      ),
      tabItem(
        tabName = "country_level_estimate",
        fluidRow(
          width = 12,
          box(
            width = 12,
            fluidRow(bs4ValueBoxOutput("patients_impacted", width = 6),
                     bs4ValueBoxOutput("Total_cost_KPI", width = 6)),
            fluidRow(
              #bs4ValueBoxOutput("Total_cost_KPI", width = 3),
              column(width =1 ,),
              bs4ValueBoxOutput("Total_clnic_visit_KPI", width = 2),
              bs4ValueBoxOutput("Total_RX_cost_KPI", width = 2),
              bs4ValueBoxOutput("Total_prod_loss_cost_KPI", width = 2),
              bs4ValueBoxOutput("Total_ER_cost", width = 2),
              bs4ValueBoxOutput("Total_transport_cost", width = 2),
              column(width =1 ,)
            )),
          box(
            width = 12,

            fluidRow(
              column(width = 6, highchartOutput("Patient_count_indications")),
              column(width = 6, highchartOutput("total_cost_per_condition"))
              
            )
          ),
          dataTableOutput("temp_table_2")
          
        )
      ),
      tabItem(
        tabName = "rx_cost_estimate",
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              fluidRow(
                column(6, selectInput("country5", "Country:", choices = NULL)),
                column(6, selectInput("Category5", "Category:", choices = NULL))
              ),
              div(class = "animated-box1", valueBoxOutput("cautionNote1", width = NULL)),
              bs4Accordion(
                id = "Collapsable_panel",
                bs4AccordionItem(
                  id = "patient_pop",
                  title = "Patient Population Chart",
                  highchartOutput("waterfall_patient_population", height = "500px")
                ),
                bs4AccordionItem(
                  id = "rx_cost",
                  title = "Total cost of consumed - ibuprofen Rx",
                  highchartOutput("waterfall_ibuprofen_cost_rx", height = "500px")
                ),
                bs4AccordionItem(
                  id = "alt_cost",
                  title = "Total cost of consumed - ibuprofen alt",
                  highchartOutput("waterfall_ibuprofen_cost_alt", height = "500px")
                ),
                bs4AccordionItem(
                  id = "alt_cost_non_med",
                  title = "Total cost of consumed - ibuprofen alt non med",
                  highchartOutput("waterfall_ibuprofen_cost_alt_non_med", height = "500px")
                ),
                bs4AccordionItem(
                  id = "nothing_cost",
                  title = "Total cost of consumed - ibuprofen nothing",
                  highchartOutput("waterfall_ibuprofen_cost_not", height = "500px")
                ),
                bs4AccordionItem(
                  id = "visits",
                  title = "Visits",
                  highchartOutput("waterfall_patient_visits", height = "500px")
                ),
                bs4AccordionItem(
                  id = "visit_cost",
                  title = "Visit cost",
                  highchartOutput("waterfall_patient_visits_costs", height = "500px")
                ),
                bs4AccordionItem(
                  id = "rx_health_cost",
                  title = "Rx healthcare cost",
                  highchartOutput("waterfall_rx_health_care_cost", height = "500px")
                ),
                bs4AccordionItem(
                  id = "alt_health_cost",
                  title = "Total cost for Alt persona",
                  highchartOutput("waterfall_alt_health_care_cost", height = "500px")
                ),
                bs4AccordionItem(
                  id = "alt_health_non_med_cost",
                  title = "Total cost for Alt non med persona",
                  highchartOutput("waterfall_alt_non_med_health_care_cost", height = "500px")
                ),
                bs4AccordionItem(
                  id = "nothing_health_cost",
                  title = "Nothing healthcare cost",
                  highchartOutput("waterfall_not_health_care_cost", height = "500px")
                ),
                bs4AccordionItem(
                  id = "total_health_cost",
                  title = "Total healthcare cost",
                  highchartOutput("waterfall_total_health_care_cost", height = "500px")
                ),
                bs4AccordionItem(
                  id = "productivity",
                  title = "Productivity loss due to clinic visits",
                  highchartOutput("waterfall_prodcutivity_related_cost", height = "500px")
                ),
                bs4AccordionItem(
                  id = "rx_persona",
                  title = "Total cost for Rx persona",
                  highchartOutput("Total_cost_rx_persona", height = "500px")
                ),
                bs4AccordionItem(
                  id = "alt_persona",
                  title = "Total cost for Alt persona",
                  highchartOutput("Total_cost_alt_persona", height = "500px")
                ),
                bs4AccordionItem(
                  id = "alt__non_med_persona",
                  title = "Total cost for Alt non med persona",
                  highchartOutput("Total_cost_alt_non_med_persona", height = "500px")
                ),
                bs4AccordionItem(
                  id = "not_persona",
                  title = "Total cost for Not persona",
                  highchartOutput("Total_cost_not_persona", height = "500px")
                ),
                bs4AccordionItem(
                  id = "total_cost",
                  title = "Total cost",
                  highchartOutput("Total_cost", height = "500px")
                  #dataTableOutput("temp_table_2")
                )
              )
            )
          )
        )
      )
    )
  ),
  controlbar = bs4DashControlbar(
    id = "controlbar",
    skin = "dark",
    title = "Settings",
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = "Settings",
        tags$div(
          class = "form-check form-switch",
          checkboxInput(
            inputId = "navbarTheme",
            label = "Theme",
            value = TRUE
          )
        ),
        p("Additional settings can go here.")
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize theme with a default value
  theme <- reactiveVal("dark")
  
  # Update controlbar skin and theme based on navbarTheme
  observeEvent(input$navbarTheme, {
    theme_val <- ifelse(input$navbarTheme, "dark", "light")
    skin_class <- ifelse(theme_val == "light", 
                         "control-sidebar-light", 
                         "control-sidebar-dark")
    runjs(sprintf("
      $('#controlbar').removeClass('control-sidebar-light control-sidebar-dark');
      $('#controlbar').addClass('%s');
    ", skin_class))
    theme(theme_val)
  })
  
  # Update Highcharts theme based on theme()
  observe({
    if (theme() == "light") {
      runjs("Highcharts.setOptions({
        chart: {
          style: {
            color: '#000000'  // Black color for light theme
          }
        }
      });")
    } else {
      runjs("Highcharts.setOptions({
        chart: {
          style: {
            color: '#FFFFFF'  // White color for dark theme
          }
        }
      });")
    }
  })
  
  output$cautionNote <- renderValueBox({
    valueBox(
      "The magnitude of exposure & cost estimates are subject to change based on updates to natural history parameters.",
      subtitle = tags$b("Note:"),
      icon = icon("exclamation-triangle"),
      color = "warning"
    )
  })
  
  output$cautionNote1 <- renderValueBox({
    valueBox(
      "The magnitude of exposure & cost estimates are subject to change based on updates to natural history parameters.",
      subtitle = tags$b("Note:"),
      icon = icon("exclamation-triangle"),
      color = "warning"
    )
  })
  
  static_file_path <- 'www/mererged_midas_med.xlsx'
  sheet_name <- 'Sheet1'
  
  static_data <- read_excel(static_file_path, sheet = sheet_name)
  
  filteredData <- reactive({
    req(input$country)
    static_data %>% filter(country == input$country)
  })
  
  pivotData <- reactive({
    filtered <- filteredData() %>%
      group_by(Category) %>%
      summarise(total_prescriptions = sum(prescriptions_rx_year_2023_1, na.rm = TRUE))
    
    total_sum <- sum(filtered$total_prescriptions)
    filtered <- filtered %>%
      mutate(percentage = total_prescriptions / total_sum * 100) %>%
      arrange(desc(percentage)) %>%
      select(Category, percentage)
    
    filtered
  })
  
  output$pivotChart <- renderHighchart({
    data <- pivotData()
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Indication share") %>%
      hc_xAxis(categories = data$Category, title = list(text = "Category"), labels = list(style = list(color = 'black'))) %>%
      hc_yAxis(title = list(text = "Indication share")) %>%
      hc_plotOptions(
        series = list(
          colorByPoint = TRUE,
          colors = list(
            list(
              linearGradient = list(x1 = 0, x2 = 1, y1 = 0, y2 = 1),
              stops = list(
                list(0, '#CC1474'),  # Start color
                list(1, '#FF4949')   # End color
              )
            )
          )
        )
      ) %>%
      hc_series(list(
        name = "Percentage",
        data = data$percentage,
        dataLabels = list(enabled = TRUE, formatter = JS("
        function() {
          return this.y.toFixed(2) + '%';
        }
      "))
      ))
  })
  
  rx_results_path <- 'www/combined_Rx_results.xlsx'
  
  rx_data <- read_excel(rx_results_path, sheet = "Sheet1")
  
  observe({
    updateSelectInput(session, "country2", choices = unique(rx_data$country))
    updateSelectInput(session, "category2", choices = unique(rx_data$Category))
  })
  
  
  observe({
    updateSelectInput(session, "country5", choices = unique(rx_data$country))
    updateSelectInput(session, "Category5", choices = unique(rx_data$Category))
  })
  
  filteredRxData <- reactive({
    req(input$country2, input$category2)
    filtered <- rx_data %>%
      filter(country == input$country2 & Category == input$category2)
    total_sum <- sum(filtered$prescriptions_rx_year_2023_1, na.rm = TRUE)
    filtered <- filtered %>%
      group_by(mlist) %>%
      summarise(total_prescriptions = sum(prescriptions_rx_year_2023_1, na.rm = TRUE)) %>%
      mutate(percentage = total_prescriptions / total_sum * 100) %>%
      arrange(desc(percentage)) %>%
      head(10)
    filtered
  })
  
  output$topRxChart <- renderHighchart({
    data <- filteredRxData()
    
    # Calculate gradient colors
    color_scale <- colorRampPalette(c("#FF4949", "#CC1474"))
    colors <- color_scale(nrow(data))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Top 10 Rx molecules used for relevant indication") %>%
      hc_xAxis(categories = data$mlist, title = list(text = "Rx Molecule")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_series(list(
        name = "Percentage",
        data = data$percentage,
        dataLabels = list(enabled = TRUE, formatter = JS("
          function() {
            return this.y.toFixed(2) + '%';
          }
        "))
      )) %>%
      hc_plotOptions(series = list(
        colorByPoint = TRUE,
        colors = colors
      ))
  })
  
  output$kpiBox <- renderValueBox({
    data <- filteredRxData()
    total_percentage <- round(sum(data$percentage), 2)
    
    valueBox(
      paste0(total_percentage, "%"),
      subtitle = "Top molecules show",
      icon = icon("list"),
      color = "purple"
    )
  })
  
  filteredData2 <- reactive({
    req(input$country3)
    static_data %>% filter(country == input$country3)
  })
  
  pivotData2 <- reactive({
    filtered <- filteredData2() %>%
      group_by(Category) %>%
      summarise(total_prescriptions = sum(prescriptions_rx_year_2023_1, na.rm = TRUE))
    
    total_sum <- sum(filtered$total_prescriptions)
    filtered <- filtered %>%
      mutate(percentage = total_prescriptions / total_sum * 100) %>%
      arrange(desc(percentage)) %>%
      select(Category, percentage)
    
    filtered
  })
  
  burden_estimate_path <- 'www/burden_estimate_data_v1.xlsx'
  
  burden_data <- read_excel(burden_estimate_path, sheet = "Sheet1")
  
  filteredBurdenData <- reactive({
    req(input$country3)
    burden_summary <- burden_data %>%
      filter(grepl("ibuprofen", `Molecule Combination`, ignore.case = TRUE) & Country != "Romania" & Country == input$country3) %>%
      #mutate(Total_MG = strength * SU) %>%
      group_by(`calendar.year`) %>%
      summarize(total_mg = sum(Total_MG, na.rm = TRUE))
    
    pivot_data <- pivotData2()
    
    # Add year information to pivot data
    pivot_data <- pivot_data %>%
      mutate(year = rep(2019:2029, each = nrow(pivot_data) / length(2019:2029)))
    
    # Merge pivot data with burden summary
    pivot_data <- pivot_data %>%
      left_join(burden_summary, by = c("year" = "Calendar Year"))
    
    pivot_data
  })
  
  output$otcSalesChartMG1 <- renderHighchart({
    data <- filteredBurdenData() %>%
      mutate(MG_per_category = total_mg * percentage / 100000000000) %>%
      select(Category, MG_per_category)
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Total MG of ibuprofen in target markets") %>%
      hc_xAxis(categories = data$Category, title = list(text = "Category")) %>%
      hc_yAxis(title = list(text = "Total MG")) %>%
      hc_series(list(
        name = "Total MG",
        data = data$MG_per_category,
        dataLabels = list(enabled = TRUE, formatter = JS("
          function() {
            return this.y.toFixed(2) + ' Billion MGs';
          }
        "))
      ))
  })
  
  output$otcSalesChartPercentage <- renderHighchart({
    data <- filteredBurdenData() %>%
      select(Category, percentage)
    
    # Calculate gradient colors
    color_scale <- colorRampPalette(c("#FF4949", "#CC1474"))
    colors <- color_scale(nrow(data))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Percentage of ibuprofen in target markets") %>%
      hc_xAxis(categories = data$Category, title = list(text = "Category")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_series(list(
        name = "Percentage",
        data = data$percentage,
        dataLabels = list(enabled = TRUE, formatter = JS("
          function() {
            return this.y.toFixed(2) + '%';
          }
        "))
      )) %>%
      hc_plotOptions(series = list(
        colorByPoint = TRUE,
        colors = colors
      ))
  })
  
  
  filteredData3 <- reactive({
    req(input$country4)
    static_data %>% filter(country == input$country4)
  })
  
  pivotData3 <- reactive({
    filtered <- filteredData3() %>%
      group_by(Category) %>%
      summarise(total_prescriptions = sum(prescriptions_rx_year_2023_1, na.rm = TRUE))
    
    total_sum <- sum(filtered$total_prescriptions)
    filtered <- filtered %>%
      mutate(percentage = total_prescriptions / total_sum * 100) %>%
      arrange(desc(percentage)) %>%
      select(Category, percentage)
    
    filtered
  })
  
  filteredBurdenData3 <- reactive({
    req(input$country4)
    burden_summary <- burden_data %>%
      filter(grepl("ibuprofen", `Molecule Combination`, ignore.case = TRUE) & Country != "Romania" & Country == input$country4) %>%
      mutate(Total_MG = strength * SU)
    
    pivot_data <- pivotData3()
    pivot_data$total_mg <- sum(burden_summary$Total_MG, na.rm = TRUE)
    pivot_data
  })
  
  population_factor_path <- 'www/Persona_share.xlsx'
  
  pop_factor <- read_excel(population_factor_path, sheet = "Sheet1")
  
  output$otcSalesbuden_point_esitmete <- renderHighchart({
    x <- filteredBurdenData3() %>%
      mutate(MG_per_category = (total_mg * percentage)/100 ) %>%
      select(Category, MG_per_category)
    
    result <- x %>% left_join(pop_factor, by = "Category") %>%
      select(Category, MG_per_category, Factor_to_population) %>%
      mutate(Patients_per_category = (MG_per_category * Factor_to_population) / 1000000) %>%
      arrange(desc(Patients_per_category))
    
    # Calculate gradient colors
    color_scale <- colorRampPalette(c("#FF4949", "#CC1474"))
    colors <- color_scale(nrow(result))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Total patients on ibuprofen in target markets") %>%
      hc_xAxis(categories = result$Category, title = list(text = "Category")) %>%
      hc_yAxis(title = list(text = "Total patients")) %>%
      hc_series(list(
        name = "Total patients",
        data = result$Patients_per_category,
        dataLabels = list(enabled = TRUE, formatter = JS("
          function() {
            return this.y.toFixed(2) + ' Million patients';
          }
        "))
      )) %>%
      hc_plotOptions(series = list(
        colorByPoint = TRUE,
        colors = colors
      ))
  })
  
  
  filteredData3_1 <- reactive({
    req(input$country5)
    static_data %>% filter(country == input$country5)
  })
  
  pivotData3_1 <- reactive({
    filtered <- filteredData3_1() %>%
      group_by(Category) %>%
      summarise(total_prescriptions = sum(prescriptions_rx_year_2023_1, na.rm = TRUE))
    
    total_sum <- sum(filtered$total_prescriptions)
    filtered <- filtered %>%
      mutate(percentage = total_prescriptions / total_sum * 100) %>%
      arrange(desc(percentage)) %>%
      select(Category, percentage)
    
    filtered
  })
  
  
  filteredBurdenData3_1 <- reactive({
    req(input$country5)
    burden_summary <- burden_data %>%
      filter(grepl("ibuprofen", `Molecule Combination`, ignore.case = TRUE) & Country != "Romania" & Country == input$country5) %>%
      mutate(Total_MG = strength * SU)
    
    pivot_data <- pivotData3_1()
    pivot_data$total_mg <- sum(burden_summary$Total_MG, na.rm = TRUE)
    pivot_data
  })
  
  population_factor_path <- 'www/Persona_share.xlsx'
  
  pop_factor <- read_excel(population_factor_path, sheet = "Sheet1")
  
  population_factor_path_2 <- 'www/Survey_data.xlsx'
  
  pop_factor_2 <- read_excel(population_factor_path_2, sheet = "Sheet1")
  
  population_factor_path_3 <- 'www/Price_of_ibuprofen_per_mg.xlsx'
  
  pop_factor_3 <- read_excel(population_factor_path_3, sheet = "Sheet1")
  
  
  PPMG_Data <- reactive({
    req(input$country5)
    PPMG_raw_data <- pop_factor_3 %>%
      filter(Country != "Romania" & Country == input$country5) 
    
    pivot_data <- pivotData3_1()
    PPMG_value <- sum(PPMG_raw_data$PPMG, na.rm = TRUE)
    PPMG_value
    #PPMG_df <- data.frame(PPMG = PPMG_value) 
    #PPMG_df
  })
  
  
  
  population_factor_path_4 <- 'www/Prod_cost_others.xlsx'
  pop_factor_4 <- read_excel(population_factor_path_4, sheet = "Sheet1")
  
  
  Other_costs_bus <- reactive({
    req(input$country5)
    PPMG_raw_data <- pop_factor_4 %>%
      filter(Country != "Romania" & Country == input$country5) 
    
    
    PPMG_value <- sum(PPMG_raw_data$Bus_cost, na.rm = TRUE)
    PPMG_value
    #PPMG_df <- data.frame(PPMG = PPMG_value) 
    #PPMG_df
  })
  
  
  Other_costs_ER <- reactive({
    req(input$country5)
    PPMG_raw_data <- pop_factor_4 %>%
      filter(Country != "Romania" & Country == input$country5) 
    
    
    PPMG_value <- sum(PPMG_raw_data$ER_visit_cost, na.rm = TRUE)
    PPMG_value
    #PPMG_df <- data.frame(PPMG = PPMG_value) 
    #PPMG_df
  })
  
  Other_visits_ER <- reactive({
    req(input$country5)
    PPMG_raw_data <- pop_factor_4 %>%
      filter(Country != "Romania" & Country == input$country5) 
    
    
    PPMG_value <- sum(PPMG_raw_data$ER_visit_rate, na.rm = TRUE)
    PPMG_value
    #PPMG_df <- data.frame(PPMG = PPMG_value) 
    #PPMG_df
  })
  
  
  population_factor_path_oecd <- 'www/Clinic_visit_cost_OEXCD.xlsx'
  
  pop_factor_oecd <- read_excel(population_factor_path_oecd, sheet = "Sheet1")
  
  CLinic_visit_cost_data <- reactive({
    req(input$country5)
    PPMG_raw_data <- pop_factor_oecd %>%
      filter(Country != "Romania" & Country == input$country5) 
    
    pivot_data <- pivotData3_1()
    PPMG_value <- sum(PPMG_raw_data$clinic_visit_cost, na.rm = TRUE)
    PPMG_value
    #PPMG_df <- data.frame(PPMG = PPMG_value) 
    #PPMG_df
  })
  
  
  population_factor_path_prod_cost <- 'www/Prod_cost_oecd.xlsx'
  
  pop_factor_oecd_prod_cost <- read_excel(population_factor_path_prod_cost, sheet = "Sheet1")
  
  prod_cost_data <- reactive({
    req(input$country5)
    PPMG_raw_data <- pop_factor_oecd_prod_cost %>%
      filter(Country != "Romania" & Country == input$country5) 
    
    pivot_data <- pivotData3_1()
    PPMG_value <- sum(PPMG_raw_data$Prod_cost, na.rm = TRUE)
    PPMG_value
    #PPMG_df <- data.frame(PPMG = PPMG_value) 
    #PPMG_df
  })
  
  otcSalesbuden_point_esitmete_2 <- reactive({
    x <- filteredBurdenData3_1() %>%
      mutate(MG_per_category = (total_mg * percentage) / 100) %>%
      select(Category, MG_per_category)
    
    pop_factor_2 <- pop_factor_2 %>%
      filter(Country != "Romania" & Country == input$country5)
    
    result <- x %>%
      left_join(pop_factor, by = "Category") %>%
      select(Category, MG_per_category, Factor_to_population) %>%
      mutate(Patients_per_category = ceiling((MG_per_category * Factor_to_population) / 1)) %>%
      arrange(desc(Patients_per_category)) %>%
      select(Category, Patients_per_category) %>%
      left_join(pop_factor_2, by = "Category") %>%
      mutate(Patients_seeking_Rx = ceiling((Patients_per_category * Share_of_population_taking_Rx) / 1)) %>%
      mutate(Patients_seeking_alternatives = ceiling((Patients_per_category * Share_of_population_taking_alternatives) / 1)) %>%
      mutate(Patients_seeking_non_medical_alternatives = ceiling((Patients_per_category * Share_of_population_taking_non_medical_alternatives) / 1)) %>%
      mutate(Patients_seeking_nothing = ceiling((Patients_per_category * Share_of_population_taking_nothing) / 1)) %>%
      mutate(Patients_rx_persona1_mg = ceiling((Patients_seeking_Rx * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_rx_persona2_mg = ceiling((Patients_seeking_Rx * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_rx_persona3_mg = ceiling((Patients_seeking_Rx * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_rx_persona4_mg = ceiling((Patients_seeking_Rx * Person4 * Person4_dosage) / 1)) %>%
      mutate(Patients_alt_persona1_mg = ceiling((Patients_seeking_alternatives * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_alt_persona2_mg = ceiling((Patients_seeking_alternatives * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_alt_persona3_mg = ceiling((Patients_seeking_alternatives * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_alt_persona4_mg = ceiling((Patients_seeking_alternatives * Person4 * Person4_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona1_mg = ceiling((Patients_seeking_non_medical_alternatives * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona2_mg = ceiling((Patients_seeking_non_medical_alternatives * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona3_mg = ceiling((Patients_seeking_non_medical_alternatives * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona4_mg = ceiling((Patients_seeking_non_medical_alternatives * Person4 * Person4_dosage) / 1)) %>%
      mutate(Patients_not_persona1_mg = ceiling((Patients_seeking_nothing * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_not_persona2_mg = ceiling((Patients_seeking_nothing * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_not_persona3_mg = ceiling((Patients_seeking_nothing * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_not_persona4_mg = ceiling((Patients_seeking_nothing * Person4 * Person4_dosage) / 1)) %>%
      mutate(Total_mgs_rx = ceiling(Patients_rx_persona1_mg + Patients_rx_persona2_mg + Patients_rx_persona3_mg +Patients_rx_persona4_mg)) %>%
      mutate(Total_mgs_alt = ceiling(Patients_alt_persona1_mg + Patients_alt_persona2_mg + Patients_alt_persona3_mg +Patients_alt_persona4_mg)) %>%
      mutate(Total_mgs_alt_non_medical = ceiling(Patients_alt_non_medical_persona1_mg + Patients_alt_non_medical_persona2_mg + Patients_alt_non_medical_persona3_mg +Patients_alt_non_medical_persona4_mg)) %>%
      mutate(Total_mgs_not = ceiling(Patients_not_persona1_mg + Patients_not_persona2_mg + Patients_not_persona3_mg +Patients_not_persona4_mg)) %>%
      mutate(Clinic_visits_from_Rx_pop = ceiling(Patients_seeking_Rx * 1)) %>%
      mutate(Clinic_visits_from_Alt_pop = ceiling(Patients_seeking_alternatives * 1)) %>%
      mutate(Clinic_visits_from_Alt_non_med_pop = ceiling(Patients_seeking_non_medical_alternatives * 1)) %>%
      mutate(Clinic_visits_from_not_pop = ceiling(Patients_seeking_nothing * 1)) %>%
      select(Category, Share_of_population_taking_Rx, Patients_seeking_Rx, Patients_seeking_alternatives,Patients_seeking_non_medical_alternatives, Patients_seeking_nothing, Patients_per_category,
             Patients_rx_persona1_mg, Patients_rx_persona2_mg, Patients_rx_persona3_mg,Patients_rx_persona4_mg, Total_mgs_rx,
             Patients_alt_persona1_mg, Patients_alt_persona2_mg, Patients_alt_persona3_mg,Patients_alt_persona4_mg, Total_mgs_alt,
             Patients_not_persona1_mg, Patients_not_persona2_mg, Patients_not_persona3_mg,Patients_not_persona4_mg, Total_mgs_not,
             Patients_alt_non_medical_persona1_mg,Patients_alt_non_medical_persona2_mg,Patients_alt_non_medical_persona3_mg,Patients_alt_non_medical_persona4_mg,Total_mgs_alt_non_medical,
             Clinic_visits_from_Rx_pop, Clinic_visits_from_Alt_pop,Clinic_visits_from_Alt_non_med_pop, Clinic_visits_from_not_pop)
    
    result["PPMG"] <- PPMG_Data() 
    result["visit_cost"] <- CLinic_visit_cost_data()
    result["Prod_cost"] <- prod_cost_data()
    result["ER_visit_rate"] <- Other_visits_ER()
    result["ER_cost"] <- Other_costs_ER()
    result["BUS_cost"] <- Other_costs_bus()
    
    result <- result %>%
      mutate(Cost_rx_ibuprofen_persona_1 = ceiling((Patients_rx_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_rx_ibuprofen_persona_2 = ceiling((Patients_rx_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_rx_ibuprofen_persona_3 = ceiling((Patients_rx_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_rx_ibuprofen_persona_4 = ceiling((Patients_rx_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_rx_cost_ibuprofen = Cost_rx_ibuprofen_persona_1 + Cost_rx_ibuprofen_persona_2 + Cost_rx_ibuprofen_persona_3 +Cost_rx_ibuprofen_persona_4) %>%
      mutate(Cost_alt_ibuprofen_persona_1 = ceiling((Patients_alt_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_ibuprofen_persona_2 = ceiling((Patients_alt_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_ibuprofen_persona_3 = ceiling((Patients_alt_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_ibuprofen_persona_4 = ceiling((Patients_alt_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_alt_cost_ibuprofen = Cost_alt_ibuprofen_persona_1 + Cost_alt_ibuprofen_persona_2 + Cost_alt_ibuprofen_persona_3+Cost_alt_ibuprofen_persona_4) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_1 = ceiling((Patients_alt_non_medical_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_2 = ceiling((Patients_alt_non_medical_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_3 = ceiling((Patients_alt_non_medical_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_4 = ceiling((Patients_alt_non_medical_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_alt__non_med_cost_ibuprofen = Cost_alt_non_med_ibuprofen_persona_1 + Cost_alt_non_med_ibuprofen_persona_2 + Cost_alt_non_med_ibuprofen_persona_3+Cost_alt_non_med_ibuprofen_persona_4) %>%
      mutate(Cost_not_ibuprofen_persona_1 = ceiling((Patients_not_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_not_ibuprofen_persona_2 = ceiling((Patients_not_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_not_ibuprofen_persona_3 = ceiling((Patients_not_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_not_ibuprofen_persona_4 = ceiling((Patients_not_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_not_cost_ibuprofen = Cost_not_ibuprofen_persona_1 + Cost_not_ibuprofen_persona_2 + Cost_not_ibuprofen_persona_3+Cost_not_ibuprofen_persona_4) %>%
      mutate(Clinic_visit_cost_Rx = ceiling((Clinic_visits_from_Rx_pop * visit_cost) / 1)) %>%
      mutate(Clinic_visit_cost_alt = ceiling((Clinic_visits_from_Alt_pop * visit_cost) / 1)) %>%
      mutate(Clinic_visit_cost_alt_non_med = ceiling((Clinic_visits_from_Alt_non_med_pop * visit_cost) / 1)) %>%
      mutate(Clinic_visit_cost_nothing = ceiling((Clinic_visits_from_not_pop * visit_cost) / 1)) %>%
      mutate(Total_visits = Clinic_visits_from_Rx_pop + Clinic_visits_from_Alt_pop + Clinic_visits_from_Alt_non_med_pop+Clinic_visits_from_not_pop) %>%
      mutate(Total_visit_cost = Clinic_visit_cost_Rx + Clinic_visit_cost_alt +Clinic_visit_cost_alt_non_med+ Clinic_visit_cost_nothing) %>%
      mutate(Total_rx_health_care_cost = Clinic_visit_cost_Rx + Total_rx_cost_ibuprofen) %>%
      mutate(Total_alt_health_care_cost = Clinic_visit_cost_alt + Total_alt_cost_ibuprofen) %>%
      mutate(Total_alt_non_med_health_care_cost = Clinic_visit_cost_alt_non_med + Total_alt__non_med_cost_ibuprofen) %>%
      mutate(Total_not_health_care_cost = Clinic_visit_cost_nothing + Total_not_cost_ibuprofen) %>%
      mutate(Total_health_care_cost = Total_rx_health_care_cost + Total_alt_health_care_cost+Total_alt_non_med_health_care_cost + Total_not_health_care_cost) %>%
      mutate(Prod_loss_cost_Rx = ceiling((Clinic_visits_from_Rx_pop * Prod_cost) / 1)) %>%
      mutate(Prod_loss_cost_alt = ceiling((Clinic_visits_from_Alt_pop * Prod_cost * 1.5) / 1)) %>%
      mutate(Prod_loss_cost_alt_non_med = ceiling((Clinic_visits_from_Alt_non_med_pop * Prod_cost * 1.5) / 1)) %>%
      mutate(Prod_loss_cost_nothing = ceiling((Clinic_visits_from_not_pop * Prod_cost) / 1)) %>%
      mutate(Total_productivity_loss = Prod_loss_cost_Rx + Prod_loss_cost_alt+Prod_loss_cost_alt_non_med + Prod_loss_cost_nothing) %>%
      mutate(Total_Rx = Total_rx_health_care_cost + Prod_loss_cost_Rx) %>%
      mutate(Total_alt = Total_alt_health_care_cost + Prod_loss_cost_alt) %>%
      mutate(Total_alt_non_med = Total_alt_non_med_health_care_cost + Prod_loss_cost_alt_non_med) %>%
      mutate(Total_nothing = Total_not_health_care_cost + Prod_loss_cost_nothing) %>%
      mutate(clinic_er_cost = ceiling((Clinic_visits_from_Rx_pop * ER_visit_rate *ER_cost /100) / 1)) %>%
      mutate(clinic_transport_cost = ceiling((Total_visits * BUS_cost *2 ) / 1)) %>% 
      mutate(Total_cost = Total_Rx + Total_alt +Total_alt_non_med+ Total_nothing +clinic_er_cost+clinic_transport_cost )

    
    transposed_data <- result %>%
      filter(Category == input$Category5) %>%
      pivot_longer(cols = -Category, 
                   names_to = "Metric",
                   values_to = "Value")
    
    waterfall_data <- transposed_data %>%
      select(Metric, Value) %>%
      rename(name = Metric, y = Value) %>%
      mutate(y = as.numeric(y))
    
    waterfall_data <- as.data.frame(waterfall_data)
    waterfall_data
  })
  
  
  
  Updated_estimate <- reactive({
    x <- filteredBurdenData3_1() %>%
      mutate(MG_per_category = (total_mg * percentage) / 100) %>%
      select(Category, MG_per_category)
    
    pop_factor_2 <- pop_factor_2 %>%
      filter(Country != "Romania" & Country == input$country5)
    
    result <- x %>%
      left_join(pop_factor, by = "Category") %>%
      select(Category, MG_per_category, Factor_to_population) %>%
      mutate(Patients_per_category = ceiling((MG_per_category * Factor_to_population) / 1)) %>%
      arrange(desc(Patients_per_category)) %>%
      select(Category, Patients_per_category) %>%
      left_join(pop_factor_2, by = "Category") %>%
      mutate(Patients_seeking_Rx = ceiling((Patients_per_category * Share_of_population_taking_Rx) / 1)) %>%
      mutate(Patients_seeking_alternatives = ceiling((Patients_per_category * Share_of_population_taking_alternatives) / 1)) %>%
      mutate(Patients_seeking_non_medical_alternatives = ceiling((Patients_per_category * Share_of_population_taking_non_medical_alternatives) / 1)) %>%
      mutate(Patients_seeking_nothing = ceiling((Patients_per_category * Share_of_population_taking_nothing) / 1)) %>%
      mutate(Patients_rx_persona1_mg = ceiling((Patients_seeking_Rx * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_rx_persona2_mg = ceiling((Patients_seeking_Rx * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_rx_persona3_mg = ceiling((Patients_seeking_Rx * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_rx_persona4_mg = ceiling((Patients_seeking_Rx * Person4 * Person4_dosage) / 1)) %>%
      mutate(Patients_alt_persona1_mg = ceiling((Patients_seeking_alternatives * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_alt_persona2_mg = ceiling((Patients_seeking_alternatives * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_alt_persona3_mg = ceiling((Patients_seeking_alternatives * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_alt_persona4_mg = ceiling((Patients_seeking_alternatives * Person4 * Person4_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona1_mg = ceiling((Patients_seeking_non_medical_alternatives * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona2_mg = ceiling((Patients_seeking_non_medical_alternatives * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona3_mg = ceiling((Patients_seeking_non_medical_alternatives * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_alt_non_medical_persona4_mg = ceiling((Patients_seeking_non_medical_alternatives * Person4 * Person4_dosage) / 1)) %>%
      mutate(Patients_not_persona1_mg = ceiling((Patients_seeking_nothing * Person1 * Person1_dosage) / 1)) %>%
      mutate(Patients_not_persona2_mg = ceiling((Patients_seeking_nothing * Person2 * Person2_dosage) / 1)) %>%
      mutate(Patients_not_persona3_mg = ceiling((Patients_seeking_nothing * Person3 * Person3_dosage) / 1)) %>%
      mutate(Patients_not_persona4_mg = ceiling((Patients_seeking_nothing * Person4 * Person4_dosage) / 1)) %>%
      mutate(Total_mgs_rx = ceiling(Patients_rx_persona1_mg + Patients_rx_persona2_mg + Patients_rx_persona3_mg +Patients_rx_persona4_mg)) %>%
      mutate(Total_mgs_alt = ceiling(Patients_alt_persona1_mg + Patients_alt_persona2_mg + Patients_alt_persona3_mg +Patients_alt_persona4_mg)) %>%
      mutate(Total_mgs_alt_non_medical = ceiling(Patients_alt_non_medical_persona1_mg + Patients_alt_non_medical_persona2_mg + Patients_alt_non_medical_persona3_mg +Patients_alt_non_medical_persona4_mg)) %>%
      mutate(Total_mgs_not = ceiling(Patients_not_persona1_mg + Patients_not_persona2_mg + Patients_not_persona3_mg +Patients_not_persona4_mg)) %>%
      mutate(Clinic_visits_from_Rx_pop = ceiling(Patients_seeking_Rx * 1)) %>%
      mutate(Clinic_visits_from_Alt_pop = ceiling(Patients_seeking_alternatives * 1)) %>%
      mutate(Clinic_visits_from_Alt_non_med_pop = ceiling(Patients_seeking_non_medical_alternatives * 1)) %>%
      mutate(Clinic_visits_from_not_pop = ceiling(Patients_seeking_nothing * 1)) %>%
      select(Category, Share_of_population_taking_Rx, Patients_seeking_Rx, Patients_seeking_alternatives,Patients_seeking_non_medical_alternatives, Patients_seeking_nothing, Patients_per_category,
             Patients_rx_persona1_mg, Patients_rx_persona2_mg, Patients_rx_persona3_mg,Patients_rx_persona4_mg, Total_mgs_rx,
             Patients_alt_persona1_mg, Patients_alt_persona2_mg, Patients_alt_persona3_mg,Patients_alt_persona4_mg, Total_mgs_alt,
             Patients_not_persona1_mg, Patients_not_persona2_mg, Patients_not_persona3_mg,Patients_not_persona4_mg, Total_mgs_not,
             Patients_alt_non_medical_persona1_mg,Patients_alt_non_medical_persona2_mg,Patients_alt_non_medical_persona3_mg,Patients_alt_non_medical_persona4_mg,Total_mgs_alt_non_medical,
             Clinic_visits_from_Rx_pop, Clinic_visits_from_Alt_pop,Clinic_visits_from_Alt_non_med_pop, Clinic_visits_from_not_pop)
    
    result["PPMG"] <- PPMG_Data() 
    result["visit_cost"] <- CLinic_visit_cost_data()
    result["Prod_cost"] <- prod_cost_data()
    result["ER_visit_rate"] <- Other_visits_ER()
    result["ER_cost"] <- Other_costs_ER()
    result["BUS_cost"] <- Other_costs_bus()
    
    result <- result %>%
      mutate(Cost_rx_ibuprofen_persona_1 = ceiling((Patients_rx_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_rx_ibuprofen_persona_2 = ceiling((Patients_rx_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_rx_ibuprofen_persona_3 = ceiling((Patients_rx_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_rx_ibuprofen_persona_4 = ceiling((Patients_rx_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_rx_cost_ibuprofen = Cost_rx_ibuprofen_persona_1 + Cost_rx_ibuprofen_persona_2 + Cost_rx_ibuprofen_persona_3 +Cost_rx_ibuprofen_persona_4) %>%
      mutate(Cost_alt_ibuprofen_persona_1 = ceiling((Patients_alt_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_ibuprofen_persona_2 = ceiling((Patients_alt_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_ibuprofen_persona_3 = ceiling((Patients_alt_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_ibuprofen_persona_4 = ceiling((Patients_alt_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_alt_cost_ibuprofen = Cost_alt_ibuprofen_persona_1 + Cost_alt_ibuprofen_persona_2 + Cost_alt_ibuprofen_persona_3+Cost_alt_ibuprofen_persona_4) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_1 = ceiling((Patients_alt_non_medical_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_2 = ceiling((Patients_alt_non_medical_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_3 = ceiling((Patients_alt_non_medical_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_alt_non_med_ibuprofen_persona_4 = ceiling((Patients_alt_non_medical_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_alt__non_med_cost_ibuprofen = Cost_alt_non_med_ibuprofen_persona_1 + Cost_alt_non_med_ibuprofen_persona_2 + Cost_alt_non_med_ibuprofen_persona_3+Cost_alt_non_med_ibuprofen_persona_4) %>%
      mutate(Cost_not_ibuprofen_persona_1 = ceiling((Patients_not_persona1_mg * PPMG) / 1)) %>%
      mutate(Cost_not_ibuprofen_persona_2 = ceiling((Patients_not_persona2_mg * PPMG) / 1)) %>%
      mutate(Cost_not_ibuprofen_persona_3 = ceiling((Patients_not_persona3_mg * PPMG) / 1)) %>%
      mutate(Cost_not_ibuprofen_persona_4 = ceiling((Patients_not_persona4_mg * PPMG) / 1)) %>%
      mutate(Total_not_cost_ibuprofen = Cost_not_ibuprofen_persona_1 + Cost_not_ibuprofen_persona_2 + Cost_not_ibuprofen_persona_3+Cost_not_ibuprofen_persona_4) %>%
      mutate(Clinic_visit_cost_Rx = ceiling((Clinic_visits_from_Rx_pop * visit_cost) / 1)) %>%
      mutate(Clinic_visit_cost_alt = ceiling((Clinic_visits_from_Alt_pop * visit_cost) / 1)) %>%
      mutate(Clinic_visit_cost_alt_non_med = ceiling((Clinic_visits_from_Alt_non_med_pop * visit_cost) / 1)) %>%
      mutate(Clinic_visit_cost_nothing = ceiling((Clinic_visits_from_not_pop * visit_cost) / 1)) %>%
      mutate(Total_visits = Clinic_visits_from_Rx_pop + Clinic_visits_from_Alt_pop + Clinic_visits_from_Alt_non_med_pop+Clinic_visits_from_not_pop) %>%
      mutate(Total_visit_cost = Clinic_visit_cost_Rx + Clinic_visit_cost_alt +Clinic_visit_cost_alt_non_med+ Clinic_visit_cost_nothing) %>%
      mutate(Total_rx_health_care_cost = Clinic_visit_cost_Rx + Total_rx_cost_ibuprofen) %>%
      mutate(Total_alt_health_care_cost = Clinic_visit_cost_alt + Total_alt_cost_ibuprofen) %>%
      mutate(Total_alt_non_med_health_care_cost = Clinic_visit_cost_alt_non_med + Total_alt__non_med_cost_ibuprofen) %>%
      mutate(Total_not_health_care_cost = Clinic_visit_cost_nothing + Total_not_cost_ibuprofen) %>%
      mutate(Total_health_care_cost = Total_rx_health_care_cost + Total_alt_health_care_cost+Total_alt_non_med_health_care_cost + Total_not_health_care_cost) %>%
      mutate(Prod_loss_cost_Rx = ceiling((Clinic_visits_from_Rx_pop * Prod_cost) / 1)) %>%
      mutate(Prod_loss_cost_alt = ceiling((Clinic_visits_from_Alt_pop * Prod_cost * 1.5) / 1)) %>%
      mutate(Prod_loss_cost_alt_non_med = ceiling((Clinic_visits_from_Alt_non_med_pop * Prod_cost * 1.5) / 1)) %>%
      mutate(Prod_loss_cost_nothing = ceiling((Clinic_visits_from_not_pop * Prod_cost) / 1)) %>%
      mutate(Total_productivity_loss = Prod_loss_cost_Rx + Prod_loss_cost_alt+Prod_loss_cost_alt_non_med + Prod_loss_cost_nothing) %>%
      mutate(Total_Rx = Total_rx_health_care_cost + Prod_loss_cost_Rx) %>%
      mutate(Total_alt = Total_alt_health_care_cost + Prod_loss_cost_alt) %>%
      mutate(Total_alt_non_med = Total_alt_non_med_health_care_cost + Prod_loss_cost_alt_non_med) %>%
      mutate(Total_nothing = Total_not_health_care_cost + Prod_loss_cost_nothing) %>%
      mutate(clinic_er_cost = ceiling((Clinic_visits_from_Rx_pop * ER_visit_rate *ER_cost /100) / 1)) %>%
      mutate(clinic_transport_cost = ceiling((Total_visits * BUS_cost *2 ) / 1)) %>% 
      mutate(Total_cost = Total_Rx + Total_alt +Total_alt_non_med+ Total_nothing +clinic_er_cost+clinic_transport_cost )
    
    transposed_data <- result %>%
      #filter(Category == input$Category5) %>%
      pivot_longer(cols = -Category, 
                   names_to = "Metric",
                   values_to = "Value")
    
    waterfall_data <- transposed_data %>%
      select(Category,Metric, Value) %>%
      rename(name = Metric, y = Value) %>%
      mutate(y = as.numeric(y))
    
    waterfall_data <- as.data.frame(waterfall_data)
    waterfall_data
  })
  
  output$temp_table1 <- renderDataTable({
    temp <- filteredBurdenData()
    
    
    datatable(temp, options = list(pageLength = 15, autoWidth = TRUE))
  })
  
  
  output$temp_table <- renderDataTable({
    temp <- otcSalesbuden_point_esitmete_2()
    
    
    datatable(temp, options = list(pageLength = 15, autoWidth = TRUE))
  })
  
  output$temp_table_2 <- renderDataTable({
    datatable(Updated_estimate(), options = list(pageLength = 15, autoWidth = TRUE))
  })
  
  output$Total_cost_KPI <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "Total_cost",
      box_color = "danger",
      output_metric = "Total Cost",
      icon = "euro-sign",
      divisor=1
    )
  })
  
  
  
  output$Total_clnic_visit_KPI <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "Total_visit_cost",
      box_color = "primary",
      output_metric = "Total clinic visit Cost" ,
      icon = "hospital",
      divisor=1
    )
  })

  output$Total_RX_cost_KPI <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "Total_rx_cost_ibuprofen",
      box_color = "primary",
      output_metric = "Total cost of ibuprofen" ,
      icon = "pills",
      divisor=2
    )
  })
  
  output$Total_prod_loss_cost_KPI <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "Total_productivity_loss",
      box_color = "primary",
      output_metric = "Total prodcutivitiy loss" ,
      icon = "person-walking",
      divisor=2
    )
  })
  
  
  output$Total_ER_cost <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "clinic_er_cost",
      box_color = "primary",
      output_metric = "Total ER visit cost" ,
      icon = "bed-pulse",
      divisor=2
    )
  })
  
  
  output$Total_transport_cost <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "clinic_transport_cost",
      box_color = "primary",
      output_metric = "Total Transport cost" ,
      icon = "bus",
      divisor=2
    )
  })
  
  output$patients_impacted <- renderbs4ValueBox({
    waterfall_data <- Updated_estimate()
    
    createKPIBox(
      data = waterfall_data,
      metric_name = "Patients_per_category",
      box_color = "info",
      output_metric = "Total # patients impacted" ,
      icon = "hospital-user",
      divisor=3
    )
  })
  
  
  
  output$Patient_count_indications <- renderHighchart({
    data <- Updated_estimate() %>%
      filter(name == "Patients_per_category" & Category != "Others")
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Patients per category") %>%
      hc_xAxis(categories = data$Category, title = list(text = "Category")) %>%
      hc_yAxis(
        title = list(text = "Patients per category"),
        labels = list(formatter = JS("
        function() {
          if (this.value >= 1000000000) {
            return (this.value / 1000000000).toFixed(1) + 'B';
          } else if (this.value >= 1000000) {
            return (this.value / 1000000).toFixed(1) + 'M';
          } else if (this.value >= 1000) {
            return (this.value / 1000).toFixed(1) + 'K';
          } else {
            return this.value;
          }
        }
      "))
      ) %>%
      hc_plotOptions(
        series = list(
          colorByPoint = TRUE,
          colors = list(
            list(
              linearGradient = list(x1 = 0, x2 = 1, y1 = 0, y2 = 1),
              stops = list(
                list(0, '#17A2B8'),  # Start color
                list(1, '#24C9E4')   # End color
              )
            )
          )
        )
      ) %>%
      hc_series(list(
        name = "Patients_per_category",
        data = data$y,
        dataLabels = list(enabled = TRUE, formatter = JS("
        function() {
          if (this.y >= 1000000000) {
            return (this.y / 1000000000).toFixed(1) + 'B';
          } else if (this.y >= 1000000) {
            return (this.y / 1000000).toFixed(1) + 'M';
          } else if (this.y >= 1000) {
            return (this.y / 1000).toFixed(1) + 'K';
          } else {
            return this.y;
          }
        }
      "))
      ))
  })
  
  
  output$total_cost_per_condition <- renderHighchart({
    data <- Updated_estimate() %>%
      filter(name == "Total_cost" & Category != "Others")
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Total cost per condition") %>%
      hc_xAxis(categories = data$Category, title = list(text = "Category")) %>%
      hc_yAxis(
        title = list(text = "Total cost per condition (€)"),
        labels = list(formatter = JS("
        function() {
          if (this.value >= 1000000000) {
            return '€' + (this.value / 1000000000).toFixed(1) + 'B';
          } else if (this.value >= 1000000) {
            return '€' + (this.value / 1000000).toFixed(1) + 'M';
          } else if (this.value >= 1000) {
            return '€' + (this.value / 1000).toFixed(1) + 'K';
          } else {
            return '€' + this.value;
          }
        }
      "))
      ) %>%
      hc_plotOptions(
        series = list(
          colorByPoint = TRUE,
          colors = list(
            list(
              linearGradient = list(x1 = 0, x2 = 1, y1 = 0, y2 = 1),
              stops = list(
                list(0, '#CC1474'),  # Start color
                list(1, '#FF4949')   # End color
              )
            )
          )
        )
      ) %>%
      hc_series(list(
        name = "Total_cost",
        data = data$y,
        dataLabels = list(enabled = TRUE, formatter = JS("
        function() {
          if (this.y >= 1000000000) {
            return '€' + (this.y / 1000000000).toFixed(1) + 'B';
          } else if (this.y >= 1000000) {
            return '€' + (this.y / 1000000).toFixed(1) + 'M';
          } else if (this.y >= 1000) {
            return '€' + (this.y / 1000).toFixed(1) + 'K';
          } else {
            return '€' + this.y;
          }
        }
      "))
      ))
  })
  
  output$waterfall_rx_health_care_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Clinic_visit_cost_Rx","Total_rx_cost_ibuprofen","Total_rx_health_care_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_rx_health_care_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_rx_health_care_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_alt_non_med_health_care_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Clinic_visit_cost_alt_non_med","Total_alt__non_med_cost_ibuprofen","Total_alt_non_med_health_care_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_alt_non_med_health_care_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_alt_non_med_health_care_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$Total_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Total_Rx","Total_alt","Total_alt_non_med","Total_nothing","Total_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$Total_cost_not_persona <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Total_not_cost_ibuprofen","Clinic_visit_cost_nothing","Prod_loss_cost_nothing","Total_nothing"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_nothing", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_nothing", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$Total_cost_alt_non_med_persona <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Total_alt__non_med_cost_ibuprofen","Clinic_visit_cost_alt_non_med","Prod_loss_cost_alt_non_med","Total_alt_non_med"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_alt_non_med", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_alt_non_med", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$Total_cost_alt_persona <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Total_alt_cost_ibuprofen","Clinic_visit_cost_alt","Prod_loss_cost_alt","Total_alt"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_alt", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_alt", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$Total_cost_rx_persona <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Total_rx_cost_ibuprofen","Clinic_visit_cost_Rx","Prod_loss_cost_Rx","Total_Rx"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_Rx", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_Rx", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$waterfall_prodcutivity_related_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Prod_loss_cost_Rx","Prod_loss_cost_alt","Prod_loss_cost_alt_non_med","Prod_loss_cost_nothing","Total_productivity_loss"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_productivity_loss", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_productivity_loss", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  output$waterfall_alt_health_care_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Clinic_visit_cost_alt","Total_alt_cost_ibuprofen","Total_alt_health_care_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_rx_health_care_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_rx_health_care_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_not_health_care_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Clinic_visit_cost_nothing","Total_not_cost_ibuprofen","Total_not_health_care_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_rx_health_care_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_rx_health_care_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_total_health_care_cost <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Total_health_care_cost","Total_rx_health_care_cost", "Total_alt_health_care_cost","Total_alt_non_med_health_care_cost","Total_not_health_care_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_health_care_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_health_care_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_patient_visits <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Clinic_visits_from_Rx_pop","Clinic_visits_from_Alt_pop","Clinic_visits_from_Alt_non_med_pop", "Clinic_visits_from_not_pop","Total_visits"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_visits", "#006400", "#FFA500"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_visits", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' K visits';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_patient_visits_costs <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Clinic_visit_cost_Rx","Clinic_visit_cost_alt","Clinic_visit_cost_alt_non_med", "Clinic_visit_cost_nothing","Total_visit_cost"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_visit_cost", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_visit_cost", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million euros';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_patient_population <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Patients_per_category","Patients_seeking_Rx", "Patients_seeking_alternatives","Patients_seeking_non_medical_alternatives", "Patients_seeking_nothing"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Patients_per_category", "#006400", "#FFA500"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Patients_per_category", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#006400",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' K patients';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_mgs_chart_rx <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Patients_rx_persona1_mg", "Patients_rx_persona2_mg", "Patients_rx_persona3_mg","Total_mgs_rx"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 100000000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_mgs_rx", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_mgs_rx", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Billion MGs';
              }
            ")
          )
        )
      )
  })
  
  output$waterfall_mgs_chart_alt <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Patients_alt_persona1_mg", "Patients_alt_persona2_mg", "Patients_alt_persona3_mg","Total_mgs_alt"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 100000000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_mgs_alt", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_mgs_alt", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking alternatives in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Billion MGs';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_mgs_chart_alt_non_med <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Patients_alt_non_medical_persona1_mg", "Patients_alt_non_medical_persona2_mg", "Patients_alt_non_medical_persona3_mg","Patients_alt_non_medical_persona4_mg","Total_mgs_alt"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 100000000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_mgs_alt", "#0000FF", "#FF0000"),# Set color for Total_mgs
                                                                isSum = ifelse(waterfall_data$name[i] == "Total_mgs_alt", TRUE, FALSE)  # Set isSum for Total_mgs  
    )
  })
  
  highchart() %>%
    hc_chart(type = "waterfall") %>%
    hc_title(text = "Total Rx cost for patients taking alternatives in the event of reverse switch") %>%
    hc_xAxis(type = "category", gridLineWidth = 0) %>%
    hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
    hc_series(
      list(
        data = highchart_data,
        upColor = "#00FF00",
        dataLabels = list(
          enabled = TRUE,
          align = "center",
          verticalAlign = "bottom",
          inside = FALSE,
          formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Billion MGs';
              }
            ")
        )
      )
    )
})
  
  output$waterfall_mgs_chart_not <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Patients_not_persona1_mg", "Patients_not_persona2_mg", "Patients_not_persona3_mg","Total_mgs_not"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 100000000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_mgs_not", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_mgs_not", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking nothing in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Billion MGs';
              }
            ")
          )
        )
      )
  })
  
  
  
  output$waterfall_ibuprofen_cost_rx <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Cost_rx_ibuprofen_persona_1", "Cost_rx_ibuprofen_persona_2", "Cost_rx_ibuprofen_persona_3","Cost_rx_ibuprofen_persona_4","Total_rx_cost_ibuprofen"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_rx_cost_ibuprofen", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_rx_cost_ibuprofen", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking Rx in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million USD';
              }
            ")
          )
        )
      )
  })
  
  
  
  output$waterfall_ibuprofen_cost_alt <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Cost_alt_ibuprofen_persona_1", "Cost_alt_ibuprofen_persona_2", "Cost_alt_ibuprofen_persona_3","Cost_alt_ibuprofen_persona_4","Total_alt_cost_ibuprofen"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_alt_cost_ibuprofen", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_alt_cost_ibuprofen", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking alternatives in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million USD';
              }
            ")
          )
        )
      )
  })
  
  
  output$waterfall_ibuprofen_cost_alt_non_med <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Cost_alt_non_med_ibuprofen_persona_1", "Cost_alt_non_med_ibuprofen_persona_2", "Cost_alt_non_med_ibuprofen_persona_3","Cost_alt_non_med_ibuprofen_persona_4","Total_alt__non_med_cost_ibuprofen"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_alt__non_med_cost_ibuprofen", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_alt__non_med_cost_ibuprofen", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking alternatives in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million USD';
              }
            ")
          )
        )
      )
  })
  
  output$waterfall_ibuprofen_cost_not <- renderHighchart({
    
    waterfall_data <- otcSalesbuden_point_esitmete_2()
    waterfall_data <- waterfall_data[waterfall_data$name %in% c("Cost_not_ibuprofen_persona_1", "Cost_not_ibuprofen_persona_2", "Cost_not_ibuprofen_persona_3","Cost_not_ibuprofen_persona_4","Total_not_cost_ibuprofen"), ]
    
    
    waterfall_data <- waterfall_data %>%
      mutate(y = y  / 1000000) %>%
      select(name, y)
    
    highchart_data <- lapply(1:nrow(waterfall_data), function(i) {
      list(
        name = waterfall_data$name[i], 
        y = waterfall_data$y[i],
        color = ifelse(waterfall_data$name[i] == "Total_not_cost_ibuprofen", "#0000FF", "#FF0000"),# Set color for Total_mgs
        isSum = ifelse(waterfall_data$name[i] == "Total_not_cost_ibuprofen", TRUE, FALSE)  # Set isSum for Total_mgs  
      )
    })
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_title(text = "Total Rx cost for patients taking nothing in the event of reverse switch") %>%
      hc_xAxis(type = "category", gridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "USD"), gridLineWidth = 0) %>%
      hc_series(
        list(
          data = highchart_data,
          upColor = "#00FF00",
          dataLabels = list(
            enabled = TRUE,
            align = "center",
            verticalAlign = "bottom",
            inside = FALSE,
            formatter = JS("
              function() {
                return this.y.toFixed(2) + ' Million USD';
              }
            ")
          )
        )
      )
  })
  
  
  ICD_data <- 'www/mererged_midas_med.xlsx'
  
  ICD_data <- read_excel(ICD_data, sheet = "Sheet1")
  
  observe({
    updateSelectInput(session, "country_diag", choices = unique(ICD_data$country))
    updateSelectInput(session, "category_diag", choices = unique(ICD_data$Category))
  })
  
  filteredICD_data <- reactive({
    req(input$country_diag, input$category_diag)
    filtered <- ICD_data %>%
      filter(country == input$country_diag & Category == input$category_diag)
    total_sum <- sum(filtered$prescriptions_rx_year_2023_1, na.rm = TRUE)
    filtered <- filtered %>%
      group_by(diag3) %>%
      summarise(total_prescriptions = sum(prescriptions_rx_year_2023_1, na.rm = TRUE)) %>%
      mutate(percentage = total_prescriptions / total_sum * 100) %>%
      arrange(desc(percentage)) %>%
      head(10)
    filtered
  })
  
  output$topICDChart <- renderHighchart({
    data <- filteredICD_data()
    
    # Calculate gradient colors
    color_scale <- colorRampPalette(c("#FF4949", "#CC1474"))
    colors <- color_scale(nrow(data))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Top 10 ICD used for selected indication") %>%
      hc_xAxis(categories = data$diag3, title = list(text = "ICD")) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_series(list(
        name = "Percentage",
        data = data$percentage,
        dataLabels = list(enabled = TRUE, formatter = JS("
          function() {
            return this.y.toFixed(2) + '%';
          }
        "))
      )) %>%
      hc_plotOptions(series = list(
        colorByPoint = TRUE,
        colors = colors
      ))
  })
  
  output$kpiICDBox <- renderbs4ValueBox({
    data <- filteredICD_data()
    total_percentage <- round(sum(data$percentage), 2)
    
    bs4ValueBox(
      value = tags$b(paste0(total_percentage, "%")),
      subtitle = "Top 10 ICDs",
      icon = icon("percent"),
      color = "purple",
      #status = "danger",  # This sets the color to red
      width = 12
    )
  })
  
  
  burden_estimate_path <- 'www/Burden_estimate.xlsx'
  
  burden_data <- read_excel(burden_estimate_path, sheet = "Sheet1")
  
  filteredBurdenData <- reactive({
    req(input$country3)
    burden_summary <- burden_data %>%
      filter(grepl("ibuprofen", `Molecule Combination`, ignore.case = TRUE) & Country != "Romania" & Country == input$country3) %>%
      mutate(Total_MG = strength * SU)
    
    pivot_data <- pivotData2()
    pivot_data$total_mg <- sum(burden_summary$Total_MG, na.rm = TRUE)
    pivot_data
  })
  
  output$otcSalesChartMG <- renderHighchart({
    data <- filteredBurdenData() %>%
      mutate(MG_per_category = total_mg * percentage / 100000000000) %>%
      select(Category, MG_per_category)
    
    # Calculate gradient colors
    color_scale <- colorRampPalette(c("#FF4949", "#CC1474"))
    colors <- color_scale(nrow(data))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Total MG of ibuprofen in target markets") %>%
      hc_xAxis(categories = data$Category, title = list(text = "Category")) %>%
      hc_yAxis(title = list(text = "Total MG")) %>%
      hc_series(list(
        name = "Total MG",
        data = data$MG_per_category,
        dataLabels = list(enabled = TRUE, formatter = JS("
          function() {
            return this.y.toFixed(2) + ' Billion MGs';
          }
        "))
      )) %>%
      hc_plotOptions(series = list(
        colorByPoint = TRUE,
        colors = colors
      ))
  })
  
  
  ibu_mol_chart_data <- 'www/Burden_estimate.xlsx'
  
  ibu_mol_chart_data <- read_excel(ibu_mol_chart_data, sheet = "Sheet1")
  
  filteredmolchartData <- reactive({
    req(input$country_mol)
    data <- ibu_mol_chart_data %>%
      filter(grepl("ibuprofen", `Molecule Combination`, ignore.case = TRUE) & Country != "Romania" & Country == input$country_mol) %>%
      mutate(Total_MG = strength * SU)
    
    total_sum_all <- sum(data$SU, na.rm = TRUE)
    data <- data %>%
      group_by(`Molecule Combination`) %>%
      summarise(Total_units = sum(SU, na.rm = TRUE)) %>%
      mutate(percentage = Total_units / total_sum_all * 100) %>%
      arrange(desc(percentage)) %>%
      head(10)
    data
    
    
  })
  
  

  
  output$otc_mol_chart <- renderHighchart({
    data <- filteredmolchartData() %>%
      select(`Molecule Combination`, percentage)
    
    highchart() %>%
      hc_chart(type = "bar", backgroundColor = "#FFFFFF") %>%
      hc_title(text = "Total molecules containing ibuprofen in target markets", style = list(color = "#000000")) %>%
      hc_xAxis(categories = data$`Molecule Combination`, title = list(text = "Molecule", style = list(color = "#000000")),
               labels = list(style = list(color = "#000000"))) %>%
      hc_yAxis(title = list(text = "Total MG", style = list(color = "#000000")),
               labels = list(style = list(color = "#000000"))) %>%
      hc_plotOptions(
        series = list(
          colorByPoint = TRUE,
          colors = list(
            list(
              linearGradient = list(x1 = 0, x2 = 1, y1 = 0, y2 = 1),
              stops = list(
                list(0, '#CC1474'),  # Start color
                list(1, '#FF4949')   # End color
              )
            )
          )
        )
      ) %>%
      hc_series(list(
        name = "volume in standard units",
        data = data$percentage,
        dataLabels = list(enabled = TRUE, formatter = JS("
      function() {
        return this.y.toFixed(3) + ' %';
      }"), style = list(color = "#FFFFFF"))
      ))
  })
  
  
  
  output$flowchart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "organization", inverted = TRUE, height = 400) %>%  # Set chart height
      hc_add_series(
        data = list(
          list(from = "Start", to = "Step 1"),
          list(from = "Step 1", to = "Step 2"),
          list(from = "Step 2", to = "Step 3"),
          list(from = "Step 3", to = "Step 4"),
          list(from = "Step 4", to = "Step 5")
        ),
        nodes = list(
          list(id = "Start", title = "Molecules sold", width = 400),
          list(id = "Step 1", title = "Share of indications", width = 400),
          list(id = "Step 2", title = "Top ICD codes", width = 400),
          list(id = "Step 3", title = "OTC ibuprofen sales", width = 400),
          list(id = "Step 4", title = "Burden estimate", width = 400),
          list(id = "Step 5", title = "Potential alternate molecules", width = 400)
        )
      ) %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          animation = list(duration = 1000),  # Add animation with duration
          point = list(
            events = list(
              click = JS("function() { Shiny.onInputChange('node_click', this.id); }")
            )
          ),
          dataLabels = list(
            overflow = "justify",  # Prevent text overflow
            style = list(fontSize = '12px', width = 300)  # Ensure width for data labels
          ),
          organization = list(
            nodeWidth = 300  # Set node width
          )
        )
      ) %>%
      hc_tooltip(
        enabled = TRUE,
        formatter = JS("function() { return this.point.title; }")
      )
  })
  
  
  
  observeEvent(input$node_click, {
    node_id <- input$node_click
    
    if (node_id == "Start") {
      updateTabsetPanel(session, "tabs", selected = "Ibuporofen Molecules")
    } else if (node_id == "Step 1") {
      updateTabsetPanel(session, "tabs", selected = "Indication % from MIDAS MED")
    } else if (node_id == "Step 2") {
      updateTabsetPanel(session, "tabs", selected = "Top ICD codes")
    } else if (node_id == "Step 3") {
      updateTabsetPanel(session, "tabs", selected = "OTC sales of ibuprofen")
    } else if (node_id == "Step 4") {
      updateTabsetPanel(session, "tabs", selected = "Burden  estimate")
    } else if (node_id == "Step 5") {
      updateTabsetPanel(session, "tabs", selected = "Top Rx molecules")
    }
  })
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)



