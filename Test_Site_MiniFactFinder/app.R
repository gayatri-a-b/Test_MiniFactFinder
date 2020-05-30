library(gt)
library(shiny)
library(janitor)
library(tidyverse)
library(formattable)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput(
                "eeo_file", 
                "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
            ),
            
            selectInput(
                "occupation", 
                "Occupation (empty until file selected)",
                c()
            ),
            
            selectInput(
                "geo", 
                "Geography (empty until file selected)",
                c()
            ),
            
            radioButtons(
                "est_mar", 
                "Table type",
                c("Estimate", "Margin of Error")
            ),
            
            radioButtons(
                "num_per", 
                "Data type",
                c("Number", "Percent")
            )
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            h4("Output Data"),
            
            tableOutput("final_output")
            
        )
        
    )
)


# Define server logic to read selected file

server <- function(input, output, session) {
    
    # returns out data table
    dt_extract <- function(){
        ################################################# READ FILE WITH DATA
        
        # read in csv file
        # set colnames to second row
        
        raw_data <- read.csv(input$eeo_file$datapath, colClasses = 'character', skip=1, check.names=FALSE)
        
        ################################################# CLEAN DATA
        
        x <- raw_data
        
        # pivot columns from wide to narrow
        
        x <- x %>%
            pivot_longer(
                # don't include these columns
                -c("Id", "Id2", "Geography", "Occupation Code"),
                
                # set the new pivoted cols to statistic_name
                names_to = "statistic_name",
                
                # set their pivoted value to count
                values_to = "count"
            )
        
        # clean up the names for x
        
        x <- x %>% 
            clean_names()
        
        return (x)
    }
    
    
    # Populate dropdowns based on input file
    
    observe({
        
        # input$eeo_file will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$eeo_file)
        
        
        # get our data table
        
        x <- dt_extract()
        
        # get the list of occupations and geographies
        
        occupations <- x %>% distinct(occupation_code) %>% pull(occupation_code) %>%  sort()
        geographies <- x %>% distinct(geography)
        geographies <- rbind("All", geographies)
        
        updateSelectInput(session, "occupation",
                          label = "Occupation",
                          choices = occupations,
                          selected = tail(occupations, 1)
        )
        
        updateSelectInput(session, "geo",
                          label = "Geography",
                          choices = geographies,
                          selected = head(geographies, 1)
        )
        
    })
    
    # Create output table
    
    output$final_output <- renderTable({
        req(input$eeo_file, input$occupation, input$est_mar, input$num_per, input$geo)
        
        # get our data table
        
        x <- dt_extract()
        
        ################################################# vARIABLES
        
        occupation <- input$occupation
        type <- input$est_mar
        num_type <- input$num_per
        geo <- input$geo
        
        ################################################# FILTER DATA BY INTAKE PARAMETERS
        
        # treat count as number
        # only obtain number (or percentage)
        
        x_num_type <- x %>% 
            mutate(count = as.numeric(count)) %>% # change type
            filter(
                grepl(num_type, statistic_name, fixed = TRUE)
            )
        
        # filter by occupation
        
        x_occupation <- x_num_type %>% 
            filter(
                occupation_code == occupation
            )
        
        # filter by geography
        
        if (geo == "All") {
            x_geography <- x_occupation
        } else{
            x_geography <- x_occupation %>%
                filter(
                    grepl(geo, geography, fixed = TRUE)
                )
        }
        
        # filter by estimate (or margin of error)
        
        x_type <- x_geography %>% 
            filter(
                grepl(type, statistic_name, fixed = TRUE)
            )
        
        
        ################################################################### DATAFRAMES FOR SPANNER VERTICAL COLS
        
        # Total, race and ethnicity column
        
        total_race_ethnicity <- x_type %>% 
            filter(
                grepl('Total, race and ethnicity', statistic_name, fixed = TRUE)
            )
        
        # Hispanic or Latino column
        
        hispanic_or_latino <- x_type %>% 
            filter(
                grepl('Hispanic or Latino', statistic_name, fixed = TRUE) &
                    !(grepl('Not Hispanic or Latino', statistic_name, fixed = TRUE)) &
                    !(grepl('Balance of not Hispanic or Latino', statistic_name, fixed = TRUE))
            )
        
        # Not Hispanic or Latino, one or more races column
        
        not_hispanic_or_latino_one_race <- x_type %>% 
            filter(
                grepl('Not Hispanic or Latino, one race', statistic_name, fixed = TRUE)
            )
        
        # Not Hispanic or Latino, two or more races column
        
        not_hispanic_or_latino_two_ore_more_races <- x_type %>% 
            filter(
                grepl('Not Hispanic or Latino, two or more races', statistic_name, fixed = TRUE)
            )
        
        
        ################################################################### DATAFRAMES FOR INDIVIDUAL VERT COLS (RACES)
        
        #### Hispanic or Latino
        
        # White alone Hispanic or Latino
        
        white_alone_hispanic_or_latino <- hispanic_or_latino %>% 
            filter(
                grepl('White alone Hispanic or Latino', statistic_name, fixed = TRUE)
            )
        
        # All other Hispanic or Latino
        
        all_other_hispanic_or_latino <- hispanic_or_latino %>% 
            filter(
                grepl('All other Hispanic or Latino', statistic_name, fixed = TRUE)
            )
        
        
        #### Not Hispanic or Latino, one or more races
        
        # White alone
        
        white_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('White alone', statistic_name, fixed = TRUE)
            )
        
        # Black or African American alone
        
        black_or_african_american_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('Black or African American alone', statistic_name, fixed = TRUE)
            )
        
        # American Indian and Alaska Native alone
        
        american_indian_and_alask_native_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('American Indian and Alaska Native alone', statistic_name, fixed = TRUE)
            )
        
        # Asian alone
        
        asian_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('Asian alone', statistic_name, fixed = TRUE)
            )
        
        # Native Hawaiian and Other Pacific Islander alone
        
        native_hawaiian_and_other_pacific_islander_alone <- not_hispanic_or_latino_one_race %>% 
            filter(
                grepl('Native Hawaiian and Other Pacific Islander alone', statistic_name, fixed = TRUE)
            )
        
        
        #### Not Hispanic or Latino, two or more races
        
        # White and Black
        
        white_and_black <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('White and Black', statistic_name, fixed = TRUE)
            )
        
        # White and AIAN
        
        white_and_aian <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('White and AIAN', statistic_name, fixed = TRUE)
            )
        
        # White and Asian
        
        white_and_asian <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('White and Asian', statistic_name, fixed = TRUE)
            )
        
        # Black and Black
        
        black_and_aian <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('Black and AIAN', statistic_name, fixed = TRUE)
            )
        
        # NHPI and White (Hawaii only)
        
        nhpi_and_white_Hawaii_only <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('NHPI and White (Hawaii only)', statistic_name, fixed = TRUE)
            )
        
        # NHPI and Asian (Hawaii only)
        
        nhpi_and_asian_Hawaii_only <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('NHPI and Asian (Hawaii only)', statistic_name, fixed = TRUE)
            )
        
        # NHPI and Asian and White (Hawaii only)
        
        nhpi_and_asian_and_white_Hawaii_only <- not_hispanic_or_latino_two_ore_more_races %>% 
            filter(
                grepl('NHPI and Asian and White (Hawaii only)', statistic_name, fixed = TRUE)
            )
        
        
        ################################################################### FUNCTIONS FOR GENDER HORIZONTAL ROWS
        
        # output Total, both sexes total
        
        getTotal <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl('Total, both sexes', statistic_name, fixed = TRUE)
                    ) %>% 
                    summarise(sum(count)) %>% 
                    pull()
            )
        }
        
        # output Female total
        
        getFemale <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl("Female", statistic_name, fixed = TRUE)
                    ) %>% 
                    summarise(sum(count)) %>% 
                    pull()
            )
        }
        
        # output Male total
        
        getMale <- function(dataframe) {
            return(
                dataframe %>% 
                    filter(
                        grepl('Male', statistic_name, fixed = TRUE)
                    ) %>% 
                    summarise(sum(count)) %>% 
                    pull()
            )
        }
        
        
        ################################################################### VALUES FOR EACH CELL
        
        #### Total, both sexes row
        
        total_both_sexes_total <- getTotal(total_race_ethnicity)
        total_both_sexes_hisp_white <- getTotal(white_alone_hispanic_or_latino)
        total_both_sexes_hisp_all_other <- getTotal(all_other_hispanic_or_latino)
        total_both_sexes_not_hisp_one_race_white <- getTotal(white_alone)
        total_both_sexes_not_hisp_one_race_black <- getTotal(black_or_african_american_alone)
        total_both_sexes_not_hisp_one_race_american_indian <- getTotal(american_indian_and_alask_native_alone)
        total_both_sexes_not_hisp_one_race_asian <- getTotal(asian_alone)
        total_both_sexes_not_hisp_one_race_nhpi <- getTotal(native_hawaiian_and_other_pacific_islander_alone)
        total_both_sexes_not_hisp_two_races_white_black <- getTotal(white_and_black)
        total_both_sexes_not_hisp_two_races_white_aian <- getTotal(white_and_aian)
        total_both_sexes_not_hisp_two_races_white_asian <- getTotal(white_and_asian)
        total_both_sexes_not_hisp_two_races_black_aian <- getTotal(black_and_aian)
        total_both_sexes_not_hisp_two_races_nhpi_white <- getTotal(nhpi_and_white_Hawaii_only)
        total_both_sexes_not_hisp_two_races_nhpi_asian <- getTotal(nhpi_and_asian_Hawaii_only)
        total_both_sexes_not_hisp_two_races_nhpi_asian_white <- getTotal(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Male row
        
        total_male_total <- getMale(total_race_ethnicity)
        total_male_hisp_white <- getMale(white_alone_hispanic_or_latino)
        total_male_hisp_all_other <- getMale(all_other_hispanic_or_latino)
        total_male_not_hisp_one_race_white <- getMale(white_alone)
        total_male_not_hisp_one_race_black <- getMale(black_or_african_american_alone)
        total_male_not_hisp_one_race_american_indian <- getMale(american_indian_and_alask_native_alone)
        total_male_not_hisp_one_race_asian <- getMale(asian_alone)
        total_male_not_hisp_one_race_nhpi <- getMale(native_hawaiian_and_other_pacific_islander_alone)
        total_male_not_hisp_two_races_white_black <- getMale(white_and_black)
        total_male_not_hisp_two_races_white_aian <- getMale(white_and_aian)
        total_male_not_hisp_two_races_white_asian <- getMale(white_and_asian)
        total_male_not_hisp_two_races_black_aian <- getMale(black_and_aian)
        total_male_not_hisp_two_races_nhpi_white <- getMale(nhpi_and_white_Hawaii_only)
        total_male_not_hisp_two_races_nhpi_asian <- getMale(nhpi_and_asian_Hawaii_only)
        total_male_not_hisp_two_races_nhpi_asian_white <- getMale(nhpi_and_asian_and_white_Hawaii_only)
        
        
        #### Female row
        
        total_female_total <- getFemale(total_race_ethnicity)
        total_female_hisp_white <- getFemale(white_alone_hispanic_or_latino)
        total_female_hisp_all_other <- getFemale(all_other_hispanic_or_latino)
        total_female_not_hisp_one_race_white <- getFemale(white_alone)
        total_female_not_hisp_one_race_black <- getFemale(black_or_african_american_alone)
        total_female_not_hisp_one_race_american_indian <- getFemale(american_indian_and_alask_native_alone)
        total_female_not_hisp_one_race_asian <- getFemale(asian_alone)
        total_female_not_hisp_one_race_nhpi <- getFemale(native_hawaiian_and_other_pacific_islander_alone)
        total_female_not_hisp_two_races_white_black <- getFemale(white_and_black)
        total_female_not_hisp_two_races_white_aian <- getFemale(white_and_aian)
        total_female_not_hisp_two_races_white_asian <- getFemale(white_and_asian)
        total_female_not_hisp_two_races_black_aian <- getFemale(black_and_aian)
        total_female_not_hisp_two_races_nhpi_white <- getFemale(nhpi_and_white_Hawaii_only)
        total_female_not_hisp_two_races_nhpi_asian <- getFemale(nhpi_and_asian_Hawaii_only)
        total_female_not_hisp_two_races_nhpi_asian_white <- getFemale(nhpi_and_asian_and_white_Hawaii_only)
        
        
        ################################################################### CREATE MATRIX USING VALUES
        
        table_x <- matrix(
            # column headers
            
            c(
                "Geography, Occupation, Subject",
                " ",
                "White alone\nHispanic or Latino",
                "All other\nHispanic or Latino",
                "White alone",
                "Black or African American alone",
                "American Indian and\nAlaska Native alone",
                "Asian alone",
                "Native Hawaiian\nand Other Pacific Islander alone",
                "White and\nBlack",
                "White and\nAIAN",
                "White and\nAsian",
                "Black and\nAIAN",
                "NHPI and White\n(Hawaii only)",
                "NHPI and Asian\n(Hawaii only)",
                "NHPI and Asian and\nWhite (Hawaii only)",
                
                'Kentucky','','','','','','','','','','','','','','','',
                occupation,'','','','','','','','','','','','','','','',
                
                'Total, both sexes','','','','','','','','','','','','','','','',
                
                
                # Total, both sexes Number values
                
                'Number',
                round(total_both_sexes_total, 2),
                round(total_both_sexes_hisp_white, 2),
                round(total_both_sexes_hisp_all_other, 2),
                round(total_both_sexes_not_hisp_one_race_white, 2),
                round(total_both_sexes_not_hisp_one_race_black, 2),
                round(total_both_sexes_not_hisp_one_race_american_indian, 2),
                round(total_both_sexes_not_hisp_one_race_asian, 2),
                round(total_both_sexes_not_hisp_one_race_nhpi, 2),
                round(total_both_sexes_not_hisp_two_races_white_black, 2),
                round(total_both_sexes_not_hisp_two_races_white_aian, 2),
                round(total_both_sexes_not_hisp_two_races_white_asian, 2),
                round(total_both_sexes_not_hisp_two_races_black_aian, 2),
                round(total_both_sexes_not_hisp_two_races_nhpi_white, 2),
                round(total_both_sexes_not_hisp_two_races_nhpi_asian, 2),
                round(total_both_sexes_not_hisp_two_races_nhpi_asian_white, 2),
                
                # Total, both sexes Percent values
                
                'Percent',
                round(total_both_sexes_total / total_both_sexes_total * 100, 2),
                round(total_both_sexes_hisp_white / total_both_sexes_total * 100, 2),
                round(total_both_sexes_hisp_all_other / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_one_race_white / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_one_race_black / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_one_race_american_indian / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_one_race_asian / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_one_race_nhpi / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_white_black / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_white_aian / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_white_asian / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_black_aian / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_nhpi_white / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_nhpi_asian / total_both_sexes_total * 100, 2),
                round(total_both_sexes_not_hisp_two_races_nhpi_asian_white / total_both_sexes_total * 100, 2),
                
                
                'Male','','','','','','','','','','','','','','','',
                
                # Male Number values
                
                'Number',
                round(total_male_total, 2),
                round(total_male_hisp_white, 2),
                round(total_male_hisp_all_other, 2),
                round(total_male_not_hisp_one_race_white, 2),
                round(total_male_not_hisp_one_race_black, 2),
                round(total_male_not_hisp_one_race_american_indian, 2),
                round(total_male_not_hisp_one_race_asian, 2),
                round(total_male_not_hisp_one_race_nhpi, 2),
                round(total_male_not_hisp_two_races_white_black, 2),
                round(total_male_not_hisp_two_races_white_aian, 2),
                round(total_male_not_hisp_two_races_white_asian, 2),
                round(total_male_not_hisp_two_races_black_aian, 2),
                round(total_male_not_hisp_two_races_nhpi_white, 2),
                round(total_male_not_hisp_two_races_nhpi_asian, 2),
                round(total_male_not_hisp_two_races_nhpi_asian_white, 2),
                
                # Male Percent values
                
                'Percent',
                round(total_male_total / total_both_sexes_total * 100, 2),
                round(total_male_hisp_white / total_both_sexes_total * 100, 2),
                round(total_male_hisp_all_other / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_one_race_white / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_one_race_black / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_one_race_american_indian / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_one_race_asian / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_one_race_nhpi / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_white_black / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_white_aian / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_white_asian / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_black_aian / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_nhpi_white / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_nhpi_asian / total_both_sexes_total * 100, 2),
                round(total_male_not_hisp_two_races_nhpi_asian_white / total_both_sexes_total * 100, 2),
                
                
                'Female','','','','','','','','','','','','','','','',
                
                # Female Number values
                
                'Number',
                round(total_female_total, 2),
                round(total_female_hisp_white, 2),
                round(total_female_hisp_all_other, 2),
                round(total_female_not_hisp_one_race_white, 2),
                round(total_female_not_hisp_one_race_black, 2),
                round(total_female_not_hisp_one_race_american_indian, 2),
                round(total_female_not_hisp_one_race_asian, 2),
                round(total_female_not_hisp_one_race_nhpi, 2),
                round(total_female_not_hisp_two_races_white_black, 2),
                round(total_female_not_hisp_two_races_white_aian, 2),
                round(total_female_not_hisp_two_races_white_asian, 2),
                round(total_female_not_hisp_two_races_black_aian, 2),
                round(total_female_not_hisp_two_races_nhpi_white, 2),
                round(total_female_not_hisp_two_races_nhpi_asian, 2),
                round(total_female_not_hisp_two_races_nhpi_asian_white, 2),
                
                # Female Percent values
                
                'Percent',
                round(total_female_total / total_both_sexes_total * 100, 2),
                round(total_female_hisp_white / total_both_sexes_total * 100, 2),
                round(total_female_hisp_all_other / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_one_race_white / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_one_race_black / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_one_race_american_indian / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_one_race_asian / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_one_race_nhpi / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_white_black / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_white_aian / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_white_asian / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_black_aian / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_nhpi_white / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_nhpi_asian / total_both_sexes_total * 100, 2),
                round(total_female_not_hisp_two_races_nhpi_asian_white / total_both_sexes_total * 100, 2)
            ),
            # conditions so matrix is parsed correctly
            ncol=16,byrow=TRUE
        )
        
        
        ################################################################### SET ROWS/COLS OF MATRIX
        
        colnames(table_x) = table_x[1,]
        table_x <- table_x[-1,]
        rownames(table_x) = table_x[,1]
        
        
        ################################################################### FORMAT MATRIX INTO PRETTY TABLE
        
        dt <- table_x %>%
            gt() %>%
            
            # spanner labels
            tab_spanner(
                label = "Hispanic or Latino",
                columns = vars("White alone\nHispanic or Latino", "All other\nHispanic or Latino")
            ) %>%
            tab_spanner(
                label = "Not Hispanic or Latino, one race",
                columns = vars(
                    "White alone",
                    "Black or African American alone",
                    "American Indian and\nAlaska Native alone",
                    "Asian alone",
                    "Native Hawaiian\nand Other Pacific Islander alone",
                    "White and\nBlack",
                    "White and\nAIAN",
                    "White and\nAsian",
                    "Black and\nAIAN"
                )
            ) %>%
            tab_spanner(
                label = "Not Hispanic or Latino, two or more races",
                columns = vars(
                    "NHPI and White\n(Hawaii only)",
                    "NHPI and Asian\n(Hawaii only)",
                    "NHPI and Asian and\nWhite (Hawaii only)"
                )
            ) %>%
            tab_spanner(
                label = "Race/Ethnicity",
                columns = vars(
                    "Geography, Occupation, Subject"
                )
            ) %>%
            tab_spanner(
                label = "Total, race\nand Ethnicity",
                columns = vars(
                    " "
                )
            ) %>%
            
            # format missing values to zero
            
            fmt_missing(
                columns = 3:16,
                missing_text = 0
            )
        
        dt
        
    })
    
}
# Run the app ----
shinyApp(ui, server)