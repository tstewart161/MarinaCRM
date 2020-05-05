## app.R ##
# Full App

#### Libraries ----
# read in libraries
library(shinydashboard)
library(shiny)
library(dqshiny)
library(shinyWidgets)
library(tidyr)
library(readr)
library(dplyr)
library(fullcalendar)
library(officer)
# for mailR, you have to install java 64 bit onto your machine
library(mailR)
library(DT)
library(rhandsontable)

# sourcing local functions
source("functions/DCM_functions.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

getInputs <- function(pattern){
  reactives <- names(reactiveValuesToList(input))
  reactives[grep(pattern,reactives)]
}


#### developemental portion of application ----

wd <- getwd()
message(wd)

# See views of data tables as they are read in
see_views <- FALSE

dev_date <- as.Date("05/03/2020",format = "%m/%d/%y")


#### Read in CSV's ----

# customer
customer <-read_csv(file = "data/customer.csv")
if (see_views == TRUE) {
  customer %>%
    View("Raw customer Table")
  
}

# do not rent
do_not_rent <- read_csv(file = "data/do_not_rent.csv")
if (see_views == TRUE) {
  
  
  do_not_rent %>%
    View("Raw do_not_rent Table")
}

# rental boats
# need to specify active rental boats

rental_boats <- read_csv(file = "data/rental_boats.csv")

if (see_views == TRUE) {
  rental_boats %>%
    View("Raw Customer Table")
}

active_boats <- rental_boats %>%
  filter(online == 1 & broke_down == 0)
if (see_views == TRUE) {
  active_boats %>%
    View("Active Boats Table")
}

broken_boats <- rental_boats %>%
  filter(online == 1 & broke_down == 1)
if (see_views == TRUE) {
  broken_boats %>%
    View("Broken Boats Table")
}

# rental_boater
rental_boater <- read_csv(file = "data/rental_boater.csv")
if (see_views == TRUE) {
  rental_boater %>%
    View("raw rental_boater table")
}


# staff
staff <- read_csv(file = "data/staff.csv")
if (see_views == TRUE) {
  staff %>%
    View("raw staff table")
}


# converting data types to dates if they aren't already
if (class(rental_boater$start_rental_date) != "Date") {
  print(class(rental_boater$start_rental_date))
  
  rental_boater <- rental_boater %>%
    mutate(start_rental_date = as.Date(start_rental_date, format = "%m/%d/%Y"))
  
  print(class(rental_boater$start_rental_date))
  
}

if (class(rental_boater$end_rental_date) != "Date") {
  print(class(rental_boater$end_rental_date))
  rental_boater <- rental_boater %>%
    mutate(end_rental_date = as.Date(end_rental_date, format = "%m/%d/%Y"))
  print(class(rental_boater$end_rental_date))
  
  
}



# the data for rendering the full calendar for the rental boats

full_calendar_rental_data <- rental_boater %>%
  filter(cancelled == 0) %>%
  mutate(
    time_in = if_else(
      condition = time_slot == "8:30 - 4:30",
      "4:30",
      if_else(
        condition = time_slot == "9:00 - 5:00",
        "5:00",
        if_else(
          condition = time_slot == "9:30 - 5:30",
          "5:30",
          if_else(
            condition = time_slot == "10:00 - 6:00",
            "6:00",
            "ERROR"
          )
        )
      )
    ),
    color = if_else(
      time_in == "4:30",
      "green",
      if_else(
        time_in == "5:00",
        "orange",
        if_else(
          time_in == "5:30",
          "blue",
          if_else(
            time_in == "6:00",
            "yellow",
            "red"
          )
        )
      )
    )
  )

# adding the "title" column in to display the name and their due in time on the calendar
full_calendar_rental_data$title <- with(full_calendar_rental_data, paste0(email," (",time_in,")", "Num: ",rb_number))


full_calendar_rental_data <- full_calendar_rental_data %>%
  select(title, start_rental_date, end_rental_date, color) %>% 
  rename(start = start_rental_date, end = end_rental_date)

# full_calendar_rental_data %>%
#   View("full_calendar_rental_data")


staff <- staff %>%
  mutate(date_hired = as.Date(date_hired, format = "%m/%d/%Y"),
         date_left = as.character(date_left))


# current staff
current_staff <- staff %>%
  filter(date_left == "0")

current_staff_names <- current_staff$full_name

# adding a "cancelled boats" data
cancelled_boats <- rental_boater %>% 
  filter(cancelled == 1)

# changing the rental_boater data to not include those who are cancelled
# rental_boater <- rental_boater %>%
#   filter(cancelled == 0)



current_renter_emails <- rental_boater %>%
  filter(cancelled == 0)

current_renter_emails <- current_renter_emails$email



# creating blank DF for the table for emailing lots of customer
tab_emailing_data <- data.frame("Name" = "","Email" = "","Total" = 0)

ui <- dashboardPage(
  dashboardHeader(title = "DCM Better Business Tool"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome Page", tabName = "welcome_page",icon = icon("home")),
      menuItem("Manager Page", tabName = "manager_page",icon = icon("boss")),
      
      # menuItem("Creditenials", tabName = "cred", icon = icon("lock")),
      
      #Enter new customer will have options where if you enter a certian thing, it will open other things
      # menuItem("Customer/Transaction Entry", tabName = "customer_entry",icon = icon("address-book"),
      #          menuSubItem("Camper", tabName = "camper_entry"),
      #          menuSubItem("Service", tabName = "service_entry")),
      
      menuItem("Rental Tab", tabName = "rental_information", icon = icon("anchor"),
               menuSubItem("Renter", tabName = "renter_entry"),
               menuSubItem("Calendar View", tabName = "calendar_view"),
               menuSubItem("Cancelation", tabName = "cancelation"),
               menuSubItem("Edit Fleet", tabName = "edit_fleet"),
               menuSubItem("DO NOT RENT", tabName = "do_not_rent", icon = icon("ex"))),
      
      menuItem("The Tab Book",tabName = "tab_book",icon = icon("book")),
      menuItem("Email Send", tabName = "email_send", icon = icon("paper-plane")),
      
      menuItem("Create New Email", tabName = "create_new_email",icon = icon("envelope")),
      
      menuItem("Subscribed Customer Email", tabName = "subscribed_customer_email",icon = icon("check")),
      
      menuItem("Customer Map", tabName = "map_tab", icon = icon("globe")),
      
      menuItem("Customer Visualizations", tabName = "customer_viz", icon = icon("chart-line"))
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "welcome_page",
              h1(tags$b("Dukes Creek Marina Better Business Tool"),align = "center"),
              h3("This tool allows for DCM employees to manage daily tedious tasks more efficiently to focus on customer engagements.", align = "center"),
              div(img(src="DCM_LOGO.png", width = "100%"), align = "center"),
              box(width = 14,
                  h3("Today's Outlook", align = "center"), 
                  hr(),
                  valueBoxOutput("due430"),
                  valueBoxOutput("due500"),
                  valueBoxOutput("due530"),
                  valueBoxOutput("due600")
              )
      ),
      
      tabItem(
        tabName = "manager_page",
        h1("Manager Page"),
        h4("Welcome, this page is designed to allow you, the manager to be able to make edits to the staff."),
        box(
          title = "Staff Hiring",
          textInput(
            inputId = "new_employee_first_name",
            label = "First Name",
          ),
          
          textInput(
            inputId = "new_employee_middle_initial",
            label = "Middle Initial"
          ),
          
          textInput(
            inputId = "new_employee_last_name",
            label = "Last Name"
          ),
          actionButton(
            inputId = "confirm_new_hire",
            label = "Welcome Aboard!"
          )
          
        ),
        box(
          title = "Staff Leaving",
          selectInput(
            inputId = "name_staff_leaving",
            label = "Name",
            choices = current_staff_names
            
            
          ),
          actionButton(
            inputId = "confirm_staff_leaving",
            label = "Confirm Removal From Staff"
          )
          
        )
      ),
      
      
      # Second tab content
      tabItem(
        tabName = "cred",
        box(
          title = "Enter your credentials",
          textInput("username", 
                    label = "Username", 
                    value = "dukescreekmarina@gmail.com"),
          passwordInput("password",
                        label = "Password"
          )
        )
      ),
      
      # third tab contents
      tabItem(
        tabName = "renter_entry",
        
        box(
          title = "Length of Rental",
          selectInput(
            inputId = "len_rental",
            label = "Length of Rental",
            choices = c("Single" = "single",
                        "Multi" = "multi")
          ),
          
          # only appear when there are single rentals
          conditionalPanel(
            condition = "input.len_rental == 'single'",
            radioButtons(
              inputId = "amount_of_people",
              label = "Boat Size",
              choices = unique(rental_boats$max_people),
              selected = FALSE
            )
          ),
          
          
          # single day data entry
          conditionalPanel(
            condition = "input.len_rental == 'single'",
            
            dateInput(
              inputId = "dates_of_rental",
              label = "Date of Rental (Year-Month-Day)",
              value = dev_date
            ),
            
            radioButtons(
              inputId = "len_day",
              label = "Length of Day",
              choices = c("Full Day" = "full_day",
                          "Half Day" = "half_day")
              
            )
            
            
            
          ),
          conditionalPanel(
            condition = "input.len_rental == 'multi'",
            dateRangeInput(
              inputId = "dates_of_rental_multi",
              label = "Starting and Ending Dates (Year-Month-Day)",
              start = dev_date,
              end = dev_date
              
            )
          ),
          actionButton(
            inputId = "check_availability",
            label = "Check Availability"),
          
          conditionalPanel(
            condition = "input.len_rental == 'multi'",
            tableOutput("multi_day_availability")
          )
          
          
          
        ),
        # enter personal information
        box(
          title = "Renter Information",
          textInput(
            inputId = "first_name_rental_boater",
            label = "First Name",
            value = ""
          ),
          textInput(
            inputId = "last_name_rental_boater",
            label = "Last Name",
            value = ""
          ),
          textInput(
            inputId = "email_rental_boater",
            label = "Email",
            value = ""
          ),
          textInput(
            inputId = "phone_rental_boater",
            label = "Phone",
            value = ""
          ),
          textInput(
            inputId = "address_rental_boater",
            label = "Address",
            value = ""
          ),
          textInput(
            inputId = "last_four_card",
            label = "Last Four of Card",
            value = ""
          )
          
          
        ),
        
        
        
        box(
          title = "Rental Information",
          # single, select the boat
          selectizeInput(
            inputId = "dcm_agent_for_rental",
            label = "DCM Agent",
            choices = current_staff_names,
            multiple = FALSE,
            selected = ""
            
            
          ),
          conditionalPanel(
            condition = "input.len_rental == 'single'",
            
            selectInput(
              inputId = "boat_to_rent_single",
              label = "Boat Number",
              choices = c(
                " " = " "
              )
            )
          ),
          
          # mutliple, you can either select, or have the computer do it for you
          conditionalPanel(
            "input.len_rental == 'multi'",
            
            radioButtons(inputId = "size_to_rent_multi",
                         label = "Size of Boat to Rent",
                         choices = c(" " =" " ),# unique(available_boats$max_people)),
                         selected = FALSE
                         
            )
            
          ),
          selectInput(
            inputId = "time_slot",
            label = "Time Slot",
            choices = c(
              "8:30 - 4:30" = "8:30 - 4:30",
              "9:00 - 5:00" = "9:00 - 5:00",
              "9:30 - 5:30" = "9:30 - 5:30",
              "10:00 - 6:00" = "10:00 - 6:00"
            )
          ),
          box(
            # available background colors:  red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
            title = "Times Taken Already",
            width = 4,
            background = "aqua",
            tableOutput("time_slot_table")
          )
        ),
        
        
        
        box(
          title = "Extras",
          selectInput(
            inputId = "tube_rental",
            label = "Tube Rental",
            choices = c(
              No = "No",
              Yes = "Yes"
            )
          ),
          conditionalPanel(
            condition = "input.tube_rental" == "Yes",
            numericInput(
              inputId = "cost_of_tube",
              label = "Price of Tube Rental",
              value = 25
            )
          )
        ),
        actionButton(
          inputId = "confirm_rental",
          label = "Confirm Rental and Send Email"
        )
        
        
      ),
      # fourth tab contents
      tabItem(tabName = "calendar_view",
              # will attempt to use full calendar here
              # we may need to have a refresh button
              # 
              
              fullcalendarOutput(output = "rental_full_calendar", width = "100%", height = "400px")
              
              
      ),
      tabItem(tabName = "cancelation",
              h2("Rental Cancellation", align = "center"),
              h4("By selecting a customer, you are confirming that this rental will be terminated. \nIf you would like to change the date of a rental, you must create the new rental on the New Reservation tab and then cancel the date that they do not want."),
              
              box(
                selectizeInput(
                  inputId = "renter_email_cancelation",
                  label = "Email for Renter Cancelation",
                  choices = c(unique(current_renter_emails))
                  
                ),
                
                actionBttn(
                  inputId = "search_email_for_cancelation",
                  label = "Search",
                  icon = icon("search")
                )
              ),
              
              box(
                uiOutput("dates_to_select_render"),
                uiOutput("cancelation_button_to_render")
              )
      ),
      
      
      
      
      
      #### edit_fleet ----
      tabItem(tabName = "edit_fleet",
              h2("Alter the Rental Boat Fleet"),
              h4("When something happens to a rental boat, be sure to enter it into the system"),
              box(
                title = "Addition to Fleet",
                
                # adding a boat to the fleet
                numericInput(
                  inputId = "new_rental_number",
                  label = "Rental #",
                  value = max(rental_boats$rb_number) + 1
                ),
                numericInput(
                  inputId = "max_people",
                  label = "Max # of People",
                  value = "10"
                ),
                
                numericInput(
                  inputId = "full_day_price_rental_addition",
                  label = "Full Day Price",
                  value = 350,
                  min = 0,
                  max = 500
                ),
                
                numericInput(
                  inputId = "half_day_price_rental_addition",
                  label = "Half Day Price",
                  value = 185,
                  min = 0,
                  max = 500
                ),
                
                actionButton(
                  inputId = "confirm_rental_addition",
                  label = "Confirm Addition to Fleet"
                )
              ),
              
              # remove rental boat from fleet
              # this will make it so that the rental boat specified becomes inactive, but is still in the dataset
              box(
                title = "Remove from Fleet",
                selectInput(
                  inputId = "boat_to_remove",
                  label = "# to Remove",
                  choices = active_boats$rb_number
                ),
                actionButton(
                  inputId = "confirm_boat_remove",
                  label = "Confirm Rental Boat Removal"
                )
              ),
              box(
                # issue matenience on boats
                title = "Boat Maitenence",
                
                # boat out of service
                selectInput(
                  inputId = "broken_boat_number",
                  label = "# of Broken Down Boat",
                  choices = c(active_boats$rb_number)
                ),
                
                
                actionButton(
                  inputId = "submit_broken_boat",
                  label = "Confirm Broken Boat"
                  
                ),
                
                # boat that was out of service can now be ready
                selectInput(
                  inputId = "fixed_boat_number",
                  label = "# of Fixed Boat",
                  choices = broken_boats$rb_number
                ),
                actionButton(
                  inputId = "confirm_fixed_boat",
                  label = "Confirm Fixed Boat"
                  
                )
              )
              
      ),
      
      #### Do not rent tab
      tabItem(tabName = "do_not_rent",
              h2("Unfit Renters"),
              h4("This tab allows for the DCM agent to add someone to the do not rent list as well as take an individual off of the do not rent to list"),
              
              box(
                title = "Add to Do Not Rent List",
                autocomplete_input(
                  id = "do_not_rent_addition_email",
                  label = "Email",
                  options = customer$email,
                  value = "",
                  max_options = 5
                ),
                
                textAreaInput(
                  inputId = "do_not_rent_addition_description",
                  label = "Description/Reason of Incident",
                  value = ""
                ),
                actionButton(
                  inputId = "confirm_do_not_rent_addition",
                  label = "Add to Do Not Rent list")
              ),
              box(
                title = "Remove from Do Not Rent List",
                autocomplete_input(
                  id = "do_not_rent_removal_email",
                  label = "Email",
                  options = do_not_rent$email,
                  value = "",
                  max_options = "3"
                ),
                actionButton(
                  inputId = "confirm_do_not_rent_removal",
                  label = "Submit"
                )
              )
      ),
      tabItem(tabName = "tab_book",
              h2("The Tab Book", align = "center"),
              h4("The tool for fast tab book experiences", align = "center"),
              
              # add a datatable that can be manipulated
              mainPanel(
                tabsetPanel(type = "tabs",
                            
                            tabPanel("You Owe Money",
                                     box(
                                       h3("You Owe Money"),
                                       numericInput(
                                         inputId = "number_of_customers_to_email_you_owe_money_on_your_tab",
                                         label = "Number of Customers to Email",
                                         value = 0,
                                         min = 0
                                       ),
                                       
                                       actionButton("confirm_you_owe_money_on_your_tab","Confirm"),
                                       
                                       rHandsontableOutput("tab_customers_you_owe_money_on_your_tab"),
                                       
                                       br(),
                                       
                                       actionButton("send_email_you_owe_money_on_your_tab","Send Emails"),
                                     ),
                                     box(
                                       h3("Email Contents"),
                                       h5("Dear (Name),"),
                                       h5("It's the beginning of the month again which means it's time to clear out tab sheets. You have a balance of $(total) on your tab. If you could please contact us or stop by with a payment or a credit card number, we would appreciate it. Thank you.")),
                                     
                                     
                            ),
                            tabPanel("We Ran Your Card",
                                     box(
                                     h3("We Ran Your Card"),
                                     numericInput(
                                       inputId = "number_of_customers_to_email_we_ran_your_card",
                                       label = "Number of Customers to Email",
                                       value = 0,
                                       min = 0
                                     ),
                                     
                                     actionButton("confirm_we_ran_your_card","Confirm"),
                                     
                                     rHandsontableOutput("tab_customers_we_ran_your_card"),
                                     
                                     br(),
                                     actionButton("send_email_we_ran_your_card","Send Emails")),
                                     box(
                                     h3("Email Contents"),
                                     h5("Hi!"),
                                     h5("It's that time of the month again, so we ran your tab for a total of $(total). This clears out your tab.  If you have any questions or concerns, please contact us at 540-895-5065. Thank you."))
                            ),
                            
                            tabPanel("Email then Run",
                                     box(
                                     h3("Email then Run"),
                                     numericInput(
                                       inputId = "number_of_customers_to_email_email_then_run",
                                       label = "Number of Customers to Email",
                                       value = 0,
                                       min = 0
                                     ),
                                     
                                     actionButton("confirm_email_then_run","Confirm"),
                                     
                                     rHandsontableOutput("tab_customers_email_then_run"),
                                     
                                     br(),
                                     actionButton("send_email_email_then_run","Send Emails")),
                                     
                                     box(
                                     h3("Email Contents"),
                                     h5("Dear (name),"),
                                     h5("It's the beginning of the month again which means it's time to clear out tabs. We wanted to make sure that it was okay to run your card for $(total) on the card ending in (last 4 digits). Hope to hear from you soon. Thank you."))
                            )
                            
                            
                )
              )
              
              
              
              
              
              
              
              
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  # manager page
  
  # new hire
  observeEvent(input$confirm_new_hire,{
    message("Adding new employee to the DB")
    
    newly_added_staff_id <- max(staff$staff_id) + 1
    
    message(paste("Newly added staff member's unique ID",newly_added_staff_id))
    
    staff <<- staff %>% add_row(staff_id = newly_added_staff_id,
                                first_name = input$new_employee_first_name,
                                middle_initial = input$new_employee_middle_initial,
                                last_name = input$new_employee_last_name,
                                full_name = paste(input$new_employee_first_name,input$new_employee_last_name),
                                date_hired = Sys.Date(),
                                date_left = "0")
    
    message("Adding the new staff member to the database")
    write_csv(x = staff,"data/staff.csv")
    
    sendSweetAlert(
      session = session,
      title = paste0(input$new_employee_first_name,", Welcome to the DCM crew!"),
      text = "Hope you love it here!",
      type = "success"
    )
    
  })
  
  # someone that was fired/quit
  observeEvent(input$confirm_staff_leaving,{
    
    staff_id_for_removal_df <- current_staff %>%
      filter(full_name == input$name_staff_leaving)
    
    staff_id_for_removal <- staff_id_for_removal_df$staff_id
    
    staff$date_left[staff$staff_id == staff_id_for_removal] <<- as.character(Sys.Date())
    
    write_csv(x = staff,"data/staff.csv")
    
    sendSweetAlert(
      session = session,
      title = paste0(input$name_staff_leaving," has been removed from the staff listing!"),
      text = "",
      type = "success"
    )
    
  })
  
  
  
  
  # filling in the boxes for what boats are due in at what times
  output$due430 <- renderValueBox({
    due_in_430 <- rental_boater %>%
      filter(end_rental_date == dev_date & time_slot == "8:30 - 4:30")
    
    
    boats_at_430 <- nrow(due_in_430)
    # remove(due_in_430)
    
    valueBox(
      boats_at_430," Rentals @ 4:30", 
      icon = icon("anchor"),
      color = "green"
    )
  })
  
  
  output$due500 <- renderValueBox({
    due_in_500 <- rental_boater %>%
      filter(end_rental_date == dev_date & time_slot == "9:00 - 5:00")
    
    
    boats_at_500 <- nrow(due_in_500)
    # remove(due_in_500)
    
    valueBox(
      boats_at_500," Rentals @ 5:00", 
      icon = icon("anchor"),
      color = "orange"
    )
  })
  
  
  output$due530 <- renderValueBox({
    due_in_530 <- rental_boater %>%
      filter(end_rental_date == dev_date & time_slot == "9:30 - 5:30")
    
    
    boats_at_530 <- nrow(due_in_530)
    # remove(due_in_530)
    
    valueBox(
      boats_at_530," Rentals @ 5:30", 
      icon = icon("anchor"),
      color = "blue"
    )
  })
  
  
  output$due600 <- renderValueBox({
    due_in_600 <- rental_boater %>%
      filter(end_rental_date == dev_date & time_slot == "10:00 - 6:00")
    
    
    boats_at_600 <- nrow(due_in_600)
    # remove(due_in_600)
    
    valueBox(
      boats_at_600," Rentals @ 6:00", 
      icon = icon("anchor"),
      color = "yellow"
    )
  })
  
  
  
  #### renter_entry ----
  
  
  
  
  observeEvent(input$check_availability,{
    
    
    #### Checking Availability for multi-days ----
    
    if (input$len_rental == "multi") {
      message("In the check availability multi conditional")
      
      
      print("The input date types")
      print(class(input$dates_of_rental_multi[1]))
      print(class(input$dates_of_rental_multi[2]))
      
      print("The class for columns of start date and end date")
      print(class(rental_boater$start_rental_date))
      print(class(rental_boater$end_rental_date))
      
      # grab the boats that fall within the date range
      message("Grabbing boats that fall within the date range")
      rented_boats <- rental_boater[rental_boater$start_rental_date <= input$dates_of_rental_multi[1] & rental_boater$end_rental_date <= input$dates_of_rental_multi[2] & rental_boater$cancelled == 0, ]
      
      
      
      # merge the max amount of people allowed
      message("Merging the rented boats and the rental boats tables")
      rented_boats <- merge(x = rented_boats, y = rental_boats[ , c("rb_id", "max_people")], by = "rb_id", all.x=TRUE)
      
      # viewing the rented_boats dataset
      # rented_boats %>% View(title = "rented_boats")
      
      # the individual can only rent the boat if it is available for many days in a row
      # need to figure out if any of the 6,8, or 10 person boats are available for a multiday
      # if they are not available in a row, i need to see if they could be if we moved them around
      
      
      # first check input$size_to_rent_multi person boats
      # get max amount of input$size_to_rent_multi person boats
      message("count_of_boat_size")
      # active_boats %>%
      #   filter(max_people == input$size_to_rent_multi) %>%
      #   View()
      # count_of_boat_size <- nrow(active_boats %>%
      #                                filter(max_people == input$size_to_rent_multi))
      # 
      
      # print(paste("There are",count_of_boat_size," ",input$size_to_rent_multi," person boats that are active"))
      
      
      # this small section of code retreives the amount of boats that are rented on each day
      # df_of_10_p_boat <- rented_boats %>%
      #   filter(max_people == input$size_to_rent_multi)
      
      
      message("Getting the sequence of dates")
      
      
      # creating a list of days to go through to see how many boats are assigned to that date
      list_of_days <- seq(input$dates_of_rental_multi[1],input$dates_of_rental_multi[2],by ="day")
      # empty dataframe. will add to it through the for loop to keep track of days & how many boats are rented on those days
      boats_rented_per_day <- data.frame(business_date = as.Date(character()),
                                         boats_available = character(), 
                                         max_people_allowed = integer(),
                                         stringsAsFactors=FALSE) 
      
      # boats_rented_per_day %>%
      #   View("before_loop_boats_rented_per_day")
      
      message("Starting the for loop that fills the dataframe with the date, the amount of boats available, and the max people allowed on those boats.")
      
      
      # this for loop goes through and gets the count of all the dates for each boat, the 12, 10, 8, and 6 person boats
      # the goal of this loop is that if a 4 person boat was ever added to the fleet, then it would be able to handle that
      # it seems to be working
      for (max_people_looping in unique(active_boats$max_people)) {
        
        
        for (i in 1:length(list_of_days)) {
          
          # length of the boats rented for that day
          # that are of that size that we care about in the for loop
          rented_boats_of_size <- rented_boats %>%
            filter(max_people == max_people_looping)
          
          # number of boats rented for that specific size
          number_of_boats_rented <- length(rented_boats_of_size[rented_boats_of_size$start_rental_date <= list_of_days[i] & list_of_days[i] <= rented_boats_of_size$end_rental_date, ]$rb_id)
          
          count_of_boat_size <- nrow(active_boats %>%
                                       filter(max_people == max_people_looping))
          
          
          # calculating the amount of x person boats left
          boats_left <- count_of_boat_size - number_of_boats_rented
          
          # appending this to the dataframe
          boats_rented_per_day <- rbind(boats_rented_per_day, data.frame(business_date = as.Date(list_of_days[i]),boats_available = boats_left, max_people_allowed = as.integer(max_people_looping)))
          
        }
        
      }
      
      message("Just finished that for loop, viewing the after_loop_boats_rented_per_day")
      # boats_rented_per_day %>%
      #   View("after_loop_boats_rented_per_day")
      
      
      message("Now starting the part of the code that displays to the user what sizes are available")
      # now need to display to the user that the multiday is available for the following sizes
      minimum_availability <- boats_rented_per_day %>%
        group_by(max_people_allowed) %>%
        summarise(
          min_availability = min(boats_available),
          multi_day_availability = if_else(
            condition = min_availability == 0,
            "Unavailable",
            "Available"
          )
        )
      
      # viewing
      message("Viewing Minimum Availability")
      # minimum_availability %>%
      #   View(title = "minimum availability")
      
      
      # making it pretty for DCM agents
      minimum_availability_for_dcm_view <- minimum_availability %>%
        select(max_people_allowed, multi_day_availability)
      
      colnames(minimum_availability_for_dcm_view) <- c("Max People Allowed","Status")
      
      output$multi_day_availability <- renderTable(
        {minimum_availability_for_dcm_view}
      )
      
      
      # minimum_availability %>%
      #   View("minimum availability, non dcm view")
      # 
      # getting the sizes that are available for the user to choose
      sizes_available <- minimum_availability %>%
        filter(multi_day_availability == "Available")
      
      print(sizes_available)
      
      # boats_rented_per_day %>%
      #   View(title = "boats_rented_per_day")
      
      updateRadioButtons(
        session,
        inputId = "size_to_rent_multi",
        choices = unique(sizes_available$max_people_allowed)
      )
      
      
      
      message("Finished the first if statement")
    } else { # SINGLE DAY
      
      
      #### Checking Availability for Single Days ---- 
      
      # getting the rental boats that are rented for that day (SINGLE DAY)
      rented_boats <- rental_boater[rental_boater$start_rental_date <= input$dates_of_rental & input$dates_of_rental <= rental_boater$end_rental_date, ]
      
      # merging the dataframe to give the rented boats a person limit
      rented_boats <- left_join(x = rented_boats, y = rental_boats, by = c("rb_id","rb_number")) 
      
      
      
      
      # need to filter for the boats that the user picked
      # I do not think that changing active_boats will cause any errors, so I am going to do it
      rented_boats <- rented_boats %>%
        filter(max_people %in% input$amount_of_people)
      
      active_boats_for_specified_size <- active_boats %>%
        filter(max_people %in% input$amount_of_people)
      
      
      
      
      # if the amount of rows of rented boats for that date is greater than or equalled to the amount of online boats, then there are no boats left
      # no boats left
      if (nrow(rented_boats) >= length(active_boats_for_specified_size$rb_number)) {
        
        sendSweetAlert(
          session,
          title = "This Date is Booked!",
          text = "Maximum amount of boats already rented for this date",
          type = "error"
        )
        
        
      } else{
        # there are boats
        
        
        
        # to put the boats that are available into their own dataframe
        available_boats <- setdiff(active_boats_for_specified_size$rb_number,rented_boats$rb_number)
        
        updateSelectInput(
          session,
          inputId = "boat_to_rent_single",
          choices = available_boats
          
        )
        
        
        
        sendSweetAlert(
          session,
          title = paste0(dev_date," has Availability for ",input$amount_of_people ,"!"),
          text = "This date has boats that are available",
          type = "success"
        )
        
        
        
        
        # show the user what table has been rented
        output$time_slot_table <- renderTable({
          head(rented_boats %>% 
                 select(time_slot) %>%
                 group_by(time_slot) %>%
                 mutate(
                   "Time Slot" = time_slot,
                   Boats = n()
                 ) %>%
                 ungroup() %>%
                 select(-c(time_slot))
          )
        }
        )
        
        
      }
      # output$checking_availability <- renderPrint({})
    }
  }
  )
  
  #### Confirming The Rental ----
  observeEvent(input$confirm_rental,{
    message("Starting the Process to Confirm the Rental")
    
    
    #### Updating Customer Table for Renters ---- 
    message("Checking to see if a new customer is being added to the database...")
    print(customer$email %in% input$email_rental_boater)
    print(input$email_rental_boater)
    if (customer$email %in% input$email_rental_boater) {
      # then that email already exists in the database
      message("The rental boater that was inputted already exists in the database")
      
    } else {
      # add customer to the database
      message("Customer is being added to the database")
      customer <<- customer %>% add_row(email = input$email_rental_boater, 
                                        first_name = input$first_name_rental_boater, 
                                        last_name = input$last_name_rental_boater,
                                        address = input$address_rental_boater,
                                        phone_number = input$phone_rental_boater)
      
      message("writing to customer.csv")
      write_csv(customer,"data/customer.csv")
      message("wrote to customer.csv")
    }
    
    
    #### Confirming Multiday Rental ----
    message("Checking to see if the boat is a multiday or not")
    if (input$len_rental == "multi") {
      message("Multiday was choosen as the boat rental")
      
      
      
      
      # putting someone in a multiday rental
      # not only will this put someone in a multiday rental, but it will also move people around, depending on where they are.
      
      
      # do not need to check if it is available, because there are already checks in place for that
      
      # total amount of days for the rental
      amount_of_days_for_rental <- input$dates_of_rental_multi[2] - input$dates_of_rental_multi[1]
      
      # getting the sequence of dates
      seq_of_dates_for_rental <- seq(as.Date(input$dates_of_rental_multi[1]), by = "day", length.out = amount_of_days_for_rental)
      
      
      
      # get the boats that are rented between those dates
      rented_boats_for_multiday_length <- rental_boater[rental_boater$start_rental_date <= input$dates_of_rental_multi[1] & rental_boater$end_rental_date <= input$dates_of_rental_multi[2] & rental_boater$cancelled == 0, ]
      
      
      
      # combine the dataset to get the max_people
      # rented_boats_for_multiday_length now contains the records for those renters between those dates 
      # and the max amount of people allowed
      rented_boats_for_multiday_length <- left_join(x = rented_boats_for_multiday_length, y = rental_boats, by = c("rb_id","rb_number"))
      
      # filter based off of the boat size that was inputted eariler for multidays
      # rented_boats_for_multiday_length will now contain the records of rentals between the input
      # dates, the max amount of people for each boat, and filtered for the max people size
      rented_boats_for_multiday_length <- rented_boats_for_multiday_length %>%
        filter(max_people == input$size_to_rent_multi)
      
      # viewing the data
      # this data contains all the boats that are rented during the multiday
      # rented_boats_for_multiday_length %>%
      #   View("rented_boats_for_multiday_length")
      
      # active_boats %>%
      #   View("Active Boats")
      
      # first we need to check if there is a boat available the entire way through.
      # to do this, we need to look to see if one of the boats of the correct size
      # is available through the whole time
      
      
      # need to find out if all of the boats of that size are taken
      total_boats_for_size_requested <- active_boats %>%
        filter(max_people == input$size_to_rent_multi)
      
      # need to see if there are any boats that are free
      amount_of_boats_available_all_the_way_through <- length(setdiff(total_boats_for_size_requested$rb_number, rented_boats_for_multiday_length$rb_number))
      
      # this should be the amount of boats that are available from the date the rental was requested to the date that it ended
      # amount_of_boats_available_all_the_way_through <- nrow(total_boats_for_size_requested) - nrow(rented_boats_for_multiday_length)
      
      message(paste0("The amount of boats that are available all the way through ", input$dates_of_rental_multi[1], " to ",input$dates_of_rental_multi[2], " is ",amount_of_boats_available_all_the_way_through))
      
      
      # total_boats_for_size_requested %>%
      #   View("total_boats_for_size_requested before conditional")
      
      
      # rental_boater %>%
      #   View("Check to see if there are wierd NAs here")
      
      
      # check if it is of length 0
      # if it is, that means that there are no boats available that are available through that entire selected time. fuck.
      message("Checking to see if there is a boat that is fully available through the requested dates...")
      if (amount_of_boats_available_all_the_way_through == 0) {
        message("Fuck, there are no boats that are straight available.") # will need to select a boat to put them in
        message("Def gonna be the hardest part of the whole code")
        
        # plan:
        # 1. choose a boat to move the multiday in based on the following criteria:
        #    - Needs to not be a multiday at any time in the rental
        #    - Notice that in the table rental_boater, I have a column that says "multiday"
        #      use that to determine what boat they should be in initially
        # 2. create a for loop that goes through each day
        #    2a. find out if there is a boat already taken in that slot
        #    2b. if there is a boat already in that slot, select a slot that is open, and move 
        #        the single day boat that is in the way into that slot. Then, move the multiday 
        #        boat into that slot.
        #    2c. if there is not a boat already in that slot, then move the multiday there.
        # 3. update the rental_boaters table
        #    
        # NOTE: THE BOAT BEING MOVED CANNOT BE A MULTIDAY
        # I ADDED A VARIABLE CALLED "MULTIDAY" WHICH WILL HELP KEEP TRACK IF A BOAT IS A MULTIDAY
        
        
        # step 1
        # 1. choose a boat to move the multiday in based on the following criteria:
        #    - Needs to not be a multiday at any time in the rental
        #    - Notice that in the table rental_boater, I have a column that says "multiday"
        #      use that to determine what boat they should be in initially
        
        # first get the list of all the multiday renters
        multi_day_at_one_point <- rented_boats_for_multiday_length %>%
          filter(multiday == 1)
        
        
        multi_day_at_one_point %>%
          View("Multiday_at_one_point")
        
        
        # now get the list of the renters who are not multiday
        non_multiday_boats <- rented_boats_for_multiday_length %>%
          filter(multiday == 0)
        
        # now compare the two to get boats that are never multiday
        never_multiday <- non_multiday_boats %>%
          filter(rb_number %in% setdiff(non_multiday_boats$rb_number, multi_day_at_one_point$rb_number))
        
        # now to select the boat that we will put the multiday in
        # this also meanst that this is the boat that will have to move
        
        # the_boat_for_multiday <- never_multiday %>%
        #   filter(rb_number == min(never_multiday$rb_number))
        
        # now to get the rb_number and the rb_id that we will be changing
        the_boat_for_multiday_rb_number <- min(never_multiday$rb_number)#the_boat_for_multiday$rb_number
        the_boat_for_multiday_rb_id <- never_multiday$rb_id[match(the_boat_for_multiday_rb_number,never_multiday$rb_number)]#the_boat_for_multiday$rb_number
        message(paste0("We are going to put these people in boat number ",the_boat_for_multiday_rb_number," With the ID of ",the_boat_for_multiday_rb_id))
        
        
        # quick popup message that will say "all these boats are multiday!!"
        message("Checking to see if all the boats rented during this time are multiday")
        
        
        # I THINK I WILL NEED TO WRAP THE ELSE STATEMENT IN THE REST OF THE IF STATEMENTS, BECAUSE AS
        # IT STANDS, IT WILL STILL TRY TO PUT THEM IN A SPOT
        if (nrow(rented_boats_for_multiday_length) == 0) {
          message("All the boats rented during this time are multidays. Cannot create the multiday")
          sendSweetAlert(
            session,
            title = "Multiday Error!",
            text = "Apparently there are no boats available that are not multiday"
          )
          
        } else {
          message("Not all of the boats rented are multiday. It passed this check.")
        }
        
        
        
        
        
        
        
        # 2. create a for loop that goes through each day
        message("Creating the for loop to go through each day")
        for (day in 1:amount_of_days_for_rental) {
          message("==================")
          message(paste("Iteration for the Date",seq_of_dates_for_rental[day]))
          
          #    2a. find out if there is a boat already taken in that slot
          
          # first, we need to get all the boats that are rented for that date
          rented_boats_for_multiday <<- rental_boater[rental_boater$start_rental_date <= seq_of_dates_for_rental[day] & seq_of_dates_for_rental[day] <= rental_boater$end_rental_date  & rental_boater$cancelled == 0, ]
          message("")
          message(paste("For the rented_boats_for_multiday df, We grabbed all boats where the start date of the rental was less than", seq_of_dates_for_rental[day],"and the end date was fo the rental was less than",seq_of_dates_for_rental[day]))
          message("")
          
          rented_boats_for_multiday %>%
            View("Rented Boats for Multiday")
          
          # further filter it to remove boats that are multidays in the future 
          moveable_rented_boats <- rented_boats_for_multiday %>%
            filter(multiday != 1)
          
          
          # add the column max_people onto the dataframe to further filter it later
          moveable_rented_boats <- merge(x = moveable_rented_boats, y = rental_boats[ , c("rb_id", "rb_number","max_people")], by = c("rb_id","rb_number"), all.x=TRUE)
          
          
          # filter it to only include the correct size
          moveable_rented_boats <- moveable_rented_boats %>%
            filter(max_people == input$size_to_rent_multi)
          
          # now view the boats that have the potential to be moved
          moveable_rented_boats %>%
            View("Rented Boats for multiday: have ability to be moved")
          # now we need to check if the boat is rented for that specific day
          message("Finding out if there is a boat already in that slot for that specific day...")
          message(paste("The boats rented for this day:",rented_boats_for_multiday$rb_number))
          if (the_boat_for_multiday_rb_number %in% rented_boats_for_multiday$rb_number) {
            
            # moveable_rented_boats contains all boats that can be moved
            
            message("That specific boat number is rented, we will need to move it to another boat that is not taken for that day!")
            
            
            # first find their rental_id
            renter_to_move_out <- moveable_rented_boats %>%
              filter(rb_number == the_boat_for_multiday_rb_number)
            
            
            rental_id_to_move_out <- renter_to_move_out$rental_id
            rental_number_to_move_out <- renter_to_move_out$rb_number
            
            
            # then find out where to move them into
            # find the minimum available number to move that boat
            # need to get the boats that are online and not rented for that day and fit the size and that were never a multiday
            
            # all boats for that size:
            # non_moveable_boats contains boats that are rented during this day and are multiday
            non_moveable_boats <- setdiff(moveable_rented_boats$rb_number,total_boats_for_size_requested$rb_number)
            
            # getting the boats that are not rented right now.
            free_boats <- setdiff(total_boats_for_size_requested$rb_number, append(moveable_rented_boats$rb_number,non_moveable_boats))
            
            message(paste("The boats that are rented for this day that CANNOT be moved:",non_moveable_boats))
            message(paste("The boats that are rented for this day that CAN be moved:",moveable_rented_boats$rb_number))
            message("")
            message(paste("The boats that are NOT rented for this day that CAN have boats moved into them:",free_boats))
            
            
            
            
            
            # now to find the minimum boat available
            rb_number_to_move_into <- min(free_boats)
            
            # now we need to get the rb_id_to_move_into
            rb_id_to_move_into <- active_boats$rb_id[match(rb_number_to_move_into,active_boats$rb_number)]
            
            
            
            # message(paste0("We are going to move the boat ID that was ",rental_id_to_move_out," to the rental boat ID: ",rb_id_to_move_into))
            message(paste0("We are going to move the boat number that was ",rental_number_to_move_out," to the rental boat number: ",rb_number_to_move_into))
            
            
            # now to move them from their original boat to their boat now
            # match the rental ID with the original database
            index_for_moved_renter <- match(rental_id_to_move_out,rental_boater$rental_id)
            
            # moving them to their new boat
            message(paste("Changing the rb_id of ",rental_boater$rb_id[index_for_moved_renter],"to",rb_id_to_move_into))
            message(paste("Changing the rb_number of ",rental_boater$rb_number[index_for_moved_renter],"to",rb_number_to_move_into))
            rental_boater$rb_id[index_for_moved_renter] <- rb_id_to_move_into
            rental_boater$rb_number[index_for_moved_renter] <- rb_number_to_move_into
            
            # ensure that we keep track of the change
            rental_boater$moved[index_for_moved_renter] <- 1
            
            
            
            
            
            
            
          } else {
            # 2b. if there is a boat already in that slot, select a slot that is open, and move 
            #        the single day boat that is in the way into that slot. Then, move the multiday 
            #        boat into that slot.
            message("That boat is not taken for that day, so we do not need to move anything")
            
          }
        }
        # 
        # now that we have gone through and moved all the boats out of the way, 
        # we will now put the muliday into that spot
        # this if statement should never fail, but just in case,
        # we will add an if statement to makes sure that there is not another multiday in it's spot
        # to do this if statement, we will grab all the boats that are rented between those dates,
        # and then we will test to see if the number for that rented boat is in that slot already
        rented_boats_for_multiday_length <- rental_boater[rental_boater$start_rental_date <= input$dates_of_rental_multi[1] & rental_boater$end_rental_date <= input$dates_of_rental_multi[2] & rental_boater$cancelled == 0, ]
        
        message("Unsure if this test should be here or what it does but here it goes:")
        message("Testing to see if there are any multiday boats in the future of this multiday")
        # if ("a" == "b") {
        # #(the_boat_for_multiday_rb_number %in% unique(rented_boats_for_multiday_length$rb_number)) {
        #   sendSweetAlert(
        #     session,
        #     title = "Error, this boat is rented!",
        #     text = "This should not be happening, this boat is rented after we already tried to move it",
        #     type = "error"
        #   )
        #   message("RED ALERT: The boats did not move around correctly, and therefor it is not going to work")
        
        # } else{
        message(paste0("There are no multiday boats in the future for boat number ",the_boat_for_multiday_rb_number))
        message("We can go ahead and put this person in this rentalboat for this day")
        # matching 
        multi_index_rb <- match(rb_id_to_move_into,rental_boats$rb_id)
        
        # calculating the final price for the renter
        # if greater than 5, then 10% discount
        if (amount_of_days_for_rental >= 5) {
          # the price for a single day * the amount of days for the rental * 90%
          total_price <- rental_boats$full_day_price[multi_index_rb]*amount_of_days_for_rental*0.90
          
        } else {
          # regular price
          total_price <- rental_boats$full_day_price[multi_index_rb]
          
        }
        
        
        # now we get to add the row to our dataframe
        
        message("Adding the row of the difficult multiday to the dataframe")
        
        # rental_boater <<- rental_boater %>% 
        #   add_row(rental_id = length(rental_boater$rb_id) + 1,
        #           email = input$email_rental_boater,
        #           rb_id = rb_id_to_move_into,
        #           phone_number_rental = input$phone_rental_boater,
        #           start_rental_date = input$dates_of_rental_multi[1],
        #           end_rental_date = input$dates_of_rental_multi[2],
        #           time_slot = input$time_slot,
        #           rb_number = rb_number_to_move_into,
        #           total_price = total_price ,
        #           deposit = total_price/2,
        #           tube = input$tube_rental,
        #           tube_price = input$cost_of_tube,
        #           multiday = 1,
        #           cancelled = 0,
        #           moved = 0
        #   )
        
        
        
        temp <- data.frame('rental_id' = length(rental_boater$rb_id) + 1,
                           'email' = input$email_rental_boater,
                           'rb_id' = the_boat_for_multiday_rb_id,
                           'phone_number_rental' = input$phone_rental_boater,
                           'start_rental_date' = input$dates_of_rental_multi[1],
                           'end_rental_date' = input$dates_of_rental_multi[2],
                           'time_slot' = input$time_slot,
                           'rb_number' = the_boat_for_multiday_rb_number,
                           'total_price' = total_price ,
                           'deposit' = total_price/2,
                           'tube' = input$tube_rental,
                           'tube_price' = input$cost_of_tube,
                           'multiday' = 1,
                           'cancelled' = 0,
                           'moved' = 0
        )
        
        rental_boater <- rbind(rental_boater,temp)
        
        # rental_boater %>% 
        #   add_row(rental_id = length(rental_boater$rb_id) + 1,
        #           email = input$email_rental_boater,
        #           rb_id = rb_id_to_move_into,
        #           phone_number_rental = input$phone_rental_boater,
        #           start_rental_date = input$dates_of_rental_multi[1],
        #           end_rental_date = input$dates_of_rental_multi[2],
        #           time_slot = input$time_slot,
        #           rb_number = rb_number_to_move_into,
        #           total_price = total_price ,
        #           deposit = total_price/2,
        #           tube = input$tube_rental,
        #           tube_price = input$cost_of_tube,
        #           multiday = 1,
        #           cancelled = 0,
        #           moved = 0
        #   ) %>%
        #   View("This data frame should have THE FUCKIN MULTIDAY BOAT IN IT PART 2")
        
        
        rental_boater %>%
          View("This data frame should have THE FUCKIN MULTIDAY BOAT IN IT")
        
        message("The multiday boat was added to the dataframe")
        
        write_csv(rental_boater, "data/rental_boater.csv")
        message("Wrote to the database")
        
        
        
        sendSweetAlert(
          session,
          title = "Difficult Multiday Success!!",
          text = "The multiday has been added. Some boats were moved in the process of doing so",
          type = "success"
        )
        
        # declaring the variable that will be used as the rental boat number 
        # with the officer package to create the word document that is then
        # saved to the local computer
        rental_boat_number_rented <- input$boat_to_rent_single
        
        
        # }
        
        
        
        
        
        
        
        
        # old code that i do not think that i will use, I may use it later
        # 
        # # free boats for that specific day
        # free_boats <- rental_boats %>%
        #   filter(rb_number %in% setdiff(rental_boats$rb_number, rented_boats$rb_number))
        # 
        # free_boats %>%
        #   View("Free Boats for multiday: step 1")
        # 
        # # step 2
        # # move the boat from the minimum boat number to boat into a slot that is opened
        # # first find the boat to take
        # free_boat_to_take <- free_boats %>%
        #   filter(rb_number == max(free_boats$rb_number))
        # 
        # 
        # # time to relate this to the rental_boats column
        # free_to_taken_rb_number
        
        
        
        
        
        
        
        
        
        
        
        
      } else {
        message("There is a boat available through all those dates")
        message("This should mean that we are good to go. There should be a boat or two all the way through that we can just rent.")
        amount_of_boats_available_all_the_way_through <- nrow(total_boats_for_size_requested) - nrow(rented_boats_for_multiday_length)
        boats_available_for_duration <- setdiff(total_boats_for_size_requested$rb_number,rented_boats_for_multiday_length$rb_number)
        message(paste0("We can put them in rental boats: ",boats_available_for_duration))
        
        # getting the rental boat that they will be in
        rb_number_to_move_into <- min(boats_available_for_duration)
        
        
        total_boats_for_size_requested %>%
          View("total_boats_for_size_requested")
        
        message(paste0("We will put them in rental boat ", rb_number_to_move_into))
        
        # now getting the rental boat id that they will be in
        rb_id_to_move_into <- total_boats_for_size_requested$rb_id[match(rb_number_to_move_into,total_boats_for_size_requested$rb_number)]
        
        # calculating the final price for the renter
        # if greater than 5, then 10% discount
        if (amount_of_days_for_rental >= 5) {
          # the price for a single day * the amount of days for the rental * 90%
          total_price <- rental_boats$full_day_price[match(rb_id_to_move_into,rental_boats$rb_id)]*amount_of_days_for_rental*0.90
          
        } else {
          # regular price
          total_price <- rental_boats$full_day_price[match(rb_id_to_move_into,rental_boats$rb_id)]
          
        }
        
        message(paste0("The price for this rental is ", total_price))
        
        message("Adding this rental to the database")
        # now we get to add the row to our dataframe
        
        
        rental_boater <<- rental_boater %>% 
          add_row(rental_id = length(rental_boater$rb_id) + 1,
                  email = input$email_rental_boater,
                  rb_id = rb_id_to_move_into,
                  rb_number = rb_number_to_move_into,
                  phone_number_rental = input$phone_rental_boater,
                  start_rental_date = input$dates_of_rental_multi[1],
                  end_rental_date = input$dates_of_rental_multi[2],
                  time_slot = input$time_slot,
                  total_price = total_price,
                  deposit = total_price/2,
                  tube = input$tube_rental,
                  tube_price = input$cost_of_tube,
                  multiday = 1,
                  cancelled = 0,
                  moved = 0
          )
        
        message("The multiday boat was added to the dataframe")
        
        write_csv(rental_boater, "data/rental_boater.csv")
        message("Wrote to the database")
        
        sendSweetAlert(
          session,
          title = "Easy Multiday Success!!",
          text = "The multiday has been added.",
          type = "success"
        )
        
        # declaring the variable that will be used as the rental boat number 
        # with the officer package to create the word document that is then
        # saved to the local computer
        rental_boat_number_rented <- input$boat_to_rent_single
        
      }
      
      
      # first need to check to see if any boats are fully available through those days
      # rented_boats contains all the boats that are rented through these multidays
      # i believe this will get the boats that are not rented for those selected dates
      # boats_available_for_duration <- setdiff(active_boats$rb_number,rented_boats$rb_number)
      # boats_available_for_duration %>% View("boats_available_for_duration")
      # message("Should be printing the boats that are available")
      # print(boats_available_for_duration)
      
      
      
      
      # then, if okay, need to add this boat to the database
      # if not okay, then we need to move boats around
      # the user needs to be notified what boats were moved around
      
      
      
      
      
      
      
      
      
      
      
      
    } else {
      
      #### Confirming Single Day Rental ----
      # single date
      # add it to the database
      
      # update customer database if the email is unique
      
      
      
      
      
      
      
      
      # update rental_boater data based on full day or half day
      if (input$len_day == "full_day") {
        
        # calculating total price
        total_price <- active_boats[active_boats$rb_number == input$boat_to_rent_single,]$full_day_price
        
        
        # adding for full day
        rental_boater <<- rental_boater %>% add_row(rental_id = length(rental_boater$rental_id) + 1, 
                                                    email = input$email_rental_boater, 
                                                    rb_id = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$rb_id, 
                                                    phone_number_rental = input$phone_rental_boater,
                                                    start_rental_date = input$dates_of_rental,
                                                    end_rental_date = input$dates_of_rental,
                                                    time_slot = input$time_slot,
                                                    rb_number = input$boat_to_rent_single,
                                                    total_price = total_price,
                                                    deposit = total_price / 2,
                                                    tube = input$tube_rental,
                                                    tube_price = input$cost_of_tube,
                                                    multiday = 0,
                                                    cancelled = 0,
                                                    moved = 0
        )
        
        # declaring the variable that will be used as the rental boat number 
        # with the officer package to create the word document that is then
        # saved to the local computer
        rental_boat_number_rented <- input$boat_to_rent_single
        
      } else {
        
        total_price <- active_boats[active_boats$rb_number == input$boat_to_rent_single,]$half_day_price
        
        # adding for single day
        rental_boater <<- rental_boater %>% add_row(rental_id = length(rental_boater$rental_id) + 1, 
                                                    email = input$email_rental_boater, 
                                                    rb_id = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$rb_id, 
                                                    phone_number_rental = input$phone_rental_boater,
                                                    start_rental_date = input$dates_of_rental,
                                                    end_rental_date = input$dates_of_rental,
                                                    time_slot = input$time_slot,
                                                    rb_number = input$boat_to_rent_single,
                                                    total_price = total_price,
                                                    deposit = total_price / 2,
                                                    tube = input$tube_rental,
                                                    tube_price = input$cost_of_tube,
                                                    multiday = 0,
                                                    cancelled = 0,
                                                    moved = 0
        )
        # declaring the variable that will be used as the rental boat number 
        # with the officer package to create the word document that is then
        # saved to the local computer
        rental_boat_number_rented <- input$boat_to_rent_single
      }
      
      # viewing the data
      # rental_boater %>%
      #   filter(start_rental_date == input$dates_of_rental) %>%
      #   View()
      
      
      # writing the data to the rental_boater dataset
      write_csv(rental_boater,"data/rental_boater.csv")
      
      
      # sending alert to tell the user that it was added to the data
      sendSweetAlert(
        session,
        title = "Successfully added to Schedule",
        text = paste0("Rental Boat #",input$boat_to_rent_single," has been added to the schedule for ", input$start_rental_date),
        type = "success"
      )
    }
    
    message("Filling out the rental sheet")
    # now to fill the document with the correct data
    output_doc <- officer::read_docx("DCM_Forms/Boat Rental Reservation.docx")
    
    
    # The code below grabs basic info and replaces it in the word document
    
    # the length of the rental
    message("Replacing: Day_type")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "Day_type",new_value = input$len_rental)
    
    
    # for a multiday, this will have to have an if statement to 
    # determine if a paste is necessary
    if (input$len_rental == "single") {
      # the Dates reserved for
      message("Replacing: The_Reserved_Date")
      output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "The_Reserved_Date",new_value = as.character(input$dates_of_rental))
      # max number of people allowed on the boat
      message("Replacing: max_people_allowed")
      output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "max_people_allowed",new_value = input$amount_of_people)
      
    } else {
      
      message("Replacing: The_Reserved_Date")
      output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "The_Reserved_Date",new_value = paste(as.character(input$dates_of_rental_multi[1]),"-",as.character(input$dates_of_rental_multi[2])))
      # max number of people allowed on the boat
      message("Replacing: max_people_allowed")
      output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "max_people_allowed",new_value = input$size_to_rent_multi)
      
    }
    
    # today's date
    message("Replacing: date_made")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "date_made",new_value = as.character(dev_date))
    
    # the DCM agent that made the reservation (initials only)
    message("Replacing: agent_made")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "agent_made",new_value = input$dcm_agent_for_rental) # going to have to make this an input
    
    
    # personal information
    # full name of customer
    message("Replacing: customer_full_name")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "customer_full_name",new_value = paste0(input$first_name_rental_boater," ",input$last_name_rental_boater))
    
    # the address of the customer
    message("Replacing: address_for_customer")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "address_for_customer",new_value = input$address_rental_boater)
    
    # the home telephone
    message("Replacing: home_phone")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "home_phone",new_value = input$phone_rental_boater)
    
    # the cell phone number
    message("Replacing: cell_phone")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "cell_phone",new_value = input$phone_rental_boater)
    
    # the email of the customer
    message("Replacing: CUSTOMER_EMAIL")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "CUSTOMER_EMAIL",new_value = input$email_rental_boater)
    
    
    # rental day information
    # boat that they rented
    message("Replacing: boat_number")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "boat_number",new_value = rental_boat_number_rented)
    
    
    
    # other?????
    message("Replacing: other_thing")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "other_thing",new_value = "What the hell is this")
    
    # tube pricing
    message("Replacing: tube_price")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "tube_price", new_value = "NTF") # need to finish
    
    # the DCM agent will fill out the amount of tube vests
    
    # the rental fee
    message("Replacing: RENTAL_FEE")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "RENTAL_FEE", new_value = as.character(total_price))
    
    # the rental deposit
    message("Replacing: rental_deposit")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "rental_deposit",new_value = as.character(total_price / 2))
    
    # will not put anything down for the extra fees that may occur
    
    # time due in
    message("Replacing: time_due_in")
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "time_due_in",new_value = input$time_slot)
    
    message("Finished filling it out")
    # outputting the document to a word file
    print(output_doc, target = paste0("confirmations/",dev_date, "_", input$email_rental_boater,"_Rental_Boats.docx"))
    
    message(paste0("Saved it locally with the name: ",dev_date, "_", input$email_rental_boater,"_Rental_Boats.docx"))
    
    
    
    
    # now for the emailing part
    # depending on what type of day it is depends on what email I send
    if (input$len_rental == "single" & input$len_day == "full_day") {
      
      # send the email for single full day
      single_day_full_confirmation(input$first_name_rental_boater,input$last_name_rental_boater,input$dates_of_rental,input$amount_of_people, strsplit(x = input$time_slot,split = " - ")[[1]][1], strsplit(x = input$time_slot,split = " - ")[[1]][2], total_price,input$last_four_card,total_price/2,input$email_rental_boater)
      
    } 
    
    if (input$len_rental == "single" & input$len_day == "half_day") {
      
      # send the email for a single half day
      single_day_half_confirmation(input$first_name_rental_boater,input$last_name_rental_boater,input$dates_of_rental,input$amount_of_people, total_price,input$last_four_card,input$email_rental_boater)
      
    }
    
    if (input$len_rental == "multi") {
      
      # send the email for a multiday
      multi_day_confirmation(first_name,last_name,starting_date,ending_date,maximum_occupacy, total_price,last_four_card,deposit,renter_email,strsplit(x = input$time_slot,split = " - ")[[1]][2]) 
      
    }
    
  }
  )
  
  
  
  
  
  
  
  #### do not rent edits
  observeEvent(input$confirm_do_not_rent_addition,{
    
    # add the customer to the do not rent to list
    # first check to make sure that they are not already added
    if (do_not_rent$email %in% input$do_not_rent_addition_email) {
      message("email is already there")
      sendSweetAlert(
        session,
        title = "Already exists",
        text = "That person already exists in the do not rent to list!",
        type = "error"
      )
      
      
    } else {
      message("That person is not in our do not rent to list yet")
      
      do_not_rent <- do_not_rent %>% addrow(email = input$do_not_rent_addition_email,
                                            rentable = 0,
                                            description = input$do_not_rent_addition_description)
      
      
      sendSweetAlert(
        session,
        title = "Individual Added",
        text = "That person has been added to our do not rent to list",
        type = "success"
      )
      
    }
    
    
    
  })
  
  # remove person from do not rent to list
  observeEvent(input$confirm_do_not_rent_removal, {
    
    if (do_not_rent$email %in% input$do_not_rent_removal_email) {
      message("The email entered is in the do not rent email column")
      
      # getting index value to test if the person was already removed or not
      index_value <- which(do_not_rent$email == input$do_not_rent_removal_email)
      
      if (do_not_rent$rentable[index_value] == 0) {
        message("They are already not rentable")
        sendSweetAlert(
          session,
          title = "They are already in!",
          text = "This individual is already not rentable",
          type = "error"
        )
        
      } else {
        message("They are rentable, they will now become unrentable")
        do_not_rent$rentable[index_value] <- 1
        
        write.csv(do_not_rent,file = "data/do_not_rent.csv")
        
        sendSweetAlert(
          session,
          title = "Added to do not rent list",
          text = "At one point, they were in the do not rent to list. Looks like they are back now.",
          type = "success"
        )
      }
      
      
    } else {
      message("They are new to the do not rent to list, so they will now be added")
      do_not_rent <<- do_not_rent %>% add_row(email = input$do_not_rent_removal_email,
                                              rentable = 0,
                                              description = input$do_not_rent_removal_description)
      
      write_csv(do_not_rent,file = "data/do_not_rent.csv")
      
    }
    
    
    
  })
  
  
  
  
  # Rental Cancellation ----
  observeEvent(input$search_email_for_cancelation,{
    
    potential_cancelations <- rental_boater %>%
      filter(email == input$renter_email_cancelation)
    
    
    output$dates_to_select_render <- renderUI({
      
      
      selectInput(
        inputId = "dates_to_select",
        label = "Starting Date to Cancel",
        choices = potential_cancelations$start_rental_date
      )
    })
    
    output$cancelation_button_to_render <- renderUI({
      actionButton(
        inputId = "confirm_cancelation",
        label = "Confirm Cancelation"
      )
      
    })
    
  })
  
  
  # cancelation button
  observeEvent(input$confirm_cancelation, {
    
    message("Getting information about the renter")
    message(paste0("Email of Cancelation: ",input$renter_email_cancelation))
    message(paste0("Date of Cancelation: ",input$dates_to_select))
    
    
    # cancelled_info <- rental_boater %>%
    #   filter(cancelled == 0 & email == input$renter_email_cancelation & start_rental_date == input$dates_to_select)
    # 
    # 
    # cancelled_first_name <- cancelled_info$last_name
    # cancelled_last_name <- cancelled_info$first_name
    
    
    
    message("Cancelation button was hit. We will now cancel the rental.")
    
    # rental_boater[rental_boater$email == input$renter_email_cancelation & rental_boater$start_rental_date == input$dates_to_select, "cancelled"] <- 1
    
    rental_boater %>%
      View("Before cancel")
    
    rental_boater <<- within(rental_boater, cancelled[email == input$renter_email_cancelation & start_rental_date == input$dates_to_select & cancelled == 0] <- 1)
    
    
    
    message("Changing the current potential cancelation rental email list")
    current_renter_emails <- rental_boater %>%
      filter(cancelled == 0)
    
    current_renter_emails <- current_renter_emails$email
    
    
    
    message("Writing to the rental_boats CSV")
    
    # rental_boater %>%
    #   View("Rental boater cancelled")
    
    write_csv(rental_boater,"data/rental_boater.csv")
    
    message("Alerting the individual that it was a success")
    
    sendSweetAlert(session,
                   title = "Success!!",
                   text = paste0(input$renter_email_cancelation,"'s rental reservation has been cancelled for ",input$dates_to_select),
                   type = "success"
                   
    )
    
    
    message("Updating the choices for cancelation incase they want to cancel another boat")
    updateSelectizeInput(
      session,
      inputId = "renter_email_cancelation",
      choices = current_renter_emails
    )
    
  })
  
  
  
  
  #### rental boat fleet edits ----
  
  # a new boat is added to the fleet
  observeEvent(input$confirm_rental_addition, {
    
    # add rental boat to fleet if it is unique
    
    print(input$new_rental_number)
    message(class(input$new_rental_number))
    print(active_boats$rb_number)
    message(class(active_boats$rb_number))
    
    
    if (active_boats$rb_number %in% input$new_rental_number) {
      
      
      
      sendSweetAlert(
        session,
        title = "Title",
        text = "That boat is already in the system!",
        type = "error"
        
      )
      
    } else {
      # add the row into our dataframe
      # attempting to add a row in the dataset in a better way
      # rental_boats[nrow(rental_boats) + 1,] = list(rb_id=length(rental_boats$rb_id) + 1,
      #                                              rb_number = input$new_rental_number,
      #                                              max_people = input$max_people,
      #                                              half_day_price = input$half_day_price_rental_addition,
      #                                              full_day_price = input$full_day_price_rental_addition,
      #                                              broke_down = 0,
      #                                              online = 1
      #                                              )
      # 
      rental_boats <<- rental_boats %>% add_row(rb_id = length(rental_boats$rb_id) + 1,
                                                rb_number = input$new_rental_number,
                                                max_people = input$max_people,
                                                half_day_price = input$half_day_price_rental_addition,
                                                full_day_price = input$full_day_price_rental_addition,
                                                broke_down = 0,
                                                online = 1)
      
      
      
      # writing the change to the csv
      write_csv(rental_boats,"data/rental_boats.csv")
      # rm(rental_boats)
      
      # rental_boats <- read_csv("data/rental_boats.csv")
      
      # updating the active_boats and broken_boats for additional data entry
      active_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 0)
      broken_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 1)
      
      
      sendSweetAlert(
        session,
        title = "Title",
        text = "That boat has been added to the system!",
        type = "success"
      )
    }
    
    
    
    
  })
  
  # remove the boat
  observeEvent(input$confirm_boat_remove,{
    
    boat_to_remove <- as.integer(input$boat_to_remove)
    
    # if the boat to remove is in the active boats, remove it
    if (active_boats$rb_number %in% boat_to_remove) {
      
      # removing boat from df
      rental_boats[rental_boats$rb_number == boat_to_remove & rental_boats$online == 1,]["online"] <- 0
      
      
      # change active boats and broken boats
      active_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 0)
      broken_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 1)
      
      
      # writing to csv
      write_csv(rental_boats,"data/rental_boats.csv")
      
      
      # send alert
      sendSweetAlert(
        session,
        title = "Boat Offline",
        text = "Boat has been removed from the system",
        type = "success"
      )
      
      
      
      
    } else {
      
      sendSweetAlert(
        ssession,
        title = "Whoah, that boat doesn't exist!",
        text = "Boat is already inactive or does not exist yet!",
        type = "error"
      )
      
      
      
    }
    
    
  })
  
  # broken down boat
  observeEvent(input$submit_broken_boat,{
    
    broken_boat_number <- as.integer(input$broken_boat_number)
    
    # first check to see if it is in active_boats
    if (active_boats$rb_number %in% broken_boat_number) {
      
      
      # change the that boat's number to 0
      rental_boats[rental_boats$rb_number == broken_boat_number & rental_boats$online == 1,]["broke_down"] <- 1
      # rental_boats %>% View()
      
      
      # change active boats and broken boats
      active_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 0)
      broken_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 1)
      
      
      
      # write it to CSV
      write_csv(rental_boats,"data/rental_boats.csv")
      
      
      # send an alert
      sendSweetAlert(
        session,
        title = "Boat Condition Updated",
        text = "This boat is now listed as broken down",
        type = "success"
        
      )
      
      
      
      
      
    } else {
      sendSweetAlert(
        session,
        title = "Error",
        text = "That boat is not listed as active!",
        type = "error"
      )
    }
    
    
    
  })
  
  observeEvent(input$confirm_fixed_boat,{
    
    
    fixed_boat_number <- as.integer(input$fixed_boat_number)
    
    # first check to see if it is in active_boats
    if ((broken_boats$rb_number == fixed_boat_number)) {
      
      
      # change the that boat's broke_down to 0
      rental_boats[rental_boats$rb_number == fixed_boat_number & rental_boats$online == 1,]["broke_down"] <- 0
      
      
      
      # change active boats and broken boats
      active_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 0)
      broken_boats <- rental_boats %>%
        filter(online == 1 & broke_down == 1)
      
      
      
      # write it to CSV
      write_csv(rental_boats,"data/rental_boats.csv")
      
      
      # send an alert
      sendSweetAlert(
        session,
        title = "Boat Condition Updated",
        text = "This boat is now listed as fixed!",
        type = "success"
        
      )
      
      
      
      
      
    } else {
      sendSweetAlert(
        session,
        title = "Error",
        text = "That boat is not listed as broken down!",
        type = "error"
      )
    }
    
    
    
  }
  )
  
  #### Full Calendar for the Rental
  # here, we will create the full calendar based on the existing rental boats
  
  
  output$rental_full_calendar <- renderFullcalendar({
    fullcalendar(full_calendar_rental_data)
  })
  
  
  
  #### Tab Book ----
  
  # you_owe_money_on_your_tab
  
  observeEvent(input$confirm_you_owe_money_on_your_tab, {
    inserted_row <- data.frame("Name" = "","Email" = "","Total" = 0)
    for (i in input$number_of_customers_to_email_you_owe_money_on_your_tab) {
      
      tab_emailing_data <- rbind(tab_emailing_data,inserted_row)
    }
    
    output$tab_customers_you_owe_money_on_your_tab <- renderRHandsontable({
      rhandsontable(tab_emailing_data)
    })
    
    
  })
  
  
  
  observeEvent(input$send_email_you_owe_money_on_your_tab,{
    
    tab_emailing_data <- as.data.frame(hot_to_r(input$tab_customers))
    
    tab_emailing_data %>%
      View("Before email")
    
    you_owe_money_on_your_tab(tab_emailing_data)
    
  })
  
  
  
  # We Ran your card for your tab
  
  observeEvent(input$confirm_we_ran_your_card, {
    inserted_row <- data.frame("Name" = "","Email" = "","Total" = 0)
    for (i in input$number_of_customers_to_email_we_ran_your_card) {
      
      tab_emailing_data <- rbind(tab_emailing_data,inserted_row)
    }
    
    output$tab_customers_we_ran_your_card <- renderRHandsontable({
      rhandsontable(tab_emailing_data)
    })
    
    
  })
  
  
  
  observeEvent(input$send_email_we_ran_your_card,{
    
    tab_emailing_data <- as.data.frame(hot_to_r(input$tab_customers))
    
    tab_emailing_data %>%
      View("Before email")
    
    we_ran_your_card_for_your_tab(tab_emailing_data)
    
  })
  
  
  # Email then Run
  
  observeEvent(input$confirm_we_ran_your_card, {
    inserted_row <- data.frame("Name" = "","Email" = "","Total" = 0)
    for (i in input$number_of_customers_to_email_we_ran_your_card) {
      
      tab_emailing_data <- rbind(tab_emailing_data,inserted_row)
    }
    
    output$tab_customers_we_ran_your_card <- renderRHandsontable({
      rhandsontable(tab_emailing_data)
    })
    
    
  })
  
  
  
  observeEvent(input$send_email_we_ran_your_card,{
    
    tab_emailing_data <- as.data.frame(hot_to_r(input$tab_customers))
    
    tab_emailing_data %>%
      View("Before email")
    
    email_then_run(tab_emailing_data)
    
  })
  
  
  
  # add a row
  # observeEvent(input$add_row_tab_customer,{
  #   
  #   tab_emailing_data <- as.data.frame(hot_to_r(input$tab_customers))
  #   
  #   print(tab_emailing_data)
  #   
  #   inserted_row <- data.frame("Name" = "","Email" = "","Total" = 0)
  #   
  #   # tab_emailing_data %>%
  #   #   View("email data")
  #   
  #   # inserted_row %>%
  #   #   View("inserted row")
  #   
  #   tab_emailing_data <- rbind(tab_emailing_data,inserted_row)
  #   
  #   message("outputting the table")
  #   # tab_emailing_data %>%
  #   #   View("post")
  #   
  #   output$tab_customers <- renderRHandsontable({
  #     rhandsontable(tab_emailing_data)
  #   })
  #   
  #   
  # })
  # 
  # # delete a row
  # observeEvent(input$delete_row_tab_customer,{
  #   
  #   tab_emailing_data <- as.data.frame(hot_to_r(input$tab_customers))
  #   
  #   
  #   tab_emailing_data <- head(tab_emailing_data,n = length(tab_emailing_data$Name) - 1)
  #   
  #   output$tab_customers <- renderRHandsontable({
  #     rhandsontable(tab_emailing_data)
  #   })
  #   
  #   
  # })
  
  
  
  
  
  
}

shinyApp(ui, server)