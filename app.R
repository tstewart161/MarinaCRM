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

'%!in%' <- function(x,y)!('%in%'(x,y))

#### Read in CSV's ----

# customer
customer <-read_csv(file = "data/customer.csv")

# do not rent
do_not_rent <- read_csv(file = "data/do_not_rent.csv")



# rental boats
# need to specify active rental boats
rental_boats <- read_csv(file = "data/rental_boats.csv")
active_boats <- rental_boats %>%
  filter(online == 1 & broke_down == 0)


broken_boats <- rental_boats %>%
  filter(online == 1 & broke_down == 1)


# rental_boater
rental_boater <- read_csv(file = "data/rental_boater.csv")



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
full_calendar_rental_data$title <- with(full_calendar_rental_data, paste0(email," (",time_in,")"))


full_calendar_rental_data <- full_calendar_rental_data %>%
  select(title, start_rental_date, end_rental_date, color) %>% 
  rename(start = start_rental_date, end = end_rental_date)

full_calendar_rental_data %>%
  View("full_calendar_rental_data")


ui <- dashboardPage(
  dashboardHeader(title = "DCM Better Business Tool"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome Page", tabName = "welcome_page",icon = icon("home")),
      
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
            choices = c(Single = "single",
                        Multi = "multi")
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
              value = Sys.Date()
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
              label = "Starting and Ending Dates (Year-Month-Day)"
            )
          ),
          actionButton(
            inputId = "check_availability",
            label = "Check Availability"),
          
          tableOutput("multi_day_availability")
          
          
          
          
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
          )
          
          
        ),
        box(
          title = "Boat Information",
          # single, select the boat
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
            
            condition = "input.len_rental == 'multi'",
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
              h2("Rental Cancelation", align = "center"),
              h4("Active rentals", align = "left"),
              # active boats table
              
              DT::dataTableOutput("cancelActiveRentalsTable"),
              
              selectInput(
                inputId = "email_of_cancel",
                label = "Email of renter to cancel",
                choices = rental_boater$email
              ),
              # the date that they are going to cancel
              dateInput(
                inputId = "date_of_cancellation",
                label = "Date slot of renter to cancel",
                value = Sys.Date()
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
      )
    )
  )
)


server <- function(input, output, session) {
  
  # filling in the boxes for what boats are due in at what times
  output$due430 <- renderValueBox({
    due_in_430 <- rental_boater %>%
      filter(end_rental_date == Sys.Date() & time_slot == "8:30 - 4:30")
    
    
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
      filter(end_rental_date == Sys.Date() & time_slot == "9:00 - 5:00")
    
    
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
      filter(end_rental_date == Sys.Date() & time_slot == "9:30 - 5:30")
    
    
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
      filter(end_rental_date == Sys.Date() & time_slot == "10:00 - 6:00")
    
    
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
      rented_boats %>% View(title = "rented_boats")
      
      # the individual can only rent the boat if it is available for many days in a row
      # need to figure out if any of the 6,8, or 10 person boats are available for a multiday
      # if they are not available in a row, i need to see if they could be if we moved them around
      
      
      # first check 10 person boats
      # get max amount of 10 person boats
      count_of_boat_size <- length(active_boats %>%
                                filter(max_people == input$size_to_rent_multi))
      
      
      print(paste("There are",count_of_boat_size," ",input$size_to_rent_multi," person boats that are active"))
      
      
      # this small section of code retreives the amount of boats that are rented on each day
      df_of_10_p_boat <- rented_boats %>%
        filter(max_people == input$size_to_rent_multi)
      
      
      message("Getting the sequence of dates")
      
      
      # creating a list of days to go through to see how many boats are assigned to that date
      list_of_days <- seq(input$dates_of_rental_multi[1],input$dates_of_rental_multi[2],by ="day")
      # empty dataframe. will add to it through the for loop to keep track of days & how many boats are rented on those days
      boats_rented_per_day <- data.frame(business_date = as.Date(character()),
                                         boats_available = character(), 
                                         max_people_allowed = integer(),
                                         stringsAsFactors=FALSE) 
      
      
      
      message("Starting the for loop")
      
      
      # this for loop goes through and gets the count of all the dates for each boat, the 12, 10, 8, and 6 person boats
      # the goal of this loop is that if a 4 person boat was ever added to the fleet, then it would be able to handle that
      # it seems to be working
      for (max_people in unique(active_boats$max_people)) {
        
        
        for (i in 1:length(list_of_days)) {
          
          # length of the boats rented for that day
          number_of_boats_rented <- length(rented_boats[rented_boats$start_rental_date <= list_of_days[i] & list_of_days[i] <= rented_boats$end_rental_date, ]$rb_id)
          
          # calculating the amount of x person boats left
          boats_left <- count_of_boat_size - number_of_boats_rented
          
          # appending this to the dataframe
          boats_rented_per_day <- rbind(boats_rented_per_day, data.frame(business_date = as.Date(list_of_days[i]),boats_available = boats_left, max_people_allowed = as.integer(max_people)))
          
        }
        
      }
      
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
      minimum_availability %>%
        View(title = "minimum availability")
      
      
      # making it pretty for DCM agents
      minimum_availability_for_dcm_view <- minimum_availability %>%
        select(max_people_allowed, multi_day_availability)
      
      colnames(minimum_availability_for_dcm_view) <- c("Max People Allowed","Status")
      
      output$multi_day_availability <- renderTable(
        {minimum_availability_for_dcm_view}
      )
      
      
      minimum_availability %>%
        View("minimum availability, non dcm view")
      
      # getting the sizes that are available for the user to choose
      sizes_available <- minimum_availability %>%
        filter(multi_day_availability == "Available")
      
      print(sizes_available)
      
      boats_rented_per_day %>%
        View(title = "boats_rented_per_day")
      
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
        
        sendSweetAlert(
          session,
          title = paste0("This Date has Availability for ",input$amount_of_people ,"!"),
          text = "This date has boats that are available",
          type = "success"
        )
      }
      # output$checking_availability <- renderPrint({})
    }
  }
  )
  
  #### Confirming The Rental ----
  observeEvent(input$confirm_rental,{
    
    
    
    #### Updating Customer Table for Renters ---- 
    message("Starting the if statement for adding to customer data")
    print(customer$email %in% input$email_rental_boater)
    print(input$email_rental_boat)
    if (customer$email %in% input$email_rental_boater) {
      # then that email already exists in the database
      message("The rental boater that was inputted already exists in the database")
      
    } else {
      # add customer to the database
      message("Customer is being added to the database")
      customer %>%
        View()
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
    message("About to start the confirm multiday rental if statement")
    if (input$len_rental == "multi") {
      message("inside the confirming multiday rental")
      
      
      # putting someone in a multiday rental
      # not only will this put someone in a multiday rental, but it will also move people around, depending on where they are.
      
      
      # do not need to check if it is available, because there are already checks in place for that
      
      # total amount of days for the rental
      amount_of_days_for_rental <- input$dates_of_rental_multi[2] - input$dates_of_rental_multi[1]
      
      # getting the sequence of dates
      seq_of_dates_for_rental <- seq(as.Date(input$start_date), by = "day", length.out = amount_of_days_for_rental)
      
      
        
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
      rented_boats_for_multiday_length %>%
        View("rented_boats_for_multiday_length")
      
      
      # check if it is of length 0
      # if it is, that means that there are no boats available that are available through that entire selected time. fuck.
      if (nrow(rented_boats_for_multiday_length) == 0) {
        message("Fuck, there are no boats that are straight available. That means that we are going to have to pick one and make that rental happen while moving the rest of the boats. Will use a for loop.")
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
          filter(rn_number %in% setdiff(non_multiday_boats$rb_number, multi_day_at_one_point$rb_number))
        
        # now to select the boat that we will put the multiday in
        # this also meanst that this is the boat that will have to move
        the_boat_for_multiday <- never_multiday %>%
          fitler(rb_number == min(never_multiday$rb_number))
        
        # now to get the rb_number and the rb_id that we will be changing
        the_boat_for_multiday_rb_number <- the_boat_for_multiday$rb_number
        the_boat_for_multiday_rb_id <- the_boat_for_multiday$rb_number
        
        
        
        # quick popup message that will say "all these boats are multiday!!"
        if (nrow(rerented_boats_for_multiday_length) == 0) {
          sendSweetAlert(
            session,
            title = "Multiday Error!",
            text = "Apparently there are no boats available that are not multiday"
          )
          
        }
        
        # 2. create a for loop that goes through each day
        
        for (day in 1:days_of_rental) {
          
          
          #    2a. find out if there is a boat already taken in that slot
          
          # first, we need to get all the boats that are rented for that date
          rented_boats <- rental_boater[rental_boater$start_rental_date <= seq_of_dates_for_rental[day] & rental_boater$end_rental_date <= seq_of_dates_for_rental[day] & rental_boater$cancelled == 0, ]
          
          
          # further filter it to remove boats that are multidays in the future 
          moveable_rented_boats <- subset(rented_boats, !(rb_number %in% multi_day_at_one_point$rb_number))
          
          # add the column max_people onto the dataframe to further filter it later
          moveable_rented_boats <- merge(x = moveable_rented_boats, y = rental_boats[ , c("rb_id", "rb_number","max_people")], by = c("rb_id","rb_number"), all.x=TRUE)
          
          
          # fitler it to only include the correct size
          moveable_rented_boats <- moveable_rented_boats %>%
            filter(max_people == input$size_to_rent_multi)
          
          # now view the boats that have the potential to be moved
          rented_boats %>%
            View("Rented Boats for multiday: step 1")
                    # now we need to check if the boat is rented for that specific day
          
          if (the_boat_for_multiday_rb_number %in% rented_boats$rb_number) {
            
            message("That specific boat number is rented, we will need to move it to another boat that is not taken for that day!")
            # first find their rental_id
            rental_id_to_move_out <- rented_boats %>%
              filter(rented_boats$rb_number == the_boat_for_multiday_rb_number)$rental_id
            
            
            
            # find the minimum available number to move that boat
            # need to get the boats that are online and not rented for that day and fit the size
            available_boats_to_move <- rental_boats %>%
              filter(online == 1 & max_people == input$size_to_rent_multi & rb_number %!in% rented_boats$rb_number)
            
            
            # now to find the minimum boat available
            rb_id_to_move_into <- min(available_boats_to_move$rb_id)
            # based on the lowest rb_id, get the position where the rb_number_to_move is
            rb_number_to_move_into <- available_boats_to_move$rb_number[match(rb_id_to_move,available_boats_to_move$rb_id)]
            
            
            
            
            # now to move them from their original boat to their boat now
            # match the rental ID with the original database
            index_for_moved_renter <- match(rental_id,rental_boater)
            
            # moving them to their new boat
            rental_boater$rb_id[index_for_moved_renter] <<- rb_id_to_move_into
            rental_boater$rb_number[index_for_moved_renter] <<- rb_number_to_move_into
            
            # ensure that we keep track of the change
            rental_boater$moved[index_for_moved_renter] <<- 1
            
            
            
            
            
            
            
          } else {
            # 2b. if there is a boat already in that slot, select a slot that is open, and move 
            #        the single day boat that is in the way into that slot. Then, move the multiday 
            #        boat into that slot.
            message("That boat is not taken for that day, so we do not need to move anything")
            
          }
          
          # 
          # now that we have gone through and moved all the boats out of the way, 
          # we will now put the muliday into that spot
          # this if statement should never fail, but just in case,
          # we will add an if statement to makes sure that there is not another multiday in it's spot
          # to do this if statement, we will grab all the boats that are rented between those dates,
          # and then we will test to see if the number for that rented boat is in that slot already
          rented_boats_for_multiday_length <- rental_boater[rental_boater$start_rental_date <= input$dates_of_rental_multi[1] & rental_boater$end_rental_date <= input$dates_of_rental_multi[2] & rental_boater$cancelled == 0, ]
          
          if (the_boat_for_multiday_rb_number %in% unique(rented_boats_for_multiday_length$rb_number)) {
            sendSweetAlert(
              session,
              title = "Error, this boat is rented!",
              text = "This should not be happening, this boat is rented after we already tried to move it",
              type = "error"
            )
            message("RED ALERT: The boats did not move around correctly, and therefor it is not going to work")
            
          } else{
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
            
            
            rental_boater <<- rental_boater %>% 
              add_row(rental_id = length(rental_boater$rb_id) + 1,
                      email = input$email_rental_boater,
                      rb_id = rb_id_to_move_into,
                      phone_number_rental = input$phone_rental_boater,
                      start_rental_date = input$dates_of_rental_multi[1],
                      end_rental_date = input$dates_of_rental_multi[2],
                      time_slot = input$time_slot,
                      rb_number = rb_number_to_move_into,
                      total_price = total_price <- rental_boats$full_day_price[multi_index_rb],
                      deposit = total_price/2,
                      tube = input$tube_rental,
                      tube_price = input$cost_of_tube,
                      multiday = 1,
                      cancelled = 0,
                      moved = 0
                      )
            
            
            sendSweetAlert(
              session,
              title = "Difficult Multiday Success!!",
              text = "The multiday has been added. Some boats were moved in the process of doing so",
              type = "success"
            )
            
          }
          
          
          
          
          
          
          
          
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
          
          
          
          
          
          
          
        }
        
        
        
        
      } else {
        message("This should mean that we are good to go. There should be a boat or two all the way through that we can just rent.")
        
      }
      
      
      # first need to check to see if any boats are fully available through those days
      # rented_boats contains all the boats that are rented through these multidays
      # i believe this will get the boats that are not rented for those selected dates
      boats_available_for_duration <- setdiff(active_boats$rb_number,rented_boats$rb_number)
      boats_available_for_duration %>% View("boats_available_for_duration")
      message("Should be printing the boats that are available")
      print(boats_available_for_duration)
      
      
      
      
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
        
        # adding for full day
        rental_boater <<- rental_boater %>% add_row(rental_id = length(rental_boater$rental_id) + 1, 
                                                   email = input$email_rental_boater, 
                                                   rb_id = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$rb_id, 
                                                   start_rental_date = input$dates_of_rental,
                                                   end_rental_date = input$dates_of_rental,
                                                   time_slot = input$time_slot,
                                                   rb_number = input$boat_to_rent_single,
                                                   total_price = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$full_day_price,
                                                   deposit = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$full_day_price / 2,
                                                   tube = input$tube_rental,
                                                   tube_price = input$cost_of_tube,
                                                   multiday = 0,
                                                   cancelled = 0
        )
      } else {
        # adding for single day
        rental_boater <<- rental_boater %>% add_row(rental_id = length(rental_boater$rental_id) + 1, 
                                                   email = input$email_rental_boater, 
                                                   rb_id = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$rb_id, 
                                                   start_rental_date = input$dates_of_rental,
                                                   end_rental_date = input$dates_of_rental,
                                                   time_slot = input$time_slot,
                                                   rb_number = input$boat_to_rent_single,
                                                   total_price = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$half_day_price,
                                                   deposit = active_boats[active_boats$rb_number == input$boat_to_rent_single,]$half_day_price / 2,
                                                   tube = input$tube_rental,
                                                   tube_price = input$cost_of_tube,
                                                   multiday = 0,
                                                   cancelled = 0
        )
      }
      
      # viewing the data
      rental_boater %>%
        filter(start_rental_date == input$dates_of_rental) %>%
        View()
      
      
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
    
    # now to fill the document with the correct data
    output_doc <- officer::read_docx("/DCM_Emails/Boat Rental Reservation.docx")
    
    
    # The code below grabs basic info and replaces it in the word document
    
    # the length of the rental\
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "Day_type",new_value = "Single")
    
    # the Dates reserved for
    # for a multiday, this will have to have an if statement to 
    # determine if a paste is necessary
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "The_Reserved_Date",new_value = "12/03/2020")
    
    
    # today's date
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "date_made",new_value = as.character(Sys.Date()))
    
    # the DCM agent that made the reservation (initials only)
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "agent_made",new_value = "CCH")
    
    
    # personal information
    # full name of customer
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "customer_full_name",new_value = "Henley, Collin")
    
    # the address of the customer
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "address_for_customer",new_value = "12000 Sycamore Shoals Dr. Bumpass, Va.")
    
    # the home telephone
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "home_phone",new_value = "540-895-4042")
    
    # the cell phone number
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "cell_phone",new_value = "540-872-6378")
    
    # the email of the customer
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "CUSTOMER_EMAIL",new_value = "collinhenley24@gmail.com")
    
    
    # rental day information
    # boat that they rented
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "boat_number",new_value = "2")
    
    # max number of people allowed on the boat
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "max_people_allowed",new_value = "10")
    
    # other?????
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "other_thing",new_value = "What the hell is this")
    
    # tube pricing
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "tube_price", new_value = "$35")
    
    # the DCM agent will fill out the amount of tube vests
    
    # the rental fee
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "RENTAL_FEE",new_value = "$365.00")
    
    # the rental deposit
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "rental_deposit",new_value = "$182.50")
    
    # will not put anything down for the extra fees that may occur
    
    # time due in
    output_doc <- officer::body_replace_all_text(x = output_doc,old_value = "time_due_in",new_value = "4:30")
    
    
    # outputting the document to a word file
    # print(output_doc, target = paste("/confirmations/",NEED TO ENTER THE DATE HERE"_Rental_Boats.docx"))
    
    
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
      rental_boats %>% View()
      
      
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
  
  
  #### Cancelation page
  rental_boater_display <- rental_boater %>%
    select(email, start_rental_date, end_rental_date, time_slot)
  
  output$cancelActiveRentalsTable = DT::renderDataTable({
    rental_boater_display
  })
  
  observeEvent(input$button, {
    
    # filter to get the rental boater that needs to be cancelled
    cancel <- rental_boater %>% 
      filter(start_date > sys.Date() & email == input$email_of_cancel)
    
    
    # update the select input to have only that persons dates in it
    updateSelectInput(session = session,
                      inputid = "date_of_cancellation",
                      choices = cancel$start)
  
  })
}

shinyApp(ui, server)