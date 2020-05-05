# functions for the DCM application


# general form for functions
# function_name <- function() {
#   # code
#   # return(variable_to_return)
# }





#### typical rental boat email functions ----

# standard single day rental
single_day_full_confirmation <- function(first_name,last_name,date_of_rental,maximum_occupacy, pickup_time, drop_off_time, total_price,last_four_card,deposit,renter_email) {
  
  # this function will send the confirmation email
  
  single_day_body <- paste0("Dear ", first_name," ",last_name, ",", "
                            
  This email is to confirm your reservation made to rent a pontoon boat on ", date_of_rental,". This boat holds no more than ", maximum_occupacy," people.No matter what size or weight a person is, infant to adult, they count as one person. Please do not bring more people than are allowed on the boat. Your boat can be picked up any time after ", pickup_time," AM and must be returned no more than 8 hours later at ",drop_off_time," PM. If you do not pick the boat up by 12 noon you forfeit your deposit and rights to the boat. The price for the rental is $",total_price," plus the gas, oil and taxes. 
    
  [] By putting a 'X' in the box that precedes this sentence and sending this email back to DUKES_CREEK_EMAIL@GMAIL.COM you are agreeing that the above information is correct and give us permission to run your card ending in ", last_four_card," for a deposit of $",deposit,".
  Please be aware by allowing us to run your card you are agreeing to our cancellation policy. This policy states that you must cancel at least 72 hours prior to your reservation, or your deposit will not be refunded. This means the latest you can cancel or reschedule is by 10:00 AM on ",date_of_rental - 3,". If you do cancel or reschedule on time, there will still be a cancellation fee of $20.00, which means only a total of $",deposit - 20," will be refunded back onto your card.  If you reschedule, a fee of $20.00 will be taken out of your deposit.  
  Also, if it is raining and the day is a complete washout, we will credit your deposit for another day's use in the calendar year. Please be aware we do not give rain checks based on weather predictions, only on the weather that actually happens that day. 
    
  Please be aware that no response to this email and failure to check the box within 24 hrs will result in the cancellation of your rental.

  Thank you for your reservation.")
  
  send.mail(from = "better.business.apps@gmail.com",
            to = c(renter_email),
            #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
            subject = "Boat Reservation",
            body = single_day_body,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            debug = TRUE)
  
  
}


# standard email for a single half day rental

single_day_half_confirmation <- function(first_name,last_name,date_of_rental,maximum_occupacy, total_price,last_four_card,deposit,renter_email) {
  half_day_body <- paste0("Dear ", first_name," ",last_name,",",
                          "This email is to confirm your reservation to rent a pontoon boat on ",date_of_rental,"." ,
                          "This boat holds no more than ",maximum_occupacy," people. 
No matter what size or weight a person is, infant to adult, they count as one person. 
Please do not bring more people than are allowed on the boat. 
Your boat can be picked up any time after 8:30 AM but must be picked up no later than 12:00 PM. 
It will need to be returned 4 hours after pickup. 
If you do not pick the boat up by 12 noon you forfeit your deposit and rights to the boat. 
The price for the rental is $",total_price," plus the gas, oil and taxes. 
  
  [] By putting an “X” in the box that is preceding this sentence and sending this email back to DCM@GMAIL.COM you are agreeing that the above information is correct and give us permission to run your card ending in ", last_four_card," for a deposit of $",total_price/2,".",
                          "Please be aware by allowing us to run your card you are agreeing to our cancellation policy. 
  This policy states that you must cancel at least 72 hours prior to your reservation, or your deposit will not be refunded. 
  This means the latest you can cancel or reschedule is by 10:00 AM on ", date_of_rental - 3,".", 
                          "If you do cancel or reschedule on time, there will still be a cancellation fee of $20.00, which means only a total of $",deposit - 20," will be refunded back onto your card.  
  If you reschedule, a fee of $20.00 will be taken out of your deposit.  
  Also, if it is raining and the day is a complete washout, we will credit your deposit for another day’s use in the calendar year. Please be aware we do not give rain checks based on weather predictions, only on the weather that actually happens that day. 
                                                                                                                                                                                                                  
Please be aware that no response to this email and failure to check the box within 24 hrs will result in the cancellation of your rental.

Thank you for your reservation.")
  
  
  
  send.mail(from = "better.business.apps@gmail.com",
            to = c(renter_email),
            #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
            subject = "Boat Reservation",
            body = half_day_body,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            debug = TRUE)
  
  
  
}





# multiday boat rental

multi_day_confirmation <- function(first_name,last_name,starting_date,ending_date,maximum_occupacy, total_price,last_four_card,deposit,renter_email,due_in_time) {
  
  multiday_body <- paste0("Dear ",first_name," ",last_name,",",
                          
                          "This email is to confirm your reservation to rent a pontoon boat from ",starting_date," to ", ending_date,".", "This boat holds no more than ",max_occupacy," people. No matter what size a person is, infant to adult, they count as one person. Please do not bring more people than are allowed on the boat.  Your boat can be picked up any time after 8:30 AM on ",starting_date," and must be returned by ",due_in_time," PM on ",ending_date,". The price for the rental is $",total_price," plus the gas and oil you use, and taxes.
    
[] By putting an “X” in the highlighted box that precedes this sentence and sending this email back to DCM@GMAIL.COM you are agreeing that the above information is correct and give us permission to run your card ending in ", last_four_card," for a deposit of $,",total_price/2,".
Please be aware, by allowing us to run your card, you are agreeing to our cancellation policy. This policy states that you must cancel or reschedule at least 72 hours prior to your reservation, or your deposit will not be refunded. This means the latest you can cancel or reschedule is by 10:00 AM on ",starting_date,". If you do cancel or reschedule on time there will still be a cancellation fee of $20.00 per day, which means only a total of $",total_price/2 - 20*(ending_date-starting_date)," will be refunded back onto your card. If you reschedule, a fee of $20.00 will be taken out of your deposit. 
The policy also states that there is no refund for rain, and that we do not give rain checks for any day during multiple day boat rentals.
No response to this email and failure to check the box within 24 hrs will result in the cancellation of your rental. 
Thank you for your reservation.")
  
  
  send.mail(from = "better.business.apps@gmail.com",
            to = c(renter_email),
            #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
            subject = "Boat Reservation",
            body = half_day_body,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            debug = TRUE)
  
  
  
}












#### Tab Book Stuff ----

# you owe money on your tab
you_owe_money_on_your_tab <- function(tab_emailing_data) {
  
  
  message("Starting the emails...")
  for (i in 1:length(tab_emailing_data$Name)) {
    message(paste0("Emailing ",tab_emailing_data$Name[i], " Email adress: ",tab_emailing_data$Email[i]))
    
    email_body <- paste0("Dear ",tab_emailing_data$Name[i],"

It's the beginning of the month again which means it's time to clear out tab sheets. You have a balance of $",tab_emailing_data$Total[i] ," on your tab. If you could please contact us or stop by with a payment or a credit card number, we would appreciate it. Thank you.
")
    
    email_to <- tab_emailing_data$Email[i]
    send.mail(from = "better.business.apps@gmail.com",
              to = c(as.character(email_to)),
              #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
              subject = "You Owe Money on Your Tab",
              body = email_body,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              debug = TRUE)
    
    
  }
}





# we ran your card for your tab
we_ran_your_card_for_your_tab <- function(tab_emailing_data) {
  
  
  message("Starting the emails...")
  for (i in 1:length(tab_emailing_data$Name)) {
    message(paste0("Emailing ",tab_emailing_data$Name[i], " Email adress: ",tab_emailing_data$Email[i]))
    
    email_body <- paste0("Hello ",tab_emailing_data$Name[i], "!

It's that time of the month again, so we ran your tab for a total of $",tab_emailing_data$Total[i],". This clears out your tab.  If you have any questions or concerns, please contact us at 540-895-5065. Thank you")
    
    email_to <- tab_emailing_data$Email[i]
    send.mail(from = "better.business.apps@gmail.com",
              to = c(as.character(email_to)),
              #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
              subject = "You Owe Money on Your Tab",
              body = email_body,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              debug = TRUE)
    
    
  }
}


# email then run
email_then_run <- function(tab_emailing_data) {
  
  
  message("Starting the emails...")
  for (i in 1:length(tab_emailing_data$Name)) {
    message(paste0("Emailing ",tab_emailing_data$Name[i], " Email adress: ",tab_emailing_data$Email[i]))
    
    email_body <- paste0("Dear ",tab_emailing_data$Name[i] ,",

It's the beginning of the month again which means it's time to clear out tabs. We wanted to make sure that it was okay to run your card for $",tab_emailing_data$Total[i],"on the card ending in (last 4 digits). Hope to hear from you soon. Thank you.
")
    
    email_to <- tab_emailing_data$Email[i]
    send.mail(from = "better.business.apps@gmail.com",
              to = c(as.character(email_to)),
              #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
              subject = "You Owe Money on Your Tab",
              body = email_body,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              debug = TRUE)
    
    
  }
}


#### Monthly Storage ----


monthly_storage_due <- function(tab_emailing_data) {
  
  
  message("Starting the emails...")
  for (i in 1:length(tab_emailing_data$Name)) {
    message(paste0("Emailing ",tab_emailing_data$Name[i], " Email adress: ",tab_emailing_data$Email[i]))
    
    email_body <- paste0("")
    
    email_to <- tab_emailing_data$Email[i]
    send.mail(from = "better.business.apps@gmail.com",
              to = c(as.character(email_to)),
              #replyTo = c("Reply to someone else <someone.else@gmail.com>"),
              subject = "You Owe Money on Your Tab",
              body = email_body,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "better.business.apps", passwd = "BetterB12000", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              debug = TRUE)
    
    
  }
}








































































