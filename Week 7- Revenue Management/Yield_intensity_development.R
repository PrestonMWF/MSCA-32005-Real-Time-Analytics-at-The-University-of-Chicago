library(tidyverse)
library(RM2)

#conducted this on different prices; example for $117
one_seventeen <- sold_tickets %>%
  filter(price == 117) %>%
  mutate(sold_tickets = seq(1, n(), 1)) %>%
  select(time, price, ticket_sold)

price_integrate <- function(x){
  sold_tickets <- sold_tickets %>%
    mutate(ticket_sold = seq(1, n(), 1))
  
  price_data <- rbind(price_data, sold_tickets)
}


price_data <- price_integrate()

write.csv(price_data, "ongoing_price_data.csv")

#something around 12 seems to be suitable
price_data %>%
  group_by(price) %>%
  summarise(tickets_sold = n(),
            time_span = difftime(time1 = max(time), 
                                 time2 = min(time)) %>%
              round(3) %>% 
              as.numeric())  %>%
  mutate(intensity = tickets_sold / time_span)

write_csv(x = price_data, "Price_intensity_development_data.csv")

#choice 1
fare <- sort(c(80, 110, 120), decreasing = TRUE)

intensity <- sort(c(12.8, 10.59, 5), decreasing = FALSE)

EMSRb(Fare = fare, 
      Mean = intensity *6,
      Var = intensity *6, 
      cap = 40)

(140 * 5) + (120 * 8) + (110 * 81)

#choice 2
fare <- sort(c(115, 125, 135, 145), decreasing = TRUE)

intensity <- sort(c(6.8, 7.8, 9.3, 10.3), decreasing = FALSE)


