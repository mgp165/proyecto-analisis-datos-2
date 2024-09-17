data = read.csv('hotel_reservations.csv')
head(data)

noprecio = which(data$avg_price_per_room <= 10)
notipohabitacion = which(data$room_type_reserved=='Room_Type 3')
notiporeserva = which(data$market_segment_type == 'Aviation' | data$market_segment_type == 'Complementary')
nomenu = which(data$type_of_meal_plan == 'Meal Plan 3')
outlierprecio = which(data$avg_price_per_room == 540)
eliminados = c(noprecio, notipohabitacion, notiporeserva, nomenu)

newdata = data[-eliminados ,-c(1,10)]



### modelos

booking_canceled <- rep(1,35528)
booking_canceled[newdata$booking_status == 'Not_Canceled'] = 0

library('ggplot2')

reglog_todo = glm(booking_canceled ~ newdata$no_of_adults + newdata$no_of_children + newdata$repeated_guest  + newdata$no_of_previous_bookings_not_canceled + newdata$no_of_previous_cancellations + newdata$avg_price_per_room + newdata$no_of_week_nights + newdata$no_of_weekend_nights + newdata$type_of_meal_plan + newdata$no_of_special_requests + newdata$required_car_parking_space + newdata$room_type_reserved + newdata$market_segment_type + as.factor(newdata$arrival_month) + newdata$lead_time + newdata$arrival_date,  family=binomial)

summary(reglog_todo)

step(reglog_todo, direction = 'both')


reglog2 = glm(booking_canceled ~ newdata$no_of_adults + newdata$no_of_children + newdata$repeated_guest +
                newdata$no_of_previous_cancellations + 
                newdata$avg_price_per_room + newdata$no_of_week_nights + newdata$no_of_weekend_nights + 
                newdata$type_of_meal_plan + newdata$no_of_special_requests +
                newdata$required_car_parking_space + newdata$room_type_reserved +
                newdata$market_segment_type + as.factor(newdata$arrival_month) + newdata$lead_time,  family=binomial)
summary(reglog2)


newdata$yhat2 = predict(reglog2, newdata, type = "response")
datarank2 = rank(newdata$yhat2)

ggplot(newdata) +
  geom_point(aes(x = datarank2, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank2, y = newdata$yhat2, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.1 < newdata$yhat2) & (newdata$yhat2 < 0.9) ) / nrow(newdata))


step(reglog2, direction='both')


mealselected <- rep(1,35528)
mealselected[newdata$type_of_meal_plan == 'Not Selected'] = 0


reglog3 =  glm(booking_canceled ~ newdata$no_of_adults + newdata$no_of_children + newdata$repeated_guest +
                 newdata$no_of_previous_cancellations + 
                 newdata$avg_price_per_room + newdata$no_of_week_nights + newdata$no_of_weekend_nights + 
                 mealselected + newdata$no_of_special_requests +
                 newdata$required_car_parking_space + newdata$room_type_reserved +
                 newdata$market_segment_type + as.factor(newdata$arrival_month) + newdata$lead_time,  family=binomial)
summary(reglog3)


newdata$yhat3 = predict(reglog3, newdata, type = "response")
datarank3 = rank(newdata$yhat3)

ggplot(newdata) +
  geom_point(aes(x = datarank3, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank3, y = newdata$yhat3, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.1 < newdata$yhat3) & (newdata$yhat3 < 0.9) ) / nrow(newdata))


step(reglog3, direction = 'both')


reglog4 =  glm(booking_canceled ~ newdata$no_of_children + newdata$repeated_guest +
                 newdata$no_of_previous_cancellations + 
                 newdata$avg_price_per_room + newdata$no_of_week_nights + newdata$no_of_weekend_nights + 
                 mealselected + newdata$no_of_special_requests +
                 newdata$required_car_parking_space + newdata$room_type_reserved +
                 newdata$market_segment_type + as.factor(newdata$arrival_month) + 
                 newdata$lead_time,  family=binomial)
summary(reglog4)


newdata$yhat4 = predict(reglog4, newdata, type = "response")
datarank4 = rank(newdata$yhat4)

ggplot(newdata) +
  geom_point(aes(x = datarank4, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank4, y = newdata$yhat4, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.1 < newdata$yhat4) & (newdata$yhat4 < 0.9) ) / nrow(newdata))


reglog5 = glm(booking_canceled ~ newdata$repeated_guest +
                newdata$no_of_previous_cancellations + 
                newdata$avg_price_per_room + newdata$no_of_week_nights + newdata$no_of_weekend_nights + 
                mealselected + newdata$no_of_special_requests +
                newdata$required_car_parking_space + newdata$room_type_reserved +
                newdata$market_segment_type + as.factor(newdata$arrival_month) + 
                newdata$lead_time,  family=binomial)
summary(reglog5)


newdata$yhat5 = predict(reglog5, newdata, type = "response")
datarank5 = rank(newdata$yhat5)

ggplot(newdata) +
  geom_point(aes(x = datarank5, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank5, y = newdata$yhat5, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.25 < newdata$yhat5) & (newdata$yhat5 < 0.75) ) / nrow(newdata))


reglog6 = glm(booking_canceled ~ newdata$lead_time, family=binomial)
summary(reglog6)


newdata$yhat6 = predict(reglog6, newdata, type = "response")
datarank6 = rank(newdata$yhat6)

ggplot(newdata) +
  geom_point(aes(x = datarank6, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank6, y = newdata$yhat6, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.1 < newdata$yhat6) & (newdata$yhat6 < 0.9) ) / nrow(newdata))


reglog7 = glm(booking_canceled ~ newdata$lead_time + newdata$avg_price_per_room + newdata$no_of_special_requests, family=binomial)
summary(reglog7)

newdata$yhat7 = predict(reglog7, newdata, type = "response")
datarank7 = rank(newdata$yhat7)

ggplot(newdata) +
  geom_point(aes(x = datarank7, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank7, y = newdata$yhat7, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.1 < newdata$yhat7) & (newdata$yhat7 < 0.9) ) / nrow(newdata))


reglog8 = glm(booking_canceled ~ 1, family=binomial)
summary(reglog8)

newdata$yhat8 = predict(reglog8, newdata, type = "response")
datarank8 = rank(newdata$yhat8)

ggplot(newdata) +
  geom_point(aes(x = datarank8, y = booking_canceled, colour='Cancel')) +
  geom_point(aes(x = datarank8, y = newdata$yhat8, colour='Logistic Regression')) +
  scale_color_manual(values = c("Cancel" = '#ff00ff', 'Logistic Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Cancel probability')

sprintf("%0.f%%", 100*sum( (0.1 < newdata$yhat8) & (newdata$yhat8 < 0.9) ) / nrow(newdata))


reglin_todo <- lm(newdata$avg_price_per_room ~ newdata$no_of_adults + newdata$no_of_children +
                    newdata$no_of_week_nights + newdata$no_of_weekend_nights + 
                    mealselected + newdata$no_of_special_requests +
                    newdata$required_car_parking_space + newdata$room_type_reserved +
                    newdata$market_segment_type + as.factor(newdata$arrival_month) + newdata$lead_time)

summary(reglin_todo)
plot(reglin_todo)


pricehat = predict(reglin_todo, newdata)
pricehatrank = rank(pricehat)

ggplot(newdata) +
  geom_point(aes(x = pricehatrank, y = newdata$avg_price_per_room, colour='Price')) +
  geom_point(aes(x = pricehatrank, y = pricehat, colour='Linear Regression')) +
  scale_color_manual(values = c("Price" = '#ff00ff', 'Linear Regression' = '#33ff36')) +
  xlab(label = 'Predicted rank') +
  ylab(label = 'Average price')





































