#checking for existence, installing, loading packages for data analysis

if (require('tidyverse')==FALSE){install.packages("tidyverse")}
library('tidyverse')
if (require('dply')==FALSE){install.packages("dplyr")}
library('dplyr')




str(DelayedFlights)
dim(DelayedFlights)

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset

emp <- c() # Constraction of an empty vector
for (i in 1:30){
  emp <- c(emp,length(which(is.na(DelayedFlights[,i]))))
} # Gathering of the number of empty cells corresponding to each column of our dataset 
names(emp) <- 1:30
emp  # Α vector containing the number of empty cell for each variable

#Ερώτηση 2: να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων

delay <- matrix(0, nrow = 12, ncol = 31)
  for (i in 1:12){
    for(j in 1:31){
      delay[i,j] <- length(which((DelayedFlights$Month==i) & (DelayedFlights$DayofMonth==j))) 
  }
}
max_delay <- max(delay)
max_delay  # The maximum number of delays are 10857
which(delay == max_delay)  # The result of this function is 13 which means that the maximum value of delays is at the 1,2 element of matrix delay
 # As a result we had the most delays at January the 2nd.

#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008

mean_summer_delays <- c()
for (i in 6:8) {
  mean_summer_delays <- c(mean_summer_delays, (sum(delay[i,])/length(which(delay[i,] != 0))))
}
names(mean_summer_delays) <- c("June", "July", "August")
mean_summer_delays # A vector of the average number of delays during summer months

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β

b <- DelayedFlights %>%
  group_by(UniqueCarrier) %>%
  filter(CancellationCode == "B") %>%
  summarize(max_del = length(CancellationCode)) %>%
  arrange(-max_del)
b[1,]  # Company MQ has the most cancellations of type B

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων

library(data.table)
D <- data.table(DelayedFlights)
counts <- D[,.(Count = .N), by = FlightNum]
counts <- as.data.frame(counts)
flightnum <- counts %>%
  arrange(-Count)
head(flightnum, n = 10)  # The flight with FlightNum = 16 has 1586 delays which makes it the flight with the most delays

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις

max_dist1 <- DelayedFlights %>%
  filter(Distance == max(Distance)) %>%
  group_by(Dest) %>%
  select(Dest, Distance) %>%
  summarise(most_delays = length(Dest)) %>%
  arrange(-most_delays) 
head(max_dist1, n = 1) # The longest destination with the most delays is HNL with 266 delays.

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)

minutes_delay <- DelayedFlights %>%
  filter(Cancelled == 0) %>%
  group_by(Dest) %>%
  select(Dest, ActualElapsedTime) %>%
  arrange(-ActualElapsedTime)
head(minutes_delay, n = 10)   # Destination HNL had the longest delay
  
#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών

arrival_delay <- DelayedFlights %>%
  select(UniqueCarrier, LateAircraftDelay) %>%
  arrange(-LateAircraftDelay)
head(arrival_delay, n=1) # The company with the longest delays caused from LateAircraftDelat is AA

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα

thirteen <- c()
for (i in 1:12){
  thirteen <- c(thirteen, length(which(DelayedFlights$Month ==i & DelayedFlights$DayofMonth == 13 & DelayedFlights$CancellationCode == "A")))
}
names(thirteen) <- 1:12
thirteen # We had 2 cancellations of type A in November and 3 in December

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008

mean_april_delay <- DelayedFlights %>%
  filter(Month == 4 & DayofMonth >= 10 & DayofMonth <= 23) %>%
  summarise(mean_delay_april = mean(ArrDelay, na.rm=TRUE))
mean_april_delay # The average delay from 10 - 23 of April 2008 is 35.5 minutes 

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00

month_delay <- DelayedFlights %>%
  filter(DepTime >= 0600 & DepTime <= 1400) %>%
  select(Month, SecurityDelay) %>%
  arrange(-SecurityDelay)
head(month_delay, n = 1)
  
#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της

estimated_arrival_nov <- DelayedFlights %>%
  filter(DayofMonth >= 1 & DayofMonth <= 10 & Month == 11) %>%
  select(FlightNum, CRSElapsedTime) %>%
  arrange(-CRSElapsedTime)
head(estimated_arrival_nov, n = 1)

#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς

august_carrier_del <- DelayedFlights %>%
  filter(Month == 8 & DayofMonth > 10 & DayofMonth < 21 & CarrierDelay > 30) %>%
  select(Origin)
A <- data.table(august_carrier_del)
counts <- A[,.(Count = .N), by = Origin] 
counts <- as.data.frame(counts)
august <- counts %>%
  arrange(-Count)
head(august, n = 1) # Atlanta's airport had the most delayed flights with over 30 minutes delay


#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε

divflights <- DelayedFlights %>%
  filter(Diverted == 1 & Cancelled == 0) %>%
  select(FlightNum, AirTime) %>%
  arrange(-AirTime)
head(divflights, n = 20) # Variable AirTime contains only NA nalues for this subset of our dataset
  
#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης

max_deviation <- DelayedFlights %>%
  select(Month, ArrDelay) %>%
  group_by(Month) %>%
  summarize(deviations = sd(ArrDelay, na.rm=TRUE)) %>%
  arrange(-deviations)
head(max_deviation, n = 1) # The month with the biggest standard deviation of delays is December

#Τρόπος υποβολής εργασίας:αποστολή email στο smos@uom.edu.gr με το όνομα του github repository από το github
#όπου έχετε ανεβάσει το παραπάνω αρχείο με συμπληρωμένες τις αντίστοιχες ενότητες των απαντήσεων




