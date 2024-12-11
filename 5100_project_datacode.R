#data cleaning process
data<- read.csv("/.csv")
data$Arrest.Date <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", data$Arrest.Date), 
                           data$Arrest.Date, 
                           format(as.Date(data$Arrest.Date, format = "%m/%d/%Y"), "%Y-%m-%d"))
#after first chunk, i noticed problem year starts with 00 in some row not 20
#replace 00 with 20
data$Arrest.Date <- ifelse(grepl("^00\\d{2}-\\d{2}-\\d{2}$", data$Arrest.Date), 
                           sub("^00", "20", data$Arrest.Date), 
                           data$Arrest.Date)
data$Arrest.Date<- as.Date(data$Arrest.Date)
#subset data
arrest<- data
arrest<- arrest[c(2,3,5,7,13)]
# assign age group
library(dplyr)
arrest  <- arrest %>%
  mutate(age.group = case_when(
    Age <= 25 ~ "18-25",
    Age >25 & Age <= 30 ~ "26-30",
    Age>30 & Age <=35 ~ "31-35",
    Age >35 & Age <=45 ~ "36-45",
    Age >45 & Age <= 55 ~ "46-55",
    Age >55 ~ "56+"
  ))
# fraud and financial crime and release violation has 4 different category
#although they are similar
arrest <- arrest %>%
  mutate(Arrest.Category = case_when(
    grepl("Fraud and Financial Crime", Arrest.Category, ignore.case = TRUE) ~ "Fraud and Financial Crimes",
    grepl("Release Violations", Arrest.Category, ignore.case = TRUE) ~ "Release Violations",
    TRUE ~ Arrest.Category
  ))

#remove other crimes since it is ambiguous 
arrest <- arrest %>%
  filter(Arrest.Category != "Other Crimes")

arrest <- arrest %>%
  mutate(
    crime.category = case_when(
      grepl("Simple Assault", Arrest.Category, ignore.case = TRUE) ~ "Simple Assault",
      grepl("Release Violations", Arrest.Category, ignore.case = TRUE) ~ "Release Violation",
      grepl("Traffic Violations", Arrest.Category, ignore.case = TRUE) ~ "Traffic Violation",
      grepl("Narcotics|Driving/Boating While Intoxicated", Arrest.Category, ignore.case = TRUE) ~ "Substance related crime",
      grepl("Theft|Damage to Property|Property Crimes|Burglary|Robbery|Motor Vehicle Theft|Theft from Auto", Arrest.Category, ignore.case = TRUE) ~ "Theft Crime",
      grepl("Assault with a Dangerous Weapon|Assault on a Police Officer|Aggravated Assault|Arson|Homicide", Arrest.Category, ignore.case = TRUE) ~ "Violent Crimes",
      grepl("Weapon Violations|Liquor Law Violation|Disorderly Conduct|Gambling|Vending Violation|Fraud and Financial Crimes", Arrest.Category, ignore.case = TRUE) ~ "General Law Violation",
      grepl("Prostitution|Offenses Against Family & Children|Sex Offenses|Kidnapping|Sex Abuse", Arrest.Category, ignore.case = TRUE) ~ "Family,Sex related Crime",
      TRUE ~"other"
    )
  )
#divide data into age group for markov chain transition matrix
arrest<- arrest[!is.na(arrest$Arrest.Category),]
age_groups <- split(arrest, arrest$age.group)
age_18_25 <- age_groups[["18-25"]]
age_26_30 <- age_groups[["26-30"]]
age_31_35 <- age_groups[["31-35"]]
age_36_45 <- age_groups[["36-45"]]
age_46_55 <- age_groups[["46-55"]]
age_56 <- age_groups[["56+"]]

#divide data for bootstrap test
before_covid<- arrest[arrest$Arrest.Year<2020,]
covid<- arrest[arrest$Arrest.Year>2021,]

age_groups1 <- split(before_covid, before_covid$age.group)
age_18_25_1 <- age_groups1[["18-25"]]
age_26_30_1 <- age_groups1[["26-30"]]
age_31_35_1 <- age_groups1[["31-35"]]
age_36_45_1 <- age_groups1[["36-45"]]
age_46_55_1 <- age_groups1[["46-55"]]
age_56_1 <- age_groups1[["56+"]]


age_groups2 <- split(covid, covid$age.group)
age_18_25_2 <- age_groups2[["18-25"]]
age_26_30_2 <- age_groups2[["26-30"]]
age_31_35_2 <- age_groups2[["31-35"]]
age_36_45_2 <- age_groups2[["36-45"]]
age_46_55_2 <- age_groups2[["46-55"]]
age_56_2 <- age_groups2[["56+"]]


before_counts <- c(
  nrow(age_18_25_1), 
  nrow(age_26_30_1), 
  nrow(age_31_35_1), 
  nrow(age_36_45_1), 
  nrow(age_46_55_1), 
  nrow(age_56_1)
)

after_counts <- c(
  nrow(age_18_25_2), 
  nrow(age_26_30_2), 
  nrow(age_31_35_2), 
  nrow(age_36_45_2), 
  nrow(age_46_55_2), 
  nrow(age_56_2)
)

# bootstrapping
library(dplyr)
# set n for bootstraap 
n<- 5000
bootstrap_mean_diff <- function(data1, data2, n) {
  before_means <- numeric(n)
  covid_means <- numeric(n)
  
  # create loop for sampling
  for (i in 1:n) {
    before_sample <- sample(data1, size = length(data1), replace = TRUE)
    covid_sample <- sample(data2, size = length(data2), replace = TRUE)
    
    before_means[i] <- mean(before_sample)
    covid_means[i] <- mean(covid_sample)
  }
  
  diff_distribution <- covid_means - before_means
  observed_diff <- mean(data2) - mean(data1)
  #get pvalue for testing whether post covid has more covid
  p_value <- mean(diff_distribution >= observed_diff)
  
  list(observed_diff = observed_diff, p_value = p_value)
}

#setting dataframe that will be later used to put results inside dataframe
results <- data.frame(Age_Group = character(), Observed_Difference = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# get unique age groups
age_groups <- unique(before_covid$age.group)

for (group in age_groups) {
  data1 <- before_covid %>% filter(age.group == group) %>% pull(Age)
  data2 <- covid %>% filter(age.group == group) %>% pull(Age)
  #use bootstrap function created above to perfrom bootstrap
  result <- bootstrap_mean_diff(data1, data2, n)
  
  # insert results to the data frame created before
  results <- rbind(results, data.frame(
    age_group = group,
    observed_difference = result$observed_diff,
    p_value = result$p_value
  ))
}
results

#EDA 
before_counts <- table(before_covid$age.group)
after_counts <- table(covid$age.group)

before_proportions <- prop.table(before_counts)
after_proportions <- prop.table(after_counts)
age_groups <- intersect(names(before_counts), names(after_counts))
plot_data <- data.frame(
  Age.Group = rep(age_groups, 2),
  Proportion = c(as.numeric(before_proportions[age_groups]), as.numeric(after_proportions[age_groups])),
  Period = rep(c("Before COVID", " After  COVID"), each = length(age_groups))
)
plot_prop<- ggplot(plot_data, aes(x = Age.Group, y = Proportion, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "proportion of Arrests before and  after  COVID-19 by Age Group",
    x = "Age Group",
    y = "Proportion of Arrests",caption = "VIII_plot2"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white") )


crime_category_age_group <- arrest %>%
  group_by(age.group, crime.category) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = crime.category, values_from = count, values_fill = 0)

crime_category_age_group_long <- crime_category_age_group %>%
  pivot_longer(-age.group, names_to = "crime.category", values_to = "count")


plot2<-ggplot(crime_category_age_group_long, aes(x = age.group, y = count, fill = crime.category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Crime Categories by Age Group",
       x = "Age Group",
       y = "Number of Arrests",caption = "VIII_plot1") +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) 



#markov chain analysis:
#for all age group,proability for transition to other crimes(divide into crime groups manually)
transition_matrix <- function(group_data) {
  # Order input data by arrest date
  group_data <- group_data[order(group_data$Arrest.Date), ]
  
  # Initialize the Next.Crime.Group column
  group_data$Next.Crime.Group <- NA
  
  # using index to set next crime, next crime must be at least one day, some crime are in same day
  indexing <- which(group_data$Arrest.Date[-1] > group_data$Arrest.Date[-nrow(group_data)])
  
  # use index to assign next crime group from before
  group_data$Next.Crime.Group[indexing] <- group_data$crime.category[indexing + 1]
  
  # filtering rows that both crime group and next crime group are not NA
  transitions <- group_data[!is.na(group_data$crime.category) & !is.na(group_data$Next.Crime.Group), ]
  
  # get crime group that i set it before and name as crime type
  crime_types <- unique(c(transitions$crime.category, transitions$Next.Crime.Group))
  
  # creating matrix initializing with 0
  transition_matrix <- matrix(0, nrow = length(crime_types), ncol = length(crime_types),
                              dimnames = list(crime_types, crime_types))
  
  #use for loop to assign value for transition matrix
  for (i in 1:nrow(transitions)) {
    from <- transitions$crime.category[i]
    to <- transitions$Next.Crime.Group[i]
    transition_matrix[from, to] <- transition_matrix[from, to] + 1
  }
  
  # Convert counts to probabilities by dividing by row sums
  row_totals <- rowSums(transition_matrix)
  transition_matrix <- sweep(transition_matrix, 1, row_totals, "/")
  
  
  return(transition_matrix)
}

mat1<-transition_matrix(age_18_25)
mat2<-transition_matrix(age_26_30)
mat3<-transition_matrix(age_31_35)
mat4<-transition_matrix(age_36_45)
mat5<-transition_matrix(age_46_55)
mat6<-transition_matrix(age_56)


#creating plot by creating function since there are six age group

library(reshape2)
library(ggplot2)


visualize<- function(transition_matrix,age_group) {
  transition_mat <- as.data.frame.matrix(transition_matrix)
  transition_mat$Crime.Group <- rownames(transition_mat)
  transition_mat <- melt(transition_mat, id.vars = "Crime.Group")
  
  colnames(transition_mat) <- c("Current.Crime", "Next.Crime", "Probability")
  
  ggplot(transition_mat, aes(x = Next.Crime, y = Current.Crime, fill = Probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Probability, 2)), color = "black", size = 3) +
    scale_fill_gradient(low = "skyblue", high = "orange", name = "Transition\nProbability", limits = c(0, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white")) +
    labs(title = paste("Crime Type Transition Probabilities for Age Group:", age_group), 
         x = "Next Crime Group", 
         y = "Current Crime Group")
}

m1<-visualize(mat1,"18-25")
m2<-visualize(mat2,"26-30")
m3<-visualize(mat3,"31-35")
m4<-visualize(mat4,"36-45")
m5<-visualize(mat5,"46-55")
m6<-visualize(mat6,"56+")





