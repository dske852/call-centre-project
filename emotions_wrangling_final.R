#!!!PREPARATION TO RUN THE COD!!!!#
turn_data_correct <-NAME OF YOUR DATASET FOR CORRECT TURN DATASET (new one)
call_data_correct <-NAME OF YOUR DATASET FOR CORRECT CALL DATASET (new one)

library(mltools)
library(vctrs)
library(hms)
library(data.table)
library(tidyverse)
####################


negative_ratio
###Scoring system for the bad emotions during the call - Customer (BadEmotionsScore)####
turn_data_summary<-turn_data_correct%>%mutate_if(is.character, as.factor)%>%rename(Dial_Str=`Dialogue Structure`)
turn_data_summary<-filter(turn_data_summary, Dial_Str != "missing")

newdata <- mltools::one_hot(as.data.table(turn_data_summary), cols=c("Dial_Str", "Emotion")) #one-hot encoding

customer_index_data<-filter(newdata, Person == "customer") #customer data

#rewriting one-hot encoding value in the way that given emotions and call stages have different importance (weights) for the total score
customer_index_data$Dial_Str_nds<-ifelse(customer_index_data$Dial_Str_nds==1, 0, 0)
customer_index_data$Dial_Str_back_office<-ifelse(customer_index_data$Dial_Str_back_office==1, 2, 0)
customer_index_data$Dial_Str_consultation <-ifelse(customer_index_data$Dial_Str_consultation==1, 1, 0)
customer_index_data$Dial_Str_customer_recognition <-ifelse(customer_index_data$Dial_Str_customer_recognition==1, 0, 0)
customer_index_data$Dial_Str_farewell  <-ifelse(customer_index_data$Dial_Str_farewell ==1, 4, 0)
customer_index_data$Dial_Str_follow_procedure  <-ifelse(customer_index_data$Dial_Str_follow_procedure ==1, 0, 0)
customer_index_data$Dial_Str_greeting  <-ifelse(customer_index_data$Dial_Str_greeting ==1, 0, 0)
customer_index_data$Dial_Str_mechanic_planned  <-ifelse(customer_index_data$Dial_Str_mechanic_planned ==1, 2, 0)
customer_index_data$Dial_Str_my_problem_is   <-ifelse(customer_index_data$Dial_Str_my_problem_is  ==1, 1, 0)
customer_index_data$Dial_Str_solved   <-ifelse(customer_index_data$Dial_Str_solved  ==1, 4, 0)
customer_index_data$Dial_Str_suggest_solution    <-ifelse(customer_index_data$Dial_Str_suggest_solution   ==1, 3, 0)
customer_index_data$Dial_Str_transfer    <-ifelse(customer_index_data$Dial_Str_transfer   ==1, 2, 0)
#emotions
customer_index_data$Emotion_angry    <-ifelse(customer_index_data$Emotion_angry   ==1, 2, 0)
customer_index_data$Emotion_fear    <-ifelse(customer_index_data$Emotion_fear   ==1, 1, 0)
customer_index_data$Emotion_happy    <-ifelse(customer_index_data$Emotion_happy   ==1, -1, 0)
customer_index_data$Emotion_sad    <-ifelse(customer_index_data$Emotion_sad   ==1, 1, 0)
customer_index_data$Emotion_nvt   <-ifelse(customer_index_data$Emotion_nvt   ==1, 0, 0)

#summing all together
customer_index_data$w.Dial_Str_nds<-customer_index_data$Dial_Str_nds*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_nds*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_nds*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_nds*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_back_office<-customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_consultation<-customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_customer_recognition<-customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_farewell<-customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_follow_procedure<-customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_greeting<-customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_mechanic_planned<-customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_my_problem_is<-customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_solved<-customer_index_data$Dial_Str_solved*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_solved*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_solved*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_solved*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_suggest_solution<-customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_transfer<-customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_sad

#grouping and summing by each call
customer_index_data$index<- rowSums(customer_index_data[,c(24:35)])
groups<-customer_index_data%>%group_by(AO_transcription_id)%>%summarise(BadEmotionsScore_Cust=sum(index))
unique(groups$suming)

###agent

agent_index_data<-filter(newdata, Person == "agent") #agent data

#rewriting one-hot encoding value in the way that given emotions and call stages have different importance (weights) for the total score

agent_index_data$Dial_Str_nds<-ifelse(agent_index_data$Dial_Str_nds==1, 0, 0)
agent_index_data$Dial_Str_back_office<-ifelse(agent_index_data$Dial_Str_back_office==1, 2, 0)
agent_index_data$Dial_Str_consultation <-ifelse(agent_index_data$Dial_Str_consultation==1, 3, 0)
agent_index_data$Dial_Str_customer_recognition <-ifelse(agent_index_data$Dial_Str_customer_recognition==1, 0, 0)
agent_index_data$Dial_Str_farewell  <-ifelse(agent_index_data$Dial_Str_farewell ==1, 4, 0)
agent_index_data$Dial_Str_follow_procedure  <-ifelse(agent_index_data$Dial_Str_follow_procedure ==1, 0, 0)
agent_index_data$Dial_Str_greeting  <-ifelse(agent_index_data$Dial_Str_greeting ==1, 1, 0)
agent_index_data$Dial_Str_mechanic_planned  <-ifelse(agent_index_data$Dial_Str_mechanic_planned ==1, 2, 0)
agent_index_data$Dial_Str_my_problem_is   <-ifelse(agent_index_data$Dial_Str_my_problem_is  ==1, 1, 0)
agent_index_data$Dial_Str_solved   <-ifelse(agent_index_data$Dial_Str_solved  ==1, 4, 0)
agent_index_data$Dial_Str_suggest_solution    <-ifelse(agent_index_data$Dial_Str_suggest_solution   ==1, 3, 0)
agent_index_data$Dial_Str_transfer    <-ifelse(agent_index_data$Dial_Str_transfer   ==1, 2, 0)
#emotions
agent_index_data$Emotion_angry    <-ifelse(agent_index_data$Emotion_angry   ==1, 2, 0)
agent_index_data$Emotion_fear    <-ifelse(agent_index_data$Emotion_fear   ==1, 2, 0)
agent_index_data$Emotion_happy    <-ifelse(agent_index_data$Emotion_happy   ==1, -1, 0)
agent_index_data$Emotion_sad    <-ifelse(agent_index_data$Emotion_sad   ==1, 1, 0)
agent_index_data$Emotion_nvt   <-ifelse(agent_index_data$Emotion_nvt   ==1, 0, 0)

#summing all together

agent_index_data$w.Dial_Str_nds<-agent_index_data$Dial_Str_nds*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_nds*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_back_office<-agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_consultation<-agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_customer_recognition<-agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_farewell<-agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_follow_procedure<-agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_greeting<-agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_mechanic_planned<-agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_my_problem_is<-agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_solved<-agent_index_data$Dial_Str_solved*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_solved*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_suggest_solution<-agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_transfer<-agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_sad

#grouping and summing by each call

agent_index_data$index<- rowSums(agent_index_data[,c(24:35)])
groups2<-agent_index_data%>%group_by(AO_transcription_id)%>%summarise(BadEmotionsScore_Empl=sum(index))
unique(groups2$suming)


#######SCORE OF BAD EMOTION FROM AGENT DURING THE CALL#####
turn_data_summary<-turn_data_correct%>%rename(Dial_Str=`Dialogue Structure`)

filtering_data<-filter(turn_data_summary, Dial_Str != "missing")

#one-hot-encoding for both emotion and call phase data (columns with 0/1 values)
newdata <- mltools::one_hot(as.data.table(filtering_data), cols=c("Dial_Str", "Emotion"))
#agent data only
agent_index_data<-filter(newdata, Person == "agent")

#giving different importance for each call phase
agent_index_data$Dial_Str_nds<-ifelse(agent_index_data$Dial_Str_nds==1, 0, 0)
agent_index_data$Dial_Str_back_office<-ifelse(agent_index_data$Dial_Str_back_office==1, 2, 0)
agent_index_data$Dial_Str_consultation <-ifelse(agent_index_data$Dial_Str_consultation==1, 3, 0)
agent_index_data$Dial_Str_customer_recognition <-ifelse(agent_index_data$Dial_Str_customer_recognition==1, 0, 0)
agent_index_data$Dial_Str_farewell  <-ifelse(agent_index_data$Dial_Str_farewell ==1, 4, 0)
agent_index_data$Dial_Str_follow_procedure  <-ifelse(agent_index_data$Dial_Str_follow_procedure ==1, 0, 0)
agent_index_data$Dial_Str_greeting  <-ifelse(agent_index_data$Dial_Str_greeting ==1, 1, 0)
agent_index_data$Dial_Str_mechanic_planned  <-ifelse(agent_index_data$Dial_Str_mechanic_planned ==1, 2, 0)
agent_index_data$Dial_Str_my_problem_is   <-ifelse(agent_index_data$Dial_Str_my_problem_is  ==1, 1, 0)
agent_index_data$Dial_Str_solved   <-ifelse(agent_index_data$Dial_Str_solved  ==1, 4, 0)
agent_index_data$Dial_Str_suggest_solution    <-ifelse(agent_index_data$Dial_Str_suggest_solution   ==1, 3, 0)
agent_index_data$Dial_Str_transfer    <-ifelse(agent_index_data$Dial_Str_transfer   ==1, 2, 0)

#giving different importance for each emotion 
agent_index_data$Emotion_angry    <-ifelse(agent_index_data$Emotion_angry   ==1, 2, 0)
agent_index_data$Emotion_fear    <-ifelse(agent_index_data$Emotion_fear   ==1, 2, 0)
agent_index_data$Emotion_happy    <-ifelse(agent_index_data$Emotion_happy   ==1, -1, 0)
agent_index_data$Emotion_sad    <-ifelse(agent_index_data$Emotion_sad   ==1, 1, 0)
agent_index_data$Emotion_nvt   <-ifelse(agent_index_data$Emotion_nvt   ==1, 0, 0)

#calculating emotional score for each call stage with given "weights" 

agent_index_data$w.Dial_Str_nds<-agent_index_data$Dial_Str_nds*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_nds*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_back_office<-agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_consultation<-agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_customer_recognition<-agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_farewell<-agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_follow_procedure<-agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_greeting<-agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_mechanic_planned<-agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_my_problem_is<-agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_solved<-agent_index_data$Dial_Str_solved*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_solved*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_suggest_solution<-agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_transfer<-agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_sad

#summing it and grouping based on calls ID 
agent_index_data$index<- rowSums(agent_index_data[,c(23:33)])
groups2<-agent_index_data%>%group_by(GENESYS_CONVERSATION_ID)%>%summarise(suming=sum(index))

#joining!!
joinik<-right_join(groups2, call_data_correct, "AO_transcription_id")
final_data1<-right_join(groups, joinik , by=c("AO_transcription_id"))

###############################Personal characterictics of an agent - emotion handling##################
str(turn_data_correct)
agent <- filter(turn_data_correct, Person == "agent") #to choose only agent's emotions 
customer <- filter(turn_data_correct, Person == "customer") ##to choose only customers's emotions 

#extracting only hours from the timestamp
library(hms)
agent$hour<-as_hms(agent$Timestamp)
class(agent$hour)


#EXTRACTING ONLY HOURS FROM THE TIMESTAMP - IMPROTANT!!!
turn_data_correct$hour<-as_hms(turn_data_correct$Timestamp)

#just some graphs - cannot remind what, try to run  - probably "road maps"
turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3")%>%ggplot(.,aes(x=`Dialogue Structure`,y=as.factor(Person), colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)
turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3" & Emotion != "nvt")%>%ggplot(.,aes(x=hour,y=as.factor(Person), colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)

#to joint data of 2 dataset - call and turn data
joinicek<-full_join(call_data_correct, turn_data_correct, "AO_transcription_id")

#employees
frequent_employees<-call_data_correct%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(n=n())%>%arrange(desc(n))
only_frequent<-joinicek%>%filter(AO_GENESYS_EMPLOYEE_LOGIN %in% c(frequent_employees$AO_GENESYS_EMPLOYEE_LOGIN))

#coding emotions into binary num variables
only_frequent_agent<-filter(only_frequent,Person == "agent")
only_frequent_agent$angry<-ifelse(only_frequent_agent$Emotion == "angry", 1, 0)
only_frequent_agent$happy<-ifelse(only_frequent_agent$Emotion == "happy", 1, 0)
only_frequent_agent$sad<-ifelse(only_frequent_agent$Emotion == "sad", 1, 0)
only_frequent_agent$fear<-ifelse(only_frequent_agent$Emotion == "fear", 1, 0)
only_frequent_agent$nvt<-ifelse(only_frequent_agent$Emotion == "nvt", 1, 0)

#sum of angre of the agents in different call phases
agent_angry<-only_frequent_agent%>%group_by( `Dialogue Structure` ,AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(s=sum(angry))
agent_happy<-only_frequent_agent%>%group_by( `Dialogue Structure` ,AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(s=sum(happy))

#weights for each phase -> TO UPDATE - state just random
agent_angry$w<-ifelse(agent_angry$`Dialogue Structure`=="greeting",0.2,ifelse(agent_angry$`Dialogue Structure`=="follow_procedure",0.1,ifelse(agent_angry$`Dialogue Structure`=="customer_recognition", 0.1,ifelse(agent_angry$`Dialogue Structure`=="my_problem_is",0.3,ifelse(agent_angry$`Dialogue Structure`=="consultation",0.8,ifelse(agent_angry$`Dialogue Structure`=="transfer",0.4, ifelse(agent_angry$`Dialogue Structure`=="suggest_solution",0.8,ifelse(agent_angry$`Dialogue Structure`=="back_office",0.2, ifelse(agent_angry$`Dialogue Structure`=="mechanic_planed",0.3, ifelse(agent_angry$`Dialogue Structure`=="solved",1,ifelse(agent_angry$`Dialogue Structure`=="farewell",1,0.1)))))))))))
agent_happy$w<-ifelse(agent_happy$`Dialogue Structure`=="greeting",0.2,ifelse(agent_happy$`Dialogue Structure`=="follow_procedure",0.1,ifelse(agent_happy$`Dialogue Structure`=="customer_recognition", 0.1,ifelse(agent_happy$`Dialogue Structure`=="my_problem_is",0.3,ifelse(agent_happy$`Dialogue Structure`=="consultation",0.8,ifelse(agent_happy$`Dialogue Structure`=="transfer",0.4, ifelse(agent_happy$`Dialogue Structure`=="suggest_solution",0.8,ifelse(agent_happy$`Dialogue Structure`=="back_office",0.2, ifelse(agent_happy$`Dialogue Structure`=="mechanic_planed",0.3, ifelse(agent_happy$`Dialogue Structure`=="solved",1,ifelse(agent_happy$`Dialogue Structure`=="farewell",1,0.1)))))))))))

#joining data about call frequencies and angre
agent_angry$frequency<-right_join(agent_angry, frequent_employees, "AO_GENESYS_EMPLOYEE_LOGIN")$n
agent_happy$frequency<-right_join(agent_happy, frequent_employees, "AO_GENESYS_EMPLOYEE_LOGIN")$n

#calculating already with weights
agent_angry$agent_r<-(agent_angry$w*agent_angry$s)
agent_happy$agent_r<-(agent_happy$w*agent_happy$s)

#making it relative to the frequencies of the calls of each agent being comparable among them
agent_angry$agent_rk<-((agent_angry$w*agent_angry$s)/agent_angry$frequency)*100
agent_angry_personal<-agent_angry%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(agent_personal_score_angry=mean(agent_rk, na.rm=T))

agent_happy$agent_rk<-((agent_happy$w*agent_happy$s)/agent_happy$frequency)*100
agent_happy_personal<-agent_happy%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(agent_personal_score_happy=mean(agent_rk, na.rm=T))

#graph
agent_angry%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(m=mean(agent_rk))%>%ggplot(.,aes(x=AO_GENESYS_EMPLOYEE_LOGIN,y=m))+geom_bar(stat="identity")+coord_flip()

final_data_all<-right_join(final_data_all, agent_happy_personal, "AO_GENESYS_EMPLOYEE_LOGIN")
final_data_all<-right_join(final_data_all, agent_angry_personal, "AO_GENESYS_EMPLOYEE_LOGIN")

################CHANGES OF EMOTIONS BETWEEN DIFFERENT CALL STAGES
customer<-turn_data_correct%>%filter(Person =="customer")

#choosing the stages to compare (my_problem_is; farewell);grouping by a call, dialogue and emotion; COUNT FOR EACH EMOTION (how many times observed in each phase)
#and making the data "wide" (just run and have a look)
ab<-customer%>%filter(`Dialogue Structure` == "my_problem_is" | `Dialogue Structure` == "farewell")%>%group_by(AO_transcription_id, `Dialogue Structure`,Emotion)%>%count(Emotion)%>%spread(key=c("Emotion"), value=n, fill=0)

#the most frequent counts (emotions) in the given call and call stage
ab$Max <- apply(ab[,3:7], 1, function(x) max(x))

#CONDITIONS:
#if MAX (ab$Max) value (the line above) is equal to ANGRY OR equal to ANGRY and NVT -> to state emotion as ANGRY (happens that nvt and other emotions have the same counts,
#to focus on more relevant emotions - the other than nvt is choosen)
ab$general_mood<-ifelse(ab$angry==ab$Max | (ab$angry==ab$Max & ab$nvt==ab$Max),"angry",
                        ifelse(ab$fear==ab$Max | (ab$fear==ab$Max & ab$nvt==ab$Max) ,"fear",
                               ifelse(ab$sad==ab$Max| (ab$sad==ab$Max & ab$nvt==ab$Max),"sad",
                                      ifelse(ab$nvt==ab$Max & (ab$nvt!=ab$angry | ab$nvt!=ab$fear | ab$nvt!=ab$sad| ab$nvt!=ab$happy),"neutral",
                                             ifelse(ab$happy==ab$Max| (ab$happy==ab$Max & ab$nvt==ab$Max),"happy",0)))))

#again making data "wide" (just run and check for understanding);transforming char. to fcts and making some basic dplyr you understand
customer_moods<-ab%>%spread(key=c(`Dialogue Structure`), value=general_mood)
customer_moods<-customer_moods%>%mutate_if(is.character, as.factor)
cust_fin<-customer_moods%>%group_by(AO_transcription_id)%>%select("farewell", "my_problem_is")%>%count(farewell, my_problem_is)

summary(customer_moods)

summary(as.factor(customer$`Dialogue Structure`))

###the final process - LOOP with CONDITIONS: ###


frame<-tibble()
for (u in unique(cust_fin$AO_transcription_id)[1:100]){ #unique calls to loops through = only 100 for testing purposes
  c <- setDT(cust_fin)[AO_transcription_id ==  as.character(u),] #data.table package filtering (faster than dplyr)
  if(nrow(c)>1){ #conditions for n of rows in selected call - some of them have only 1 and so emotions are not comparable (some stage is missing)
    
    ##CONDITIONS:
    #c[1,2] = emotions for the given call for "farewell"; c[2,3]=emotions for the given call for "my_problem_is".
    #if my_problem_is = angry and farewell = "happy" -> the final value = "happy-to-angry" etc....
    if(c[1,2] == "happy" & c[2,3] == "angry"){ 
      mood<-"angry-to-happy"}else if(c[1,2] == "happy" & c[2,3] == "neutral"){
        mood<-"neutral-to-happy"
      }else if(c[1,2] == "happy" & c[2,3] == "sad"){mood<-"sad-to-happy"}else if(c[1,2] == "happy" & c[2,3] == "fear"){mood<-"fear-to-happy"}else if(c[1,2] == "happy" & c[2,3] == "happy"){mood<-"no-change"}
    
    else if(c[1,2] == "sad" & c[2,3] == "angry"){
      mood<-"angry-to-sad"}else if(c[1,2] == "sad" & c[2,3] == "neutral"){
        mood<-"neutral-to-sad"
      }else if(c[1,2] == "sad" & c[2,3] == "sad"){mood<-"no-change"}else if(c[1,2] == "sad" & c[2,3] == "fear"){mood<-"fear-to-sad"}else if(c[1,2] == "sad" & c[2,3] == "happy"){mood<-"sad-to-happy"}    
    
    else if(c[1,2] == "fear" & c[2,3] == "angry"){
      mood<-"angry-to-fear"}else if(c[1,2] == "fear" & c[2,3] == "neutral"){
        mood<-"neutral-to-fear"
      }else if(c[1,2] == "fear" & c[2,3] == "sad"){mood<-"sad-to-fear"}else if(c[1,2] == "fear" & c[2,3] == "fear"){mood<-"no-change"}else if(c[1,2] == "fear" & c[2,3] == "happy"){mood<-"fear-to-happy"}    
    
    else if(c[1,2] == "neutral" & c[2,3] == "angry"){
      mood<-"angry-to-neutral"}else if(c[1,2] == "neutral" & c[2,3] == "neutral"){
        mood<-"no-change"
      }else if(c[1,2] == "neutral" & c[2,3] == "sad"){mood<-"sad-to-neutral"}else if(c[1,2] == "neutral" & c[2,3] == "fear"){mood<-"fear-to-neutral"}else if(c[1,2] == "neutral" & c[2,3] == "happy"){mood<-"neutral-to-happy"}    
    
    else if(c[1,2] == "angry" & c[2,3] == "angry"){
      mood<-"no-change"}else if(c[1,2] == "angry" & c[2,3] == "neutral"){
        mood<-"neutral-to-angry"
      }else if(c[1,2] == "angry" & c[2,3] == "sad"){mood<-"sad-to-angry"}else if(c[1,2] == "angry" & c[2,3] == "fear"){mood<-"fear-to-angry"}else if(c[1,2] == "angry" & c[2,3] == "happy"){mood<-"angry-to-happy"}    
    
    else{mood<-"error"}
  }else{
    mood<-"no_data" #for call with missing stages
  }
  data<-tibble(ID=u, moods=mood)
  frame<-rbind(frame,data)
}

#######WORKING WITH TIME DATA and EMOTIONS #####

#function to extract the most frequent emotion (modus) - NOT USED FOR NOW
extract_max <- function(.vec){
  my_max <- .vec %>% vctrs::vec_count()
  my_max <- my_max$key[1]  }

#LOOP AGAIN - FOR NOW, SOLUTION BELOW USED------------------------------------------------------
ccccc<-tibble()
for (u in unique(customer$AO_transcription_id)[1:1000]){  #unique calls, 1000 for testing purposes
  #data.table filtering, cols selecting and sorting
  c2<-setDT(customer)[AO_transcription_id ==  as.character(u),c("Emotion", "hour")][order(hour)]
  if(nrow(c2)>4){ #as we take into account at least 4 observation to do conclusion about the most frequnet emotion (WE CAN DISCUSS HOW MANY)
    start<-c(extract_max(c2$Emotion[1:4])) #extracting modus of emotion at the begging of the call (first 4 observations (timestamps) in data that are order by time)
    end<-c(extract_max(c2$Emotion[c(length(c2$Emotion)-4):length(c2$Emotion)])) #last 4 observations (timestamps)
  }else{next} #if there are not enough data (4), just to skip to another one 
  data<-tibble(ID=u, mood_start=c4, mood_end=end)
  ccccc<-rbind(ccccc,data)
  rm(data)
}
#---------------------------------------------------------------------------------------------------------
#to see what stages are the richest for emotion data
nm<-customer%>%group_by(`Dialogue Structure`)%>%count(Emotion)%>%arrange(desc(n))

`%notin%` <- Negate(`%in%`) #function to negate %in% function

person<-customer #"agent" or "customer" #to specify dataset of customer or agent - MUST BE DONE FOR BOTH

#Loops to extract emotions from the beginning of the call (first 4 obervations) & the end of the call (last 4 obervations)
#Extract = if there was mention of other emotion than neutral - take it into consideration
gc()
ccccc5<-tibble()
for (u in unique(person$AO_transcription_id)){ 
  c2<-setDT(person)[AO_transcription_id ==  as.character(u),c("Emotion", "hour")][order(hour)] #to order observations based on hour - so to do not be dependent on the phase name by class. model
  
  if(nrow(c2)>4){
    start_t<-c2[1:4,] #first 4 observations (timestamps) of the call
    #CONDTIONS: if emotion "angry" present -> present, followed by "happy" etc and the last option: neutral
    if("angry" %in% start_t$Emotion){
      start<-"angry"
    }else if("happy" %in% start_t$Emotion & ("angry" %notin% start_t$Emotion)){
      start<-"happy"
    } else if("sad" %in% start_t$Emotion  & ("angry" %notin% start_t$Emotion | "happy" %notin% start_t$Emotion)){
      start<-"sad"
    }else if("fear" %in% start_t$Emotion & ("angry" %notin% start_t$Emotion | "happy" %notin% start_t$Emotion| "sad" %notin% start_t$Emotion)){
      start<-"fear"
    }else{start<-"neutral"}
    
    end_t<-c2[c(length(c2$Emotion)-4):length(c2$Emotion),] #last 4 observations (timestamps) of the call
    #CONDTIONS: if emotion "angry" present -> present, followed by "happy" etc and the last option: neutral
    if("angry" %in% end_t$Emotion){
      end<-"angry"
    }else if("happy" %in% end_t$Emotion & ("angry" %notin% end_t$Emotion)){
      end<-"happy"
    } else if("sad" %in% end_t$Emotion  & ("angry" %notin% end_t$Emotion | "happy" %notin% end_t$Emotion)){
      end<-"sad"
    }else if("fear" %in% end_t$Emotion & ("angry" %notin% end_t$Emotion | "happy" %notin% end_t$Emotion| "sad" %notin% end_t$Emotion)){
      end<-"fear"
    }else{end<-"neutral"}
  }else{
    end<-"no_data"
    start<-"no_data"}
  data<-tibble(ID=u, mood_start=start, mood_end=end)
  ccccc5<-rbind(ccccc5,data)
  rm(data)
  rm(start)
  rm(end)
}

#filtering calls w/o data
emotions_time_customer<-filter(emotions_time_customer, mood_start != "no_data" | mood_end!="no_data")

emotions_time_agent<-filter(emotions_time_agent, mood_start != "no_data" | mood_end!="no_data")

#joining cust and agent data for comparison purposes
emotions_time_join<-inner_join(emotions_time_customer, emotions_time_agent, "ID")
emotions_time_join$start_diff<-ifelse(emotions_time_join$mood_start.x ==  emotions_time_join$mood_start.y,0,1)
sum(emotions_time_join$start_diff)
emotions_time_join$end_diff<-ifelse(emotions_time_join$mood_end.x ==  emotions_time_join$mood_end.y,0,1)
sum(emotions_time_join$end_diff)

#comparing behavior of the customer vs reaction of the agent at the beginning of the call (emotional intelligence)
emotions_time_join$change_start<-ifelse(emotions_time_join$mood_start.x == "angry" & emotions_time_join$mood_start.y == "angry", "bad-negative",
                                        ifelse(emotions_time_join$mood_start.x == "angry" & emotions_time_join$mood_start.y != "angry", "bad-positive ",
                                               ifelse(emotions_time_join$mood_start.x == "happy" & emotions_time_join$mood_start.y == "happy", "good-positive",
                                                      ifelse((emotions_time_join$mood_start.x == "happy") & (emotions_time_join$mood_start.y != "happy"),"good-negative",
                                                             ifelse((emotions_time_join$mood_start.x == "sad" | emotions_time_join$mood_start.x == "fear") & (emotions_time_join$mood_start.y != "angry" | emotions_time_join$mood_start.y != "happy"),"meekness",
                                                                    ifelse(emotions_time_join$mood_start.x == "neutral" & emotions_time_join$mood_start.y == "neutral","neutrality",
                                                                           ifelse(emotions_time_join$mood_start.x == "neutral" & (emotions_time_join$mood_start.y == "happy"),"good-changer",
                                                                                  ifelse(emotions_time_join$mood_start.x == "neutral" & (emotions_time_join$mood_start.y == "sad" | emotions_time_join$mood_start.y == "fear" | emotions_time_join$mood_start.y == "angry"),"bad-changer","to-define"))))))))


#comparing how a customer changed emotions between the start and end of the call
emotions_time_join$change_start_end<-ifelse(emotions_time_join$mood_start.x == "angry" & (emotions_time_join$mood_end.x == "angry" | emotions_time_join$mood_end.x == "sad" | emotions_time_join$mood_end.x == "fear") , "bad-change",
                                            ifelse(emotions_time_join$mood_start.x == "angry" & (emotions_time_join$mood_end.x == "happy" | emotions_time_join$mood_end.x == "neutral"), "good-change",
                                                   ifelse(emotions_time_join$mood_start.x == "happy" & (emotions_time_join$mood_end.x != "happy" | emotions_time_join$mood_end.x != "neutral"), "bad-change",
                                                          ifelse(emotions_time_join$mood_start.x == "happy" & (emotions_time_join$mood_end.x == "happy" | emotions_time_join$mood_end.x == "neutral"), "good-change",      
                                                                 ifelse((emotions_time_join$mood_start.x == "sad") & (emotions_time_join$mood_end.x == "angry" | emotions_time_join$mood_end.x == "fear" | emotions_time_join$mood_end.x == "sad"),"bad-change",
                                                                        ifelse((emotions_time_join$mood_start.x == "sad") & (emotions_time_join$mood_end.x == "happy" | emotions_time_join$mood_end.x == "neutral"),"good-change",
                                                                               ifelse((emotions_time_join$mood_start.x == "fear") & (emotions_time_join$mood_end.x == "happy" | emotions_time_join$mood_end.x == "neutral"),"good-change",
                                                                                      ifelse((emotions_time_join$mood_start.x == "fear") & (emotions_time_join$mood_end.x != "happy" | emotions_time_join$mood_end.x != "neutral"),"bad-change",       
                                                                                             ifelse(emotions_time_join$mood_start.x == "neutral" & (emotions_time_join$mood_end.x == "neutral" | emotions_time_join$mood_end.x == "happy"),"good-change",
                                                                                                    ifelse(emotions_time_join$mood_start.x == "neutral" & (emotions_time_join$mood_end.x != "neutral" | emotions_time_join$mood_end.x != "happy"),"bad-change","to-define"))))))))))

view(filter(emotions_time_join, change_start_end=="to-define"))

emotions_time_join$change_start<-as.factor(emotions_time_join$change_start)
emotions_time_join$change_start_end<-as.factor(emotions_time_join$change_start_end)
#releveling
emotions_time_join$change_start<- relevel(emotions_time_join$change_start, ref="neutrality")
emotions_time_join$change_start_end<- relevel(emotions_time_join$change_start_end, ref="good-change")

#join to the whole dataset
test_join<-right_join(emotions_time_join, final_data1, by=c("ID"="AO_transcription_id"))                                        
unique(test_join$FCR_24H_CLI)

#############################------------------------------------------------------------

####IRIS::: Negativity / Turns per agents

#first: transfers --> value of 5 when 5th transfer is made etc and 0 if IND variable is 0
call_data_correct$transfer <- 0
call_data_correct$transfer<- ifelse(!(call_data_correct$FIFTH_TRANSFER_VDN== "Onbekend"), 5,
                                    ifelse(!(call_data_correct$FOURH_TRANSFER_VDN== "Onbekend"), 4, 
                                           ifelse(!(call_data_correct$THIRD_TRANSFER_VDN== "Onbekend"), 3,
                                                  ifelse(!(call_data_correct$SEC_TRANSFER_VDN== "Onbekend"), 2,
                                                         ifelse(call_data_correct$IND_TRANSFER==1, 1, 0))))                                    )
table(call_data_correct$transfer)  
table(call_data_correct$IND_TRANSFER)
call_data_correct$transfer <- as.integer(call_data_correct$transfer)


#independence variable
aggregate(as.numeric(transfer)~AO_GENESYS_EMPLOYEE_LOGIN, data = call_data_correct, FUN = sum)
call_data_correct$AO_GENESYS_EMPLOYEE_LOGIN

frequent_employees<-call_data_correct%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(n=n())%>%arrange(desc(n))
calldata <- call_data_correct%>%filter(AO_GENESYS_EMPLOYEE_LOGIN %in% c(frequent_employees$AO_GENESYS_EMPLOYEE_LOGIN)) 

#for each agent --> total calls + total transfers

calldata <- right_join(calldata, frequent_employees, "AO_GENESYS_EMPLOYEE_LOGIN")
calldata$totalcall <- calldata$n

idk <-aggregate(IND_TRANSFER~AO_GENESYS_EMPLOYEE_LOGIN, data = calldata, FUN = sum)
calldata <- right_join(calldata, idk, "AO_GENESYS_EMPLOYEE_LOGIN")
calldata$totaltransfer <- calldata$IND_TRANSFER.y

calldata$independence <- calldata$totaltransfer/calldata$totalcall
#when variable independence is large, the agent is dependent

final_data_all<-cbind(final_data_all, agent_independence=calldata$independence)

#negative words agent
nega <- aggregate(NEGATIVITY~AO_GENESYS_EMPLOYEE_LOGIN, data = calldata, FUN = mean)
calldata <- right_join(calldata, nega, "AO_GENESYS_EMPLOYEE_LOGIN")
calldata$negative <- calldata$NEGATIVITY.y
calldata1<-tibble(negative_ratio=calldata$negative, transfers_ratio=calldata$totaltransfer, ID = calldata$AO_transcription_id)

final_data<-left_join(test_join,calldata1, "ID")

########ROHAN::::SKILLS

call<- final_data
df<-dplyr::filter(call, FCR_24H_CLI != -2)

library(fastDummies)
library(caret)
library(dplyr)
library(tidyr)
library(mltools)
library(vctrs)
library(hms)
library(data.table)
library(tidyverse)

#Delete square brackets, apostrophes, and commas (cleaning)
final_data$LAST_SKILL_NUM <- gsub("\\[|\\]", "", final_data$LAST_SKILL_NUM)
final_data$LAST_SKILL_NUM <- gsub("'", "", final_data$LAST_SKILL_NUM)
final_data$LAST_SKILL_NUM <- gsub(",", "", final_data$LAST_SKILL_NUM)

#Split individual skills into seperate columns using regular expression 
call<-separate(final_data, LAST_SKILL_NUM, into = c('Skill1', 'Skill2'), sep = " (?=[^ ]+$)")
factor(call$Skill1)
factor(call$Skill2)

#number of missing skill 2 value (how many calls only required one skill), missing values because when splitting there are sometimes only one skill
sum(is.na(call$Skill2))

#rename missing values
call$Skill2[is.na(call$Skill2)] <- "No_Skill" 

#filter only agent data for turn dataset
turn_agent<-filter(turn_data_correct, Person == "agent")
#use this to merge
merger<- call[c('Skill1', 'Skill2', 'AO_transcription_id')]
merger_df<- dplyr::left_join(turn_agent, merger)
unique(merger$Skill2)

#one hot encode both variables after merge
final_df <- dummy_cols(merger_df, 
                       select_columns = c('Skill1', 'Skill2', 'Emotion'))


#Assign Importance for skills (Need to decide on weights which is hard without natural language definitions of skills) (most frequent first skill is a 1, assuming most CRMs need the skill), (-1 is less frequent thus a higher weight as for all skill2s)
final_df['Skill1_18f26f88-a791-4055-a811-59414b6b21e7']<-ifelse(final_df$Skill1=='18f26f88-a791-4055-a811-59414b6b21e7', 1, 0)
final_df['Skill1_-1']<-ifelse(final_df$Skill1=='-1', 2, 0)
final_df['Skill2_24ff1fae-7594-49df-8f3e-fdfd8ecfc59d']<-ifelse(final_df$Skill2=='24ff1fae-7594-49df-8f3e-fdfd8ecfc59d', 2, 0)
final_df['Skill2_fdcad9c9-fd2f-45b8-94e2-91371247d201']<-ifelse(final_df$Skill2=='fdcad9c9-fd2f-45b8-94e2-91371247d201', 2, 0)
final_df['Skill2_6fb9aa96-7542-4cd1-b90e-438445f12027']<-ifelse(final_df$Skill2=='6fb9aa96-7542-4cd1-b90e-438445f12027', 2, 0)
final_df['Skill2_048c2d60-38db-4e2b-9cc9-67e2c15f8e5f']<-ifelse(final_df$Skill2=='048c2d60-38db-4e2b-9cc9-67e2c15f8e5f', 2, 0)
final_df['Skill2_cb499461-0532-4a74-998f-ea34b87a539c']<-ifelse(final_df$Skill2=='cb499461-0532-4a74-998f-ea34b87a539c', 2, 0)
final_df['Skill2_73613ec3-e7e1-46f4-89a9-c1d74d1a54ca']<-ifelse(final_df$Skill2=='73613ec3-e7e1-46f4-89a9-c1d74d1a54ca', 2, 0)
final_df['Skill2_80cbcf8a-1041-42e1-9210-644dbf69eb75']<-ifelse(final_df$Skill2=='80cbcf8a-1041-42e1-9210-644dbf69eb75', 2, 0)
final_df['Skill2_b1629396-1dcb-4df0-ad81-7d9d4394534e']<-ifelse(final_df$Skill2=='b1629396-1dcb-4df0-ad81-7d9d4394534e', 2, 0)
final_df['Skill2_0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04']<-ifelse(final_df$Skill2=='0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04', 2, 0)
final_df['Skill2_63826688-93b0-4da0-b585-5f30bf7a79ba']<-ifelse(final_df$Skill2=='63826688-93b0-4da0-b585-5f30bf7a79ba', 2, 0)
final_df['Skill2_785750af-7e2d-4f2d-b058-beed69608556']<-ifelse(final_df$Skill2=='785750af-7e2d-4f2d-b058-beed69608556', 2, 0)
final_df['Skill2_6172f317-ea56-4277-aa97-1998d69f6bd9']<-ifelse(final_df$Skill2=='6172f317-ea56-4277-aa97-1998d69f6bd9', 2, 0)
final_df['Skill2_b3d69698-0fa5-475d-aaaa-52cca34fee0d']<-ifelse(final_df$Skill2=='b3d69698-0fa5-475d-aaaa-52cca34fee0d', 2, 0)
final_df['Skill2_eaa52890-7c52-4c27-9e88-b04b2cb96fca']<-ifelse(final_df$Skill2=='eaa52890-7c52-4c27-9e88-b04b2cb96fca', 2, 0)
final_df['Skill2_0363fbf8-7e2d-4769-b2d7-734139208298']<-ifelse(final_df$Skill2=='0363fbf8-7e2d-4769-b2d7-734139208298', 2, 0)
final_df['Skill2_7397c59f-b335-4b0e-8822-629de75107bb']<-ifelse(final_df$Skill2=='7397c59f-b335-4b0e-8822-629de75107bb', 2, 0)
final_df['Skill2_abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613']<-ifelse(final_df$Skill2=='abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613', 2, 0)
final_df['Skill2_0edf6f57-ecc1-49d9-bfd2-6cb2364749a4']<-ifelse(final_df$Skill2=='0edf6f57-ecc1-49d9-bfd2-6cb2364749a4', 2, 0)
final_df$No_Skill<-ifelse(final_df$Skill2=='No_Skill', 0, 0)

#Assign Importance to Emotion
final_df$Emotion_angry    <-ifelse(final_df$Emotion_angry   ==1, 2, 0)
final_df$Emotion_fear    <-ifelse(final_df$Emotion_fear   ==1, 2, 0)
final_df$Emotion_happy    <-ifelse(final_df$Emotion_happy   ==1, -1, 0)
final_df$Emotion_sad    <-ifelse(final_df$Emotion_sad   ==1, 1, 0)
final_df$Emotion_nvt   <-ifelse(final_df$Emotion_nvt   ==1, 0, 0)

#Assign Score for each skill
final_df['w_Skill1_18f26f88-a791-4055-a811-59414b6b21e7']<-final_df['Skill1_18f26f88-a791-4055-a811-59414b6b21e7']*final_df$Emotion_angry +final_df['Skill1_18f26f88-a791-4055-a811-59414b6b21e7']*final_df$Emotion_fear +final_df['Skill1_18f26f88-a791-4055-a811-59414b6b21e7']*final_df$Emotion_happy+final_df['Skill1_18f26f88-a791-4055-a811-59414b6b21e7']*final_df$Emotion_sad
final_df['w_Skill1_-1']<-final_df['Skill1_-1']*final_df$Emotion_angry +final_df['Skill1_-1']*final_df$Emotion_fear +final_df['Skill1_-1']*final_df$Emotion_happy+final_df['Skill1_-1']*final_df$Emotion_sad
final_df['w_Skill2_24ff1fae-7594-49df-8f3e-fdfd8ecfc59d']<-final_df['Skill2_24ff1fae-7594-49df-8f3e-fdfd8ecfc59d']*final_df$Emotion_angry +final_df['Skill2_24ff1fae-7594-49df-8f3e-fdfd8ecfc59d']*final_df$Emotion_fear +final_df['Skill2_24ff1fae-7594-49df-8f3e-fdfd8ecfc59d']*final_df$Emotion_happy+final_df['Skill2_24ff1fae-7594-49df-8f3e-fdfd8ecfc59d']*final_df$Emotion_sad
final_df['w_Skill2_fdcad9c9-fd2f-45b8-94e2-91371247d201']<-final_df['Skill2_fdcad9c9-fd2f-45b8-94e2-91371247d201']*final_df$Emotion_angry +final_df['Skill2_fdcad9c9-fd2f-45b8-94e2-91371247d201']*final_df$Emotion_fear +final_df['Skill2_fdcad9c9-fd2f-45b8-94e2-91371247d201']*final_df$Emotion_happy+final_df['Skill2_fdcad9c9-fd2f-45b8-94e2-91371247d201']*final_df$Emotion_sad
final_df['w_Skill2_6fb9aa96-7542-4cd1-b90e-438445f12027']<-final_df['Skill2_6fb9aa96-7542-4cd1-b90e-438445f12027']*final_df$Emotion_angry +final_df['Skill2_6fb9aa96-7542-4cd1-b90e-438445f12027']*final_df$Emotion_fear +final_df['Skill2_6fb9aa96-7542-4cd1-b90e-438445f12027']*final_df$Emotion_happy+final_df['Skill2_6fb9aa96-7542-4cd1-b90e-438445f12027']*final_df$Emotion_sad
final_df['w_Skill2_048c2d60-38db-4e2b-9cc9-67e2c15f8e5f']<-final_df['Skill2_048c2d60-38db-4e2b-9cc9-67e2c15f8e5f']*final_df$Emotion_angry +final_df['Skill2_048c2d60-38db-4e2b-9cc9-67e2c15f8e5f']*final_df$Emotion_fear +final_df['Skill2_048c2d60-38db-4e2b-9cc9-67e2c15f8e5f']*final_df$Emotion_happy+final_df['Skill2_048c2d60-38db-4e2b-9cc9-67e2c15f8e5f']*final_df$Emotion_sad
final_df['w_Skill2_cb499461-0532-4a74-998f-ea34b87a539c']<-final_df['Skill2_cb499461-0532-4a74-998f-ea34b87a539c']*final_df$Emotion_angry +final_df['Skill2_cb499461-0532-4a74-998f-ea34b87a539c']*final_df$Emotion_fear +final_df['Skill2_cb499461-0532-4a74-998f-ea34b87a539c']*final_df$Emotion_happy+final_df['Skill2_cb499461-0532-4a74-998f-ea34b87a539c']*final_df$Emotion_sad
final_df['w_Skill2_73613ec3-e7e1-46f4-89a9-c1d74d1a54ca']<-final_df['Skill2_73613ec3-e7e1-46f4-89a9-c1d74d1a54ca']*final_df$Emotion_angry +final_df['Skill2_73613ec3-e7e1-46f4-89a9-c1d74d1a54ca']*final_df$Emotion_fear +final_df['Skill2_73613ec3-e7e1-46f4-89a9-c1d74d1a54ca']*final_df$Emotion_happy+final_df['Skill2_73613ec3-e7e1-46f4-89a9-c1d74d1a54ca']*final_df$Emotion_sad
final_df['w_Skill2_80cbcf8a-1041-42e1-9210-644dbf69eb75']<-final_df['Skill2_80cbcf8a-1041-42e1-9210-644dbf69eb75']*final_df$Emotion_angry +final_df['Skill2_80cbcf8a-1041-42e1-9210-644dbf69eb75']*final_df$Emotion_fear +final_df['Skill2_80cbcf8a-1041-42e1-9210-644dbf69eb75']*final_df$Emotion_happy+final_df['Skill2_80cbcf8a-1041-42e1-9210-644dbf69eb75']*final_df$Emotion_sad
final_df['w_Skill2_b1629396-1dcb-4df0-ad81-7d9d4394534e']<-final_df['Skill2_b1629396-1dcb-4df0-ad81-7d9d4394534e']*final_df$Emotion_angry +final_df['Skill2_b1629396-1dcb-4df0-ad81-7d9d4394534e']*final_df$Emotion_fear +final_df['Skill2_b1629396-1dcb-4df0-ad81-7d9d4394534e']*final_df$Emotion_happy+final_df['Skill2_b1629396-1dcb-4df0-ad81-7d9d4394534e']*final_df$Emotion_sad
final_df['w_Skill2_0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04']<-final_df['Skill2_0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04']*final_df$Emotion_angry +final_df['Skill2_0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04']*final_df$Emotion_fear +final_df['Skill2_0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04']*final_df$Emotion_happy+final_df['Skill2_0bb1c7e7-1b8a-4b0d-8b7b-58dfe89e5f04']*final_df$Emotion_sad
final_df['w_Skill2_63826688-93b0-4da0-b585-5f30bf7a79ba']<-final_df['Skill2_63826688-93b0-4da0-b585-5f30bf7a79ba']*final_df$Emotion_angry +final_df['Skill2_63826688-93b0-4da0-b585-5f30bf7a79ba']*final_df$Emotion_fear +final_df['Skill2_63826688-93b0-4da0-b585-5f30bf7a79ba']*final_df$Emotion_happy+final_df['Skill2_63826688-93b0-4da0-b585-5f30bf7a79ba']*final_df$Emotion_sad
final_df['w_Skill2_785750af-7e2d-4f2d-b058-beed69608556']<-final_df['Skill2_785750af-7e2d-4f2d-b058-beed69608556']*final_df$Emotion_angry +final_df['Skill2_785750af-7e2d-4f2d-b058-beed69608556']*final_df$Emotion_fear +final_df['Skill2_785750af-7e2d-4f2d-b058-beed69608556']*final_df$Emotion_happy+final_df['Skill2_785750af-7e2d-4f2d-b058-beed69608556']*final_df$Emotion_sad
final_df['w_Skill2_6172f317-ea56-4277-aa97-1998d69f6bd9']<-final_df['Skill2_6172f317-ea56-4277-aa97-1998d69f6bd9']*final_df$Emotion_angry +final_df['Skill2_6172f317-ea56-4277-aa97-1998d69f6bd9']*final_df$Emotion_fear +final_df['Skill2_6172f317-ea56-4277-aa97-1998d69f6bd9']*final_df$Emotion_happy+final_df['Skill2_6172f317-ea56-4277-aa97-1998d69f6bd9']*final_df$Emotion_sad
final_df['w_Skill2_b3d69698-0fa5-475d-aaaa-52cca34fee0d']<-final_df['Skill2_b3d69698-0fa5-475d-aaaa-52cca34fee0d']*final_df$Emotion_angry +final_df['Skill2_b3d69698-0fa5-475d-aaaa-52cca34fee0d']*final_df$Emotion_fear +final_df['Skill2_b3d69698-0fa5-475d-aaaa-52cca34fee0d']*final_df$Emotion_happy+final_df['Skill2_b3d69698-0fa5-475d-aaaa-52cca34fee0d']*final_df$Emotion_sad
final_df['w_Skill2_eaa52890-7c52-4c27-9e88-b04b2cb96fca']<-final_df['Skill2_eaa52890-7c52-4c27-9e88-b04b2cb96fca']*final_df$Emotion_angry +final_df['Skill2_eaa52890-7c52-4c27-9e88-b04b2cb96fca']*final_df$Emotion_fear +final_df['Skill2_eaa52890-7c52-4c27-9e88-b04b2cb96fca']*final_df$Emotion_happy+final_df['Skill2_eaa52890-7c52-4c27-9e88-b04b2cb96fca']*final_df$Emotion_sad
final_df['w_Skill2_0363fbf8-7e2d-4769-b2d7-734139208298']<-final_df['Skill2_0363fbf8-7e2d-4769-b2d7-734139208298']*final_df$Emotion_angry +final_df['Skill2_0363fbf8-7e2d-4769-b2d7-734139208298']*final_df$Emotion_fear +final_df['Skill2_0363fbf8-7e2d-4769-b2d7-734139208298']*final_df$Emotion_happy+final_df['Skill2_0363fbf8-7e2d-4769-b2d7-734139208298']*final_df$Emotion_sad
final_df['w_Skill2_7397c59f-b335-4b0e-8822-629de75107bb']<-final_df['Skill2_7397c59f-b335-4b0e-8822-629de75107bb']*final_df$Emotion_angry +final_df['Skill2_7397c59f-b335-4b0e-8822-629de75107bb']*final_df$Emotion_fear +final_df['Skill2_7397c59f-b335-4b0e-8822-629de75107bb']*final_df$Emotion_happy+final_df['Skill2_7397c59f-b335-4b0e-8822-629de75107bb']*final_df$Emotion_sad
final_df['w_Skill2_abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613']<-final_df['Skill2_abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613']*final_df$Emotion_angry +final_df['Skill2_abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613']*final_df$Emotion_fear +final_df['Skill2_abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613']*final_df$Emotion_happy+final_df['Skill2_abdbb1ac-1741-4fc3-b5f1-27cd0d7b6613']*final_df$Emotion_sad
final_df['w_Skill2_0edf6f57-ecc1-49d9-bfd2-6cb2364749a4']<-final_df['Skill2_0edf6f57-ecc1-49d9-bfd2-6cb2364749a4']*final_df$Emotion_angry +final_df['Skill2_0edf6f57-ecc1-49d9-bfd2-6cb2364749a4']*final_df$Emotion_fear +final_df['Skill2_0edf6f57-ecc1-49d9-bfd2-6cb2364749a4']*final_df$Emotion_happy+final_df['Skill2_0edf6f57-ecc1-49d9-bfd2-6cb2364749a4']*final_df$Emotion_sad
final_df$w_No_Skill<-final_df$No_Skill*final_df$Emotion_angry +final_df$No_Skill*final_df$Emotion_fear +final_df$No_Skill*final_df$Emotion_happy+final_df$No_Skill*final_df$Emotion_sad

#Putting it together, weighting skills and the emotions used when different sets of skills are used
final_df$skill_index<- rowSums(final_df[,c(35:56)])
groups22<-final_df%>%group_by(AO_transcription_id)%>%summarise(Skill_Emotion=sum(skill_index))

#merge Skill_Emotion to call dataset table
#call2=merge(x=call,y=groups22,by="AO_transcription_id")
final_data_all<-left_join(final_data,groups22, "AO_transcription_id")

sum(is.na(groups22$Skill_Emotion))
colSums(is.na(merger_df))
#If more skills are included the negativity rate will be more negative/positive than if only 1 skill is used, 
#This will show us how the emotions differ when a specific set of skills are used