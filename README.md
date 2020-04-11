# visualization and knit word document using R-Markdown

---
title: "ANALYSIS ON IPL MATCH RESULTS DATASET"
author: "Uday varun mahadeva"
date: "09/01/2020"
output: 
 word_document:
   reference: ref.docx
   toc: yes
bibliography: 'IPL.bib'  
---
```{r setup, include=FALSE, echo=TRUE,results='hide'}
knitr::opts_chunk$set(echo = TRUE,results = 'hide')
```
\newpage
# 1.Introduction:
Visualization of the sports data is fascinating. Cricket is a game where a lot of things can be analyzed which would help us understand significant factors of each match. There are about 1 billion people all over the world following updates of cricket and among which IPL which stands for **INDIAN PREMIER LEAGUE** has the major fan followers @RN2. IPL is a twenty over format of cricket which is being played every year since 2008. The game is being hosted by the BCCI (Board of Control for Cricket India). The tournament is played by different teams representing different cities in India. The performance of each team in every season is not the same. However, on the consistency basis there are teams which has performed extraordinarily in all the seasons. There are various factor that collectively contribute for a team to win.

# 2.Research question:

In Each season different teams win the cup; from this we can consider the team winning the cup to be the best team for that year only. So, With the help of the Match history dataset available on Kaggle I would want to analyze and visualize three different aspects.  

**1. Which is the best IPL team till now?   **

**2. Other than players performance, what would impact the match results?**


# 3.Rationale:
For the first question, which is the best team in IPL? I have four analysis and its visualizations which would show us which team is the best.

1. Number of matches won by each team.
2. Highest run margin of each team.
3. Total run margin from all the matches won.
4. Very close victory.

For the second question,I would analyse Percentage of matches won after losing the toss and winning the toss and visualized the matches won by each team at different cities. 

```{r warning=FALSE,message=FALSE,echo=FALSE}
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("scales")
#install.packages("ggmap")
#install.packages("ggthemes")
```

# 4.Libraries used:
```{r warning=FALSE,message=FALSE,echo=TRUE}
#Using different library required.
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggmap)
library(ggthemes)
```

```{r echo=FALSE}
theme_update(axis.title = element_text(size = 25),axis.text = element_text(size = 15),title = element_text(size = 25))
```


# 5.About the Dataset:  
I have selected the match result dataset to visualize and analyze different aspects of the matches that has been played. This data set contains details of every match played since 2008 till 2019 @RN1. The description for each column of the dataset is shown in the below table.

Columns                         |             Description        
--------------------------------|------------------------------------  
id	                            | id of each match
season                        	| year of the match
city                          	| City at which the match was played
date                          	| Date of the match
team1 and team2               	| Name of the teams playing the match
toss_winner	                    | Team that won the toss
toss_decision	                  | Decision after the toss was won
result	                        | Type of the match result
dl_applied                     	| Was Duckworth-Lewis method applied 
winner                         	| Team that won the Match
win_by_runs                   	| Winning run margin 
win_by_wickets                	| Winning wicket margin 
player_of_match	                | Man of the match
venue                         	| Stadium the match was played
umpire1,umpire2 and umpire3   	| umpires for that match
## 5.1.Importing the dataset:   

using the below code to import the selected csv dataset, along with loading the CSV file i am also assigning the empty cells with 'NA'.
```{r}
Match_data <- read.csv("matches.csv", header=TRUE, na.strings=c("","NA"),stringsAsFactors = FALSE)
#check the structure of the dataframe
str(Match_data)
#See the data for the first 10 rows
head(Match_data)
```


# 6. DATA Cleaning:
There is a need to perform few cleaning on the dataset as we need to detect and fix the defective or incomplete information from the dataset.I have found that two city names are given for single city and this needs to be changed. Bengaluru and Banglore is same city but given in two different names. So i have done the necessary fix for the name of the city. 

```{r}
Match_data$city[Match_data$city %in% "Bangalore"] <- "Bengaluru"
#check if each city is represented with a single name
unique( Match_data$city )
```

I have performed data cleaning on the Venue column where few names of the venue were different even though they represent the same venue. These Venue names have been manipulated so that each venue is represented by a single name only. 

```{r}
Match_data$venue[Match_data$venue %in% "M Chinnaswamy Stadium"] <- "M. Chinnaswamy Stadium"
Match_data$venue[Match_data$venue %in% "Punjab Cricket Association IS Bindra Stadium, Mohali"] <- "Punjab Cricket Association Stadium, Mohali"
Match_data$venue[Match_data$venue %in% "Feroz Shah Kotla Ground"] <- "Feroz Shah Kotla"
Match_data$venue[Match_data$venue %in% "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium"] <- "ACA-VDCA Stadium"
Match_data$venue[Match_data$venue %in% "Rajiv Gandhi Intl. Cricket Stadium"] <- "Rajiv Gandhi International Stadium, Uppal"

#check if each venue is represented with a single name
unique( Match_data$venue )
```
A team(Rising Pune Supergiant) was given with two names hence assigning a single name to it.
```{r}
Match_data$toss_winner[Match_data$toss_winner %in% c("Rising Pune Supergiants")] <- "Rising Pune Supergiant"
Match_data$winner[Match_data$winner %in% c("Rising Pune Supergiants")] <- "Rising Pune Supergiant"
unique( Match_data$toss_winner )
unique( Match_data$winner )
```


From the above data I am deleting the unwanted columns for my visualization, the columns are dl_applied and result,as I am concerned about the final result of the match.Few columns  umpire1, umpire2 and umpire3 were deleted as these are not required to answer my research question.

```{r}
Match_data$umpire1<- NULL
Match_data$umpire2<- NULL
Match_data$umpire3<- NULL
Match_data$result<-NULL
Match_data$dl_applied<-NULL
Match_data
```

# 7. Data Manipulation:
Along with loading the CSV file I am also assigning the empty cells with â€˜NAâ€™. The dataset contains 756 rows which tells us that in total 756 matches being played from 2008 to 2009. out of these 756 matches 4 matches do not have any results as these matches were dismissed due to rain. Hence, these 4 empty columns were replaced by â€˜NAâ€™ while importing the dataset so that it would not affect the visualization as the â€˜NAâ€™ can be filtered out.  
```{r}
Sorted_Match_data <- Match_data[order(Match_data$date),]
Sorted_Match_data
```
I have sorted the data based on the season from 2008 to 2019. I have observed that there is an uneven id for the matches. Hence these values need to be changed and new series of number should be assigned. 
```{r}
#I am doing this operation with help of seq in the below code. Now new id is given to each row order wise.

Sorted_Match_data$id <- seq.int(nrow(Sorted_Match_data))
Sorted_Match_data
```

There are various manipulation done to extract different data for different visualization, this can be seen in the *visualization* section.  

# 8. Analysis and Visualization:
There are 756 matches played out of which  752 matches have a final match result.
```{r echo=TRUE,results='hold'}
Sorted_Match_data%>%
  filter(!is.na(winner))%>% #filter matches with no result
  summarize(
    total_match_played = n() #count the total matches played
    )
```


## 8.1. Total number of matches won by each team.

```{r fig.width=10, fig.height=8}

#filtering the match with no results
total_winner<-Sorted_Match_data%>%filter(!is.na(winner))

#count number of matches won by each team
total_win<-count(total_winner,winner)

# creating vector with required colors.
clr<-c('gold','lightseagreen','cadetblue1','chartreuse','darkgoldenrod1','darkorange','khaki1','hotpink','blue','darkseagreen1','red','orchid','pink2','tan1') 

# plot visualizing of total match won by each team
Plot1<-ggplot(total_win,aes(winner,n))+geom_col(fill=clr)+
  labs(title = "Number of matches won by each team",x= "Teams ",y = "Total match Won")+
  geom_text(aes(label= n, hjust= 0.5,vjust=1),size=5)+
  theme(axis.text.x = element_text(color='Black',angle=60,hjust = 1,vjust = 1))+
  theme(panel.background = element_rect(fill = "white", colour = "#6D9EC1",size = 2, linetype ="solid"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "black"), panel.grid.minor = element_line(size = 1,linetype = 'solid',colour = "black")
  )
Plot1
```

The above visualization illustrates the total number of matches won by each team. In this graph we can clearly visualize that all the teams have different number of matches won. Out of 756 matches Mumbai Indians are at the top by winning 109 matches. Chennai Super Kings are at the second position as they have won 100 matches. CSK is then followed by Kolkata Knight Riders by winning 92 matches. This tells us the overall performance of each team in the entire history of Indian premier league. For now, let us consider these three teams to be the top. However, it is not right to decide the best team with a single reference, I have performed different analysis to see which team is the best.

## 8.2. Highest run margin of each team.  

```{r fig.width=10, fig.height=8}
#filtering rows having 'Na' in Win_by_runs and winner column
#Grouping by each winner to pick the max win margin for all the teams
Max_Dif<-Sorted_Match_data%>%
 filter(!is.na(win_by_runs)& !is.na(winner))%>%
 group_by(winner)%>%
 summarise(
    Total=max(win_by_runs)
  )
#Plots visualization of higest run margin of each team
Plot2<-ggplot(Max_Dif,aes(winner,Total))+geom_line(group=1,color='green',size=2)+
  geom_text(aes(label= Total, hjust= 0.5,vjust=-0.5),size=5)+
  theme(axis.text.x = element_text(color='Black',angle=60,hjust = 1,vjust = 1))+geom_point(shape=24, fill="gold", size=4)+labs(title = "Higest run margin of each winning team",
x= "Teams ",
y = "Max win Runs"
)+theme(
  panel.background = element_rect(fill = "white", colour = "#6D9EC1",
                                size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                colour = "black")
  )
Plot2
```

This visualization tells us about the team performance based on setting the target to the opponent team. In cricket the first batting team would set up a target to the opponent team and the opponent team would lose the match if all their 10 basemen are out or if they cannot reach the target set for them. This run which they will not be able to score is known as the run margin. The Graph shows the highest margin that has been set by each team. From this graph it can be clearly seen that Mumbai Indians have the highest run margin of 146 runs. In the second place we have Royal Challengers Bangalore with a run margin of 144 which is just 2 runs less from that of the Mumbai Indians. In the third position we have Kolkata Knight Riders who has a run margin of 140 runs. These is not much difference between run margins of these three teams. As this would show us the result of a single match letâ€™s see for the total run margin each team has in the next visualization.

## 8.3. Total run margin of each team.  

```{r fig.width=12, fig.height=8}
#From the below code i am visualizing  Total run margin of each team since 2008 till 2019
#filtering rows having 'Na' in Win_by_runs and winner column
#Grouping by winner to pick the sum of  win margin for all the teams
total_win_Runs<-Sorted_Match_data%>%
  filter(!is.na(winner))%>%
 group_by(winner)%>%
  summarise(
    Total=sum(win_by_runs)
  )
#total_win_Runs
plot3<-ggplot(total_win_Runs,aes(winner,Total))+geom_col(aes(fill=winner))+
  coord_flip()+geom_text(aes(label= Total, vjust= 0.5,hjust=0.9),size=5)+labs(title = "Total run margin of each team",
x= "Teams ",
y = "Total Runs"
)+ theme(axis.text.x = element_text(color="blue", angle=0,hjust = 1),legend.position = "none")+theme(
  panel.background = element_rect(fill = "white", colour = "#6D9EC1",
                                size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 1, linetype = 'solid',
                                colour = "black")
  )
plot3
```

This plot shows us the total margin runs by summing the run margin of each match the team has won.The X-axis represents the teams and the Y-axis represents total of the Run Margins. Evidently, Mumbai Indians have the maximum total run margin of 1866 runs. This also tell us that the team has shown good performance in most of the matches. After Mumbai Indian it is the Chennai Super Kings who has the second highest total run margin of 1778. There is a difference of 88 runs. However, the Chennai super kings are not too far but Mumbai Indians are ahead of Chennai Super kings. In the third position we have the Royal Challengers Bangalore with a total run margin of 1252 and there is a huge difference when compared with that of Mumbai Indians and Chennai Super Kings.

## 8.4. Very close victory.  

```{r fig.width=10, fig.height=8}
#below code Filter match where win_by_runs is 0 and next filter again for least win_by_runs, select the winner, win_by_runs and season
close_win<-Sorted_Match_data%>%
  filter(win_by_runs != 0)%>%
  filter(win_by_runs == min(win_by_runs))%>%
  select(winner, win_by_runs,season)

#Plots number of matches won by teams with minimum win_by_runs
  plot4<-ggplot(close_win)+
  geom_bar(aes(winner,fill = factor(season)))+coord_flip()+labs(title = "Close victory",
x= "Teams ",
y = "Number Of matches")+theme(
  panel.background = element_rect(fill = "white", colour = "#6D9EC1",
                                size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 1, linetype = 'solid',
                                colour = "black"), 
  panel.grid.minor = element_line(size = 1, linetype = 'solid',
                                colour = "white")
  )+labs(fill = "IPL Season")
plot4
```

When a match is won by the team with a narrow margin it a very close victory. This visualization would tell us the performance of the bowlers in the team in depth overs of the match. Usually in Indian premier league the depth overs are very crucial, sometimes even 20 runs from 6 balls would not be a big deal. The X-axis represents the number of matches and the Y-axis represents teams. The above plot tells us the about the teams which has won the matches only with 1 run. We can clearly observe that Mumbai Indians have won 3 matches by defending the batsmen of the opponent team. On the other hand, we have Royal Challengers Bangalore and Kings eleven Punjab who have won 2 very close matches. From this we can clearly tell that Mumbai Indians have shown a tremendous performance in most of the matches of Indian Premier League.

## 8.5. Percentage of toss deciding the match result.

```{r message=FALSE,warning=FALSE,fig.width=5, fig.height=5,title = element_text(size = 1)}
#filter match where winner contains 'Na'.(Match with no results)
#count the matches won by each team
#Pick the for which team has won both the toss and match#Pick the for which team has won both the toss and match
#Sum the matches
Toss_won <- Sorted_Match_data%>%
  filter(!is.na(winner))
both_win <- as.character(Toss_won$toss_winner)==as.character(Toss_won$winner)
Won_toss_and_Match<-Toss_won[both_win,]
Won_after_Winning_toss<-count(Won_toss_and_Match,winner)
total1=sum(Won_after_Winning_toss$n)

#filter match where winner contains 'Na'.(Match with no results)
#Pick the team which haslost the toss but won the match
#count the matches won by each team
#Sum the matches
toss_lost <- Sorted_Match_data%>%
  filter(!is.na(winner))
Won_only_Match <- as.character(toss_lost$toss_winner)!=as.character(toss_lost$winner)
Won_match_lost_toss<-toss_lost[Won_only_Match,]
Won_after_loosing_toss<-count(Won_match_lost_toss,winner)

total2=sum(Won_after_loosing_toss$n)
#calculating the percentage and plotting a pai chart to represent the %
tos_data <- data.frame(
  group = c("won_toss_won_match","lost_toss_won_match"),
  value = c(total1,total2)
  )

New_data<-tos_data%>%
mutate(Group = factor(tos_data$group),
          cumulative = cumsum(value),
          midpoint = cumulative - value / 2,
          label = paste(round(value / sum(value) * 100, 1), "%"))

ggplot(New_data, aes(x = 1, weight = value, fill = group)) +
   geom_bar() +
   coord_polar(theta = "y") +
   geom_text(aes(x = 1, y = midpoint, label = label),size=6)+theme(panel.grid = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(), legend.position="left", axis.title = element_blank())+labs(title = "% of match won v/s Winning & loosing toss")+theme(
  plot.title = element_text(color = "Blue", size = 8, face = "bold"),
  plot.subtitle = element_text(color = "gold"),
  plot.caption = element_text(color = "green", face = "italic")
)+ labs(fill = "Scenario")
```


This visualization is based on the percentage of matches won after winning the toss verses the percentage of the matches won after losing the toss.The pie-chart shows the percentage of two scenarios mentioned above. We can see that the winning percentage of matches won if they had won the toss is 52.3% whereas the matches which were won after losing the toss is 47.7%. There is only a difference of 4.6%, which means there is a probability of 4.6% for the team winning the toss to win the match too.However, winning the toss would be a plus point for the team as we have a bit greater percentage of matches won if the team has won the toss.

## 8.6. Visualization of the teams winning the match in different cities.

```{r fig.width=10, fig.height=10,message=FALSE}
#filter the match with no results
#group by the city and winner
#count the matches won by each team in different city
data<-Sorted_Match_data%>%
  filter(!is.na(winner)&(!is.na(city)))%>%
  group_by(winner,city)
data1<-count(data,city)

g <- ggplot(data1, aes(winner, city))
plot6<-g+geom_point(aes(col=winner, size=n))+geom_smooth()+theme(axis.text.x = element_text(color="blue", angle=60,hjust = 1))+labs(title = "Matches won by each team at different cities",x= "Teams ",y = "Cities match Won")
plot6
```

As IPL teams have a great fan following in their respective cities and most of their practice session would be held in their home ground which would help them perform better as they would be having a better hold on their home ground.There would also be more support from the audience to the team playing in their home ground. From this visualization we can clearly see at which city each team have won most of their matches. In IPL we have 8 constant teams who have been playing since 2008, they are Kolkata Knight Riders(KOlkata), Chennai Super Kings(Chennai), Delhi Daredevils(Delhi), Royal Challengers Bangalore(Bengaluru), Rajasthan Royals(Jaipur), Kings XI Punjab(Chandigarh), Mumbai Indians(Mumbai) and Sunrisers Hyderabad(Hyderabad). Whereas the rest of the team have played in one or two seasons of the IPL for all the eight teams mentioned above with the city they represent, it is crystal clear that each team have most of their matches won in their home city.

# 9. Conclusion:

The visualization in R would make our analysis easy and answer the required questions that arises with respect to a dataset. Likewise, it has helped me to analyse and understand various things. From the first four analysis we could see that Mumbai Indians team have most of the matches in the history of IPL. When coming to the run margin again it is the Mumbai Indians team who has the maximum run margin and have the highest total of the run margin. From the analysis of the close victories we could see that Mumbai Indians team have three close victory which shows the ability of the team to handle the match even at the last moment of the match. From all these points we can clearly state that â€œMumbai Indians is the best IPL team till dateâ€.  

When coming to the factors other than players performance that would impact the match result, it was found that 4.6% of matches are won by the team which has won the toss. In the visualization of the team winning the matches in different cities, we have seen more evidently that each team have won most of their matches in their home city. From this two visualization we can state that "Toss and the city the match is played have an impact on the match results".

\newpage

# Reference:


```{r}
```


```{r}
```


```{r}
