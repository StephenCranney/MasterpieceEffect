library(data.table)
library(stargazer)
library(readxl)

# setting the file directory:

setwd("/Users/stephencranney/Desktop/")

#Data is here: https://osf.io/ve5yn/

# ------ Analyzing the Masterpiece effect on the Experimental sample ------------------
# ----------------------- w/o the control group ---------------------------------
# --------------------------- (N=906 Businesses) --------------------------------------

#Includes the anonymized data of the entire experimental sample, without the control group (N=906 businesses).
#Not the businesses that were only included afterwards.Matches OA4.4 numbers. 

Dataset <- read.csv("Coding_No_Control.csv", header=TRUE, sep=",", quote="\"", row.names = NULL,
                    dec = ".", fill = TRUE, comment.char = "", stringsAsFactors=TRUE)
DTA=data.table(Dataset)

#Setting up different kinds of responses.
Dataset$Same.Sex.String<-ifelse(Dataset$Same.Sex==1, "Same-Sex", "Opposite-Sex")
table(Dataset$Same.Sex.String, Dataset$Same.Sex)
Dataset$Post_Court.String<-ifelse(Dataset$Post_Court==1, "Post Court", "Pre Court")
Dataset$Binary.Response.String<-ifelse(Dataset$Binary.Response==1, "Positive response", "Not a positive response")

#Creating a measure of binary response for a hard rejection. 
Dataset$Binary.Response_Rej<- ifelse(Dataset$Nuanced.Response==-1 | Dataset$Nuanced.Response==-.5,1,0)
DTA$Binary.Response_Rej<- ifelse(DTA$Nuanced.Response==-1 | DTA$Nuanced.Response==-.5,1,0)

table(Dataset$Binary.Response_Rej, Dataset$Nuanced.Response)

# nuanced response (scale = 1 'positive response', 
#                 .5 'asking for more information (date/location)', 
#                  0 'no response',
#                -.5 'refusal yet references to other providers/services', 
#                 -1 'negative response')

table(DTA$Nuanced.Response) # == summary(as.factor((DTA$Nuanced.Response)))
#Shows that Binary.Response is a response for a hard acceptance. 
table(DTA$Binary.Response, DTA$Nuanced.Response)

####

#Subsetting for post_court responses
Post_Court<- DTA[DTA$Post_Court=="1",]
#Subsetting for before_court responses
Ante_Court <-DTA[DTA$Post_Court=="0",]

#Subsetting for same-sex
SS<- DTA[DTA$Same.Sex=="1",]
#Subsetting for opposite-sex
OS<- DTA[DTA$Same.Sex=="0",]

#########################
#Our analysis led to different results than those reached by Professor Barak-Corren’s conclusions based on non-responses.  
#For opposite-sex couples, the explicit rejection rate before Masterpiece was 5.6% while after Masterpiece that rate was 8.7%.   
#This difference is statistically significant at p=.011.  

t.test(Binary.Response_Rej~Post_Court, data=OS) # Explicit rejections incresed from 5.6% to 8.7%, significant at .011
t.test(Binary.Response_Rej~Post_Court, data=SS) #Not significant (.06)

#Replicating in-text numbers, compare to table OA4.4.  
t.test(Binary.Response~Post_Court, data=OS)

#While Professor Barak-Corren points out that there is a significant decline in willingness to serve same-sex couples between 
#Wave 1 and Waves 3 and 4, when we perform the same analysis on opposite-sex couples we find that their decline  just missed significance
#by a hair at p=.053 (or a 5.3% chance that the decline was due to statistical chance; again anything under .05 being significant) 

#Almost sig (.053) for their binary for mixed-sex after, clearly sig for same-sex
t.test(Binary.Response~Post_Court, data=OS) #Matches table OA4.4. 
t.test(Binary.Response~Post_Court, data=SS) #Matches table OA4.4. 

#We examined only those businesses that explicitly declined to serve a same-sex couple before Masterpiece and looked at how many in this group also 
#explicitly declined to serve an opposite-sex couple. 

#Subset businesses that explicitly declined to serve same-sex before masterpiece. 
PreServeSS<-subset(Dataset, Same.Sex==1 & Post_Court==0 & Binary.Response_Rej==1)
#Reduce it down to one column (the business ID)
myvars <- c("Business")
PreServeSS <- as.data.table(PreServeSS[myvars])
#Merge that list of businesses onto the main dataset, so now we just have businesses that explicitly declined to serve same-sex before masterpiece.
PreServeSS2<-merge(PreServeSS, Dataset, by="Business")

#Before the decision, we found a stunning 41% gap, with only 59% of those who refused to serve
#same-sex couples also refusing to serve opposite-sex couples: this difference was highly statistically significant [p<.001]). 
PreServeSS44<-subset(PreServeSS2, Post_Court==0)
t.test(Binary.Response_Rej~Same.Sex, data=PreServeSS44) 

#However, after the decision, the same pre-Masterpiece  group that explicitly declined an inquiry to provide services for a same-sex wedding 
#was no more likely to explicitly deny a same-sex inquiry (59% rejection) than an opposite-sex inquiry (52% rejection) 
#Subset to only have post-Masterpiece cases
PreServeSS5<-subset(PreServeSS2, Post_Court==1)
t.test(Binary.Response_Rej~Same.Sex, data=PreServeSS5) 

#For this group, there was a statistically-significant decline in same-sex rejections post-Masterpiece, 
PreServeSS3<-subset(PreServeSS2, Same.Sex==1)
t.test(Binary.Response_Rej~Post_Court, data=PreServeSS3)

#while opposite-sex rejections did not significantly decline.  
PreServeSS4<-subset(PreServeSS2, Same.Sex==0)
t.test(Binary.Response_Rej~Post_Court, data=PreServeSS4) 

