library(data.table)
library(survival)
library(survminer)
library(dplyr)
setwd("/Users/hp/Desktop/work exposure project/Primarydata/")
list.files()

###提取ONET数据###
Abilities <- as.data.frame(fread("Abilities.csv"))
Activities <- as.data.frame(fread("Activites.csv"))
Context <- as.data.frame(fread("Context.csv"))
Newdata<-function(x,j){
  data<-grep(x,j[,3])
  data<-j[data,]
  return(data)
}

### Physical exposures ###
GenPsysc <- Newdata("Performing General Physical Activities",Activities)
GenPsysc <- group_by(GenPsysc,`O*NET-SOC Code`)
GenPsysc <- summarise(GenPsysc,
                      GenPsysc_avg = mean(`Data Value`))
StcSth<-Newdata("Static Strength",Abilities)
StcSth <- group_by(StcSth,`O*NET-SOC Code`)
StcSth <- summarise(StcSth,
                    StcSth_avg = mean(`Data Value`))
DsnSth <- Newdata("Dynamic Strength",Abilities)
DsnSth <- group_by(DsnSth,`O*NET-SOC Code`)
DsnSth <- summarise(DsnSth,
                    DsnSth_avg = mean(`Data Value`))
Stamina <- Newdata("Stamina",Abilities)
Stamina <- group_by(Stamina,`O*NET-SOC Code`)
Stamina <- summarise(Stamina,
                     Stamina_avg = mean(`Data Value`))
Sitting <- Newdata("Spend Time Sitting",Context)
Sitting <- group_by(Sitting,SOCCode)
Sitting <- summarise(Sitting,
                     Sit_avg = mean(Value))
Standing <- Newdata("Spend Time Standing",Context)
Standing <- group_by(Standing,SOCCode)
Standing <- summarise(Standing,
                      Stand_avg = mean(Value))
WalkRun<-Newdata("Spend Time Walking and Running",Context)
WalkRun <- group_by(WalkRun,SOCCode)
WalkRun <- summarise(WalkRun,
                     WalkRun_avg = mean(Value))

WorkWithoutMental_Physical <- left_join(WorkWithoutMental8,GenPsysc,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Physical <- left_join(WorkWithoutMental_Physical,StcSth,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Physical <- left_join(WorkWithoutMental_Physical,DsnSth,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Physical <- left_join(WorkWithoutMental_Physical,Stamina,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Physical <- left_join(WorkWithoutMental_Physical,Sitting,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Physical <- left_join(WorkWithoutMental_Physical,Standing,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Physical <- left_join(WorkWithoutMental_Physical,WalkRun,by= c("USSOC"="SOCCode"))
fwrite(WorkWithoutMental_Physical,"WorkWithoutMental_Physical8.csv")

WorkWith_Physical <- left_join(WorkWithBaseline,GenPsysc,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Physical <- left_join(WorkWith_Physical,StcSth,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Physical <- left_join(WorkWith_Physical,DsnSth,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Physical <- left_join(WorkWith_Physical,Stamina,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Physical <- left_join(WorkWith_Physical,Sitting,by= c("USSOC"="SOCCode"))
WorkWith_Physical <- left_join(WorkWith_Physical,Standing,by= c("USSOC"="SOCCode"))
WorkWith_Physical <- left_join(WorkWith_Physical,WalkRun,by= c("USSOC"="SOCCode"))
fwrite(WorkWith_Physical,"WorkWith_Physical.csv")

### Social interactions ###
ComSuperior <- Newdata("Communicating with Supervisors, Peers, or Subordinates",Activities)
ComSuperior <- group_by(ComSuperior,`O*NET-SOC Code`)
ComSuperior <- summarise(ComSuperior,
                         ComSuperior_avg = mean(`Data Value`))
ComOut <- Newdata("Communicating with People Outside the Organization",Activities)
ComOut <- group_by(ComOut,`O*NET-SOC Code`)
ComOut <- summarise(ComOut,
                    ComOut_avg = mean(`Data Value`))
InterRelation <- Newdata("Establishing and Maintaining Interpersonal Relationships",Activities)
InterRelation <- group_by(InterRelation,`O*NET-SOC Code`)
InterRelation <- summarise(InterRelation,
                           InterRelation_avg = mean(`Data Value`))
Conflict <- Newdata("Resolving Conflicts and Negotiating with Others",Activities)
Conflict <- group_by(Conflict,`O*NET-SOC Code`)
Conflict <- summarise(Conflict,
                      Conflict_avg = mean(`Data Value`))
WithPublic <- Newdata("Performing for or Working Directly with the Public",Activities)
WithPublic <- group_by(WithPublic,`O*NET-SOC Code`)
WithPublic <- summarise(WithPublic,
                        WithPublic_avg = mean(`Data Value`))
Coord <- Newdata("Coordinating the Work and Activities of Others",Activities)
Coord <- group_by(Coord,`O*NET-SOC Code`)
Coord <- summarise(Coord,
                   Coord_avg = mean(`Data Value`))
Compreh <- Newdata("Oral Comprehension",Abilities)
Compreh <- group_by(Compreh,`O*NET-SOC Code`)
Compreh <- summarise(Compreh,
                     Compreh_avg = mean(`Data Value`))
Express <- Newdata("Oral Expression",Abilities)
Express <- group_by(Express,`O*NET-SOC Code`)
Express <- summarise(Express,
                     Express_avg = mean(`Data Value`))
PubSpeak <- Newdata("Public Speaking",Context)
PubSpeak <- group_by(PubSpeak,`SOCCode`)
PubSpeak <- summarise(PubSpeak,
                      PubSpeak_avg = mean(`Value`))
Face <- Newdata("Face-to-Face Discussions",Context)
Face <- group_by(Face,`SOCCode`)
Face <- summarise(Face,
                  Face_avg = mean(`Value`))
Contact <- Newdata("Contact With Others",Context)
Contact <- group_by(Contact,`SOCCode`)
Contact <- summarise(Contact,
                     Contact_avg = mean(`Value`))
TeamWork <- Newdata("Work With Work Group or Team",Context)
TeamWork <- group_by(TeamWork,`SOCCode`)
TeamWork <- summarise(TeamWork,
                      TeamWork_avg = mean(`Value`))
DealCust <- Newdata("Deal With External Customers",Context)
DealCust <- group_by(DealCust,`SOCCode`)
DealCust <- summarise(DealCust,
                      DealCust_avg = mean(`Value`))
CoordLead <- Newdata("Coordinate or Lead Others",Context)
CoordLead <- group_by(CoordLead,`SOCCode`)
CoordLead <- summarise(CoordLead,
                       CoordLead_avg = mean(`Value`))

WorkWithoutMental_Interaction <- left_join(WorkWithoutMental8,ComSuperior,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,ComOut,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,InterRelation,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,Conflict,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,WithPublic,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,Coord,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,Compreh,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,Express,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,PubSpeak,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,Face,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,Contact,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,TeamWork,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,DealCust,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Interaction <- left_join(WorkWithoutMental_Interaction,CoordLead,by= c("USSOC"="SOCCode"))
fwrite(WorkWithoutMental_Interaction,"WorkWithoutMental_Interaction8.csv")

WorkWith_Interaction <- left_join(WorkWithBaseline,ComSuperior,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,ComOut,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,InterRelation,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,Conflict,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,WithPublic,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,Coord,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,Compreh,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,Express,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,PubSpeak,by= c("USSOC"="SOCCode"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,Face,by= c("USSOC"="SOCCode"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,Contact,by= c("USSOC"="SOCCode"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,TeamWork,by= c("USSOC"="SOCCode"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,DealCust,by= c("USSOC"="SOCCode"))
WorkWith_Interaction <- left_join(WorkWith_Interaction,CoordLead,by= c("USSOC"="SOCCode"))
fwrite(WorkWith_Interaction,"WorkWith_Interaction.csv")


### Decision making, problem solving and creatively thinking ###
DecisProb <- Newdata("Making Decisions and Solving Problems",Activities)
DecisProb <- group_by(DecisProb,`O*NET-SOC Code`)
DecisProb <- summarise(DecisProb,
                       DecisProb_avg = mean(`Data Value`))
Think <- Newdata("Thinking Creatively",Activities)
Think <- group_by(Think,`O*NET-SOC Code`)
Think <- summarise(Think,
                   Think_avg = mean(`Data Value`))
Develop <- Newdata("Developing Objectives and Strategies",Activities)
Develop <- group_by(Develop,`O*NET-SOC Code`)
Develop <- summarise(Develop,
                     Develop_avg = mean(`Data Value`))
FluIdea <- Newdata("Fluency of Ideas",Abilities)
FluIdea <- group_by(FluIdea,`O*NET-SOC Code`)
FluIdea <- summarise(FluIdea,
                     FluIdea_avg = mean(`Data Value`))
Original <- Newdata("Originality",Abilities)
Original <- group_by(Original,`O*NET-SOC Code`)
Original <- summarise(Original,
                      Original_avg = mean(`Data Value`))
ProbSen <- Newdata("Problem Sensitivity",Abilities)
ProbSen <- group_by(ProbSen,`O*NET-SOC Code`)
ProbSen <- summarise(ProbSen,
                     ProbSen_avg = mean(`Data Value`))
DedReason <- Newdata("Deductive Reasoning",Abilities)
DedReason <- group_by(DedReason,`O*NET-SOC Code`)
DedReason <- summarise(DedReason,
                       DedReason_avg = mean(`Data Value`))
IndReason <- Newdata("Inductive Reasoning",Abilities)
IndReason <- group_by(IndReason,`O*NET-SOC Code`)
IndReason <- summarise(IndReason,
                       IndReason_avg = mean(`Data Value`))
FreqDecis <- Newdata("Frequency of Decision Making",Context)
FreqDecis <- group_by(FreqDecis,`SOCCode`)
FreqDecis <- summarise(FreqDecis,
                       FreqDecis_avg = mean(`Value`))
FreedDecis <- Newdata("Freedom to Make Decisions",Context)
FreedDecis <- group_by(FreedDecis,`SOCCode`)
FreedDecis <- summarise(FreedDecis,
                        FreedDecis_avg = mean(`Value`))

WorkWithoutMental_Decision <- left_join(WorkWithoutMental8,DecisProb,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,Think,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,Develop,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,FluIdea,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,Original,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,ProbSen,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,DedReason,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,IndReason,by= c("USSOC"="O*NET-SOC Code"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,FreqDecis,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Decision <- left_join(WorkWithoutMental_Decision,FreedDecis,by= c("USSOC"="SOCCode"))
fwrite(WorkWithoutMental_Decision,"WorkWithoutMental_Decision8.csv")

WorkWith_Decision <- left_join(WorkWithBaseline,DecisProb,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,Think,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,Develop,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,FluIdea,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,Original,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,ProbSen,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,DedReason,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,IndReason,by= c("USSOC"="O*NET-SOC Code"))
WorkWith_Decision <- left_join(WorkWith_Decision,FreqDecis,by= c("USSOC"="SOCCode"))
WorkWith_Decision <- left_join(WorkWith_Decision,FreedDecis,by= c("USSOC"="SOCCode"))
fwrite(WorkWith_Decision,"WorkWith_Decision.csv")


### Physically uncomfortable ###
Noise <- Newdata("Sounds, Noise Levels Are Distracting or Uncomfortable",Context)
Noise <- group_by(Noise,`SOCCode`)
Noise <- summarise(Noise,
                   Noise_avg = mean(`Value`))
Tempeprature <- Newdata("Very Hot or Cold Temperatures",Context)
Tempeprature <- group_by(Tempeprature,`SOCCode`)
Tempeprature <- summarise(Tempeprature,
                          Tempeprature_avg = mean(`Value`))
Light <- Newdata("Extremely Bright or Inadequate Lighting",Context)
Light <- group_by(Light,`SOCCode`)
Light <- summarise(Light,
                   Light_avg = mean(`Value`))
Contaminant <- Newdata("Exposed to Contaminants",Context)
Contaminant <- group_by(Contaminant,`SOCCode`)
Contaminant <- summarise(Contaminant,
                         Contaminant_avg = mean(`Value`))
Position <- Newdata("Cramped Work Space, Awkward Positions",Context)
Position <- group_by(Position,`SOCCode`)
Position <- summarise(Position,
                      Position_avg = mean(`Value`))
Vibration <- Newdata("Exposed to Whole Body Vibration",Context)
Vibration <- group_by(Vibration,`SOCCode`)
Vibration <- summarise(Vibration,
                       Vibration_avg = mean(`Value`))
Radiation <- Newdata("Exposed to Radiation",Context)
Radiation <- group_by(Radiation,`SOCCode`)
Radiation <- summarise(Radiation,
                       Radiation_avg = mean(`Value`))
Infection <- Newdata("Exposed to Disease or Infections",Context)
Infection <- group_by(Infection,`SOCCode`)
Infection <- summarise(Infection,
                       Infection_avg = mean(`Value`))
High <- Newdata("Exposed to High Places",Context)
High <- group_by(High,`SOCCode`)
High <- summarise(High,
                  High_avg = mean(`Value`))
HazardCon <- Newdata("Exposed to Hazardous Conditions",Context)
HazardCon <- group_by(HazardCon,`SOCCode`)
HazardCon <- summarise(HazardCon,
                       HazardCon_avg = mean(`Value`))
HazardEqu <- Newdata("Exposed to Hazardous Equipment",Context)
HazardEqu <- group_by(HazardEqu,`SOCCode`)
HazardEqu <- summarise(HazardEqu,
                       HazardEqu_avg = mean(`Value`))
Burn <- Newdata("Exposed to Minor Burns, Cuts, Bites, or Stings",Context)
Burn <- group_by(Burn,`SOCCode`)
Burn <- summarise(Burn,
                  Burn_avg = mean(`Value`))


WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental8,Noise,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Tempeprature,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Light,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Contaminant,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Position,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Vibration,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Radiation,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Infection,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,High,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,HazardCon,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,HazardEqu,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Uncomfort <- left_join(WorkWithoutMental_Uncomfort,Burn,by= c("USSOC"="SOCCode"))
fwrite(WorkWithoutMental_Uncomfort,"WorkWithoutMental_Uncomfort8.csv")

WorkWith_Uncomfort <- left_join(WorkWithBaseline,Noise,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Tempeprature,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Light,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Contaminant,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Position,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Vibration,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Radiation,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Infection,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,High,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,HazardCon,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,HazardEqu,by= c("USSOC"="SOCCode"))
WorkWith_Uncomfort <- left_join(WorkWith_Uncomfort,Burn,by= c("USSOC"="SOCCode"))
fwrite(WorkWith_Uncomfort,"WorkWith_Uncomfort.csv")

### Responsibility, competition and pressure ###
ResponOth <- Newdata("Responsible for Others' Health and Safety",Context)
ResponOth <- group_by(ResponOth,`SOCCode`)
ResponOth <- summarise(ResponOth,
                       ResponOth_avg = mean(`Value`))
ResponOut <- Newdata("Responsibility for Outcomes and Results",Context)
ResponOut <- group_by(ResponOut,`SOCCode`)
ResponOut <- summarise(ResponOut,
                       ResponOut_avg = mean(`Value`))
FreqConf <- Newdata("Frequency of Conflict Situations",Context)
FreqConf <- group_by(FreqConf,`SOCCode`)
FreqConf <- summarise(FreqConf,
                      FreqConf_avg = mean(`Value`))
Unpleasant <- Newdata("Deal With Unpleasant or Angry People",Context)
Unpleasant <- group_by(Unpleasant,`SOCCode`)
Unpleasant <- summarise(Unpleasant,
                        Unpleasant_avg = mean(`Value`))
Aggressive <- Newdata("Deal With Physically Aggressive People",Context)
Aggressive <- group_by(Aggressive,`SOCCode`)
Aggressive <- summarise(Aggressive,
                        Aggressive_avg = mean(`Value`))
Error <- Newdata("Consequence of Error",Context)
Error <- group_by(Error,`SOCCode`)
Error <- summarise(Error,
                   Error_avg = mean(`Value`))
Impact <- Newdata("Impact of Decisions on Co-workers or Company Results",Context)
Impact <- group_by(Impact,`SOCCode`)
Impact <- summarise(Impact,
                    Impact_avg = mean(`Value`))
Competition <- Newdata("Level of Competition",Context)
Competition <- group_by(Competition,`SOCCode`)
Competition <- summarise(Competition,
                         Competition_avg = mean(`Value`))
Time <- Newdata("Time Pressure",Context)
Time <- group_by(Time,`SOCCode`)
Time <- summarise(Time,
                  Time_avg = mean(`Value`))

WorkWithoutMental_Responsible <- left_join(WorkWithoutMental8,ResponOth,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,ResponOut,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,FreqConf,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,Unpleasant,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,Aggressive,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,Error,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,Impact,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,Competition,by= c("USSOC"="SOCCode"))
WorkWithoutMental_Responsible <- left_join(WorkWithoutMental_Responsible,Time,by= c("USSOC"="SOCCode"))
fwrite(WorkWithoutMental_Responsible,"WorkWithoutMental_Responsible8.csv")

WorkWith_Responsible <- left_join(WorkWithBaseline,ResponOth,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,ResponOut,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,FreqConf,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,Unpleasant,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,Aggressive,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,Error,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,Impact,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,Competition,by= c("USSOC"="SOCCode"))
WorkWith_Responsible <- left_join(WorkWith_Responsible,Time,by= c("USSOC"="SOCCode"))
fwrite(WorkWith_Responsible,"WorkWith_Responsible.csv")

library(data.table)
library(survival)
library(survminer)
library(dplyr)
setwd("/Users/hp/Desktop/work exposure project/Primarydata/")
list.files()
WorkPhysical <- as.data.frame(fread("WorkWithoutMental_Physical8.csv"))
WorkInteraction <- as.data.frame(fread("WorkWithoutMental_Interaction8.csv"))
WorkDecision <- as.data.frame(fread("WorkWithoutMental_Decision8.csv"))
WorkResponsible <- as.data.frame(fread("WorkWithoutMental_Responsible8.csv"))
WorkUncomfort <- as.data.frame(fread("WorkWithoutMental_Uncomfort8.csv"))
Anxiety <- as.data.frame(fread("Anxiety_FO.csv"))
Depression <- as.data.frame(fread("Depression_FO.csv"))
model<-vector(mode = "list",length = 4)

Main_Anxiety <- left_join(WorkUncomfort,Anxiety ,by="ID")
Main_Depression <- left_join(WorkUncomfort,Depression ,by="ID")

### Main Analyses ###
Main_Cox <- function(d,i){
  result1 <- coxph(Surv(DATE,IF)~Burn_avg,data=Main_Anxiety)
  result2 <- coxph(Surv(DATE,IF)~Burn_avg+Age+Sex+Ancestory+Socioeconomic+Qaulification,data=Main_Anxiety)
  result3 <- coxph(Surv(DATE,IF)~Burn_avg+Age+Sex+Ancestory+Socioeconomic+Qaulification+Years+Hours+TravelFreq+Distance,data=Main_Anxiety)
  result4 <- coxph(Surv(DATE,IF)~Burn_avg+Age+Sex+Ancestory+Socioeconomic+Qaulification+Obesity+Smoking+Alcohol+SocIso,data=Main_Anxiety)
  test<-list(result1,result2,result3,result4)
  for (j in 1:4) {
    model[[j]]<-summary(test[[j]])
    coefficients=as.data.frame(model[[j]]$coefficients)
    conf.int = as.data.frame(model[[j]]$conf.int)
    result = cbind(coefficients,conf.int[,3:4])
    setwd(dir = "/Users/hp/Desktop/work exposure project/Results/Prospective cohort/Mental only 8 years/Uncomfortable/Anxiety/")
    fwrite(result,paste("Cox_",i,j,".csv",sep = ""))
  }
}
Main_Cox(Main_Anxiety,"Burn_Anxiety")

##### Forest Plot #####
setwd("/Users/hp/Desktop/work exposure project/Results/Prospective cohort/Mental only 8 years/")
list.files()
library(forestplot)
forestplot <- read.csv("ForestDATA_Anxiety.csv",header = FALSE,fileEncoding = "UTF-8-BOM")
tabletext = cbind(c("Work Exposures",
                    "Physical Activities",
                    "    General physical activities","    Static strength","    Dynamic strength",
                    "    Stamina","    Sitting","    Standing","    Walk and Run",
                    "Social Interactions",
                    "    Supervisors, Peers, or Subordinates","    People Outside the Organization",
                    "    Interpersonal Relationships","    Conflicts and Negotiating",
                    "    Directly with the Public","    Coordinating the Work and Activities",
                    "    Oral Comprehension","    Oral Expression","    Public Speaking",
                    "    Face-to-Face Discussions","    Contact With Others","    Work With Work Group or Team",
                    "    Deal With External Customers","    Coordinate or Lead Others",
                    "Thinking, Decision and problem solving",
                    "    Decision and Problem solving","    Thinking Creatively","    Develop",
                    "    Fluency of Ideas","    Originality","    Problem Sensitivity","    Deductive Reasoning",
                    "    Inductive Reasoning","    Decision Frequency","    Decision Freedom",
                    "Responsibility, competition and pressure ",
                    "    Responsible for Others","    Responsibility for Outcomes","    Frequency of Conflict",
                    "    Unpleasant or Angry People","    Physically Aggressive People","    Consequence of Error",
                    "    Impact of Decisions","    Level of Competition","    Time Pressure",
                    "Physical Uncomfortable",
                    "    Noise","    Temperature","    Light","    Contaminants","    Work Space",
                    "    Whole Body Vibration","    Radiation","    Disease or Infections","    High Places",
                    "    Hazardous Conditions","    Hazardous Equipment","    Minor Burns, Cuts, Bites, or Stings"),
                  c(forestplot$V2),
                  c(forestplot$V3))
pdf('forestplot_Anxiety.pdf',width = 18, height = 25)
forestplot(tabletext,
           mean = forestplot$V4,
           lower = forestplot$V5,
           upper = forestplot$V6,
           is.summary=c(T,T,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,F,F,F),
           new_page=FALSE,
           zero=1,
           boxsize=0.3,
           lineheight=unit(1.0,'cm'),
           colgap=unit(2,'cm'),
           lwd.zero=2,
           lwd.ci=2,
           col=fpColors(box="#F08080",summary="#F08080",line="#F08080",zero="#999999"),
           xlab='Hazard ratio',
           lwd.xaxis=3,
           xticks=c(seq(from=0.70,to=1.30,by=0.1)),
           lty.ci=1,
           fn.ci_norm=fpDrawNormalCI,
           clip =c(0.5, 1.5),
           line.margin=.1,
           graph.pos=2,
           txt_gp=fpTxtGp(xlab=gpar(fontsize=30,col='black'),title=gpar(fontsize=24),ticks=gpar(fontsize=30)),
           
)
dev.off()

forestplot <- read.csv("ForestDATA_Depression.csv",header = FALSE,fileEncoding = "UTF-8-BOM")
tabletext = cbind(c("Work Exposures",
                    "Physical Activities",
                    "    General physical activities","    Static strength","    Dynamic strength",
                    "    Stamina","    Sitting","    Standing","    Walk and Run",
                    "Social Interactions",
                    "    Supervisors, Peers, or Subordinates","    People Outside the Organization",
                    "    Interpersonal Relationships","    Conflicts and Negotiating",
                    "    Directly with the Public","    Coordinating the Work and Activities",
                    "    Oral Comprehension","    Oral Expression","    Public Speaking",
                    "    Face-to-Face Discussions","    Contact With Others","    Work With Work Group or Team",
                    "    Deal With External Customers","    Coordinate or Lead Others",
                    "Thinking, Decision and problem solving",
                    "    Decision and Problem solving","    Thinking Creatively","    Develop",
                    "    Fluency of Ideas","    Originality","    Problem Sensitivity","    Deductive Reasoning",
                    "    Inductive Reasoning","    Decision Frequency","    Decision Freedom",
                    "Responsibility, competition and pressure ",
                    "    Responsible for Others","    Responsibility for Outcomes","    Frequency of Conflict",
                    "    Unpleasant or Angry People","    Physically Aggressive People","    Consequence of Error",
                    "    Impact of Decisions","    Level of Competition","    Time Pressure",
                    "Physical Uncomfortable",
                    "    Noise","    Temperature","    Light","    Contaminants","    Work Space",
                    "    Whole Body Vibration","    Radiation","    Disease or Infections","    High Places",
                    "    Hazardous Conditions","    Hazardous Equipment","    Minor Burns, Cuts, Bites, or Stings"),
                  c(forestplot$V2),
                  c(forestplot$V3))
pdf('forestplot_Depression.pdf',width = 18, height = 25)
forestplot(tabletext,
           mean = forestplot$V4,
           lower = forestplot$V5,
           upper = forestplot$V6,
           is.summary=c(T,T,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,
                        T,F,F,F,F,F,F,F,F,F,F,F,F),
           new_page=FALSE,
           zero=1,
           boxsize=0.3,
           lineheight=unit(1.0,'cm'),
           colgap=unit(2,'cm'),
           lwd.zero=2,
           lwd.ci=2,
           col=fpColors(box="#F08080",summary="#F08080",line="#F08080",zero="#999999"),
           xlab='Hazard ratio',
           lwd.xaxis=3,
           xticks=c(seq(from=0.75,to=1.35,by=0.1)),
           lty.ci=1,
           fn.ci_norm=fpDrawNormalCI,
           clip =c(0.5, 1.5),
           line.margin=.1,
           graph.pos=2,
           txt_gp=fpTxtGp(xlab=gpar(fontsize=30,col='black'),title=gpar(fontsize=24),ticks=gpar(fontsize=30)),
           
)
dev.off()

library(data.table)
library(survival)
library(survminer)
library(dplyr)
setwd("D:/work/UKB_subset/")
memory.limit(size=100000)
UKB4 <- as.data.frame(fread("UKB_subset_4.csv"))
Nscore <- UKB4[,c("eid","20127-0.0")] 
write.csv(Nscore,"Nscore.csv")
setwd("/Users/hp/Desktop/work exposure project/Primarydata/")
list.files()
WorkPhysical <- as.data.frame(fread("WorkWithoutMental_Physical8.csv"))
WorkInteraction <- as.data.frame(fread("WorkWithoutMental_Interaction8.csv"))
WorkDecision <- as.data.frame(fread("WorkWithoutMental_Decision8.csv"))
WorkResponsible <- as.data.frame(fread("WorkWithoutMental_Responsible8.csv"))
WorkUncomfort <- as.data.frame(fread("WorkWithoutMental_Uncomfort8.csv"))
Anxiety <- as.data.frame(fread("Anxiety_FO.csv"))
Depression <- as.data.frame(fread("Depression_FO.csv"))

##### Subgroup #####
Nscore <-  Nscore[,c("eid","20127-0.0")]
Nscore <- rename(Nscore,"ID"="eid")
library(psych)
describe(Nscore$`20127-0.0`) #mean=4.12
quantile(Nscore$`20127-0.0`,na.rm = T)
WorkPhysical <- left_join(WorkPhysical,Nscore)

White <- WorkResponsible[which(WorkResponsible$Ancestory=="White"),]
Others <- WorkResponsible[which(WorkResponsible$Ancestory=="Others"),]
White_Anxiety <- left_join(White,Anxiety,by="ID")
Others_Anxiety <- left_join(Others,Anxiety,by="ID")
White_Depression <- left_join(White,Depression,by="ID")
Others_Depression <- left_join(Others,Depression,by="ID")

Cox_Main <- function(d,i){
  result4 <- coxph(Surv(DATE,IF)~Time_avg+Age+Sex
                   +Qaulification+Socioeconomic
                   +Obesity+Smoking+Alcohol+SocIso,data=d)
  model4 <- summary(result4)
  coefficients=as.data.frame(model4$coefficients)
  conf.int = as.data.frame(model4$conf.int)
  result = cbind(coefficients,conf.int[,3:4])
  setwd(dir = "/Users/hp/Desktop/work exposure project/Results/Prospective cohort/Subgroup_8years/Ethicicy/")
  fwrite(result,paste("Cox_Time__",i,".csv",sep = ""))
}
Cox_Main(White_Anxiety,"White_Anxiety")
Cox_Main(Others_Anxiety,"Others_Anxiety")
Cox_Main(White_Depression,"White_Depression")
Cox_Main(Others_Depression,"Others_Depression")


AgeY <- WorkUncomfort[which(WorkUncomfort$Age<=60),]
AgeO <- WorkUncomfort[which(WorkUncomfort$Age>60),]
AgeY_Anxiety <- left_join(AgeY,Anxiety,by="ID")
AgeO_Anxiety <- left_join(AgeO,Anxiety,by="ID")
AgeY_Depression <- left_join(AgeY,Depression,by="ID")
AgeO_Depression <- left_join(AgeO,Depression,by="ID")


Male <- WorkUncomfort[which(WorkUncomfort$Sex==1),]
Female <- WorkUncomfort[which(WorkUncomfort$Sex==2),]
Male_Anxiety <- left_join(Male,Anxiety,by="ID")
Female_Anxiety <- left_join(Female,Anxiety,by="ID")
Male_Depression <- left_join(Male,Depression,by="ID")
Female_Depression <- left_join(Female,Depression,by="ID")

LowEdu <- WorkUncomfort[which(WorkUncomfort$Qaulification==0),]
HighEdu <- WorkUncomfort[which(WorkUncomfort$Qaulification==1),]
LowEdu_Anxiety <- left_join(LowEdu,Anxiety,by="ID")
HighEdu_Anxiety <- left_join(HighEdu,Anxiety,by="ID")
LowEdu_Depression <- left_join(LowEdu,Depression,by="ID")
HighEdu_Depression <- left_join(HighEdu,Depression,by="ID")

Poor <- WorkUncomfort[which(WorkUncomfort$Socioeconomic>0),]
Rich <- WorkUncomfort[which(WorkUncomfort$Socioeconomic<=0),]
Poor_Anxiety <- left_join(Poor,Anxiety,by="ID")
Rich_Anxiety <- left_join(Rich,Anxiety,by="ID")
Poor_Depression <- left_join(Poor,Depression,by="ID")
Rich_Depression <- left_join(Rich,Depression,by="ID")

NFat <- WorkUncomfort[which(WorkUncomfort$Obesity<=24),]
Overweight <- WorkUncomfort[which(WorkUncomfort$Obesity>24),]
Overweight <- Overweight[which(Overweight$Obesity<=28),]
Fat <- WorkUncomfort[which(WorkUncomfort$Obesity>28),]
NFat_Anxiety <- left_join(NFat,Anxiety,by="ID")
Overweight_Anxiety <- left_join(Overweight,Anxiety,by="ID")
Fat_Anxiety <- left_join(Fat,Anxiety,by="ID")
NFat_Depression <- left_join(NFat,Depression,by="ID")
Overweight_Depression <- left_join(Overweight,Depression,by="ID")
Fat_Depression <- left_join(Fat,Depression,by="ID")

Neversmoke <- WorkUncomfort[which(WorkUncomfort$Smoking==1),]
Smoked <- WorkUncomfort[which(WorkUncomfort$Smoking==2),]
CurrentSmoke <- WorkUncomfort[which(WorkUncomfort$Smoking==3),]
Neversmoke_Anxiety <- left_join(Neversmoke,Anxiety,by="ID")
Smoked_Anxiety <- left_join(Smoked,Anxiety,by="ID")
CurrentSmoke_Anxiety <- left_join(CurrentSmoke,Anxiety,by="ID")
Neversmoke_Depression <- left_join(Neversmoke,Depression,by="ID")
Smoked_Depression <- left_join(Smoked,Depression,by="ID")
CurrentSmoke_Depression <- left_join(CurrentSmoke,Depression,by="ID")

Drink <- WorkUncomfort[which(WorkUncomfort$Alcohol>0),]
NeverDrink <- Drink[which(Drink$Alcohol==6),]
Specialonly <- Drink[which(Drink$Alcohol<6),]
Specialonly <- Specialonly[which(Specialonly$Alcohol>3),]
PerWeekorMore <- Drink[which(Drink$Alcohol<=3),]
NeverDrink_Anxiety <- left_join(NeverDrink,Anxiety,by="ID")
Specialonly_Anxiety <- left_join(Specialonly,Anxiety,by="ID")
PerWeekorMore_Anxiety <- left_join(PerWeekorMore,Anxiety,by="ID")
NeverDrink_Depression <- left_join(NeverDrink,Depression,by="ID")
Specialonly_Depression <- left_join(Specialonly,Depression,by="ID")
PerWeekorMore_Depression <- left_join(PerWeekorMore,Depression,by="ID")

Social <- WorkUncomfort[which(WorkUncomfort$SocIso>0),]
MoreSocial <- Social[which(Social$SocIso<=3),]
LessSocial <- Social[which(Social$SocIso>=4),]
MoreSocial_Anxiety <- left_join(MoreSocial,Anxiety,by="ID")
LessSocial_Anxiety <- left_join(LessSocial,Anxiety,by="ID")
MoreSocial_Depression <- left_join(MoreSocial,Depression,by="ID")
LessSocial_Depression <- left_join(LessSocial,Depression,by="ID")

Cox_Main <- function(d,i){
  result4 <- coxph(Surv(DATE,IF)~Burn_avg+Age+Sex+Ancestory
                   +Qaulification+Socioeconomic
                   +Obesity+Smoking+Alcohol+SocIso,data=d)
  model4 <- summary(result4)
  coefficients=as.data.frame(model4$coefficients)
  conf.int = as.data.frame(model4$conf.int)
  result = cbind(coefficients,conf.int[,3:4])
  setwd(dir = "/Users/hp/Desktop/work exposure project/Results/Prospective cohort/Subgroup_8years/Uncomfortable/Burn/")
  fwrite(result,paste("Cox_",i,".csv",sep = ""))
}

Cox_Main(AgeY_Anxiety,"AgeY_Anxiety")
Cox_Main(AgeO_Anxiety,"AgeO_Anxiety")
Cox_Main(AgeY_Depression,"AgeY_Depression")
Cox_Main(AgeO_Depression,"AgeO_Depression")

Cox_Main(Male_Anxiety,"Male_Anxiety")
Cox_Main(Female_Anxiety,"Female_Anxiety")
Cox_Main(Male_Depression,"Male_Depression")
Cox_Main(Female_Depression,"Female_Depression")

Cox_Main(LowEdu_Anxiety,"LowEdu_Anxiety")
Cox_Main(HighEdu_Anxiety,"HighEdu_Anxiety")
Cox_Main(LowEdu_Depression,"LowEdu_Depression")
Cox_Main(HighEdu_Depression,"HighEdu_Depression")

Cox_Main(Poor_Anxiety,"Poor_Anxiety")
Cox_Main(Rich_Anxiety,"Rich_Anxiety")
Cox_Main(Poor_Depression,"Poor_Depression")
Cox_Main(Rich_Depression,"Rich_Depression")

Cox_Main(NFat_Anxiety,"NFat_Anxiety")
Cox_Main(Overweight_Anxiety,"Overweight_Anxiety")
Cox_Main(Fat_Anxiety,"Fat_Anxiety")
Cox_Main(NFat_Depression,"NFat_Depression")
Cox_Main(Overweight_Depression,"Overweight_Depression")
Cox_Main(Fat_Depression,"Fat_Depression")

Cox_Main(Neversmoke_Anxiety,"Neversmoke_Anxiety")
Cox_Main(Smoked_Anxiety,"Smoked_Anxiety")
Cox_Main(CurrentSmoke_Anxiety,"CurrentSmoke_Anxiety")
Cox_Main(Neversmoke_Depression,"Neversmoke_Depression")
Cox_Main(Smoked_Depression,"Smoked_Depression")
Cox_Main(CurrentSmoke_Depression,"CurrentSmoke_Depression")

Cox_Main(NeverDrink_Anxiety,"NeverDrink_Anxiety")
Cox_Main(Specialonly_Anxiety,"Specialonly_Anxiety")
Cox_Main(PerWeekorMore_Anxiety,"PerWeekorMore_Anxiety")
Cox_Main(NeverDrink_Depression,"NeverDrink_Depression")
Cox_Main(Specialonly_Depression,"Specialonly_Depression")
Cox_Main(PerWeekorMore_Depression,"PerWeekorMore_Depression")

Cox_Main(MoreSocial_Anxiety,"MoreSocial_Anxiety")
Cox_Main(LessSocial_Anxiety,"LessSocial_Anxiety")
Cox_Main(MoreSocial_Depression,"MoreSocial_Depression")
Cox_Main(LessSocial_Depression,"LessSocial_Depression")