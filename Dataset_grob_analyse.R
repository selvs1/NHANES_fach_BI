########################## Thema Diet #############################


#setwd("..")
#setwd("./BI/Project_Daten2")
#setwd("./Project_Daten2")
#getwd()

library(dplyr)
library(ggplot2)
library(readxl)




# Woerterbuch laden -> https://www.kaggle.com/cdc/national-health-and-nutrition-examination-survey/discussion/47796 --> do not use
#rawdictionary <- read.csv2("dictionary.csv", stringsAsFactors = FALSE)
rawDietData <- read.csv("diet.csv", stringsAsFactors = FALSE)

# https://wwwn.cdc.gov/Nchs/Nhanes/Search/variablelist.aspx?Component=Dietary&CycleBeginYear=2013
rawDietaryVariableList <- read.csv("2013 - 2014 Dietary Variable List.csv", stringsAsFactors = FALSE)


# Backups
#dictionary <- rawdictionary
diet <- rawDietData
#View(diet) # für uns Menschen ist das schwierig zu lesen
dietaryVariableList <- rawDietaryVariableList

####################Grob Analyse################################# -> go: to grob analyse 2
# goto: Grob analyse 2


###################do no use########################## Bereinigung für den alten Dictionary aus der Diskussion - do not use
# Bereinigung Dictionary
#str(dictionary)
#class(dictionary)
#dim(dictionary) # 5382 x 2
#head(dictionary)
#View(dictionary)
#tail(dictionary)

# how many missing value in dictionary? - Zero -> test ok
#table(is.na(dictionary))



# Doppelt vorhandene Keys?
#nrow(dictionary) # Zeilen 5383


#sum(duplicated(dictionary$VarName)) # duplicated 1849


#str(duplicated(dictionary$VarName))
#duplicated(dictionary$VarName) # omitted 4383

#duplicatedRows <- dictionary$VarName[duplicated(dictionary$VarName)] # get all duplicated keys (1849 Zilene)

#dictionary[dictionary$VarName == "RIDAGEMN",2] # Wie oft kommt der Key RIDAGEMN im Dictionary redundant vor?

#length(dictionary[dictionary$VarName == "RIDAGEMN",2]) # Length of key

# description sind die Gleichen?
#length(unique(dictionary[dictionary$VarName == "RIDAGEMN",2])) # Zwei verschiedene Descriptions zum gleichen Key oder wie viele Unikate haben wir? -> es sollte nicht mehr als 1 sein


# Resultat von unique(dictionary[dictionary$VarName == "RIDAGEMN",2])
# oben ist 4x gleiche Beschreibung und 1x etwas anderes


#tmp <- unique(dictionary[dictionary$VarName == "RIDAGEMN",2]) #hier sind die verschiendnen Description drauff



# ein anderer Versuch: Dataframe erzeugen

#duplicatedRowIndexList = c()


#duplicatedRowIndexDf <- data.frame(9999999, "test")
#names(duplicatedRowIndexDf) <- c("Index", "Beschreibung")


#for (i in 1:length(duplicatedRows)) { # Duplikate deren Beschreibung überprüfen
# if (length(unique(dictionary[dictionary$VarName == duplicatedRows[i],2])) > 1) { #hier mit dem Key zugreifen
    # gebe mir die redundante Zeile aus
#   print(unique(dictionary[dictionary$VarName == duplicatedRows[i],2])) # VarName vs. Key --> sind beide gleich
#   vec <- unique(dictionary[dictionary$VarName == duplicatedRows[i],2]) #hilfs variable
#   duplicatedRowIndexDf <- rbind(duplicatedRowIndexDf, data.frame(Index = i, Beschreibung = vec))
#   cat("\n")
    
# }
  
#}

#uniqueDuplicatedRowIndexDf <- unique(duplicatedRowIndexDf$Beschreibung) # nur die unikate holen 


#View(uniqueDuplicatedRowIndexDf)

#unique(dictionary$VarName)


# Zuviel Aufwand für diese Bereinigung - Stop an dieser Stelle, die ganze Übung war für nichts


# :goto Grob Anlyse 2
####################Grob Analyse 2##########USE THIS #######################


# Dataset mal ansehen 

# Check Dictionary VariableList
#View(dietaryVariableList) # Diet Datensatz besteht aus folgenden Attributen
head(dietaryVariableList)

dim(dietaryVariableList) # 912 x 8 spalten

str(dietaryVariableList) # Dataframe

unique(dietaryVariableList$Use.Constraints) # none -> diese Spalte unnötig
unique(dietaryVariableList$Component) # Dietary -> nicht notwendig

unique(dietaryVariableList$EndYear) # 2014 2016

unique(dietaryVariableList$Data.File.Description) # Klassifierbar nach Interview / Splement
unique(dietaryVariableList$Data.File.Name) # Key zu den Klassen

unique(dietaryVariableList$Variable.Description) # Var Describition
unique(dietaryVariableList$Variable.Name) # Var Key


## Unn?tigen Spalten l?schen
drops <- c("Use.Constraints", "Component")
dietaryVariableList <- dietaryVariableList[ , !(names(dietaryVariableList) %in% drops)]
str(dietaryVariableList)


## Nur Datens?tze aus 2013-2014
dietaryVariableList <- dietaryVariableList[(dietaryVariableList$Begin.Year==2013 & dietaryVariableList$EndYear==2014),]
#View(dietaryVariableList)



######################################################## Domographic Dictionary einlesen

# https://wwwn.cdc.gov/Nchs/Nhanes/Search/variablelist.aspx?Component=Dietary&CycleBeginYear=2013
rawDemographicData <- read.csv("demographic.csv", stringsAsFactors = FALSE)
rawDemographicVariableList <- read.csv("2013-2014 Demographics Variable List.csv", stringsAsFactors = FALSE)

demographicData <- rawDemographicData
demographicVariableList <- rawDemographicVariableList



# Check Demographic Dictionary VariableList
#View(demographicVariableList) # Demographischer Datensatz hat folgende Attribute
head(demographicVariableList)

dim(demographicVariableList) # 369 x 8 spalten

str(demographicVariableList) # Dataframe

# Spalten dieser Datensatz überprüfen
unique(demographicVariableList$Variable.Name) # ok
unique(demographicVariableList$Variable.Description)

unique(demographicVariableList$Data.File.Name) # verschiedene Variable Liste von anderen Ressouren??
unique(demographicVariableList$Data.File.Description)


unique(demographicVariableList$Begin.Year) # 2013 2007 1993
unique(demographicVariableList$EndYear) # 2014 2016
# Also Daten vo 2013 u 2014 nehmen


unique(demographicVariableList$Component) # nur Demographics --> kann man ja löschen

unique(demographicVariableList$Use.Constraints) # none , RDC only


#relevantDemoVariable <- c("SEQN","DMDBORN4", "DMDCITZN", "DMDMARTL", "INDFMIN2", "RIAGENDR", "DMDHHSIZ", "DR1TCARB")


## Unn?tigen Spalten l?schen
drops <- c("Component", "Use.Constraints")
demographicVariableList <- demographicVariableList[ , !(names(demographicVariableList) %in% drops)]
str(demographicVariableList)

#View(demographicVariableList)

## Nur Datensätze aus 2013-2014
demographicVariableList <- demographicVariableList[(demographicVariableList$Begin.Year==2013 & demographicVariableList$EndYear==2014),]
#View(demographicVariableList)

##################################################### Nun unsere Dictionary Bereinigung, nun unsere haupt ... bereinigen
dictDiet <- dietaryVariableList
dictDemo <- demographicVariableList




# Check main dataset
dim(diet)
str(diet)
head(diet)
############
dim(dictDiet)
str(dictDiet)
head(dictDiet)
############
colnames(diet)
colnames(dictDiet)
############




#################do not use ############################### dont use
# Spalten renamen
#dietRenameColumn <- function(oldname, newname){
# names(diet)[names(diet) == oldname] <- newname
  #names(diet)[1]
# return(diet)
#}
# Test - ok
#diet2 <- dietRenameColumn("WTDRD1", "test")
#colnames(diet2)
#diet2 <- diet






##################use this##############################
# rename each column
diet2 <- diet
colnames(diet2)

dictDiet$Variable.Name

# Spaltennamen mit Description labeln
for (i in 1:ncol(diet2)) {
  key <- names(diet2)[i]
  description <- dictDiet[dictDiet$Variable.Name == key, 2][1] #only first match
  #diet2 <- dietRenameColumn(key, description)
  #diet2 <- diet2 %>% rename(description = key)
  colnames(diet2)[which(names(diet2) == key)] <- description
  
}

# Test - OK
colnames(diet2)

# description header --> ist ziemlich lang
#View(diet2)


################################################

# Bereinigung Dataset
str(diet2)
class(diet2)
dim(diet2) # 9813 x 168
head(diet2)
#View(diet2)
tail(diet2)
# how many missing value in diet? - 704978 are missing

# so viele NA Werte
table(is.na(diet2))

# shiss druff


###################################################################################



#################################################################################### 
##################################Analyse Hier###################################### 
####################################################################################

# Wie viele f/m 2013-2014? chueche diagramm

#View(demographicData)

# Geburtsort: DMDBORN4 Categorical -> 1: USA, 2: other, 77: Refused, 99: don't know
factor(demographicData$DMDBORN4)

# Citizen status 1: local citizen by birth, 2: kein USaner, 7: refused, 9: don't know
factor(demographicData$DMDCITZN)

# Geschlecht: RIAGENDR -> 1: male, 2: female
factor(demographicData$RIAGENDR)

#####

#DRQSDIET - On special diet? --> das lassen wir erst einmal sein
#DRQSDT91 - Other special diet --> das lassen wir erst einmal sein

#DRQSDT1 - Weight loss/Low calorie diet
#DRQSDT2 - Low fat/Low cholesterol diet
#DRQSDT3 - Low salt/Low sodium diet
#DRQSDT4 - Sugar free/Low sugar diet
#DRQSDT5 - Low fiber diet
#DRQSDT6 - High fiber diet
#DRQSDT7 - Diabetic diet
#DRQSDT8 - Weight gain/Muscle building diet
#DRQSDT9 - Low carbohydrate diet
#DRQSDT10 - High protein diet
#DRQSDT11 - Gluten-free/Celiac diet
#DRQSDT12 - Renal/Kidney diet


# Daten inner joinen mit demographischen daten
diet3 <- merge(diet, demographicData, by = "SEQN")

# korrelation zwischen diet x und Total zucker DR1TSUGR?
#diet3 <- diet3[(!is.na(diet3$DRQSDT4) & !is.na(diet3$DR1TSUGR)),]
#diet3 <- diet3[(!is.na(diet3$DR1TNUMF) & !is.na(diet3$DR1TNUMF)),]


# get index
# features engineering
supplementsInMg <- grep("(mg)", colnames(diet2)) # Index: Alle Spalten mit Wortinhalt "(mg)" als Liste returnen

colnames(diet3[supplementsInMg])


#corCheck <- na.omit(diet3[, 50:60])
corCheck <- na.omit(diet3[, supplementsInMg]) # DF für cor() vorbereiten

d <- cor(corCheck)
d
summary(d, digits = 1)



#install.packages("corrplot")
library(corrplot)

############ Plot Cor Matrix
# Plotten wir mal den cor Marix
#corrplot(d, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 60)


#d <- as.data.frame(d)
dim(d)
d
unique(d[d > 0.85 & d<1])
d > 0.85





#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
#chart.Correlation(corCheck, histogram=TRUE, pch=19) #scheint aufwändig zu sein?


# Plot Cor Heatmap
# Get some colors
numberOfCols <- 25
col<- colorRampPalette(c("blue", "white", "red"))(numberOfCols)
#heatmap(x = d, col = col, symm = TRUE)

# DR1TNUMF - Number of foods/beverages reported
# DR1TKCAL - Energy (kcal) firstday



############## Was wurde gegessen in den letzten 30 Tagen?
# Piechart 
columnEaten <- grep("eaten during past 30 days", colnames(diet2))

names(diet3[columnEaten]) # Keys
names(diet2[columnEaten]) # Description


# test: nur bestimmte index
# columnEaten <- columnEaten[22:23]

totalSum <- sum(diet3[,columnEaten], na.rm = T)
columnSums <- colSums(diet3[,columnEaten], na.rm = T)

#Relative
columnSums <- columnSums/totalSum *100 # prozentualer wert


tmp1 <- data.frame(as.list(columnSums))


df <- data.frame(
  group = c(colnames(tmp1)),
  value = columnSums
)

# Plot hist
bp <- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
#bp

# Plot pie
pie <- bp + coord_polar("y", start=0)
#pie

############################################################################# # ML Algo

# Salzkonsum ein Problem?
supplementsInMg <- grep("(mg)", colnames(diet2))
saltIndex <- grep("add ordinary salt to ", colnames(diet2))

lstOfKeys <- colnames(diet3[supplementsInMg])
colnames(diet3[saltIndex])
# Salzkonsum nicht weiterverfolgt

# f <- as.formula(paste("c(", paste(rpartInput(), collapse = "+"), sep ="~"))


#salt
#plot(DR1TCHOL  ~ DBD100 +DR1TATOA, data = diet3, )

# Gibt es eine lineare Korrelation zwischen den Stoffen?
########## Basic Scatterplot Matrix

supplementsInMg <- grep("(mg)", colnames(diet2))
randList <- sample(x = c(1:length(supplementsInMg)), size = 4)  # nur 3 spalten --> sonst dauert das zu lange
supplementsInMg <- supplementsInMg[randList]

saltIndex <- grep("add ordinary salt to ", colnames(diet2))
lstOfKeys <- colnames(diet3[supplementsInMg])
colnames(diet3[saltIndex])

param1 <- as.formula(paste(" ", paste(lstOfKeys, collapse = " + "), sep = "~"))
#pairs(param1,data=diet3, main="Linearer zusammenhang zwischen einzelnen (mg)?") # 5 sek

#library(lattice)

#Matrixplot
#pairs(diet3[supplementsInMg]) # achtung allenfalls zu viel


######## Matrix Plot von Hand auslesen
head(cor(na.omit(diet3[grep("(mg)", colnames(diet2))])) > 0.88) #linearen cor DR1TCHL vs. DR1TCHOL
#pairs(diet3[c(60, 42)], labels = names(diet2[c(60, 42)])) # mit labels machen --> aha eine hier ist der Wert sehr gut

#Boxplot
#boxplot(diet3[supplementsInMg])


########## matrix plot schöner - dauer lange #########
# http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs

# Bist du in einer Diät? 1: yes, 2: no, 9: don't know, .: Missing/NA 
names(diet3[18])
names(diet2[18])
unique(diet3$DRQSDIET)

# NA werte mit 9 ersetzen
diet3$DRQSDIET[is.na(diet3$DRQSDIET)] <- 9


#my_cols[names(diet3[,supplementsInMg])]
#install.packages("randomcoloR")
library(randomcoloR)
n <- length(lstOfKeys) # anzahl key mapping auf colors
palette <- distinctColorPalette(n)

my_cols <- palette #c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(diet3[,supplementsInMg], pch = 19,  cex = 0.5,
      col = my_cols[diet3$DRQSDIET], 
      lower.panel=NULL)

#ha <- as.factor(diet3$DRQSDIET)
#table(ha)

#sum(na.omit(diet3$DRQSDIET))

#table(is.na(diet$DRQSDIET))
#8783+1030


# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[diet3$DRQSDIET])
}
# Create the plots
pairs(na.omit(diet3[,supplementsInMg]), 
      lower.panel = panel.cor,
      upper.panel = upper.panel)







#corCheck <- na.omit(diet3[, 50:60])
corCheck <- na.omit(diet3[, supplementsInMg])


d <- cor(corCheck)
summary(d, digits = 1)


#d <- as.data.frame(d)
dim(d)
d
unique(d[d > 0.85 & d<1])
d > 0.85


grep("DR1TCHL", names(diet3)) # 60
names(diet2[60]) 

grep("DR1TCHOL", names(diet3)) # 42
names(diet2[42])


######################## Multilinear regression

############# haben die Nährstoffe einen Zusammenhalt mit unterschiedlichen Total Haushaltseinkommen oder Diet und Gewicht?

## Questionary Daten einlesen
rawQuestionaryData <- read.csv("questionnaire.csv", stringsAsFactors = FALSE)
questionaryData <- rawQuestionaryData
questionaryData <- questionaryData[c("SEQN", "WHD020")] # nur die relevanten daten holen
diet4 <- merge(diet3, questionaryData, by = "SEQN")
str(diet4$WHD020)

# WHD020 - Current self-reported weight (pounds)
weightIndex <- grep("WHD020", names(diet4)) #215

#INDHHIN2	Total household income (reported as a range value in dollars)
incomeIndex <- grep("INDHHIN2", names(diet4)) #215


#Bereinigung
demographicVariableList <- demographicVariableList[(demographicVariableList$Begin.Year==2013 & demographicVariableList$EndYear==2014),]
#View(demographicVariableList)

unique(diet4$WHD020)

diet4 <- diet4[(diet4$WHD020 != 7777),] #7777 -> refused löschen
diet4 <- diet4[(diet4$WHD020 != 9999),] #9999 -> dont know löschen
diet4 <- diet4[!is.na(diet4$WHD020),] #7777 -> refused löschen

#diet3 <- diet3[(!is.na(diet3$DRQSDT4) & !is.na(diet3$DR1TSUGR)),]

#View(diet4)



#param1 <- as.formula(paste(" ", paste(lstOfKeys, collapse = " + "), sep = "~"))
param2 <- as.formula(paste("WHD020 ~ INDHHIN2", paste(names(diet4[, grep("(mg)", names(diet2))]), collapse = " + "), sep = "+"))

param2







ml <- lm(param2, data = diet4, na.action = na.omit)
summary(ml)

ml <- lm(WHD020 ~ 
           INDHHIN2 + #Total household income
           DR1TCHOL + 
           DR1TNIAC + 
           DR1TPHOS + 
           DR1TMAGN + 
           DR1TCAFF
         , data = diet4)
summary(ml)

#plot(ml)

dd <- c("INDHHIN2", "DR1TCHOL","DR1TNIAC", "DR1TPHOS", "DR1TMAGN", "DR1TCAFF")

#sugi
getDescriptionTmp <- function(diet_key) {
  dietKeyIndex <- grep(diet_key, colnames(diet3))
  out <- names(diet2)[dietKeyIndex]
  return(out)
}

list_of_keys <- dd
list_of_description <-unlist(lapply(list_of_keys, getDescriptionTmp))
list_of_description #Einkommen beeinflust durch Phosphorus, Magnesium, Caffeine

###### shiny #################################################################
#install.packages("shiny")
library(shiny)
library(rpart)
library(ggplot2)



#install.packages("shinyBS")
library(shinyBS)
#install.packages("gridExtra")
library(gridExtra)


supplementsInMg <- grep("(mg)", colnames(diet2)) # Index: Alle Spalten mit Wortinhalt "(mg)" als Liste returnen

lstStoffe <- supplementsInMg

u <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Beispiel",
             titlePanel("title panel"),
             sidebarLayout(position = "left", 
                           sidebarPanel("sidebar panel",
                                        checkboxInput("donum1", "Make #1 plot", value = T),
                                        checkboxInput("donum2", "Make #2 plot", value = F),
                                        checkboxInput("donum3", "Make #3 plot", value = F),
                                        sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                                        sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                                        sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                           ),
                           mainPanel("main panel",
                                     column(6,plotOutput(outputId="plotgraph", width="500px",height="400px"))
                           )
             )
    ),

    tabPanel("tab 5", 
             titlePanel("Was wurde in den letzen 30 Tagen wie oft verzehrt?"),
             sidebarLayout(
               sidebarPanel({
                 categories <- c("hist", "pie")
                 radioButtons(inputId = "category",
                              label = "Diagram", categories)
               },
               {
                 selectInput(
                   "rpart",
                   "Wähle Nahrung x:",
                   c(colnames(tmp1)), # names(diet3[columnEaten])
                   multiple = TRUE,
                   selected = "examide"
                 )
               }),
               mainPanel(
                 plotOutput(outputId = "distPlot2", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                 textOutput("txt"),
                 verbatimTextOutput("summary"),
                 uiOutput("hover_info")
               )
             )
             
             ),
    tabPanel("tab 6", 
             
             titlePanel("Correlation Matrix"),
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 sliderInput(inputId = "bins",
                             label = "Anzahl Farben",
                             min = 1, 
                             max = 30, 
                             value = 20)
               ),
               mainPanel(plotOutput(outputId = "distPlot"))
             )
             
             
    ),
    tabPanel("tab 7", 
             titlePanel("Correlation Matrix 2"),
             sidebarLayout(
               sidebarPanel(
               {
                 selectInput(
                   "rpart2",
                   "Choose Variables for rpart:",
                   c(colnames(diet3[lstStoffe])), # names(diet3[columnEaten]) grep("(mg)", colnames(diet2))
                   multiple = TRUE,
                   selected = "examide"
                 )
               }),
               mainPanel(
                 plotOutput(outputId = "correlationPlot2"),
                 textOutput("correlationTxt2"),
                 verbatimTextOutput("summary2"),
               )
             )
             
             )
  )
))

s <- shinyServer(function(input, output) 
{
  # Internet bsp
  set.seed(123)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
  })
  
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
  
  ## 04: heatmap
  output$distPlot = renderPlot(
    {
      
      supplementsInMg <- grep("(mg)", colnames(diet2)) # Index: Alle Spalten mit Wortinhalt "(mg)" als Liste returnen
      colnames(diet3[supplementsInMg])

      corCheck <- na.omit(diet3[, supplementsInMg]) # DF für cor() vorbereiten
      
      heatmapDaten <- cor(corCheck)
      d
      
      
      
      numberOfCols <- input$bins
      col<- colorRampPalette(c("blue", "white", "red"))(numberOfCols)
      heatmap(x = heatmapDaten, col = col, symm = TRUE)
      
    }
  )
  
  
  
  #### 99: pie kack teil
  rpartInput <- reactive(
    {
      input$rpart
    }
  )
  
  output$txt = renderText(
    {
      i <- paste(rpartInput(), collapse = " + ")
      paste("You chose", i)
    }
  )
  
  output$hover_info = renderUI(
    {
      
      ################## Test DF bauen ######################33
      f <- c(paste(rpartInput()))
      totalSum <- sum(diet3[,f], na.rm = T)
      columnSums <- colSums(diet3[,f], na.rm = T)
      
      #Relative
      columnSums <- columnSums/totalSum
      
      
      qq <- data.frame(as.list(columnSums))
      
      
      df <- data.frame(
        xvar = c(colnames(qq)),
        yvar = columnSums
      )
      
      ################## ende ###########################
      
      
      hover <- input$plot_hover
      point <- nearPoints(df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Car: </b>", diet3[1,1], "<br/>",
                      "<b> mpg: </b>", point, "<br/>",
                      "<b> hp: </b>", "point$hp", "<br/>",
                      "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
      )
      
    }
    
  )
  
  output$distPlot2 = renderPlot(
    {
      
      #i <- paste(rpartInput(), collapse = " + ")
      #f <- as.formula(paste("c(", paste(rpartInput(), collapse = "+"), sep ="~"))
      # alle Features aus der Liste
      f <- c(paste(rpartInput()))
      
      #### hier weiter!!!!
      

      
      #diet3$DRD370M
      #names(diet3[columnEaten])
      
      # test: nur bestimmte index
      # columnEaten <- columnEaten[22:23]
      
      totalSum <- sum(diet3[,f], na.rm = T)
      columnSums <- colSums(diet3[,f], na.rm = T)
      
      #Relative
      columnSums <- columnSums/totalSum
      
      
      qq <- data.frame(as.list(columnSums))
      
      
      df <- data.frame(
        group = c(colnames(qq)),
        value = columnSums
      )
      
      if (input$category == "hist") {
        bp <- ggplot(df, aes(x="", y=value, fill=group))+
          geom_bar(width = 1, stat = "identity")
        bp
      } else{
        
        bp <- ggplot(df, aes(x="", y=value, fill=group))+
          geom_bar(width = 1, stat = "identity")
        #bp
        
        pie <- bp + coord_polar("y", start=0)
        pie
        
      }
      
      
    }
    
  )
  
  
  
  #### 100: correlation matrix version 2
  
  rpartInput2 <- reactive(
    {
      input$rpart2
    }
  )
  
  output$correlationTxt2 = renderText(
    {
      #i <- paste(rpartInput2(), collapse = " + ") 
      i <- paste(rpartInput2()) 
      
      #sugi
      getDescription <- function(diet_key) {
        dietKeyIndex <- grep(diet_key, colnames(diet3))
        out <- names(diet2)[dietKeyIndex]
        return(out)
      }
      
      list_of_keys <- i
      list_of_description <-lapply(list_of_keys, getDescription)
      
      
      paste(list_of_description, sep = "\n")
    }
  )
  
  output$correlationPlot2 = renderPlot(
    {
      
      i <- paste(rpartInput2()) 
      
      
      #sugi
      getKeyIndex <- function(diet_key) {
        dietKeyIndex <- grep(diet_key, colnames(diet3))
        #out <- names(diet2)[dietKeyIndex]
        return(dietKeyIndex)
      }
      
      list_of_keys <- i
      list_of_key_index <- unlist(lapply(list_of_keys, getKeyIndex))
      
      
      
      # get indices index scheiss
      # features engineering
      #supplementsInMg <- grep("(mg)", colnames(diet2))
      
      
      #corCheck <- na.omit(diet3[, 50:60])
      corCheck <- na.omit(diet3[, list_of_key_index])
      
      d <- cor(corCheck)
      summary(d, digits = 1)
      
      #install.packages("corrplot")
      library(corrplot)
      
      # Plotten wir mal den cor Marix
      corrplot(d, type = "upper", order = "hclust", 
               tl.col = "black", tl.srt = 60)
      
      
    }
    
  )
  
})
shinyApp(u,s)

