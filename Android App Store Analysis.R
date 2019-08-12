library("lubridate")
library("dplyr")
library("ggplot2")
library("ggExtra")
library("highcharter")
# Clearing environment
rm(list=ls())

# Clearing console 
cat("\14")

#set working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#read dataset
cleanData <- read.csv("googleplaystore.csv",na.strings="")
head(cleanData)
#cleaning table
maketShareDB <- cleanData %>%
  group_by(App) %>%
  summarise(Frequency = n())
#remove duplicate lines
cleanData <- mutate_at(cleanData, vars(App), list(toupper))
cleanData <- distinct(cleanData, App, .keep_all = TRUE)

#remove $ in Price variable
cleanData$Price <- gsub('[\\$]', '', cleanData$Price)
cleanData$Price <- as.numeric(cleanData$Price)

#remove + in Installs variable
cleanData$Installs <- gsub('\\D', '', cleanData$Installs)
cleanData$Installs <- as.numeric(cleanData$Installs)
cleanData$Installs <- format(cleanData$Installs, scientific = FALSE)

#remove M, k in Size variable
cleanData$Size <- gsub('[M]', '', cleanData$Size)
ConvertKtoM <- function(arg1){
  if (grepl("k", arg1, fixed=TRUE)){
    arg1 <- gsub('[k]', '', arg1)
    return(as.numeric(arg1)/1000)
  }
  
  return(arg1)
}
cleanData["Size"] <- sapply(cleanData$Size,ConvertKtoM)
cleanData["Size" ==  "Varies with device"] <- "na"
cleanData$Size <- as.numeric(cleanData$Size)

#current version column
cleanData$Current.Ver <- gsub('NaN', 'Varies with device', 
                              cleanData$Current.Ver)

#format datetime
cleanData$Last.Updated<-dmy(cleanData$Last.Updated)

# #pie chart with percentage of market share
maketShareDB <- cleanData %>%
  group_by(Category) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency))*100) %>%
  ungroup() %>%
  arrange(desc(Frequency)) %>%
  mutate(Category=factor(Category, levels = as.character(Category)))

maketShareDB <- na.omit(maketShareDB)
maketShareDB$Category <- factor(maketShareDB$Category, levels = rev(as.character(maketShareDB$Category)))
bp<- ggplot(data = maketShareDB, aes(x = "", y = Percentage, fill=Category)) +
  geom_bar(width = 1, stat = "identity", color="white")

pie <- bp + coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(Percentage), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "market share") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))
pie
# 
# 
# #avarage rating of apps
ratingtable <- na.omit(cleanData)
ratingtable <- ratingtable %>%
  group_by(Rating) %>%
  summarise(Frequency = n())
print(paste("Avarage rating: ", mean(ratingtable$Frequency)))
ggplot(data = ratingtable, aes(x = Rating, y = Frequency)) +
  geom_bar(position="dodge", stat="identity", fill="steelblue", colour="black") +
  geom_text(aes(label=Frequency), vjust=-0.3, size=3.5)

# #performance of apps
theme_set(theme_bw())
# plot
g <- ggplot(cleanData, aes(Category, Rating, fill=Category))
g + geom_violin() +
  labs(title="Violin plot",
       subtitle="Best performence categories",
       x="Category",
       y="Rating")+
  theme(axis.text.x=element_text(angle=90, hjust=1),legend.position="none")+
  geom_hline(yintercept = 4.5, linetype="dashed", color = "red", size = 1)
# 
#Most popular category (# of installs)
cleanData %>%
  count(Category, Installs) %>%
  group_by(Category) %>%
  summarize(
    TotalInstalls = sum(as.numeric(Installs))
  ) %>%
  arrange(-TotalInstalls) %>%
  hchart('scatter', hcaes(x = "Category", y = "TotalInstalls", size = "TotalInstalls", color = "Category")) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Most popular categories (# of installs)")

#size vs rating
g <- ggplot(cleanData, aes(Size, Rating)) +
  geom_count() +
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")

#price vs rating
g <- ggplot(cleanData, aes(Price, Rating)) +
  geom_count() +
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")

# table6 <- cleanData %>%
#   group_by(Category) %>%
#   summarise(n())

#price < 100 across Category
priceLessThan100 <- subset(cleanData, cleanData$Price < 100)
g <- ggplot(priceLessThan100, aes(Price, Category))
g + geom_jitter(width = .5, size= 1) +
  labs(subtitle="Price < 100 vs Category",
       y="Category",
       x="Price < 100",
       title="App pricing trend across categories")

#Percentage of Free vs Paid by Category
grouptype <- cleanData %>%
  group_by(Type) %>%
  summarize(
    n = n()
  )
cleanData %>%
  group_by(Category, Type) %>%
  summarize(
    n = n()
  ) %>%
  mutate(perc = round((n /sum(n))*100)) %>%
  hchart('bar', hcaes(x = 'Category', y = 'perc', group = 'Type')) %>%
  hc_plotOptions(series=list(stacking='normal')) %>%
  hc_title(text="Percentage of Free vs Paid by Category") %>%
  hc_add_theme(hc_theme_flat())


