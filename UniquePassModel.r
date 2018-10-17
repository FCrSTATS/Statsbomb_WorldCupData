### Uniqueness - Grand Master 

## ########################################################## ##
#                                                          ## ##
# Setup the Environment                                     ####
#                                                          ## ##
## ########################################################## ##

# load the packages
require(StatsBombR)

# download all of the data
events <- StatsBombFreeEvents()

## Enviornment
require(tidyverse)
require(RANN)
require(formattable)
require(SBpitch)

## ########################################################## ##
#                                                          ## ##
# Load Data and Prepare for Analysis                        ####
#                                                          ## ##
## ########################################################## ##

## removing NAs from the pass.outcome.name and pass.type.name columns
events$pass.outcome.name <- ifelse(
  is.na(events$pass.outcome.name),
  "Complete",
  as.character(events$pass.outcome.name)
)

events$pass.type.name <- ifelse(is.na(events$pass.type.name),
                                "-",
                                as.character(events$pass.type.name))

## Filter out by our criteria
passes <-
  events %>% filter(
    type.name == "Pass" &
      pass.type.name != "Goal Kick" &
      pass.type.name != "Corner" &
      pass.type.name != "Throw-in" &
      pass.type.name != "Free Kick" &
      pass.type.name != "Kick Off" & competition_id == 43
  )

## Create the reference data
PlayerDB <- unique(events %>%
                     select(id, team.name, player.name)) ## only selecting certain columns of player information

passDB <- passes %>%
  select(
    id,
    player.name,
    team.name,
    duration,
    location,
    pass.length,
    pass.angle,
    pass.end_location,
    pass.height.id,
    pass.outcome.name,
    competition_id
  )

## spilt the location data, convert to numeric and remove extra spilting columns and
passDB <-
  passDB %>% separate(location, c("location.spare", "x", "y")) %>%
  separate(pass.end_location,
           c("location.spare2", "x.destination", "y.destination")) %>%
  mutate(
    x = as.numeric(as.character(x)),
    y = as.numeric(as.character(y)),
    x.destination = as.numeric(as.character(x.destination)),
    y.destination = as.numeric(as.character(y.destination))
  ) %>%
  select(-location.spare, -location.spare2)

## Add Success
passDB$success <-
  ifelse(passDB$pass.outcome.name == "Complete", 1, 0)

## Player Position
Player.Positions <- events %>%
  filter(!is.na(position.name)) %>%
  group_by(player.name) %>%
  summarize(position.name = names(which.max(table(position.name))))

## position catergory table
position.catergory <-
  data.frame(
    position.name = unique(Player.Positions$position.name) ,
    stringsAsFactors = F
  )
position.catergory$catergory <- c(
  "Midfield",
  "Defender",
  "Defender",
  "Goalkeeper",
  "Midfield",
  "Forward",
  "Midfield",
  "Defender",
  "Midfield",
  "Defender",
  "Forward",
  "Forward",
  "Forward",
  "Forward",
  "Midfield",
  "Midfield",
  "Midfield",
  "Midfield",
  "Defender",
  "Midfield",
  "Defender",
  "Defender",
  "Midfield",
  "Forward",
  "Midfield"
)

position.catergory$left.to.right <-
  c(1, 3, 1, 2, 2, 3, 2, 3, 3, 1, 1, 1, 3, 2, 1, 3, 3, 1, 2, 2, 1, 3, 3, 2, 1)
position.catergory$back.to.front <- NA
position.catergory$back.to.front <-
  ifelse(position.catergory$catergory == "Goalkeeper",
         1,
         position.catergory$back.to.front)
position.catergory$back.to.front <-
  ifelse(position.catergory$catergory == "Defender",
         2,
         position.catergory$back.to.front)
position.catergory$back.to.front <-
  ifelse(position.catergory$catergory == "Midfield",
         3,
         position.catergory$back.to.front)
position.catergory$back.to.front <-
  ifelse(position.catergory$catergory == "Forward",
         4,
         position.catergory$back.to.front)

## Add positional data
passDB <-
  merge(passDB,
        events %>% select(id, position.name),
        by = "id",
        all = F)
passDB <-
  merge(passDB, position.catergory, by = "position.name", all = F)

## Create the normalise function
normalise <-
  function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

## Normalise the data
normalised_data <- passDB %>% select(-success,-competition_id) %>%
  mutate_if(is.numeric, normalise)

normalised_data <- merge(normalised_data,
                         passDB %>% select(id, success, competition_id),
                         by = "id")


## ########################################################## ##
#                                                          ## ##
# Calculating the uniqueness                                ####
#                                                          ## ##
## ########################################################## ##

# build the function to calculate the uniqueness
calc.uniqueness.and.conversion.per.pass <- function(row) {
  # uniqueness match NN
  nnResults <- nn2(
    normalised_data[5:12],
    query = row[5:12],
    k = min((nrow(normalised_data) / 100),
            (nrow(normalised_data) / 100)),
    treetype = "kd",
    searchtype = "standard",
    eps = 2
  )
  
  matching.scope <- 51 ## assess the first 100 passes
  
  # conversion NN
  nnResults2 <- nn2(
    normalised_data[5:12],
    query = row[5:12],
    k = min(matching.scope, matching.scope),
    treetype = "kd",
    searchtype = "standard",
    eps = 2
  )
  
  passes.in.scope <-  normalised_data[nnResults2$nn.idx, ]
  
  passes.in.scope.result <- passes.in.scope %>%
    filter(id != row$id) %>%
    summarise(xCompletion = mean(success))
  
  Results <- data.frame(
    id = row$id,
    Uniqueness = sum(nnResults$nn.dists[1, ]),
    xCompletion = passes.in.scope.result$xCompletion,
    stringsAsFactors = F
  )
  cat(".")
  
  return(Results)
  
}

# run the function over each row of data and save results into UniqueCatcher
UniqueCatcher <- normalised_data %>%
  split(1:nrow(.)) %>%
  purrr::map(calc.uniqueness.and.conversion.per.pass) %>%
  dplyr::bind_rows()

## join unique results to passDB
passDB <- merge(passDB, UniqueCatcher, by = "id")

## ########################################################## ##
#                                                          ## ##
# Exploratory Data Viz                                      ####
#                                                          ## ##
## ########################################################## ##

# Relationship between x and Uniqueness
ggplot(data = passDB) +
  geom_point(aes(x = x, y = Uniqueness), alpha = 0.04, colour = "#28AAE1") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  ) +
  xlim(c(0, 120))

# Relationship between y and Uniqueness
ggplot(data = passDB) +
  geom_point(aes(x = Uniqueness, y = y), alpha = 0.04, colour = "#28AAE1") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  ) +
  ylim(c(0, 80))


# Relationship between x.destination and Uniqueness
ggplot(data = passDB) +
  geom_point(aes(x = x.destination , y = Uniqueness),
             alpha = 0.04,
             colour = "#28AAE1") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  ) +
  xlim(c(0, 120))

# Relationship between y.destination and Uniqueness
ggplot(data = passDB) +
  geom_point(aes(x = Uniqueness, y = y.destination),
             alpha = 0.04,
             colour = "#28AAE1") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  ) +
  ylim(c(0, 80))

# Relationship between pass.end_location.y and distance
ggplot(data = passDB) +
  geom_point(aes(x = pass.angle, y =  Uniqueness),
             alpha = 0.04,
             colour = "#28AAE1") +
  coord_polar() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )

# Relationship between pass.end_location.y and distance
ggplot(data = passDB) +
  geom_point(aes(x = pass.length, y = Uniqueness),
             alpha = 0.04,
             colour = "#28AAE1") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )

## generate a pitch from the SBpitch package for pitch based plots
pitch <-
  create_Pitch(
    goaltype = "barcanumbers",
    grass_colour = "#DFDED9",
    background_colour = "#DFDED9"
  )

# x,y of event plotted with distance as shading = for pass location
pitch + geom_point(
  data = passDB,
  aes(x = x, y = y, colour = Uniqueness),
  alpha = 0.1,
  position = "jitter",
  size = 2
) +
  scale_colour_gradient(low = "#DFDED9", high = "black") +
  theme(legend.position = "none")

# x,y of event plotted with distance as shading = for pass destination
pitch + geom_point(
  data = passDB,
  aes(x = x.destination , y = y.destination, colour = Uniqueness),
  alpha = 0.1,
  position = "jitter",
  size = 2
) +
  scale_colour_gradient(low = "#DFDED9", high = "black") +
  theme(legend.position = "none")

## create a function to plot one pass and their matches

plot.a.pass.match <- function(id.select, matching.scope) {
  pass.2.plot <- normalised_data %>% filter(id == id.select)
  
  nnResults <- nn2(
    normalised_data[5:12],
    query = pass.2.plot[5:12],
    k = min(matching.scope, matching.scope),
    treetype = "kd",
    searchtype = "standard",
    eps = 2
  )
  
  passes.2.plot <-
    passDB %>% filter(id %in% normalised_data[nnResults$nn.idx, ]$id)
  passes.2.plot <- passes.2.plot %>% filter(id != pass.2.plot$id)
  
  p <-
    create_Pitch(
      goaltype = "barcanumbers",
      grass_colour = "#DFDED9",
      background_colour = "#DFDED9"
    )
  
  
  pass.2.plot <- passDB %>% filter(id == id.select)
  p <-
    p + geom_segment(
      data = passes.2.plot,
      aes(
        x = x,
        y = 80 - y,
        xend = x.destination,
        yend = 80 - y.destination
      ),
      colour = "#28AAE1",
      size = 1,
      alpha = 0.1
    ) +
    geom_point(
      data = passes.2.plot,
      aes(x = x, y = 80 - y),
      colour = "#28AAE1",
      size = 2,
      alpha = 0.15
    ) +
    geom_segment(
      data = pass.2.plot,
      aes(
        x = x,
        y = 80 - y,
        xend = x.destination,
        yend = 80 - y.destination
      ),
      colour = "#28AAE1",
      size = 1
    ) +
    geom_point(data = pass.2.plot,
               aes(x = x, y = 80 - y),
               colour = "#28AAE1",
               size = 2)
  
  return(p)
}


## ########################################################## ##
#                                                          ## ##
# Filter to Unique Attacking Passes                         ####
#                                                          ## ##
## ########################################################## ##

UAPbase <- passDB %>% arrange(-Uniqueness)
UAPbase <- UAPbase[round(nrow(UAPbase) * 0.0025, 0):nrow(UAPbase), ]

UAP1 <- UAPbase %>% filter(x > 60 & x.destination > x)
UAP2 <- UAPbase %>% filter(x > 80 & x.destination > 60)

UAP_merge <- unique(bind_rows(UAP1, UAP2))
UAP_merge <- UAP_merge %>% arrange(-Uniqueness)

U.A.Passes <- UAP_merge[1:round(nrow(UAP_merge) * 0.25, 0), ]


## ########################################################## ##
#                                                          ## ##
# Per Player Shortlist                                      ####
#                                                          ## ##
## ########################################################## ##

## add completion rate
U.A.Passes$x.Completion.rate <-
  U.A.Passes$success - U.A.Passes$xCompletion

UAP.per.player <-
  U.A.Passes %>% group_by(player.name) %>% summarise(
    No.UPA = n(),
    avg.Uniqueness = mean(Uniqueness),
    UPA.sum = sum(Uniqueness),
    x.Completion.Rate = mean(x.Completion.rate)
  )
UAP.per.player <-
  merge(UAP.per.player, Total.Mins, by = "player.name", all = F)
UAP.per.player <-
  UAP.per.player %>% group_by(player.name) %>% mutate(
    UAP.90 = (No.UPA / Total.Minutes) * 90,
    UAP.sum.90 = (UPA.sum / Total.Minutes) * 90
  )

## filter by 180 minutes
UAP.per.player.shortlist <-
  UAP.per.player %>% filter(Total.Minutes > 180)

## add position groupings
UAP.per.player.shortlist <-
  merge(UAP.per.player.shortlist,
        Player.Positions,
        by = "player.name",
        all = F)
UAP.per.player.shortlist <-
  merge(
    UAP.per.player.shortlist,
    position.catergory %>% select(position.name, catergory),
    by = "position.name",
    all = F
  )
UAP.per.player.shortlist$factor <- 1

## shortlist final
UAP.per.player.shortlist.final <-
  UAP.per.player.shortlist %>% filter(No.UPA > 10)

# + Add groupings

UAP.per.player.shortlist <- UAP.per.player.shortlist %>% mutate(
  grouping = case_when(
    No.UPA < 10 ~ "0-10",
    No.UPA < 20 ~ "10-20",
    No.UPA < 30 ~ "20-30",
    No.UPA < 40 ~ "30-40",
    TRUE ~  "40-50"
  )
)
#
xxx <-
  UAP.per.player.shortlist %>% filter(grouping == "20-30" |
                                        grouping == "30-40" |
                                        grouping == "40-50") %>%  select(player.name, UAP.90, x.Completion.Rate, grouping, catergory) %>% mutate(x.Completion.Rate = round(x.Completion.Rate * 100, 1))

#######

# display names
display.names <-
  c(
    "Christian Eriksen"  ,
    "Sergej Milinković-Savić"     ,
    "Aleksandr Golovin"          ,
    "Antoine Griezmann"     ,
    "Andrej Kramarić"           ,
    "Sergio Busquets"   ,
    "Marcelo Brozović"    ,
    "Javier Mascherano"  ,
    "Jordan Henderson"        ,
    "Lucas Torreira"  ,
    "Timo Werner"              ,
    "Aleksandar Mitrovic"      ,
    "Olivier Giroud"    ,
    "Mario Mandžukić"          ,
    "Artem Dzyuba"              ,
    "Jordi Alba"   ,
    "Yasir Al Shahrani"    ,
    "Hörður Magnússon"  ,
    "Lucas Hernández"    ,
    "Raphaël Guerreiro" ,
    "Marcelo",
    "Ludwig Augustinsson"     ,
    "Harry Maguire"           ,
    "Sergio Ramos "           ,
    "Harry Kane"          ,
    "M'Baye Niang"     ,
    "Ola Toivonen"                  ,
    "Luis Suárez"   ,
    "Toni Kroos"                ,
    "Ivan Rakitić"                  ,
    "Paul Pogba"            ,
    "Roman Zobnin"              ,
    "Granit Xhaka"                  ,
    "William de Carvalho"  ,
    "Salman Al Faraj"   ,
    "Andrés Iniesta"          ,
    "Thomas Delaney"         ,
    "Dele Alli"                  ,
    "Philippe Coutinho"             ,
    "Isco"                 ,
    "João Mário"                ,
    "Neymar"                        ,
    "Eden Hazard"           ,
    "Ivan Perišić"              ,
    "Dries Mertens"                 ,
    "Benjamin Pavard"       ,
    "Šime Vrsaljko"              ,
    "Henrik Dalsgaard"              ,
    "Martín Cáceres" ,
    "Mário Fernandes"   ,
    "Joshua Kimmich"                ,
    "Jérôme Boateng"      ,
    "Marcus Berg"               ,
    "Lionel Messi",
    "Kevin De Bruyne"     ,
    "Celso Borges"         ,
    "Jesse Lingard"                 ,
    "N'Golo Kanté"       ,
    "Luka Modrić"               ,
    "Héctor Herrera"   ,
    "Yury Gazinskiy"     ,
    "Nahitan Nández" ,
    "Kieran Trippier"               ,
    "Viktor Claesson"     ,
    "Kylian Mbappé"             ,
    "Thomas Müller"                 ,
    "Piotr Zieliński"     ,
    "Bryan Ruiz"       ,
    "Ante Rebić"                    ,
    "Xherdan Shaqiri"      ,
    "Yussuf Poulsen"      ,
    "Dusan Tadic"                   ,
    "Aleksandr Samedov"      ,
    "Thomas Meunier"
  )

xxx$display.names <- display.names

require(ggrepel)


##
xxx5 <- xxx %>% filter(catergory == "Midfield")
ggplot(data = xxx5 , aes(y = UAP.90, x = x.Completion.Rate)) + geom_point() +
  annotate(
    "text",
    x = xxx5$x.Completion.Rate,
    y = xxx5$UAP.90,
    label = xxx5$player.name
  )


p <-
  ggplot(data = xxx,
         aes(y = UAP.90, x = x.Completion.Rate, label = display.names)) +
  geom_point(data = xxx,
             aes(y = UAP.90, x = x.Completion.Rate),
             colour = "grey") +
  geom_point(color = "#28AAE1", aes(size = grouping)) +
  geom_text_repel() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )
p + facet_grid(catergory ~ .)

write.csv(p$data, "plot.csv")
xxx %>% group_by(catergory) %>% summarise(mean.UAP = mean(UAP.90),
                                          mean.con = mean(x.Completion.Rate))

ggplot(
  data = xxx %>% filter(catergory == "Defender"),
  aes(y = UAP.90, x = x.Completion.Rate, label = display.names)
) +
  geom_point(
    data = xxx %>% filter(catergory != "Defender"),
    aes(y = UAP.90, x = x.Completion.Rate),
    colour = "grey"
  ) +
  geom_point(color = "#28AAE1", aes(size = grouping)) +
  geom_text_repel() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )

ggplot(
  data = xxx %>% filter(catergory == "Forward"),
  aes(y = UAP.90, x = x.Completion.Rate, label = display.names)
) +
  geom_point(
    data = xxx %>% filter(catergory != "Forward"),
    aes(y = UAP.90, x = x.Completion.Rate),
    colour = "grey"
  ) +
  geom_point(color = "#28AAE1", aes(size = grouping)) +
  geom_text_repel() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )

ggplot(
  data = xxx %>% filter(catergory == "Midfield"),
  aes(y = UAP.90, x = x.Completion.Rate, label = display.names)
) +
  geom_point(
    data = xxx %>% filter(catergory != "Midfield"),
    aes(y = UAP.90, x = x.Completion.Rate),
    colour = "grey"
  ) +
  geom_point(color = "#28AAE1", aes(size = grouping)) +
  geom_text_repel() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )


ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text_repel() +
  geom_point(color = 'red') +
  theme_classic(base_size = 16)



## ########################################################## ##
#                                                          ## ##
# ??                                                        ####
#                                                          ## ##
## ########################################################## ##

events2 <- events
##

events2$TimeSync <- (events2$minute * 60) + events2$second

calc.xG.after.shot <- function(row) {
  row.of.events <- events2 %>% filter(id == row$id)
  time.of.event <-
    row.of.events$TimeSync <-
    (row.of.events$minute * 60) + row.of.events$second
  events.range <-
    events2 %>% filter(
      match_id == row.of.events$match_id &
        period == row.of.events$period &
        TimeSync >= time.of.event &
        TimeSync <= time.of.event + 5  &
        team.name == row.of.events$team.name
    )
  events.range <-
    events.range[as.numeric(rownames(events.range[which(events.range$id == row$id), ])):nrow(events.range), ]
  xGTotal <-
    if (length(events.range) > 0) {
      sum(events.range$shot.statsbomb_xg, na.rm = T)
    } else{
      0
    }
  cat(".")
  results <-
    data.frame(id = row$id,
               xGTotal = xGTotal,
               stringsAsFactors = F)
  return(results)
}

# run the function over each row of data and save results into UniqueCatcher
xGTotal.Catcher <- UAPbase %>%
  split(1:nrow(.)) %>%
  purrr::map(calc.xG.after.shot) %>%
  dplyr::bind_rows()



passDB3 <- merge(passDB, xGTotal.Catcher, by = "id")
passDB.non.0 <- passDB3 %>% filter(xGTotal > 0)

p <-
  ggplot(data = passDB.non.0, aes(x = Uniqueness, y = xGTotal)) +
  geom_point(color = "#72716F", alpha = 0.3) +
  geom_smooth(color = "#28AAE1", fill = "#87C3DC") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )

tiff(
  "test.tiff",
  units = "in",
  width = 5,
  height = 6,
  res = 300
)
p
dev.off()

#####

## ########################################################## ##
#                                                          ## ##
# Correlation with total xGs                                ####
#                                                          ## ##
## ########################################################## ##

per.game.xG.totals <-
  events %>% filter(!is.na(shot.outcome.name)) %>% group_by(team.name, match_id) %>% summarise(total.XG = sum(shot.statsbomb_xg),
                                                                                               Total.Shots = n())
PassTotals <-
  events %>% filter(type.name == "Pass") %>% group_by(team.name, match_id) %>% summarise(total.passes = n())
PassTotals.successful <-
  events %>% filter(type.name == "Pass" &
                      pass.outcome.name == "Complete") %>% group_by(team.name, match_id) %>% summarise(total.passes.succ = n())
U.A.P.for.summation <- U.A.Passes
U.A.P.for.summation <-
  merge(U.A.P.for.summation,
        events %>% select(id, match_id),
        by = "id",
        all = F)
U.A.P.for.summation.totals <-
  U.A.P.for.summation %>% group_by(team.name, match_id) %>% summarise(Total.UAP = n(), Sum.Unique = sum(Uniqueness))

per.game.xG.totals$match.id.long <-
  paste0(per.game.xG.totals$team.name,
         "-",
         per.game.xG.totals$match_id)
PassTotals$match.id.long <-
  paste0(PassTotals$team.name, "-", PassTotals$match_id)
PassTotals.successful$match.id.long <-
  paste0(PassTotals.successful$team.name,
         "-",
         PassTotals.successful$match_id)
U.A.P.for.summation.totals$match.id.long <-
  paste0(U.A.P.for.summation.totals$team.name,
         "-",
         U.A.P.for.summation.totals$match_id)

Summations <-
  merge(
    per.game.xG.totals,
    U.A.P.for.summation.totals %>% select(match.id.long, Total.UAP, Sum.Unique),
    by = "match.id.long"
  )
Summations <-
  merge(Summations,
        PassTotals %>% select(match.id.long, total.passes),
        by = "match.id.long")
Summations <-
  merge(Summations,
        PassTotals.successful %>% select(match.id.long, total.passes.succ),
        by = "match.id.long")
Summations$Team <- Summations$team.name.x
Summations[, 2] <- NULL
Summations[, 5] <- NULL
Summations[, 7] <- NULL
Summations[, 8] <- NULL

scatter.smooth(y = Summations$total.XG,
               x = Summations$total.passes,
               main = "total.XG ~ total.passes")
scatter.smooth(y = Summations$total.XG,
               x = Summations$total.passes.succ,
               main = "total.XG ~ total.passes.succ")
scatter.smooth(y = Summations$total.XG,
               x = Summations$Total.Shots,
               main = "total.XG ~ Total.Shots")
scatter.smooth(y = Summations$total.XG,
               x = Summations$Total.UAP,
               main = "total.XG ~ Total.UAP")
scatter.smooth(y = Summations$total.XG,
               x = Summations$Sum.Unique,
               main = "total.XG ~ Sum.Uniqueness.of.UAP")


par(mfrow = c(1, 2))  # divide graph area in 2 columns
boxplot(
  Summations$total.XG,
  main = "total.XG",
  sub = paste("Outlier rows: ", boxplot.stats(Summations$total.XG)$out)
)  # box plot for 'speed'
boxplot(
  Summations$Total.UAP,
  main = "Total.UAP",
  sub = paste("Outlier rows: ", boxplot.stats(Summations$Total.UAP)$out)
)  # box plot for 'distance'

library(e1071)
par(mfrow = c(1, 2))  # divide graph area in 2 columns
plot(
  density(Summations$total.XG),
  main = "Density Plot: total.XG",
  ylab = "Frequency",
  sub = paste("Skewness:", round(e1071::skewness(cars$speed), 2))
)  # density plot for 'speed'
polygon(density(Summations$total.XG), col = "red")
plot(
  density(Summations$Total.UAP),
  main = "Density Plot: Total.UAP",
  ylab = "Frequency",
  sub = paste("Skewness:", round(e1071::skewness(cars$dist), 2))
)  # density plot for 'dist'
polygon(density(Summations$Total.UAP), col = "red")

cor(Summations$Total.UAP, Summations$total.XG)
cor(Summations$Total.Shots, Summations$total.XG)
cor(Summations$Sum.Unique, Summations$total.XG)
cor(Summations$total.passes, Summations$total.XG)
cor(Summations$total.passes.succ, Summations$total.XG)

linearMod <-
  lm(total.XG ~ Total.UAP + Total.Shots + Sum.Unique + total.passes + total.passes.succ,
     data = Summations)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

ggplot(Summations,
       aes(
         x = total.passes,
         y = Total.UAP,
         size = total.XG,
         colour = total.XG
       )) + geom_point()

glm()


Summations
ggplot(data = Summations, aes(x = Total.UAP, y = total.XG))  + geom_point() + geom_smooth()


write.csv(p$data, "upadis.csv")


PassDB2 <- passDB
PassDB2$Zone <- as.character(cut(grid.completion$x, seq(0, 120, 10)))

grid.completion <-
  PassDB2 %>% group_by(Zone) %>% mutate(grid.completion.rate = mean(xCompletion))

ggplot(grid.completion,
       aes(x = x.destination, y = y.destination, fill = grid.completion.rate)) + geom_tile()

split(grid.completion,
      cut(grid.completion$x, seq(0, 120, 10), include.lowest = TRUE))

ggplot(grid.completion,
       aes(x = x.destination, y = y.destination)) +
  geom_raster(aes(fill = grid.completion.rate), interpolate = TRUE)


## add
options(scipen = 999)

per.player.conversion <-
  passDB %>% filter(competition_id == 43) %>%
  group_by(id) %>%
  mutate(xCompletion.Diff = success - xCompletion) %>% group_by(player.name) %>%
  summarise(No.Passes = n(),
            xCompletion.Diff.Total = round(mean(xCompletion.Diff) * 100, 1))

### show regression to the mean
per.player.conversion <- per.player.conversion %>% mutate(
  grouping = case_when(
    No.Passes < 100 ~ "0-100",
    No.Passes < 200 ~ "100-200",
    No.Passes < 300 ~ "200-300",
    No.Passes < 400 ~ "300-400",
    TRUE ~  "400-500"
  )
)

##

save <- UAP.per.player.shortlist
UAP.per.player.shortlist <-
  UAP.per.player.shortlist %>% filter(No.Passes > 3)

bin.size = 25
ggplot() +
  geom_histogram(
    data = UAP.per.player.shortlist %>% filter(grouping == "0-10"),
    aes(x = x.Completion.Rate),
    fill = "#c0d5da",
    bins = bin.size
  ) +
  geom_histogram(
    data = UAP.per.player.shortlist %>% filter(grouping == "10-20"),
    aes(x = x.Completion.Rate),
    fill = "#a7cedb",
    bins = bin.size
  ) +
  geom_histogram(
    data = UAP.per.player.shortlist %>% filter(grouping == "20-30"),
    aes(x = x.Completion.Rate),
    fill = "#8bc6dc",
    bins = bin.size
  ) +
  geom_histogram(
    data = UAP.per.player.shortlist %>% filter(grouping == "30-40"),
    aes(x = x.Completion.Rate),
    fill = "#53b6df",
    bins = bin.size
  ) +
  geom_histogram(
    data = UAP.per.player.shortlist %>% filter(grouping == "40-50"),
    aes(x = x.Completion.Rate),
    fill = "#28aae1",
    bins = bin.size
  ) + theme_minimal() + theme(
    panel.background = element_rect(fill = "#DFDED9", colour = "#DFDED9"),
    plot.background = element_rect(fill = "#DFDED9"),
    panel.grid.major = element_line(colour = "#CECDC9"),
    panel.grid.minor = element_line(colour = "#CECDC9")
  )




mean(per.player.conversion$xCompletion.Diff.Total)

#top 10 of top 2 groupings
xx <-
  per.player.conversion %>% filter(grouping == "200-300" |
                                     grouping == "300-400" | grouping == "400-500") %>%
  arrange(xCompletion.Diff.Total)

keeps <-
  c(
    "Ante Rebić",
    "David Silva",
    "Eden Hazard",
    "Johan Andrés Mojica Palacio",
    "Jordan Henderson
    ",
    "Dele Alli",
    "John Stones
    "
  )
xxx <- per.player.conversion %>% filter(player.name %in% keeps)

ggplot(data = pass.analysis) + geom_smooth(aes(x = Uniqueness, y = xCompletion))
ggplot(data = passDB) +  geom_smooth(aes(x = Uniqueness, y = xCompletion))

ggplot() + geom_point(
  data = passDB,
  aes(x = Uniqueness, y = xCompletion),
  colour = "grey",
  alpha = 0.02
) + geom_point(
  data = pass.analysis,
  aes(x = Uniqueness, y = xCompletion),
  colour = "red",
  alpha = 0.0
)

quantile(passDB$Uniqueness)

passDB2 <-
  passDB %>% group_by(id) %>% mutate(x.Comp.diff = success - xCompletion)



xxxx <-
  passDB2 %>% filter(Uniqueness > 168 &
                       x > 80 &
                       x < x.destination) %>% group_by(player.name)  %>% summarise(n = n(), x.Comp.diff = mean(x.Comp.diff))
Total.Mins <-
  minutes.played %>% group_by(player.name) %>% summarise(Total.Minutes = sum(minutes.played))
xxxx <- merge(xxxx, Total.Mins, by = "player.name")
head(xxxx)

xxxx <-
  xxxx %>% group_by(player.name) %>% mutate(UniquePass90 = (n / Total.Minutes) * 90) %>%  filter(Total.Minutes > 180)

xxxx <- merge(xxxx, Player.Positions, by = "player.name", all = F)
xxxx <-
  merge(xxxx, position.catergory, by = "position.name", all = F)

player.select = "Cristiano Ronaldo dos Santos Aveiro"
ggplot(data = xxxx) +
  geom_point(aes(x = x.Comp.diff, y = UniquePass90, colour = catergory)) +
  annotate(
    "text",
    x = xxxx$x.Comp.diff,
    y = xxxx$UniquePass90,
    label = xxxx$player.name
  ) +
  geom_vline(xintercept = mean(xxxx$x.Comp.diff)) +
  geom_point(
    data = xxxx %>% filter(player.name == player.select),
    aes(x = x.Comp.diff, y = UniquePass90),
    colour = "red",
    fill = "red"
  )


## choose just the world cup games
pass.analysis <-
  passDB %>% filter(between(x, 60, 80) & x.destination > x | x > 80)

## Selection one
#Selection1 <- passDB_and_results %>% filter(between(x, 60, 80) & x.destination > x)
#Selection2 <- passDB_and_results %>% filter(x > 80)
#Selection <- bind_rows(Selection1, Selection2)



## ########################################################## ##
#                                                          ## ##
# Sandbox                                       ####
#                                                          ## ##
## ########################################################## ##

ggplot(U.A.Passes, aes(x = x, y = as.numeric(success))) +
  geom_jitter(height = 0.02,
              alpha = 0.1,
              color = "black") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#D43B47",
    aes(outfit = fit <<- ..y..)
  ) +
  xlab("Number of leaves") +
  ylab("Survival probability") + theme_minimal()


ggplot(U.A.Passes, aes(x = x.destination, y = as.numeric(success))) +
  geom_jitter(height = 0.02,
              alpha = 0.1,
              color = "black") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#D43B47",
    aes(outfit = fit <<- ..y..)
  ) +
  xlab("Number of leaves") +
  ylab("Survival probability") + theme_minimal()

ggplot(U.A.Passes, aes(x = y, y = as.numeric(success))) +
  geom_jitter(height = 0.02,
              alpha = 0.1,
              color = "black") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#D43B47",
    aes(outfit = fit <<- ..y..)
  ) +
  xlab("Number of leaves") +
  ylab("Survival probability") + theme_minimal()

ggplot(U.A.Passes, aes(x = y.destination, y = as.numeric(success))) +
  geom_jitter(height = 0.02,
              alpha = 0.1,
              color = "black") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#D43B47",
    aes(outfit = fit <<- ..y..)
  ) +
  xlab("Number of leaves") +
  ylab("Survival probability") + theme_minimal()

ggplot(U.A.Passes, aes(x = y.destination, y = as.numeric(success))) +
  geom_jitter(height = 0.02,
              alpha = 0.1,
              color = "black") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#D43B47",
    aes(outfit = fit <<- ..y..)
  ) +
  xlab("Number of leaves") +
  ylab("Survival probability") + theme_minimal()



require(popbio)

## prepare some extra data to allow the analysis

## plot the for all passes
logi.hist.plot(
  passDB$Uniqueness,
  passDB$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by Uniqueness : All Passes",
  xlabel = "Uniqueness",
  logi.mod = 2,
  colour = "#blue"
)
logi.hist.plot(
  U.A.Passes$Uniqueness,
  U.A.Passes$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by Uniqueness : All Passes",
  xlabel = "Uniqueness",
  logi.mod = 2
)


logi.hist.plot(
  passDB$y,
  passDB$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by x : All Passes",
  xlabel = "x",
  logi.mod = 2
)
logi.hist.plot(
  passDB$x,
  passDB$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by x : All Passes",
  xlabel = "x",
  logi.mod = 2
)
logi.hist.plot(
  passDB$x.destination,
  passDB$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by x : All Passes",
  xlabel = "x",
  logi.mod = 2
)
logi.hist.plot(
  passDB$y.destination,
  passDB$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by x : All Passes",
  xlabel = "x",
  logi.mod = 2
)
logi.hist.plot(
  passDB$pass.angle,
  passDB$success,
  type = "dit",
  boxp = F,
  mainlabel = "Probabilty of Outcome by x : All Passes",
  xlabel = "x",
  logi.mod = 2
)

comp <- FreeCompetitions()
FreeMatches(comp$competition_id)

