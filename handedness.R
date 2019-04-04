# handedness in youth baseball data analysis used in FilterJoe blog post: 
# https://www.filterjoe.com/2019/04/04/handedness-in-youth-baseball/
# 2018 baseball data obtained from http://www.seanlahman.com/baseball-archive/statistics/

library(plyr)
library(ggplot2)

players <- read.csv('People.csv')
appear <- read.csv('Appearances.csv')

# I choose 1930 and beyond as I'll be grouping data by decade, and handendness data is incomplete for many players prior to 1930.
recentplayers <- subset(players, (as.Date(finalGame) > as.Date("1930-01-01")))
recentplayers <- recentplayers[order(recentplayers$finalGame),]

# Create finalYear column, first with year, then changing last digit to zero so it's easy to group by decade.
recentplayers$finalYear <- format(as.Date(recentplayers$finalGame), "%Y") # create new finalYear column
substr(recentplayers$finalYear, 4, 4) <- "0" # rounding off finalYear to decade

# In answer to the question, "How has handedness changed over time in MLB" . . .

# The following loop creates a chart showing the distribution of all hitting/throwing handendess combos by decade
# Maybe there's an "R" way of doing it without using a for loop, but this works . . .

decades = list("1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010")
for (decade in decades) {
    recenthandedness <- count(recentplayers[recentplayers$finalYear==decade,], c("finalYear", "bats", "throws"))
    recenthandedness$Perc <- recenthandedness$freq / sum(recenthandedness$freq) * 100  # add Percent of total
    # print(recenthandedness)
}

print(recenthandedness)

# The above data is a bit difficult to digest, so let's prepare two different plots to show how
# 1) hitting handedness has changed over the years
# 2) throwing handedness has changed over the years
# have to prep the data first by changing finalYear to a number

recentplayers$finalYear <-as.numeric(recentplayers$finalYear)

# In English, this (hard-to-read ggplot code) says:
# With recentplayers data, for each finalYear put up a bar graph where we count (how many B vs. L vs. R) batters
# and display it as a percentage of total players within that year.
hithandednessPlot <- ggplot(recentplayers, aes(bats, group = finalYear)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x.., labels = c("Both", "Left", "Right"))), stat="count") + 
    labs(title = "Baseball Hitting Handedness since 1930\n(1930 label counts players who last played 1930-1939)\n",
         fill = "Handedness\nLegend",
         x = "Batting Handedness",
         y = "Relative Frequencies within each Decade") +
#    scale_x_discrete(labels = c('Both','Left','Right')) +  # appears too crowded
    scale_y_continuous(labels=scales::percent) +
    facet_grid(~finalYear)

hithandednessPlot

# With recentplayers data (minus the 1 and only switch pitcher),
# for each finalYear put up a bar graph where we count (how many L vs. R) throws
# and display it as a percentage of total players within that year.
throwhandednessPlot <- ggplot(subset(recentplayers, throws!="S"), aes(throws, group = finalYear)) + # removed the switch thrower from data set
    geom_bar(aes(y = ..prop.., fill = factor(..x.., labels = c("Left", "Right"))), stat="count") + 
    labs(title = "Baseball Throwing Handedness since 1930\n(1930 label counts players who last played 1930-1939)\n",
         fill = "Handedness\nLegend",
         x = "Throwing Handedness",
         y = "Relative Frequencies within each Decade") +
    #    scale_x_discrete(labels = c('Both','Left','Right')) +  # appears too crowded
    scale_y_continuous(labels=scales::percent) +
    facet_grid(~finalYear)

# throwhandednessPlot
# don't want to write over the prior plot yet. Type throwhandednessPlot in console when ready to view

# We also want to know some much more simple stats:

# What percentage of players in MLB throw left
#    what percentage of pitchers in the MLB throw left?
#    what percentage of position players in the MLB throw left?
# What percentage of players in MLB bat left but throw right?

# next bits of code answers these questions with recenthandness data set, which only contains last 10 years:

xtabs(Perc ~ bats + throws, data = recenthandedness)

# that one line shows that during the past 10 years:
# Over 21% threw left
# Just under 29% batted left
# An amazing 11.2% throw right, bat left, and another 6.5% throw right and are switch hitters

# now we do the same thing for pitchers only

# first condense the appear data down to just 1 row per ID, with the number of games all summed up.
# sum is 0 for non-pitchers, an integer > 0 for pitchers
# merge this into player data so we can query the player data for pitchers vs. nonpitchers

pitchingappearances <- ddply(appear,.(playerID),summarize,sum=sum(G_p),number=length(playerID))

playersANDinnings <- merge(pitchingappearances, recentplayers, by = "playerID")

for (decade in decades) {
    pitcherrecenthandedness <- count(playersANDinnings[playersANDinnings$finalYear==decade & playersANDinnings$sum > 0,], c("finalYear", "bats", "throws"))
    pitcherrecenthandedness$Perc <- pitcherrecenthandedness$freq / sum(pitcherrecenthandedness$freq) * 100  # add Percent of total
    # print(pitcherrecenthandedness)
}

print(pitcherrecenthandedness)

xtabs(Perc ~ bats + throws, data = pitcherrecenthandedness)

# and then same for hitters only

for (decade in decades) {
    hitterrecenthandedness <- count(playersANDinnings[playersANDinnings$finalYear==decade & playersANDinnings$sum == 0,], c("finalYear", "bats", "throws"))
    hitterrecenthandedness$Perc <- hitterrecenthandedness$freq / sum(hitterrecenthandedness$freq) * 100  # add Percent of total
    # print(hitterrecenthandedness)
}

print(hitterrecenthandedness)

xtabs(Perc ~ bats + throws, data = hitterrecenthandedness)
