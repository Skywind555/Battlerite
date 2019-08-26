# User Guide

[See the Github Repository](https://github.com/Skywind555/Personal-Projects) for all project files.

## Pre-data processing

Specifics in the Process_Data.R file.

Files used in the dashboard are located in the /Dashboard Data folder

## File Selection

Choose the desired range of data to select.

For the "Select Days Included" and "Select files" boxes, left click the box to select more options. 

To delete selections, left click the value and then press delete on the keyboard.

Left click the "Choose Selected Data" button at the top right to choose the data you've selected.

## Champion

**View Champion specific statistics**

The 'Reset All Filters' button resets all current filters in one push

### Measures

**Win rate:** Compute the win rates of various sub categories in each category filtered to the selected champion. Each user can count multiple times.

**Win rate adjusted:** Same as Win rate except that each user can only count once per sub category in each category (User win rates are averaged).

**Pick rate:** Compute the percent distributions of the sub categories in each category filtered to the selected champion. The Pick rate adjusted buttons appear that when clicked will compute the adjusted version of the category. 

### Overall measure

The top right of the page shows the overall win rate, adjusted win rate, pick rate, or adjusted pick rate. This number only shows if no filters are applied.

### Geographic Map

Click on a point to filter the data to that region. An unfilter button will appear that allows you to undo the selection.

### Bar Plots

In general, you can **hover** the mouse over each bar in any bar graph to see the specific value of the bar and the number of observations.

The hover information may be displaced from the mouse cursor depending on resolution of the computer? Unsure if this can be fixed.

You can **double click** on a bar to **filter** the entire data to that value. **Single click** to **unfilter**.

#### Specific Bar Plots

**Total_Time_Played and Champion_Time_Played categories are in hours.

**Battlerite Choice:** The color of the graph represents battlerite type same as in-game color. The initial view shows the win rate or pick rate of the indicated battlerite if a user took that battlerite as one of the five battlerites in their match. Double clicking on a battlerite (choosing the battlerite) allows you to see the next step up. Assuming that you pick that selected battlerite, the new values are now the win rate or pick rates of each indicated battlerite assuming that you selected the first battlerite. For adjusted win rate calculation, each user can contribute once in each category. For adjusted pick rate calculation, each user's most selected set of 5 battlerites are determined and that's their only contribution. 

**Mount, Outfit, Attachment, Pose:** The color of the graphs are colored by rarity.

**Avatar:** Since the assigned avatar name does not make a whole lot of sense, the hover information contains what the image actually looks like.

### Data Tables

**Region Group (Very top):** Click on the row to filter the data to that region group. An unfilter button will appear if you want to unfilter.

**Player data (Very bottom):** Click on a player to see all the games a player has played with the selected champion. A filter button appears if you want to filter the entire data to only include games with the player. A list of games that the player has played in appears. Clicking on a row (game) will allow you to see round specific data of the player and other players in that game. Selecting on a row in this new data table allows you to see the round level statistics and overall account information on the player.

You can also type in a player's name to filter the results to the player or a part of a player's name.

**Average Stats:**

The number represents the average value per round except for:

First_Orb: Shows the probability of getting the first orb in any given round.

MVP: Shows the probability of being MVP in any given round.

Highest Score: Shows the probability of having the highest score in any given round (regardless of winning or losing the round)

Survived: Shows the probability of surviving the round on wins or forfeited losses.

Other notes:

Round_Length and Queue_Time are in seconds.

### Comps / Matchup tables

Use the double click filter on the Server_Type bar graph to filter the selection to what you want to see i.e. 2v2, Solo Queue, or 3v3. Otherwise, the table shows information from all three. Filters from other categories will further filter the results.

The results of the tables and block of text tries to filter the data to only include sub-categories of at least three observations, but if the data is not at least ten different sub-categories, it will try for 2 observations, and then default (1).

When computing pick rate (adjusted) for the [long] indicated buttons, only the top 50 largest sample sized groups will be used. Without this measure, computation time can take 10+ minutes for the full range of data. So, the Top 5 least popular comps and matchups are not literally the least popular comps or matchups.

## Overall view

**Still in development**