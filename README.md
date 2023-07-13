# MLBBattingApp


Thank you for taking the time to check out my first baseball analytics web app. 

.                                      

This web app can be ran using RShiny + Statcast data.

I used baseballr::scrape_statcast_savant() to collect pitch by pitch data from the 2019 season to the 2023 All-Star Break.

.

This web app contains dozens of customizable and interactive data visualizations & statistical tables. The combination of these tools offers uniquely specific analysis that can help:
1. team front offices evaluate their own batters or trade/free agent targets
2. pitchers scout their future opponents
3. batters/player development staff identify areas of strengths and weaknesses in their offense

.

There are 5 tabs available in my MLB Batting App.


1. Heat Maps tab


![Screenshot 2023-07-12 233824](https://github.com/josephmontes/MLBBattingApp/assets/125607783/5d6d129a-4f49-4e35-892e-c9558ea9800e)

* After selecting a player and season(s), there is a pitch type drop down menu followed by input sliders for velocity range, vert. & horiz. pitch breaks and spin rate to allow the user to get heat maps vs. a very specific pitch
  
    — adding input filters is fairly straight forward in Rshiny. X & Y release point sliders can quickly be added, filter by count, etc.

* Each set of heat maps is separated by vs. LHP and vs. RHP

* The types of heat maps in this tab are: General pitch location, Whiffs pitch location, Takes pitch location, Swings pitch location, Hits pitch location, Barrels pitch location, plus separate Groundballs, Linedrives, Flyballs, and Popups pitch location heat maps

    — adding different types of heat maps is also made easy in my code using the ‘find_plots()’ function (view code).  Homerun pitch location heat maps could quickly be added, Hard Hit pitch location heat map, etc.

* The higher the number is on the density key (to the right of each set of heat maps) the less spread out the visual representation of the pitch locations are

.

2. Strike Zone tab


![Screenshot 2023-07-12 234117](https://github.com/josephmontes/MLBBattingApp/assets/125607783/70f804dc-7a2b-4f1e-86d0-1da2e6523aab)
* After selecting a player, season(s), pitch type and pitcher handedness, there is a stat drop down menu 
* The stat filter options are: Whiff %, Swing %, Take %, Average Exit Velocity, Average Launch Angle, Run Value, Hard Hit %, Sweet Spot %, Barrel %, and Called Strike + Whiff % (CsW %)
  
    — adding different stat filters will take you a few extra minutes than the previous mentioned possible changes, but it is also a fairly straight forward process through my ‘get_zone_data()’ function (view code), possible additions could be Max Exit Velo or Contact %

* The zone is from the catchers view



![Screenshot 2023-07-12 234314](https://github.com/josephmontes/MLBBattingApp/assets/125607783/2839f398-2a14-43ec-b17f-a3fe2d0e32e5)
* These are interactive histograms made by using the plotly package in R
* When the user hovers their mouse over a bin in either of the histograms, the LA/EV value and the amount in that bin is displayed
* The red highlighted bins in the histogram represent Sweet Spot and Hard Hit
* The histograms do not update with a change in the stat type inputs, they are static for each player
  
.

3. Spray Chart + Hit Profile tab


![Screenshot 2023-07-12 234530](https://github.com/josephmontes/MLBBattingApp/assets/125607783/71595df9-5fca-4062-8e76-26ae297e22e8)
* After selecting a player, season(s), and pitch type, there is a drop down menu to select a hit type.
  
    — the hit type filter options are: Single, Double, Triple, Homerun, All hits, Groundball, Linedrive, Flyball, Popup, All batted balls
  
* The hit type and pitch type filters will update the data visualizations
* But the tables are only reactive to the selected player and seasons
* Tables show plate discipline stats and batted ball stats
* Spray charts are separated by vs. LHP and vs. RHP
* The graphic to the right is an interactive plotly map that shows a players hit profile according to launch angle and exit velo
  
    — the color of the point indicates what the hit result was for the selected player
  
    — the grey lines underneath are showing what combination of launch angles and exit velos are most frequently resulting in hits across the league
  
    — the idea is to get a profile for the type and quality of contact that a specific batter is generally making

.

4. Swing/Take Run Value Data tab


![Screenshot 2023-07-12 234952](https://github.com/josephmontes/MLBBattingApp/assets/125607783/3ea6185d-1c6f-40a2-a40d-8397137f72d1)
* After selecting a player and season(s), there are additional filters that allow the user to filter by pitch count, pitch type and pitcher handedness 
* The zones in focus for this tab are organzied by hittability; ‘heart’ of the zone (most hittable, purple), ‘shadow’ or edge of the zone (red), ‘chase’ part of the zone (yellow), and ‘waste’ or uncompetitive part of the zone (least hittable, grey)
  
     — the dashed black line in the middle of the ‘shadow’ zone represents the strike zone.
  
     — the shadow zone is approx. 8 inches; 4 inches outside the strikezone plus 4 inches inside the strikezone

     — Here is a link further breaking down these zones:  https://cdn.vox-cdn.com/thumbor/Z6D5VvEsk_bJrrCzbuGlMS_zgDA=/1400x0/filters:no_upscale()/cdn.vox-cdn.com/uploads/chorus_asset/file/22175460/shadow_zone.jpg
  
  
* The left graphic visually displays these zones along with the sum of the player’s change in run expectancy for every pitch they have seen in each zone 
* The middle graphic visually displays a color correlated Swing %/Take % horizontal bar graph organized by zone with the league average Swing %/Take % in a thinner, dashed grey bar underneath each zone's color correlated bar 
* The right graphic breaks down the player’s Run Value by zone into Swing Runs and Take Runs in a diverging double bar graph (ex: takes in the ‘waste’ zone will always be positive and takes in the ‘heart’ will always be negative) 




![Screenshot 2023-07-12 235017](https://github.com/josephmontes/MLBBattingApp/assets/125607783/3f49bf16-a6b1-460a-8763-98ddc8cf659f)
* The first table adds up the players total Run Value, Swing Runs and Take Runs
* The second table returns a set of plate discipline statistics, Chase %, Contact %, 1st Pitch Swing % etc.
* The third table basically puts the info from the visualizations into a table format, in addition it returns how often the batter gets a pitch in each of the zones
* Every pitch, the batter makes a decision to take or swing, and each decision results in a change in the expected run total for that inning. By grouping and summing these instances by zone, we can generate many insights regarding a batter's swing & take decisions, as well as their swing & take effectiveness and overall offensive productivity

.

5. Trend Charts tab


![Screenshot 2023-07-12 235127](https://github.com/josephmontes/MLBBattingApp/assets/125607783/e67044d8-5ee6-44c8-b369-337a48eb8199)
* After selecting a player, there is a pitch type filter that will show how a player is trending vs. a certain pitch type, additionally there is a date range filter that can be used to find how a batters stats are trending over any period of time over the course of a season
* These are interactive plotlys that will return the value of the line at any point if the user hovers their mouse over it 
* Only uses 1 season of data rather than 2019-2023 



![Screenshot 2023-07-13 004305](https://github.com/josephmontes/MLBBattingApp/assets/125607783/2b56ddab-8b1f-47fc-82cf-6224a715f567)

* There are 5 trending line graphs in this tab with the following statistical groupings:
    
    — BA, OBP, SLG
    
    — K %, BB %, CsW %, Take %
    
    — Z-Con %, Z-Swing %, O-Con %, O-Swing %
    
    — wOBA vs. xwOBA 
    
    — Hard %, SwSp %, Barrel %


* Trend Charts can be used to differentiate the average .250/.315/.413 hitters into 2 groups; more consistent hitters & more volatile, hot/cold type of hitters

.

In total, this app features 9 different data visualizations and 5 statistical tables. Each data visualization and table prepared with relevant filters that can enhance anyone’s baseball analysis.

.


Thank you again for taking the time to check out my first baseball analytics web app.


Please feel free to email me at josephmontes.baseball@gmail.com with any questions, suggestions, or comments about this project. I can also share the large CSV files containing the relevant Statcast pitch by pitch data.
