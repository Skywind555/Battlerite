import requests
import pandas as pd
import numpy as np
import json
import datetime
import time
import math as m
import numpy as np
import os
import shutil

header = {
  "Authorization": "YOUR KEY",
  "Accept": "application/vnd.api+json"
}

def recode_league(x):
    
    if not pd.isnull(x):

        if x == 0:
            return 'Bronze'
        elif x == 1:
            return 'Silver'
        elif x == 2:
            return 'Gold'
        elif x == 3:
            return 'Platinum'
        elif x == 4:
            return 'Diamond'
        elif x == 5:
            return 'Champion'
        else:
            return 'Grand Champion'
        
    else:
        
        return x

#Read reference files
Attachments = pd.read_csv(os.path.join('Reference Files', 'Attachment_Crosswalk.csv'))
Avatars = pd.read_csv(os.path.join('Reference Files', 'Avatar_Crosswalk.csv'))
#Don't need file name yet
Avatars = Avatars.drop('Avatar File Name', axis = 1)
Battlerites = pd.read_csv(os.path.join('Reference Files', 'Battlerite_Crosswalk.csv'))
#Can't use Battlerite type yet
Battlerites = Battlerites.drop('Battlerite Type', axis = 1)
Champions = pd.read_csv(os.path.join('Reference Files', 'Champion_Crosswalk.csv'))
#Don't need file name yet
Champions = Champions.drop('Icon File Name', axis = 1)
MapIDs = pd.read_csv(os.path.join('Reference Files', 'MapID_Crosswalk.csv'))
Mounts = pd.read_csv(os.path.join('Reference Files', 'Mount_Crosswalk.csv'))
Outfits = pd.read_csv(os.path.join('Reference Files', 'Outfit_Crosswalk.csv'))
Poses = pd.read_csv(os.path.join('Reference Files', 'Pose_Crosswalk.csv'))
Regions = pd.read_excel(os.path.join('Reference Files', 'region.xlsx'))
Titles = pd.read_csv(os.path.join('Reference Files', 'Title_Crosswalk.csv'))

current_datetime = '2019-07-10T09:00:00Z'


def get_data(header, current_datetime, stepsize, days_to_cover, Attachments, Avatars, Battlerites,
             Champions, MapIDs, Mounts, Outfits, Poses, Regions, Titles):
    
    try:
    
        link = "https://api.developer.battlerite.com/shards/global/matches"

        current_datetime_clean = datetime.datetime.strptime(current_datetime, '%Y-%m-%dT%H:%M:%SZ')
        starting_day = current_datetime_clean.strftime('%d')
        starting_month = current_datetime_clean.strftime('%b')
        starting_month_full = current_datetime_clean.strftime('%B')
        starting_year = current_datetime_clean.strftime('%Y')

        created_df = False
        created_leaver_df = False
        overall_urls = []
        resetlink = 0

        total_days_covered = 0

        for query_number in range(100000):

            #There is a page limit on how many games stored in the 'next' option. Reset before reaching it.
            if resetlink >= 1000:

                current_datetime = AllGames['createdAt'].tail(1).values[0]
                resetlink = 0
                link = "https://api.developer.battlerite.com/shards/global/matches"

            uselink = "{}".format(link)

            query = {
              "sort": "createdAt",
              "filter[createdAt-start]": "{}".format(current_datetime),
              "page[limit]": "5"}

            if query_number > 0:

                difference_runtime = end_runtime - start_runtime
                print('Runtime: ' + str(difference_runtime.total_seconds()))

                if difference_runtime.total_seconds() < 6:

                    #If re-running script, make sure only sleep if positive
                    if difference_runtime.total_seconds() > 0:

                        #Sleep to not hit API limit per hour. Added 0.01 for rounding errors
                        time_to_sleep = 6-difference_runtime.total_seconds() + 0.01
                        time.sleep(time_to_sleep)

            start_runtime = datetime.datetime.now()

            #Stop Script from failing because of connection errors
            try:

                re = requests.get(uselink, headers=header, params=query)
                re.close()

            except:

                print('ERROR')
                time.sleep(600)
                re = requests.get(uselink, headers=header, params=query)
                re.close()

            json_data = re.json()

            data = json_data['data']
            included = json_data['included']

            #Get link for the next page on query
            previous_link = link
            link = json_data['links']['next']
            resetlink += 1

            urls = []

            #Get urls from list of dictionaries (not every dictionary has url, no structure)

            for i in range(len(included)):

                try: 

                    urls.append(included[i]['attributes']['URL'])

                except:

                    continue


            #Get overall game data i.e. Arena vs Royale

            createdAt = []
            patchVersion = []
            mapID = []
            matchType = []
            gameID = []
            rankingType = []
            serverType = []

            for game in range(len(data)):

                GameType = data[game]['attributes']['stats']['game']

                if GameType == 'Royale':

                    continue

                createdAt.append(data[game]['attributes']['createdAt'])
                patchVersion.append(data[game]['attributes']['patchVersion'])
                mapID.append(data[game]['attributes']['stats']['mapID'])
                matchType.append(data[game]['attributes']['stats']['type'])
                gameID.append(data[game]['id'])
                rankingType.append(data[game]['attributes']['tags']['rankingType'])
                serverType.append(data[game]['attributes']['tags']['serverType'])

            gamesDf = pd.DataFrame(list(zip(createdAt, patchVersion, mapID, matchType, gameID, rankingType, serverType)),
                                  columns = ['createdAt', 'patchVersion', 'mapID', 'matchType', 'gameID', 'rankingType',
                                             'serverType'])

            #Get game specific information using each url(game)

            for o in range(5):

                url = urls[o]

                #Skip game if already have data
                if url in overall_urls:

                    continue

                overall_urls.append(url)

                r2 = requests.get(url)
                rounds = r2.json()
                round_info_found = False

                #Find out where match data is
                for k in range(len(rounds)):

                    if rounds[k]['type'] == 'com.stunlock.battlerite.match.avro.MatchResultEvent':

                        round_info = rounds[k]
                        round_info_found = True
                        break

                #Skip game if telemetry data does not have this data structure above
                if not round_info_found:
                    continue

                #Skip if number of players is uneven
                if (len(round_info['dataObject']['players']) % 2) != 0:
                    continue

                #Try/except because variables are named differently
                try:

                    #Skip if royale game
                    if round_info['dataObject']['serverType'][:5] == 'ROYAL':

                        continue

                except:

                        #Skip if royale game
                    if round_info['dataObject']['match_type'][:5] == 'ROYAL':

                        continue

                match_gameID = round_info['dataObject']['match_id']        
                season_id = round_info['dataObject']['season_id']
                score_to_win = round_info['dataObject']['score_to_win']

                #Iterate over total number of players in the match
                player_type = []
                user_id = []
                name = []
                match_team = []
                match_team_forfeited = []
                group = []
                user_leave_type = []
                left_during_round = []
                afk_detection_triggered = []
                league_before = []
                division_before = []
                division_rating_before = []
                character = []
                account_level_before = []
                character_level_before = []
                avatar_id = []
                title_id = []
                outfit_id = []
                attachment_id = []
                victory_pose_id = []
                mount_id = []
                br1 = []
                br2 = []
                br3 = []
                br4 = []
                br5 = []

                round_number = []
                kills = []
                deaths = []
                kill_assists = []
                damage_done = []
                damage_received = []
                protection_done = []
                protection_received = []
                control_done = []
                control_received = []
                energy_used = []
                energy_gained = []
                abilities_used = []
                orb_kills = []
                is_mvp = []
                round_length = []
                round_won = []

                total_num_players = len(round_info['dataObject']['players'])
                total_rounds_played = len(round_info['dataObject']['rounds'])

                #Check if total_rounds_played does not match up with rounds recorded by players
                if len(round_info['dataObject']['players'][0]['round_statistics']) < total_rounds_played:
                    total_rounds_played = len(round_info['dataObject']['players'][0]['round_statistics'])
                    if total_rounds_played == 0:
                        total_rounds_played = 1

                #Skip if broken api telemetry data game ID not in data gamesDf
                if match_gameID not in gamesDf['gameID'].unique():
                    continue

                #Append overall game information by matching to gamesDf
                index_match_id = gamesDf[gamesDf['gameID'] == match_gameID].index[0]

                gameID = [match_gameID] * total_num_players * total_rounds_played
                patchVersion = [gamesDf.loc[index_match_id, 'patchVersion']] * total_num_players * total_rounds_played
                createdAt = [gamesDf.loc[index_match_id, 'createdAt']] * total_num_players * total_rounds_played
                mapID = [gamesDf.loc[index_match_id, 'mapID']] * total_num_players * total_rounds_played
                matchType = [gamesDf.loc[index_match_id, 'matchType']] * total_num_players * total_rounds_played
                rankingType = [gamesDf.loc[index_match_id, 'rankingType']] * total_num_players * total_rounds_played
                serverType = [gamesDf.loc[index_match_id, 'serverType']] * total_num_players * total_rounds_played
                seasonID = [round_info['dataObject']['season_id']] * total_num_players * total_rounds_played
                score_to_win = [round_info['dataObject']['score_to_win']] * total_num_players * total_rounds_played
                match_region = [round_info['dataObject']['match_region']] * total_num_players * total_rounds_played

                for player in range(total_num_players):

                    #Iterate over each round in the match
                    for r in range(total_rounds_played):

                        #Player overall information pre-match / battlerite choices / leaver or afk or forfeit
                        player_type.append(round_info['dataObject']['players'][player]['player_type'])
                        user_id.append(round_info['dataObject']['players'][player]['user_id'])
                        name.append(round_info['dataObject']['players'][player]['name'])
                        match_team.append(round_info['dataObject']['players'][player]['match_team'])
                        match_team_forfeited.append(round_info['dataObject']['players'][player]['match_team_forfeited'])
                        group.append(round_info['dataObject']['players'][player]['group'])
                        user_leave_type.append(round_info['dataObject']['players'][player]['user_leave_type'])
                        left_during_round.append(round_info['dataObject']['players'][player]['left_during_round'])
                        afk_detection_triggered.append(round_info['dataObject']['players'][player]['afk_detection_triggered'])
                        league_before.append(round_info['dataObject']['players'][player]['league_before'])
                        division_before.append(round_info['dataObject']['players'][player]['division_before'])
                        division_rating_before.append(round_info['dataObject']['players'][player]['division_rating_before'])
                        character.append(round_info['dataObject']['players'][player]['character'])
                        account_level_before.append(round_info['dataObject']['players'][player]['account_level_before'])
                        character_level_before.append(round_info['dataObject']['players'][player]['character_level_before'])
                        avatar_id.append(round_info['dataObject']['players'][player]['avatar_id'])
                        title_id.append(round_info['dataObject']['players'][player]['title_id'])
                        outfit_id.append(round_info['dataObject']['players'][player]['outfit_id'])
                        attachment_id.append(round_info['dataObject']['players'][player]['attachment_id'])
                        victory_pose_id.append(round_info['dataObject']['players'][player]['victory_pose_id'])
                        mount_id.append(round_info['dataObject']['players'][player]['mount_id'])

                        #Check for leaver
                        if len(round_info['dataObject']['players'][player]['battlerite_loadout']) == 0:
                            br1.append(np.nan)
                            br2.append(np.nan)
                            br3.append(np.nan)
                            br4.append(np.nan)
                            br5.append(np.nan)
                        else:

                            br1.append(round_info['dataObject']['players'][player]['battlerite_loadout'][0])
                            br2.append(round_info['dataObject']['players'][player]['battlerite_loadout'][1])
                            br3.append(round_info['dataObject']['players'][player]['battlerite_loadout'][2])
                            br4.append(round_info['dataObject']['players'][player]['battlerite_loadout'][3])
                            br5.append(round_info['dataObject']['players'][player]['battlerite_loadout'][4])

                        #Fill in nans for players with no information because they never connected
                        try:

                            round_number.append(round_info['dataObject']['players'][player]['round_statistics'][r]['round'])
                            kills.append(round_info['dataObject']['players'][player]['round_statistics'][r]['kills'])
                            deaths.append(round_info['dataObject']['players'][player]['round_statistics'][r]['deaths'])
                            kill_assists.append(round_info['dataObject']['players'][player]['round_statistics'][r]['kill_assists'])
                            damage_done.append(round_info['dataObject']['players'][player]['round_statistics'][r]['damage_done'])
                            damage_received.append(round_info['dataObject']['players'][player]['round_statistics'][r]['damage_received'])
                            protection_done.append(round_info['dataObject']['players'][player]['round_statistics'][r]['protection_done'])
                            protection_received.append(round_info['dataObject']['players'][player]['round_statistics'][r]['protection_received'])
                            control_done.append(round_info['dataObject']['players'][player]['round_statistics'][r]['control_done'])
                            control_received.append(round_info['dataObject']['players'][player]['round_statistics'][r]['control_received'])
                            energy_used.append(round_info['dataObject']['players'][player]['round_statistics'][r]['energy_used'])
                            energy_gained.append(round_info['dataObject']['players'][player]['round_statistics'][r]['energy_gained'])
                            abilities_used.append(round_info['dataObject']['players'][player]['round_statistics'][r]['abilities_used'])
                            orb_kills.append(round_info['dataObject']['players'][player]['round_statistics'][r]['orb_kills'])
                            is_mvp.append(round_info['dataObject']['players'][player]['round_statistics'][r]['is_mvp'])
                            round_length.append(round_info['dataObject']['rounds'][r]['round_length'])

                            #Determine if player won or lost the round
                            if round_info['dataObject']['players'][player]['match_team'] == round_info['dataObject']['rounds'][r]['winning_match_team']:
                                round_won.append(1)
                            else:
                                round_won.append(0)

                        except:

                            #Must be first round if never connected
                            round_number.append(0)
                            kills.append(np.nan)
                            deaths.append(np.nan)
                            kill_assists.append(np.nan)
                            damage_done.append(np.nan)
                            damage_received.append(np.nan)
                            protection_done.append(np.nan)
                            protection_received.append(np.nan)
                            control_done.append(np.nan)
                            control_received.append(np.nan)
                            energy_used.append(np.nan)
                            energy_gained.append(np.nan)
                            abilities_used.append(np.nan)
                            orb_kills.append(np.nan)
                            is_mvp.append(np.nan)
                            round_length.append(np.nan)
                            round_won.append(np.nan)

                w = list(zip(gameID, patchVersion, createdAt, mapID, matchType, rankingType, serverType,
                             seasonID, score_to_win, match_region, player_type, user_id, name, match_team,
                             match_team_forfeited, group, user_leave_type, left_during_round, afk_detection_triggered,
                             league_before, division_before, division_rating_before, character, account_level_before,
                             character_level_before, avatar_id, title_id, outfit_id, attachment_id, victory_pose_id,
                             mount_id, br1, br2, br3, br4, br5, round_number, kills, deaths, kill_assists, damage_done,
                             damage_received, protection_done, protection_received, control_done, control_received,
                             energy_used, energy_gained, abilities_used, orb_kills, is_mvp, round_length, round_won))

                #Create DataFrame of the current game-round information
                Game_AllRounds = pd.DataFrame(w, 
                                      columns = ['gameID', 'patchVersion', 'createdAt', 'mapID', 'matchType', 'rankingType', 
                                                 'serverType', 'seasonID', 'score_to_win', 'match_region', 'player_type', 
                                                 'userID', 'name', 'match_team', 'match_team_forfeited', 'group', 'user_leave_type', 
                                                 'left_during_round', 'afk_detection_triggered', 'league_before', 'division_before', 
                                                 'division_rating_before', 'character', 'account_level_before', 'character_level_before', 
                                                 'avatar_id', 'title_id', 'outfit_id', 'attachment_id', 'victory_pose_id', 'mount_id', 
                                                 'br1', 'br2', 'br3', 'br4', 'br5', 'Round', 'kills', 'deaths', 'kill_assists', 
                                                 'damage_done', 'damage_received', 'protection_done', 'protection_received', 'control_done', 
                                                 'control_received', 'energy_used', 'energy_gained', 'abilities_used', 'orb_kills', 
                                                 'is_mvp', 'round_length', 'round_won'])
                #Sometimes userID not set as object every time. Need to make sure is object for instances merge on userID
                Game_AllRounds['userID'] = Game_AllRounds['userID'].astype(str)

                #Skip game if game does not have all player data
                if Game_AllRounds.loc[0, 'serverType'] == 'QUICK2V2':
                    if total_num_players != 4:
                        continue
                elif Game_AllRounds.loc[0, 'serverType'] == 'QUICK3V3':
                    if total_num_players != 6:
                        continue

                #Exclude leaver / forfeit / never connected games and output to diff DF with url
                if not all(x == 'NORMAL' for x in Game_AllRounds['user_leave_type'].values):
                    Game_AllRounds['url'] = urls[o]

                    if created_leaver_df:

                        LeaverDf = pd.concat([LeaverDf, Game_AllRounds], axis = 0)
                        end_runtime = datetime.datetime.now()
                        excluded_games += 1
                        print('Excluded games: ' + str(excluded_games))

                        continue

                    else:

                        created_leaver_df = True
                        LeaverDf = Game_AllRounds.copy()
                        end_runtime = datetime.datetime.now() 
                        excluded_games = 1
                        print('Excluded games: ' + str(excluded_games))
                        continue

                #Find total score
                Game_AllRounds['Total Score'] = Game_AllRounds['damage_done'] + Game_AllRounds['protection_done'] + \
                                                Game_AllRounds['control_done']

                #Find who had highest score in each round
                Game_AllRounds['Highest Score'] = 0
                for i in range(total_rounds_played):
                    round_subset = Game_AllRounds[Game_AllRounds['Round'] == i]
                    index_highest_score = round_subset.sort_values(by = 'Total Score', ascending = False).head(1).index[0]
                    Game_AllRounds.loc[index_highest_score, 'Highest Score'] = 1

                #Get round event information

                #List pairs to store timestamp of orb take and user id and round
                orb_lasthit_time = []
                orb_lasthit_user = []
                orb_lasthit_round = []

                #List pairs to store health shard round and user
                health_round = []
                health_user = []

                #List pairs to store energy shard round and user
                energy_round = []
                energy_user = []

                #list pairs to store energy ability usage round and user
                energy_used_round = []
                energy_used_user = []

                #Round finish event
                round_finish = []

                #Queue index
                queue_index = []

                #MatchReservedUser index
                user_index = []

                for p in range(len(rounds)):
                    if rounds[p]['type'] == 'Structures.RoundEvent':

                        if rounds[p]['dataObject']['type'] == 'RUNE_LASTHIT':
                            orb_lasthit_time.append(rounds[p]['dataObject']['time'])
                            orb_lasthit_user.append(rounds[p]['dataObject']['userID'])
                            orb_lasthit_round.append(rounds[p]['dataObject']['round'])
                        if rounds[p]['dataObject']['type'] == 'HEALTH_SHARD_PICKUP':
                            health_round.append(rounds[p]['dataObject']['round'])
                            health_user.append(rounds[p]['dataObject']['userID'])
                        if rounds[p]['dataObject']['type'] == 'ENERGY_SHARD_PICKUP':
                            energy_round.append(rounds[p]['dataObject']['round'])
                            energy_user.append(rounds[p]['dataObject']['userID'])
                        if rounds[p]['dataObject']['type'] == 'ENERGY_ABILITY_USED':
                            energy_used_round.append(rounds[p]['dataObject']['round'])
                            energy_used_user.append(rounds[p]['dataObject']['userID'])

                    #Also find index of round finish event
                    if rounds[p]['type'] == 'Structures.RoundFinishedEvent':
                        round_finish.append(p)

                    #Find queue index of rounds
                    if rounds[p]['type'] == 'com.stunlock.service.matchmaking.avro.QueueEvent':
                        queue_index.append(p)

                    #Find matchreserveduser index of Rounds
                    if rounds[p]['type'] == 'Structures.MatchReservedUser':
                        user_index.append(p)

                #Get Last orb hits information sorted in dataframe
                OrbTakes = pd.DataFrame(list(zip(orb_lasthit_round, orb_lasthit_user, orb_lasthit_time)),
                                       columns = ['Round', 'userID', 'Time'])
                OrbTakes = OrbTakes.sort_values(by = ['Round', 'Time'], ascending = [True, True])

                first_orbtake_user = []
                first_orbtake_round = []

                #Find who took first orb in each round
                for i in range(total_rounds_played):
                    OrbTakes_round = OrbTakes[OrbTakes['Round'] == i].reset_index(drop = True)
                    if len(OrbTakes_round) != 0:
                        first_orbtake_user.append(OrbTakes_round.loc[0, 'userID'])
                        first_orbtake_round.append(OrbTakes_round.loc[0, 'Round'])

                FirstOrbs = pd.DataFrame(list(zip(first_orbtake_user, first_orbtake_round)),
                                        columns = ['userID', 'Round'])
                FirstOrbs['userID'] = FirstOrbs['userID'].astype(str)

                FirstOrbs['FirstOrb'] = 1

                #Find out what team each userID above is on
                FirstOrbs['match_team'] = 1
                for index, row in FirstOrbs.iterrows():
                    index_at = Game_AllRounds[(Game_AllRounds['userID'] == row['userID']) &
                                              (Game_AllRounds['Round'] == row['Round'])].index[0]
                    FirstOrbs.loc[index, 'match_team'] = Game_AllRounds.loc[index_at, 'match_team']

                #Find how many health shards picked up by every user in each round
                HealthShards = pd.DataFrame(list(zip(health_round, health_user)), columns = ['Round', 'userID'])
                HealthShards['HealthShards'] = 1
                HealthShards = HealthShards.groupby(['userID', 'Round']).sum().reset_index()

                #Find how many energy shards picked up by every user in each round
                EnergyShards = pd.DataFrame(list(zip(energy_round, energy_user)), columns = ['Round', 'userID'])
                EnergyShards['EnergyShards'] = 1
                EnergyShards = EnergyShards.groupby(['userID', 'Round']).sum().reset_index()

                #Find how many times each user used energy in every round
                EnergyUsed = pd.DataFrame(list(zip(energy_used_round, energy_used_user)), columns = ['Round', 'userID'])
                EnergyUsed['Num_EnergyUsed'] = 1
                EnergyUsed = EnergyUsed.groupby(['userID', 'Round']).sum().reset_index()

                #Add information above to Game_AllRounds
                HealthShards['userID'] = HealthShards['userID'].astype(str)
                EnergyShards['userID'] = EnergyShards['userID'].astype(str)
                EnergyUsed['userID'] = EnergyUsed['userID'].astype(str)

                Game_AllRounds = pd.merge(Game_AllRounds, HealthShards, on = ['userID', 'Round'], how = 'left')
                Game_AllRounds = pd.merge(Game_AllRounds, EnergyShards, on = ['userID', 'Round'], how = 'left')
                Game_AllRounds = pd.merge(Game_AllRounds, EnergyUsed, on = ['userID', 'Round'], how = 'left')
                Game_AllRounds = pd.merge(Game_AllRounds, FirstOrbs, on = ['match_team', 'Round'], how = 'left')
                Game_AllRounds['userID'] = Game_AllRounds.loc[:, 'userID_x']
                Game_AllRounds = Game_AllRounds.drop(['userID_x', 'userID_y'], axis = 1)

                #Fill in nan values with 0 if not leaver, afk etc
                for index, row in Game_AllRounds.iterrows():
                    if ((pd.isnull(row['HealthShards'])) and (pd.notnull(row['kills']))):
                        Game_AllRounds.loc[index, 'HealthShards'] = 0

                    if ((pd.isnull(row['EnergyShards'])) and (pd.notnull(row['kills']))):
                        Game_AllRounds.loc[index, 'EnergyShards'] = 0

                    if ((pd.isnull(row['Num_EnergyUsed'])) and (pd.notnull(row['kills']))):
                        Game_AllRounds.loc[index, 'Num_EnergyUsed'] = 0

                    if ((pd.isnull(row['FirstOrb'])) and (pd.notnull(row['kills']))):
                        Game_AllRounds.loc[index, 'FirstOrb'] = 0

                #Find time alive information
                userID = []
                round_num = []
                timeAlive = []

                for i in range(len(round_finish)):
                    round_event = rounds[round_finish[i]]

                    for j in range(len(round_event['dataObject']['playerStats'])):

                        round_num.append(round_event['dataObject']['round'])
                        userID.append(round_event['dataObject']['playerStats'][j]['userID'])
                        timeAlive.append(round_event['dataObject']['playerStats'][j]['timeAlive'])

                TimeAlive = pd.DataFrame(list(zip(userID, round_num, timeAlive)), 
                                         columns = ['userID', 'Round', 'Time Alive'])

                TimeAlive['userID'] = TimeAlive['userID'].astype(str)

                Game_AllRounds = pd.merge(Game_AllRounds, TimeAlive, on = ['userID', 'Round'], how = 'left')

                #Fill in nan values in TimeAlive same value as all other players
                for index, row in Game_AllRounds.iterrows():
                    if pd.isnull(row['Time Alive']):

                        if pd.isnull(row['Round']):

                            continue

                        else:

                            Round_subset = Game_AllRounds[Game_AllRounds['Round'] == row['Round']].sort_values(by = 'Time Alive').reset_index(drop = True)
                            Game_AllRounds.loc[index, 'Time Alive'] = Round_subset.loc[0, 'Time Alive']

                #Add queue time, ping to server, solo queue/team, limited matchmaking
                premade = []
                queue_time = []
                ping = []
                limited_mm = []
                userID = []

                for i in range(len(queue_index)):
                    queue_event = rounds[queue_index[i]]

                    userID.append(queue_event['dataObject']['userId'])
                    queue_time.append(queue_event['dataObject']['timeInQueue'])

                    if len(queue_event['dataObject']['teamMembers']) != 0:
                        premade.append(1)

                    else:
                        premade.append(0)

                    if queue_event['dataObject']['limitMatchmakingRange'] == False:
                        limited_mm.append(0)

                    else:
                        limited_mm.append(1)

                    match_region = Game_AllRounds.loc[0, 'match_region']

                    #Check ping to match region over selected regions
                    for j in range(len(queue_event['dataObject']['regionSamples'])):

                        if queue_event['dataObject']['regionSamples'][j]['region'] == match_region:

                            ping.append(queue_event['dataObject']['regionSamples'][j]['latencyMS'])

                #Combine information to data frame
                QueueInfo = pd.DataFrame(list(zip(userID, premade, queue_time, ping, limited_mm)),
                                         columns = ['userID', 'premade', 'queue_time', 'initial_ping', 'limited_mm'])
                QueueInfo['userID'] = QueueInfo['userID'].astype(str)

                #Combine with Game_AllRounds
                Game_AllRounds = pd.merge(Game_AllRounds, QueueInfo, on = 'userID', how = 'left')

                #Get Character time played for each user in the game and total played
                characterTime = []
                totalTime = []
                userID = []

                for i in range(len(user_index)):
                    user_event = rounds[user_index[i]]

                    characterTime.append(user_event['dataObject']['characterTimePlayed'])
                    userID.append(user_event['dataObject']['accountId'])
                    totalTime.append(user_event['dataObject']['totalTimePlayed'])

                #Combine information to data frame and merge to Game AllRounds
                CharacterTime = pd.DataFrame(list(zip(userID, characterTime, totalTime)),
                                        columns = ['userID', 'Character Time Played', 'Total Time Played'])
                CharacterTime['userID'] = CharacterTime['userID'].astype(str)
                Game_AllRounds = pd.merge(Game_AllRounds, CharacterTime, on = 'userID', how = 'left')


                #Get Ally and Enemy information
                #Allies
                Ally1Champion = []
                Ally1ChampionPlayed = []
                Ally1TotalPlayed = []
                Ally1TotalScore = []
                Ally1DamageTaken = []
                Ally1TimeAlive = []
                Ally1League = []
                Ally1Br1 = []
                Ally1Br2 = []
                Ally1Br3 = []
                Ally1Br4 = []
                Ally1Br5 = []
                Ally1Ping = []
                Ally1HealthShards = []
                Ally1EnergyShards = []
                Ally1Num_EnergyUsed = []
                Ally1Highest_Score = []

                Ally2Champion = []
                Ally2ChampionPlayed = []
                Ally2TotalPlayed = []
                Ally2TotalScore = []
                Ally2DamageTaken = []
                Ally2TimeAlive = []
                Ally2League = []
                Ally2Br1 = []
                Ally2Br2 = []
                Ally2Br3 = []
                Ally2Br4 = []
                Ally2Br5 = []
                Ally2Ping = []
                Ally2HealthShards = []
                Ally2EnergyShards = []
                Ally2Num_EnergyUsed = []
                Ally2Highest_Score = []

                #Enemies
                Enemy1Champion = []
                Enemy1ChampionPlayed = []
                Enemy1TotalPlayed = []
                Enemy1TotalScore = []
                Enemy1DamageTaken = []
                Enemy1TimeAlive = []
                Enemy1League = []
                Enemy1Br1 = []
                Enemy1Br2 = []
                Enemy1Br3 = []
                Enemy1Br4 = []
                Enemy1Br5 = []
                Enemy1Ping = []
                Enemy1HealthShards = []
                Enemy1EnergyShards = []
                Enemy1Num_EnergyUsed = []
                Enemy1Highest_Score = []

                Enemy2Champion = []
                Enemy2ChampionPlayed = []
                Enemy2TotalPlayed = []
                Enemy2TotalScore = []
                Enemy2DamageTaken = []
                Enemy2TimeAlive = []
                Enemy2League = []
                Enemy2Br1 = []
                Enemy2Br2 = []
                Enemy2Br3 = []
                Enemy2Br4 = []
                Enemy2Br5 = []
                Enemy2Ping = []
                Enemy2HealthShards = []
                Enemy2EnergyShards = []
                Enemy2Num_EnergyUsed = []
                Enemy2Highest_Score = []

                Enemy3Champion = []
                Enemy3ChampionPlayed = []
                Enemy3TotalPlayed = []
                Enemy3TotalScore = []
                Enemy3DamageTaken = []
                Enemy3TimeAlive = []
                Enemy3League = []
                Enemy3Br1 = []
                Enemy3Br2 = []
                Enemy3Br3 = []
                Enemy3Br4 = []
                Enemy3Br5 = []
                Enemy3Ping = []
                Enemy3HealthShards = []
                Enemy3EnergyShards = []
                Enemy3Num_EnergyUsed = []
                Enemy3Highest_Score = []

                for i in range(len(Game_AllRounds)):
                    selfchampion = Game_AllRounds['character'][i]
                    br_round = Game_AllRounds['Round'][i]
                    br_team = Game_AllRounds['match_team'][i]

                    AllyDf = Game_AllRounds[(Game_AllRounds['match_team'] == br_team) & 
                                         (Game_AllRounds['Round'] == br_round) & 
                                         (Game_AllRounds['character'] != selfchampion)]

                    AllyDf.reset_index(inplace = True, drop = True)

                    EnemyDf = Game_AllRounds[(Game_AllRounds['match_team'] != br_team) & 
                                         (Game_AllRounds['Round'] == br_round)]

                    EnemyDf.reset_index(inplace = True, drop = True)

                    if total_num_players/2 >= 2:

                        Ally1Champion.append(AllyDf['character'][0])
                        Ally1ChampionPlayed.append(AllyDf['Character Time Played'][0])
                        Ally1TotalPlayed.append(AllyDf['Total Time Played'][0])
                        Ally1TotalScore.append(AllyDf['Total Score'][0])
                        Ally1DamageTaken.append(AllyDf['damage_received'][0])
                        Ally1TimeAlive.append(AllyDf['Time Alive'][0])
                        Ally1League.append(AllyDf['league_before'][0])
                        Ally1Br1.append(AllyDf['br1'][0])
                        Ally1Br2.append(AllyDf['br2'][0])
                        Ally1Br3.append(AllyDf['br3'][0])
                        Ally1Br4.append(AllyDf['br4'][0])
                        Ally1Br5.append(AllyDf['br5'][0])
                        Ally1Ping.append(AllyDf['initial_ping'][0])
                        Ally1HealthShards.append(AllyDf['HealthShards'][0])
                        Ally1EnergyShards.append(AllyDf['EnergyShards'][0])
                        Ally1Num_EnergyUsed.append(AllyDf['Num_EnergyUsed'][0])
                        Ally1Highest_Score.append(AllyDf['Highest Score'][0])

                    else:

                        Ally1Champion.append(np.nan)
                        Ally1ChampionPlayed.append(np.nan)
                        Ally1TotalPlayed.append(np.nan)
                        Ally1TotalScore.append(np.nan)
                        Ally1DamageTaken.append(np.nan)
                        Ally1TimeAlive.append(np.nan)
                        Ally1League.append(np.nan)
                        Ally1Br1.append(np.nan)
                        Ally1Br2.append(np.nan)
                        Ally1Br3.append(np.nan)
                        Ally1Br4.append(np.nan)
                        Ally1Br5.append(np.nan)
                        Ally1Ping.append(np.nan)
                        Ally1HealthShards.append(np.nan)
                        Ally1EnergyShards.append(np.nan)
                        Ally1Num_EnergyUsed.append(np.nan)
                        Ally1Highest_Score.append(np.nan)

                    if total_num_players/2 >= 3:

                        Ally2Champion.append(AllyDf['character'][1])
                        Ally2ChampionPlayed.append(AllyDf['Character Time Played'][1])
                        Ally2TotalPlayed.append(AllyDf['Total Time Played'][1])
                        Ally2TotalScore.append(AllyDf['Total Score'][1])
                        Ally2DamageTaken.append(AllyDf['damage_received'][1])
                        Ally2TimeAlive.append(AllyDf['Time Alive'][1])
                        Ally2League.append(AllyDf['league_before'][1])
                        Ally2Br1.append(AllyDf['br1'][1])
                        Ally2Br2.append(AllyDf['br2'][1])
                        Ally2Br3.append(AllyDf['br3'][1])
                        Ally2Br4.append(AllyDf['br4'][1])
                        Ally2Br5.append(AllyDf['br5'][1])
                        Ally2Ping.append(AllyDf['initial_ping'][1])
                        Ally2HealthShards.append(AllyDf['HealthShards'][1])
                        Ally2EnergyShards.append(AllyDf['EnergyShards'][1])
                        Ally2Num_EnergyUsed.append(AllyDf['Num_EnergyUsed'][1])
                        Ally2Highest_Score.append(AllyDf['Highest Score'][1]) 

                    else:

                        Ally2Champion.append(np.nan)
                        Ally2ChampionPlayed.append(np.nan)
                        Ally2TotalPlayed.append(np.nan)
                        Ally2TotalScore.append(np.nan)
                        Ally2DamageTaken.append(np.nan)
                        Ally2TimeAlive.append(np.nan)
                        Ally2League.append(np.nan)
                        Ally2Br1.append(np.nan)
                        Ally2Br2.append(np.nan)
                        Ally2Br3.append(np.nan)
                        Ally2Br4.append(np.nan)
                        Ally2Br5.append(np.nan)
                        Ally2Ping.append(np.nan)
                        Ally2HealthShards.append(np.nan)
                        Ally2EnergyShards.append(np.nan)
                        Ally2Num_EnergyUsed.append(np.nan)
                        Ally2Highest_Score.append(np.nan)

                    Enemy1Champion.append(EnemyDf['character'][0])
                    Enemy1ChampionPlayed.append(EnemyDf['Character Time Played'][0])
                    Enemy1TotalPlayed.append(EnemyDf['Total Time Played'][0])
                    Enemy1TotalScore.append(EnemyDf['Total Score'][0])
                    Enemy1DamageTaken.append(EnemyDf['damage_received'][0])
                    Enemy1TimeAlive.append(EnemyDf['Time Alive'][0])
                    Enemy1League.append(EnemyDf['league_before'][0])
                    Enemy1Br1.append(EnemyDf['br1'][0])
                    Enemy1Br2.append(EnemyDf['br2'][0])
                    Enemy1Br3.append(EnemyDf['br3'][0])
                    Enemy1Br4.append(EnemyDf['br4'][0])
                    Enemy1Br5.append(EnemyDf['br5'][0])
                    Enemy1Ping.append(EnemyDf['initial_ping'][0])
                    Enemy1HealthShards.append(EnemyDf['HealthShards'][0])
                    Enemy1EnergyShards.append(EnemyDf['EnergyShards'][0])
                    Enemy1Num_EnergyUsed.append(EnemyDf['Num_EnergyUsed'][0])
                    Enemy1Highest_Score.append(EnemyDf['Highest Score'][0])

                    if total_num_players/2 >= 2:

                        Enemy2Champion.append(EnemyDf['character'][1])
                        Enemy2ChampionPlayed.append(EnemyDf['Character Time Played'][1])
                        Enemy2TotalPlayed.append(EnemyDf['Total Time Played'][1])
                        Enemy2TotalScore.append(EnemyDf['Total Score'][1])
                        Enemy2DamageTaken.append(EnemyDf['damage_received'][1])
                        Enemy2TimeAlive.append(EnemyDf['Time Alive'][1])
                        Enemy2League.append(EnemyDf['league_before'][1])
                        Enemy2Br1.append(EnemyDf['br1'][1])
                        Enemy2Br2.append(EnemyDf['br2'][1])
                        Enemy2Br3.append(EnemyDf['br3'][1])
                        Enemy2Br4.append(EnemyDf['br4'][1])
                        Enemy2Br5.append(EnemyDf['br5'][1])
                        Enemy2Ping.append(EnemyDf['initial_ping'][1])
                        Enemy2HealthShards.append(EnemyDf['HealthShards'][1])
                        Enemy2EnergyShards.append(EnemyDf['EnergyShards'][1])
                        Enemy2Num_EnergyUsed.append(EnemyDf['Num_EnergyUsed'][1])
                        Enemy2Highest_Score.append(EnemyDf['Highest Score'][1]) 

                    else:

                        Enemy2Champion.append(np.nan)
                        Enemy2ChampionPlayed.append(np.nan)
                        Enemy2TotalPlayed.append(np.nan)
                        Enemy2TotalScore.append(np.nan)
                        Enemy2DamageTaken.append(np.nan)
                        Enemy2TimeAlive.append(np.nan)
                        Enemy2League.append(np.nan)
                        Enemy2Br1.append(np.nan)
                        Enemy2Br2.append(np.nan)
                        Enemy2Br3.append(np.nan)
                        Enemy2Br4.append(np.nan)
                        Enemy2Br5.append(np.nan)
                        Enemy2Ping.append(np.nan)
                        Enemy2HealthShards.append(np.nan)
                        Enemy2EnergyShards.append(np.nan)
                        Enemy2Num_EnergyUsed.append(np.nan)
                        Enemy2Highest_Score.append(np.nan)

                    if total_num_players/2 >= 3:

                        Enemy3Champion.append(EnemyDf['character'][2])
                        Enemy3ChampionPlayed.append(EnemyDf['Character Time Played'][2])
                        Enemy3TotalPlayed.append(EnemyDf['Total Time Played'][2])
                        Enemy3TotalScore.append(EnemyDf['Total Score'][2])
                        Enemy3DamageTaken.append(EnemyDf['damage_received'][2])
                        Enemy3TimeAlive.append(EnemyDf['Time Alive'][2])
                        Enemy3League.append(EnemyDf['league_before'][2])
                        Enemy3Br1.append(EnemyDf['br1'][2])
                        Enemy3Br2.append(EnemyDf['br2'][2])
                        Enemy3Br3.append(EnemyDf['br3'][2])
                        Enemy3Br4.append(EnemyDf['br4'][2])
                        Enemy3Br5.append(EnemyDf['br5'][2])
                        Enemy3Ping.append(EnemyDf['initial_ping'][2])
                        Enemy3HealthShards.append(EnemyDf['HealthShards'][2])
                        Enemy3EnergyShards.append(EnemyDf['EnergyShards'][2])
                        Enemy3Num_EnergyUsed.append(EnemyDf['Num_EnergyUsed'][2])
                        Enemy3Highest_Score.append(EnemyDf['Highest Score'][2]) 

                    else:

                        Enemy3Champion.append(np.nan)
                        Enemy3ChampionPlayed.append(np.nan)
                        Enemy3TotalPlayed.append(np.nan)
                        Enemy3TotalScore.append(np.nan)
                        Enemy3DamageTaken.append(np.nan)
                        Enemy3TimeAlive.append(np.nan)
                        Enemy3League.append(np.nan)
                        Enemy3Br1.append(np.nan)
                        Enemy3Br2.append(np.nan)
                        Enemy3Br3.append(np.nan)
                        Enemy3Br4.append(np.nan)
                        Enemy3Br5.append(np.nan)
                        Enemy3Ping.append(np.nan)
                        Enemy3HealthShards.append(np.nan)
                        Enemy3EnergyShards.append(np.nan)
                        Enemy3Num_EnergyUsed.append(np.nan)
                        Enemy3Highest_Score.append(np.nan)

                Game_AllRounds['Ally1Champion'] = Ally1Champion
                Game_AllRounds['Ally1ChampionPlayed'] = Ally1ChampionPlayed
                Game_AllRounds['Ally1TotalPlayed'] = Ally1TotalPlayed
                Game_AllRounds['Ally1TotalScore'] = Ally1TotalScore
                Game_AllRounds['Ally1DamageTaken'] = Ally1DamageTaken
                Game_AllRounds['Ally1TimeAlive'] = Ally1TimeAlive
                Game_AllRounds['Ally1League'] = Ally1League
                Game_AllRounds['Ally1Br1'] = Ally1Br1
                Game_AllRounds['Ally1Br2'] = Ally1Br2
                Game_AllRounds['Ally1Br3'] = Ally1Br3
                Game_AllRounds['Ally1Br4'] = Ally1Br4
                Game_AllRounds['Ally1Br5'] = Ally1Br5
                Game_AllRounds['Ally1Ping'] = Ally1Ping
                Game_AllRounds['Ally1HealthShards'] = Ally1HealthShards
                Game_AllRounds['Ally1EnergyShards'] = Ally1EnergyShards
                Game_AllRounds['Ally1Num_EnergyUsed'] = Ally1Num_EnergyUsed
                Game_AllRounds['Ally1Highest_Score'] = Ally1Highest_Score

                Game_AllRounds['Ally2Champion'] = Ally2Champion
                Game_AllRounds['Ally2ChampionPlayed'] = Ally2ChampionPlayed
                Game_AllRounds['Ally2TotalPlayed'] = Ally2TotalPlayed
                Game_AllRounds['Ally2TotalScore'] = Ally2TotalScore
                Game_AllRounds['Ally2DamageTaken'] = Ally2DamageTaken
                Game_AllRounds['Ally2TimeAlive'] = Ally2TimeAlive
                Game_AllRounds['Ally2League'] = Ally2League
                Game_AllRounds['Ally2Br1'] = Ally2Br1
                Game_AllRounds['Ally2Br2'] = Ally2Br2
                Game_AllRounds['Ally2Br3'] = Ally2Br3
                Game_AllRounds['Ally2Br4'] = Ally2Br4
                Game_AllRounds['Ally2Br5'] = Ally2Br5
                Game_AllRounds['Ally2Ping'] = Ally2Ping
                Game_AllRounds['Ally2HealthShards'] = Ally2HealthShards
                Game_AllRounds['Ally2EnergyShards'] = Ally2EnergyShards
                Game_AllRounds['Ally2Num_EnergyUsed'] = Ally2Num_EnergyUsed
                Game_AllRounds['Ally2Highest_Score'] = Ally2Highest_Score               

                Game_AllRounds['Enemy1Champion'] = Enemy1Champion
                Game_AllRounds['Enemy1ChampionPlayed'] = Enemy1ChampionPlayed
                Game_AllRounds['Enemy1TotalPlayed'] = Enemy1TotalPlayed
                Game_AllRounds['Enemy1TotalScore'] = Enemy1TotalScore
                Game_AllRounds['Enemy1DamageTaken'] = Enemy1DamageTaken
                Game_AllRounds['Enemy1TimeAlive'] = Enemy1TimeAlive
                Game_AllRounds['Enemy1League'] = Enemy1League
                Game_AllRounds['Enemy1Br1'] = Enemy1Br1
                Game_AllRounds['Enemy1Br2'] = Enemy1Br2
                Game_AllRounds['Enemy1Br3'] = Enemy1Br3
                Game_AllRounds['Enemy1Br4'] = Enemy1Br4
                Game_AllRounds['Enemy1Br5'] = Enemy1Br5
                Game_AllRounds['Enemy1Ping'] = Enemy1Ping
                Game_AllRounds['Enemy1HealthShards'] = Enemy1HealthShards
                Game_AllRounds['Enemy1EnergyShards'] = Enemy1EnergyShards
                Game_AllRounds['Enemy1Num_EnergyUsed'] = Enemy1Num_EnergyUsed
                Game_AllRounds['Enemy1Highest_Score'] = Enemy1Highest_Score        

                Game_AllRounds['Enemy2Champion'] = Enemy2Champion
                Game_AllRounds['Enemy2ChampionPlayed'] = Enemy2ChampionPlayed
                Game_AllRounds['Enemy2TotalPlayed'] = Enemy2TotalPlayed
                Game_AllRounds['Enemy2TotalScore'] = Enemy2TotalScore
                Game_AllRounds['Enemy2DamageTaken'] = Enemy2DamageTaken
                Game_AllRounds['Enemy2TimeAlive'] = Enemy2TimeAlive
                Game_AllRounds['Enemy2League'] = Enemy2League
                Game_AllRounds['Enemy2Br1'] = Enemy2Br1
                Game_AllRounds['Enemy2Br2'] = Enemy2Br2
                Game_AllRounds['Enemy2Br3'] = Enemy2Br3
                Game_AllRounds['Enemy2Br4'] = Enemy2Br4
                Game_AllRounds['Enemy2Br5'] = Enemy2Br5
                Game_AllRounds['Enemy2Ping'] = Enemy2Ping
                Game_AllRounds['Enemy2HealthShards'] = Enemy2HealthShards
                Game_AllRounds['Enemy2EnergyShards'] = Enemy2EnergyShards
                Game_AllRounds['Enemy2Num_EnergyUsed'] = Enemy2Num_EnergyUsed
                Game_AllRounds['Enemy2Highest_Score'] = Enemy2Highest_Score        

                Game_AllRounds['Enemy3Champion'] = Enemy3Champion
                Game_AllRounds['Enemy3ChampionPlayed'] = Enemy3ChampionPlayed
                Game_AllRounds['Enemy3TotalPlayed'] = Enemy3TotalPlayed
                Game_AllRounds['Enemy3TotalScore'] = Enemy3TotalScore
                Game_AllRounds['Enemy3DamageTaken'] = Enemy3DamageTaken
                Game_AllRounds['Enemy3TimeAlive'] = Enemy3TimeAlive
                Game_AllRounds['Enemy3League'] = Enemy3League
                Game_AllRounds['Enemy3Br1'] = Enemy3Br1
                Game_AllRounds['Enemy3Br2'] = Enemy3Br2
                Game_AllRounds['Enemy3Br3'] = Enemy3Br3
                Game_AllRounds['Enemy3Br4'] = Enemy3Br4
                Game_AllRounds['Enemy3Br5'] = Enemy3Br5
                Game_AllRounds['Enemy3Ping'] = Enemy3Ping
                Game_AllRounds['Enemy3HealthShards'] = Enemy3HealthShards
                Game_AllRounds['Enemy3EnergyShards'] = Enemy3EnergyShards
                Game_AllRounds['Enemy3Num_EnergyUsed'] = Enemy3Num_EnergyUsed
                Game_AllRounds['Enemy3Highest_Score'] = Enemy3Highest_Score

                #Create DataFrame if no current DataFrame. Otherwise, concat on top of existing

                if created_df:

                    AllGames = pd.concat([AllGames, Game_AllRounds], axis = 0)
                    end_runtime = datetime.datetime.now() 
                    print('AllGames: ' + str(len(AllGames)))
                    print('Overall Urls: ' + str(len(overall_urls)))

                else:

                    AllGames = Game_AllRounds.copy()
                    end_runtime = datetime.datetime.now()
                    print('AllGames: ' + str(len(AllGames)))
                    print('Overall Urls: ' + str(len(overall_urls)))

                created_df = True

            #Check if next day, if so, output datafile for current day
            last_datetime = AllGames.tail(1)['createdAt'].values[0]
            print(last_datetime)
            last_datetime_clean = datetime.datetime.strptime(last_datetime, '%Y-%m-%dT%H:%M:%SZ')
            last_day = last_datetime_clean.strftime('%d')

            if last_day != starting_day:

                #Convert code names to readable text
                AllGames = pd.merge(AllGames, MapIDs, left_on = 'mapID', right_on = 'Map ID', how = 'left')
                AllGames = AllGames.drop(['mapID', 'Map ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Value':'Map'})

                AllGames = pd.merge(AllGames, Champions, left_on = 'character', right_on = 'Champion ID', how = 'left')
                AllGames = AllGames.drop(['character', 'Champion ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Value':'Champion'})

                AllGames = pd.merge(AllGames, Avatars, left_on = 'avatar_id', right_on = 'Avatar ID', how = 'left')
                AllGames = AllGames.drop(['avatar_id', 'Avatar ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Value':'Avatar'})

                AllGames = pd.merge(AllGames, Titles, left_on = 'title_id', right_on = 'Title ID', how = 'left')
                AllGames = AllGames.drop(['title_id', 'Title ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Value':'Title'})

                AllGames = pd.merge(AllGames, Outfits, left_on = 'outfit_id', right_on = 'Outfit ID', how = 'left')
                AllGames = AllGames.drop(['outfit_id', 'Outfit ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Rarity':'Outfit Rarity', 'Value':'Outfit'})

                AllGames = pd.merge(AllGames, Attachments, left_on = 'attachment_id', right_on = 'Attachment ID', how = 'left')
                AllGames = AllGames.drop(['attachment_id', 'Attachment ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Rarity':'Attachment Rarity', 'Value':'Attachment'})

                AllGames = pd.merge(AllGames, Poses, left_on = 'victory_pose_id', right_on = 'Pose ID', how = 'left')
                AllGames = AllGames.drop(['victory_pose_id', 'Pose ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Rarity':'Pose Rarity', 'Value':'Pose'})

                AllGames = pd.merge(AllGames, Mounts, left_on = 'mount_id', right_on = 'Mount ID', how = 'left')
                AllGames = AllGames.drop(['mount_id', 'Mount ID'], axis = 1)
                AllGames = AllGames.rename(columns = {'Rarity':'Mount Rarity', 'Value':'Mount'})

                BrCols = ['br1', 'br2', 'br3', 'br4', 'br5']

                for i in range(1, 6):

                    AllGames = pd.merge(AllGames, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    AllGames = AllGames.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':'Battlerite ' + str(i)})

                BrCols = ['Ally1Br1', 'Ally1Br2', 'Ally1Br3', 'Ally1Br4', 'Ally1Br5']

                for i in range(1, 6):

                    AllGames = pd.merge(AllGames, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    AllGames = AllGames.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':BrCols[i-1][:5] + 'Battlerite' + str(i)})

                BrCols = ['Ally2Br1', 'Ally2Br2', 'Ally2Br3', 'Ally2Br4', 'Ally2Br5']

                for i in range(1, 6):

                    AllGames = pd.merge(AllGames, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    AllGames = AllGames.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':BrCols[i-1][:5] + 'Battlerite' + str(i)})

                BrCols = ['Enemy1Br1', 'Enemy1Br2', 'Enemy1Br3', 'Enemy1Br4', 'Enemy1Br5']

                for i in range(1, 6):

                    AllGames = pd.merge(AllGames, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    AllGames = AllGames.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':BrCols[i-1][:6] + 'Battlerite' + str(i)})

                BrCols = ['Enemy2Br1', 'Enemy2Br2', 'Enemy2Br3', 'Enemy2Br4', 'Enemy2Br5']

                for i in range(1, 6):

                    AllGames = pd.merge(AllGames, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    AllGames = AllGames.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':BrCols[i-1][:6] + 'Battlerite' + str(i)})

                BrCols = ['Enemy3Br1', 'Enemy3Br2', 'Enemy3Br3', 'Enemy3Br4', 'Enemy3Br5']

                for i in range(1, 6):

                    AllGames = pd.merge(AllGames, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    AllGames = AllGames.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':BrCols[i-1][:6] + 'Battlerite' + str(i)})

                ChampCols = ['Ally1Champion', 'Ally2Champion']

                for i in range(1, 3):

                    AllGames = pd.merge(AllGames, Champions, how = 'left', left_on = ChampCols[i-1], right_on = 'Champion ID')
                    AllGames = AllGames.drop([ChampCols[i-1], 'Champion ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':ChampCols[i-1], 'Role_x':'Role', 'Role_y':ChampCols[i-1][:5]+'Role'})

                ChampCols = ['Enemy1Champion', 'Enemy2Champion', 'Enemy3Champion']

                for i in range(1, 4):

                    AllGames = pd.merge(AllGames, Champions, how = 'left', left_on = ChampCols[i-1], right_on = 'Champion ID')
                    AllGames = AllGames.drop([ChampCols[i-1], 'Champion ID'], axis = 1)
                    AllGames = AllGames.rename(columns = {'Value':ChampCols[i-1], 'Role_x':'Role', 'Role_y':ChampCols[i-1][:6]+'Role'})

                #Compute new column values    

                num_ultimates = []
                unused_energy = []
                enemy_total_healthshards = []
                enemy_total_energyshards = []
                solo_queue = []

                battlerites = []
                ally1battlerites = []
                ally2battlerites = []
                enemy1battlerites = []
                enemy2battlerites = []
                enemy3battlerites = []
                teamcomp = []
                enemycomp = []
                teamroles = []
                enemyroles = []

                for index, row in AllGames.iterrows():
                    #Find number of times ultimate used during round
                    ultimate_energy = row['energy_used'] - 25*row['Num_EnergyUsed']
                    num_ultimates.append(ultimate_energy/100)
                    unused_energy.append(row['energy_gained'] - row['energy_used'])

                    #Add set of battlerites together
                    battlerites.append(row['Battlerite 1'] + ', ' + row['Battlerite 2'] + ', ' + row['Battlerite 3'] + ', ' + \
                                       row['Battlerite 4'] + ', ' + row['Battlerite 5'])


                    if row['serverType'] == 'QUICK2V2':
                        #Find total health/energy shards taken by enemy
                        enemy_total_healthshards.append(row['Enemy1HealthShards'] + row['Enemy2HealthShards'])
                        enemy_total_energyshards.append(row['Enemy1EnergyShards'] + row['Enemy2EnergyShards'])
                        #Find set of battlerites for ally/enemy
                        ally1battlerites.append(row['Ally1Battlerite1'] + ', ' + row['Ally1Battlerite2'] + ', ' + \
                                                row['Ally1Battlerite3'] + ', ' + row['Ally1Battlerite4'] + ', ' + \
                                                row['Ally1Battlerite5'])
                        ally2battlerites.append('None')
                        enemy1battlerites.append(row['Enemy1Battlerite1'] + ', ' + row['Enemy1Battlerite2'] + ', ' + \
                                                 row['Enemy1Battlerite3'] + ', ' + row['Enemy1Battlerite4'] + ', ' + \
                                                 row['Enemy1Battlerite5'])
                        enemy2battlerites.append(row['Enemy2Battlerite1'] + ', ' + row['Enemy2Battlerite2'] + ', ' + \
                                                 row['Enemy2Battlerite3'] + ', ' + row['Enemy2Battlerite4'] + ', ' + \
                                                 row['Enemy2Battlerite5'])
                        enemy3battlerites.append('None')
                        teamcomp.append(row['Champion'] + ', ' + row['Ally1Champion'])
                        enemycomp.append(row['Enemy1Champion'] + ', ' + row['Enemy2Champion'])
                        teamroles.append(row['Role'] + ', ' + row['Ally1Role'])
                        enemyroles.append(row['Enemy1Role'] + ', ' + row['Enemy2Role'])

                    else:
                        #Repeat above for 3v3 games
                        enemy_total_healthshards.append(row['Enemy1HealthShards'] + row['Enemy2HealthShards'] + row['Enemy3HealthShards'])
                        enemy_total_energyshards.append(row['Enemy1EnergyShards'] + row['Enemy2EnergyShards'] + row['Enemy3EnergyShards'])
                        ally1battlerites.append(row['Ally1Battlerite1'] + ', ' + row['Ally1Battlerite2'] + ', ' + \
                                                row['Ally1Battlerite3'] + ', ' + row['Ally1Battlerite4'] + ', ' + \
                                                row['Ally1Battlerite5'])
                        ally2battlerites.append(row['Ally2Battlerite1'] + ', ' + row['Ally2Battlerite2'] + ', ' + \
                                                row['Ally2Battlerite3'] + ', ' + row['Ally2Battlerite4'] + ', ' + \
                                                row['Ally2Battlerite5'])
                        enemy1battlerites.append(row['Enemy1Battlerite1'] + ', ' + row['Enemy1Battlerite2'] + ', ' + \
                                                 row['Enemy1Battlerite3'] + ', ' + row['Enemy1Battlerite4'] + ', ' + \
                                                 row['Enemy1Battlerite5'])
                        enemy2battlerites.append(row['Enemy2Battlerite1'] + ', ' + row['Enemy2Battlerite2'] + ', ' + \
                                                 row['Enemy2Battlerite3'] + ', ' + row['Enemy2Battlerite4'] + ', ' + \
                                                 row['Enemy2Battlerite5'])
                        enemy3battlerites.append(row['Enemy3Battlerite1'] + ', ' + row['Enemy3Battlerite2'] + ', ' + \
                                                 row['Enemy3Battlerite3'] + ', ' + row['Enemy3Battlerite4'] + ', ' + \
                                                 row['Enemy3Battlerite5'])
                        teamcomp.append(row['Champion'] + ', ' + row['Ally1Champion'] + ', ' + row['Ally2Champion'])
                        enemycomp.append(row['Enemy1Champion'] + ', ' + row['Enemy2Champion'] + ', ' + row['Enemy3Champion'])
                        teamroles.append(row['Role'] + ', ' + row['Ally1Role'] + ', ' + row['Ally2Role'])
                        enemyroles.append(row['Enemy1Role'] + ', ' + row['Enemy2Role'] + ', ' + row['Enemy3Role'])

                    #Determine if solo queue game
                    if ((row['matchType'] == 'LEAGUE3V3') and (row['premade'] == 0)):
                        solo_queue.append(1)
                    else:
                        solo_queue.append(0)

                    #Round ping to step 5
                    if not pd.isnull(row['initial_ping']):
                        AllGames.loc[index, 'initial_ping'] = 5 * round(row['initial_ping']/5)

                    #Display actual round number
                    AllGames.loc[index, 'Round'] = row['Round'] + 1

                #Create new columns
                AllGames['Number of Ultimates Used'] = num_ultimates
                AllGames['Unused Energy'] = unused_energy
                AllGames['Enemy Total Health Shards'] = enemy_total_healthshards
                AllGames['Enemy Total Energy Shards'] = enemy_total_energyshards
                AllGames['Solo Queue'] = solo_queue
                AllGames['Battlerites'] = battlerites
                AllGames['Ally1Battlerites'] = ally1battlerites
                AllGames['Ally2Battlerites'] = ally2battlerites
                AllGames['Enemy1Battlerites'] = enemy1battlerites
                AllGames['Enemy2Battlerites'] = enemy2battlerites
                AllGames['Enemy3Battlerites'] = enemy3battlerites
                AllGames['Team Comp'] = teamcomp
                AllGames['Enemy Comp'] = enemycomp
                AllGames['Team Roles'] = teamroles
                AllGames['Enemy Roles'] = enemyroles

                #Convert League numbers to names
                AllGames['League'] = AllGames['league_before'].apply(recode_league)
                AllGames['Ally1League'] = AllGames['Ally1League'].apply(recode_league)
                AllGames['Ally2League'] = AllGames['Ally2League'].apply(recode_league)
                AllGames['Enemy1League'] = AllGames['Enemy1League'].apply(recode_league)
                AllGames['Enemy2League'] = AllGames['Enemy2League'].apply(recode_league)
                AllGames['Enemy3League'] = AllGames['Enemy3League'].apply(recode_league)


                #Remove useless columns
                AllGames = AllGames.drop(['Enemy1HealthShards', 'Enemy2HealthShards', 'Enemy3HealthShards',
                                          'Enemy1EnergyShards', 'Enemy2EnergyShards', 'Enemy3EnergyShards',
                                          'score_to_win', 'match_team', 'league_before'], axis = 1)

                FinalColNames = ['Game_ID', 'Patch_Version', 'Date', 'Match_Type', 'Ranking_Type', 'Server_Type', 'Season_ID', 'Region',
                                 'Player_Type', 'Name', 'Forfeited', 'Group', 'User_Leave_Type', 'Left_During_Round', 'AFK_Detection',
                                 'Division', 'Division_Rating', 'Account_Level', 'Champion_Level', 'Round', 'Kills', 'Deaths', 'Assists',
                                 'Damage', 'Damage_Received', 'Protection', 'Protection_Received', 'Control', 'Control_Received',
                                 'Energy_Used', 'Energy_Gained', 'Abilities_Used', 'Orb_Kills', 'MVP', 'Round_Length', 'Round_Won',
                                 'Total_Score', 'Highest_Score', 'Health_Shards', 'Energy_Shards', 'Num_Energy_Used', 'First_Orb',
                                 'User_ID', 'Time_Alive', 'Premade', 'Queue_Time', 'Ping', 'Limited_MM', 'Champion_Time_Played',
                                 'Total_Time_Played', 'Ally1ChampionPlayed', 'Ally1TotalPlayed', 'Ally1TotalScore', 'Ally1DamageReceived',
                                 'Ally1TimeAlive', 'Ally1League', 'Ally1Ping', 'Ally1HealthShards', 'Ally1EnergyShards', 'Ally1NumEnergyUsed',
                                 'Ally1HighestScore', 'Ally2ChampionPlayed', 'Ally2TotalPlayed', 'Ally2TotalScore', 'Ally2DamageReceived',
                                 'Ally2TimeAlive', 'Ally2League', 'Ally2Ping', 'Ally2HealthShards', 'Ally2EnergyShards', 'Ally2NumEnergyUsed',
                                 'Ally2HighestScore', 'Enemy1ChampionPlayed', 'Enemy1TotalPlayed', 'Enemy1TotalScore', 'Enemy1DamageReceived',
                                 'Enemy1TimeAlive', 'Enemy1League', 'Enemy1Ping', 'Enemy1NumEnergyUsed', 'Enemy1HighestScore', 
                                 'Enemy2ChampionPlayed', 'Enemy2TotalPlayed', 'Enemy2TotalScore', 'Enemy2DamageReceived',
                                 'Enemy2TimeAlive', 'Enemy2League', 'Enemy2Ping', 'Enemy2NumEnergyUsed', 'Enemy2HighestScore',
                                 'Enemy3ChampionPlayed', 'Enemy3TotalPlayed', 'Enemy3TotalScore', 'Enemy3DamageReceived',
                                 'Enemy3TimeAlive', 'Enemy3League', 'Enemy2Ping', 'Enemy3NumEnergyUsed', 'Enemy3HighestScore',
                                 'Map', 'Role', 'Champion', 'Avatar',  'Title', 'Outfit Rarity', 'Outfit',
                                 'Attachment Rarity', 'Attachment', 'Pose Rarity', 'Pose', 'Mount Rarity', 'Mount', 'Battlerite 1',
                                 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5', 'Ally1Battlerite1',
                                 'Ally1Battlerite2', 'Ally1Battlerite3', 'Ally1Battlerite4', 'Ally1Battlerite5', 'Ally2Battlerite1',
                                 'Ally2Battlerite2', 'Ally2Battlerite3', 'Ally2Battlerite4', 'Ally2Battlerite5', 
                                 'Enemy1Battlerite1', 'Enemy1Battlerite2', 'Enemy1Battlerite3', 'Enemy1Battlerite4', 'Enemy1Battlerite5',
                                 'Enemy2Battlerite1', 'Enemy2Battlerite2', 'Enemy2Battlerite3', 'Enemy2Battlerite4', 'Enemy2Battlerite5',
                                 'Enemy3Battlerite1', 'Enemy3Battlerite2', 'Enemy3Battlerite3', 'Enemy3Battlerite4', 'Enemy3Battlerite5',
                                 'Ally1Role', 'Ally1Champion', 'Ally2Role', 'Ally2Champion', 'Enemy1Role', 'Enemy1Champion',
                                 'Enemy2Role', 'Enemy2Champion', 'Enemy3Role', 'Enemy3Champion', 'Num_Ultimates', 'Unused_Energy',
                                 'Enemy_Total_HealthShards', 'Enemy_Total_EnergyShards', 'Solo_Queue', 'Battlerites',
                                 'Ally1Battlerites', 'Ally2Battlerites', 'Enemy1Battlerites', 'Enemy2Battlerites', 'Enemy3Battlerites',
                                 'Team_Comp', 'Enemy_Comp', 'Team_Roles', 'Enemy_Roles', 'League']

                AllGames.columns = FinalColNames

                #Add longitude/latitude coordinates    
                AllGames = pd.merge(AllGames, Regions, how = 'left', on = 'Region')

                #Organize final output better to include Ally/Enemy information after everything else
                AllGames = AllGames[['Game_ID', 'Patch_Version', 'Date', 'Map', 'Match_Type', 'Ranking_Type', 'Server_Type', 'Solo_Queue', 'Season_ID', 
                                    'Region', 'Limited_MM',
                                    'Player_Type', 'Name', 'User_ID', 'Champion', 'Role', 'Forfeited', 
                                    'Group', 'User_Leave_Type', 'Left_During_Round', 'AFK_Detection',
                                    'League', 'Division', 'Division_Rating', 'Total_Time_Played', 'Account_Level', 'Champion_Level', 'Champion_Time_Played', 
                                    'Round', 'Kills', 'Deaths', 'Assists',
                                    'Damage', 'Damage_Received', 'Protection', 'Protection_Received', 'Control', 'Control_Received',
                                    'Energy_Used', 'Energy_Gained', 'Abilities_Used', 'Num_Ultimates', 'Unused_Energy', 'Num_Energy_Used',
                                    'Orb_Kills', 'First_Orb', 'MVP', 'Round_Length', 'Round_Won',
                                    'Total_Score', 'Highest_Score', 'Health_Shards', 'Energy_Shards',  
                                    'Time_Alive', 'Premade', 'Queue_Time', 'Ping', 'Avatar',  'Title', 'Outfit Rarity', 'Outfit',
                                    'Attachment Rarity', 'Attachment', 'Pose Rarity', 'Pose', 'Mount Rarity', 'Mount', 
                                    'Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5', 'Battlerites',
                                    'Team_Comp', 'Team_Roles',
                                    'Ally1Role', 'Ally1Champion',
                                    'Ally1ChampionPlayed', 'Ally1TotalPlayed', 'Ally1TotalScore', 'Ally1DamageReceived',
                                    'Ally1TimeAlive', 'Ally1League', 'Ally1Ping', 'Ally1HealthShards', 'Ally1EnergyShards', 'Ally1NumEnergyUsed',
                                    'Ally1HighestScore',
                                    'Ally1Battlerite1',
                                    'Ally1Battlerite2', 'Ally1Battlerite3', 'Ally1Battlerite4', 'Ally1Battlerite5', 'Ally1Battlerites',
                                    'Ally2Role', 'Ally2Champion',
                                    'Ally2ChampionPlayed', 'Ally2TotalPlayed', 'Ally2TotalScore', 'Ally2DamageReceived',
                                    'Ally2TimeAlive', 'Ally2League', 'Ally2Ping', 'Ally2HealthShards', 'Ally2EnergyShards', 'Ally2NumEnergyUsed',
                                    'Ally2HighestScore',
                                    'Ally2Battlerite1', 'Ally2Battlerite2', 'Ally2Battlerite3', 'Ally2Battlerite4', 'Ally2Battlerite5', 'Ally2Battlerites',
                                    'Enemy_Comp', 'Enemy_Roles', 'Enemy_Total_HealthShards', 'Enemy_Total_EnergyShards',
                                    'Enemy1Role', 'Enemy1Champion',
                                    'Enemy1ChampionPlayed', 'Enemy1TotalPlayed', 'Enemy1TotalScore', 'Enemy1DamageReceived',
                                    'Enemy1TimeAlive', 'Enemy1League', 'Enemy1Ping', 'Enemy1NumEnergyUsed', 'Enemy1HighestScore',
                                    'Enemy1Battlerite1', 'Enemy1Battlerite2', 'Enemy1Battlerite3', 'Enemy1Battlerite4', 'Enemy1Battlerite5', 'Enemy1Battlerites',
                                    'Enemy2Role', 'Enemy2Champion',
                                    'Enemy2ChampionPlayed', 'Enemy2TotalPlayed', 'Enemy2TotalScore', 'Enemy2DamageReceived',
                                    'Enemy2TimeAlive', 'Enemy2League', 'Enemy2Ping', 'Enemy2NumEnergyUsed', 'Enemy2HighestScore',
                                    'Enemy3Role', 'Enemy3Champion',
                                    'Enemy3ChampionPlayed', 'Enemy3TotalPlayed', 'Enemy3TotalScore', 'Enemy3DamageReceived',
                                    'Enemy3TimeAlive', 'Enemy3League', 'Enemy2Ping', 'Enemy3NumEnergyUsed', 'Enemy3HighestScore',
                                    'Enemy2Battlerite1', 'Enemy2Battlerite2', 'Enemy2Battlerite3', 'Enemy2Battlerite4', 'Enemy2Battlerite5', 'Enemy2Battlerites',
                                    'Enemy3Battlerite1', 'Enemy3Battlerite2', 'Enemy3Battlerite3', 'Enemy3Battlerite4', 'Enemy3Battlerite5', 'Enemy3Battlerites']]

                #Repeat above steps for LeaverDf when applicable    
                #Convert code to text for LeaverDf
                LeaverDf = pd.merge(LeaverDf, MapIDs, left_on = 'mapID', right_on = 'Map ID', how = 'left')
                LeaverDf = LeaverDf.drop(['mapID', 'Map ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Value':'Map'})

                LeaverDf = pd.merge(LeaverDf, Champions, left_on = 'character', right_on = 'Champion ID', how = 'left')
                LeaverDf = LeaverDf.drop(['character', 'Champion ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Value':'Champion'})

                LeaverDf = pd.merge(LeaverDf, Avatars, left_on = 'avatar_id', right_on = 'Avatar ID', how = 'left')
                LeaverDf = LeaverDf.drop(['avatar_id', 'Avatar ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Value':'Avatar'})

                LeaverDf = pd.merge(LeaverDf, Titles, left_on = 'title_id', right_on = 'Title ID', how = 'left')
                LeaverDf = LeaverDf.drop(['title_id', 'Title ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Value':'Title'})

                LeaverDf = pd.merge(LeaverDf, Outfits, left_on = 'outfit_id', right_on = 'Outfit ID', how = 'left')
                LeaverDf = LeaverDf.drop(['outfit_id', 'Outfit ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Rarity':'Outfit Rarity', 'Value':'Outfit'})

                LeaverDf = pd.merge(LeaverDf, Attachments, left_on = 'attachment_id', right_on = 'Attachment ID', how = 'left')
                LeaverDf = LeaverDf.drop(['attachment_id', 'Attachment ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Rarity':'Attachment Rarity', 'Value':'Attachment'})

                LeaverDf = pd.merge(LeaverDf, Poses, left_on = 'victory_pose_id', right_on = 'Pose ID', how = 'left')
                LeaverDf = LeaverDf.drop(['victory_pose_id', 'Pose ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Rarity':'Pose Rarity', 'Value':'Pose'})

                LeaverDf = pd.merge(LeaverDf, Mounts, left_on = 'mount_id', right_on = 'Mount ID', how = 'left')
                LeaverDf = LeaverDf.drop(['mount_id', 'Mount ID'], axis = 1)
                LeaverDf = LeaverDf.rename(columns = {'Rarity':'Mount Rarity', 'Value':'Mount'})

                BrCols = ['br1', 'br2', 'br3', 'br4', 'br5']

                for i in range(1, 6):

                    LeaverDf = pd.merge(LeaverDf, Battlerites, how = 'left', left_on = BrCols[i-1], right_on = 'Battlerite ID')
                    LeaverDf = LeaverDf.drop([BrCols[i-1], 'Battlerite ID'], axis = 1)
                    LeaverDf = LeaverDf.rename(columns = {'Value':'Battlerite ' + str(i)})

                battlerites = []

                for index, row in LeaverDf.iterrows():

                    #Add set of battlerites together
                    if pd.notnull(row['Battlerite 1']):
                        battlerites.append(row['Battlerite 1'] + ', ' + row['Battlerite 2'] + ', ' + row['Battlerite 3'] + ', ' + \
                                           row['Battlerite 4'] + ', ' + row['Battlerite 5'])
                    else:
                        battlerites.append('None')

                    #Display actual round number
                    LeaverDf.loc[index, 'Round'] = row['Round'] + 1

                LeaverDf['Battlerites'] = battlerites

                #Convert League numbers to names
                LeaverDf['League'] = LeaverDf['league_before'].apply(recode_league)

                #Remove useless columns
                LeaverDf = LeaverDf.drop(['score_to_win', 'match_team', 'league_before'], axis = 1)

                FinalColNames = ['Game_ID', 'Patch_Version', 'Date', 'Match_Type', 'Ranking_Type', 'Server_Type', 'Season_ID',
                                 'Region', 'Player_Type', 'User_ID', 'Name', 'Forfeited', 'Group', 'User_Leave_Type',
                                 'Left_During_Round', 'AFK_Detection', 'Division', 'Division_Rating', 'Account_Level',
                                 'Champion_Level', 'Round', 'Kills', 'Deaths', 'Assists', 'Damage', 'Damage_Received',
                                 'Protection', 'Protection_Received', 'Control', 'Control_Received', 'Energy_Used',
                                 'Energy_Gained', 'Abilities_Used', 'Orb_Kills', 'MVP', 'Round_Length', 'Round_Won',
                                 'URL', 'Map', 'Role', 'Champion', 'Avatar',  'Title', 'Outfit Rarity',
                                 'Outfit', 'Attachment Rarity', 'Attachment', 'Pose Rarity', 'Pose', 'Mount Rarity', 'Mount',
                                 'Battlerite 1', 'Battlerite 2', 'Battlerite 3', 'Battlerite 4', 'Battlerite 5', 'Battlerites',
                                 'League']
                LeaverDf.columns = FinalColNames

                #Add longitude/latitude coordinates
                LeaverDf = pd.merge(LeaverDf, Regions, how = 'left', on = 'Region')
                
                
                
                output_date_format = '{}-{}-{}'.format(starting_month, starting_day, starting_year)
                AllGames = AllGames.reset_index(drop = True)
                LeaverDf = LeaverDf.reset_index(drop = True)

                #Check if directory exists, if not create
                data = 'Data'
                if not os.path.exists(data):
                    os.mkdir(data)
                if not os.path.exists(os.path.join(data, starting_year)):
                    os.mkdir(os.path.join(data, starting_year))
                if not os.path.exists(os.path.join(data, starting_year, starting_month_full)):
                    os.mkdir(os.path.join(data, starting_year, starting_month_full))
                if not os.path.exists(os.path.join(data, starting_year, starting_month_full, starting_day)):
                    os.mkdir(os.path.join(data, starting_year, starting_month_full, starting_day))
                else:
                    #Remove data for day if already data in case of errors or re-run
                    shutil.rmtree(os.path.join(data, starting_year, starting_month_full, starting_day))
                    os.mkdir(os.path.join(data, starting_year, starting_month_full, starting_day))


                #Split overall df into multiple parts to accomodate for GitHub file size limit

                #Roughly every stepsize rows + include a few more to finish current game ID
                lower_bound = 0
                initial_index = stepsize
                current_index = stepsize

                for step in range(1, (m.floor(len(AllGames)/stepsize)+1)):

                    last_gameID = AllGames.loc[initial_index, 'Game ID']

                    #Check the next 30 rows. Max if last row is one player for the first round + 5 then 6*4 rounds = 29
                    for i in range(30):
                        current_index += 1
                        if AllGames.loc[current_index, 'Game ID'] != last_gameID:
                            break

                    upper_bound = current_index - 1

                    #Slice AllGames
                    SliceDf = AllGames.loc[lower_bound:upper_bound, :]

                    #Output csv file to previously created directory
                    filename = output_date_format + '_' + str(step) + '.csv'
                    SliceDf.to_csv(os.path.join(data, starting_year, starting_month_full, starting_day, filename), index = False)

                    #Set new values for next loop
                    lower_bound = current_index
                    initial_index = lower_bound + stepsize
                    current_index += stepsize

                #Output last left over slice
                filename = output_date_format + '_' + str(step+1) + '.csv'

                AllGames.loc[lower_bound:, :].to_csv(os.path.join(data, starting_year, starting_month_full, 
                                                                  starting_day, filename), index = False)

                #Output LeaverDf
                filename = 'Excluded_' + output_date_format + '.csv'
                LeaverDf.to_csv(os.path.join(data, starting_year, starting_month_full, starting_day,
                                            filename), index = False)

                #Set new date values for next day
                starting_month = last_datetime_clean.strftime('%b')
                starting_month_full = last_datetime_clean.strftime('%B')
                starting_day = last_datetime_clean.strftime('%d')
                starting_year = last_datetime_clean.strftime('%Y')

                #Reset DataFrame for next game stored
                created_df = False
                created_leaver_df = False
                total_days_covered += 1

                if total_days_covered == days_to_cover:
                    break
        
        
        return (AllGames, LeaverDf, current_datetime, json_data, round_info, url)
    
    except:
        
        #Output on except to determine root of error
        print('SCRIPT STOPPED ERROR')
        AllGames.to_csv()
        return (AllGames, LeaverDf, current_datetime, json_data, round_info, url)
    

    
AllGames, LeaverDf, date, json_data, round_info, url = get_data(header, current_datetime, stepsize = 14000, days_to_cover = 2, Attachments,
                                                                Avatars, Battlerites, Champions, MapIDs, Mounts, Outfits, Poses, Regions,
                                                                Titles)
        
    