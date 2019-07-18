import requests
import pandas as pd
import json
import datetime
import time
import os

header = {
  "Authorization": "YOUR KEY",
  "Accept": "application/vnd.api+json"
}

overall_urls = []
resetlink = 0
current_datetime = '2019-07-10T09:00:00Z'
link = "https://api.developer.battlerite.com/shards/global/matches"
current_datetime_clean = datetime.datetime.strptime(current_datetime, '%Y-%m-%dT%H:%M:%SZ')
starting_day = current_datetime_clean.strftime('%d')
starting_month = current_datetime_clean.strftime('%b')
starting_month_full = current_datetime_clean.strftime('%B')
starting_year = current_datetime_clean.strftime('%Y')

#Battlerite Asset files for current patch (Need to re-run on new asset files for new patch on whenever
#patch version changes when gathering data) located at
#https://github.com/StunlockStudios/battlerite-assets/tree/master/mappings

gameplay_json = json.loads(requests.get('https://raw.githubusercontent.com/StunlockStudios/battlerite-assets/master/mappings/67104/gameplay.json').text)

vanity_json = json.loads(requests.get('https://raw.githubusercontent.com/StunlockStudios/battlerite-assets/master/mappings/67104/AccountVanity.json').text)
#English.txt is the same as English.ini except in .txt extension

with open(os.path.join('Reference Files','English.txt')) as f:
    content = f.readlines()
content = [x.strip() for x in content] 

#Create df of conversion from Lookup ID to the actual value
codes = []
names = []
for line in content:
    code_names = line.split('=')
    codes.append(code_names[0].upper())
    names.append(code_names[1].upper())
    
CodeDf = pd.DataFrame(list(zip(codes, names)), columns = ['Code', 'Value'])

'''Create Map ID Crosswalk to convert to english name'''

final_mapid = []
final_mapid2 = []


for query_number in range(100000):

    uselink = "{}".format(link)

    query = {
      "sort": "createdAt",
      "filter[createdAt-start]": "{}".format(current_datetime),
      "page[limit]": "5"}

    if query_number > 0:

        difference_runtime = end_runtime - start_runtime

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
    link = json_data['links']['next']
    resetlink += 1

    urls = []

    #Get urls from list of dictionaries (not every dictionary has url, no structure)

    for i in range(len(included)):

        try: 

            urls.append(included[i]['attributes']['URL'])

        except:

            continue


   #Get Map ID and game ID

    mapID = []
    gameID = []
    mapID2 = []
    match_gameID = []

    for game in range(len(data)):

        GameType = data[game]['attributes']['stats']['game']

        if GameType == 'Royale':

            continue

        mapID.append(data[game]['attributes']['stats']['mapID'])
        gameID.append(data[game]['id'])

    gamesDf = pd.DataFrame(list(zip(mapID, gameID)),
                          columns = ['mapID', 'gameID'])

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

        match_gameID.append(round_info['dataObject']['match_id'])
        mapID2.append(round_info['dataObject']['map_id'])

    Game_MapDf = pd.DataFrame(list(zip(match_gameID, mapID2)), columns = ['gameID', 'mapID2'])
    Game_MapDf = pd.merge(Game_MapDf, gamesDf, on = 'gameID', how = 'inner')

    for index, row in Game_MapDf.iterrows():
        if row['mapID'] not in final_mapid:
            final_mapid.append(row['mapID'])
            final_mapid2.append(row['mapID2'])
      
    #There are only 18 different maps as of current
    if len(final_mapid) == 18:
        break

    end_runtime = datetime.datetime.now()

#Create df of Conversion of Map ID to Lookup Map ID
MapDf = pd.DataFrame(list(zip(final_mapid, final_mapid2)), columns = ['Map ID', 'Lookup Map ID'])

#Create df of conversion from Lookup Map ID to Lookup Map ID 2

lookupID = []
lookupID2 = []

map_list = gameplay_json['maps']

for i in range(len(map_list)):
    lookupID.append(map_list[i]['assetID'].upper())
    lookupID2.append(map_list[i]['name'].upper())
    
lookupdf = pd.DataFrame(list(zip(lookupID, lookupID2)), columns = ['Lookup Map ID', 'Lookup Map ID2'])
lookupdf = pd.merge(MapDf, lookupdf, how = 'inner', on = 'Lookup Map ID')

#Create final crosswalk of Map ID to actual map name
FinalMaps = pd.merge(lookupdf, CodeDf, how = 'left', left_on = 'Lookup Map ID2', right_on = 'Code')

FinalMaps.to_csv(os.path.join('Reference Files', 'MapID_Crosswalk_Full.csv'), index = False)

#Only need MapID and name for practical purposes
FinalMaps = FinalMaps.drop(['Lookup Map ID', 'Lookup Map ID2', 'Code'], axis = 1)

FinalMaps.to_csv(os.path.join('Reference Files', 'MapID_Crosswalk.csv'), index = False)

'''Get Champion Crosswalk'''

champid = []
lookupid = []
role = []
icon = []

champions = gameplay_json['characters']

for i in range(len(champions)):
    champid.append(champions[i]['typeID'])
    lookupid.append(champions[i]['name'].upper())
    role.append(champions[i]['archetype'].upper())
    icon.append(champions[i]['icon'])
    
ChampDf = pd.DataFrame(list(zip(champid, lookupid, role, icon)), 
                       columns = ['Champion ID', 'Champion Lookup ID', 'Role', 'Icon File Name'])

FinalChamps = pd.merge(ChampDf, CodeDf, how = 'left', left_on = 'Champion Lookup ID', right_on = 'Code')
FinalChamps.to_csv(os.path.join('Reference Files','Champion_Crosswalk_Full.csv'), index = False)

FinalChamps = FinalChamps.drop(['Champion Lookup ID', 'Code'], axis = 1)
FinalChamps.to_csv(os.path.join('Reference Files','Champion_Crosswalk.csv'), index = False)
    
'''Get Title crosswalk'''

titleid = []
value = []

titles = vanity_json['Titles']

for i in range(len(titles)):
    titleid.append(titles[i]['StackableID'])
    value.append(titles[i]['Text'])
    
TitleDf = pd.DataFrame(list(zip(titleid, value)), columns = ['Title ID', 'Value'])
TitleDf.to_csv(os.path.join('Reference Files', 'Title_Crosswalk.csv'), index = False)

'''Get Avatar Crosswalk'''
#imageID will be the file name of the image located at:
#https://github.com/StunlockStudios/battlerite-assets/tree/master/mappings/assets

avatarid = []
value = []
imageid = []

avatars = vanity_json['Pictures']

for i in range(len(avatars)):
    avatarid.append(avatars[i]['StackableID'])
    value.append(avatars[i]['File'])
    imageid.append(avatars[i]['Hash'])
    
AvatarDf = pd.DataFrame(list(zip(avatarid, value, imageid)), columns = ['Avatar ID', 'Value', 'Avatar File Name'])
AvatarDf.to_csv(os.path.join('Reference Files', 'Avatar_Crosswalk.csv'), index = False)

'''Get Outfit Crosswalk'''

outfitid = []
lookupid = []
rarity = []

outfits = gameplay_json['outfits']

for i in range(len(outfits)):
    outfitid.append(outfits[i]['typeID'])
    lookupid.append(outfits[i]['name'].upper())
    rarity.append(outfits[i]['dropRarity'])
    
OutfitDf = pd.DataFrame(list(zip(outfitid, lookupid, rarity)), columns = ['Outfit ID', 'Outfit Lookup ID', 'Rarity'])

FinalOutfit = pd.merge(OutfitDf, CodeDf, how = 'left', left_on = 'Outfit Lookup ID', right_on = 'Code')

FinalOutfit.to_csv(os.path.join('Reference Files', 'Outfit_Crosswalk_Full.csv'), index = False)

FinalOutfit = FinalOutfit.drop(['Outfit Lookup ID', 'Code'], axis = 1)
FinalOutfit.to_csv(os.path.join('Reference Files', 'Outfit_Crosswalk.csv'), index = False)

'''Get Attachment Crosswalk'''

attachmentid = []
lookupid = []
rarity = []

attachments = gameplay_json['attachments']

for i in range(len(attachments)):
    attachmentid.append(attachments[i]['typeID'])
    lookupid.append(attachments[i]['name'].upper())
    rarity.append(attachments[i]['dropRarity'])
    
AttachmentDf = pd.DataFrame(list(zip(attachmentid, lookupid, rarity)), columns = ['Attachment ID', 'Attachment Lookup ID', 'Rarity'])

FinalAttachment = pd.merge(AttachmentDf, CodeDf, how = 'left', left_on = 'Attachment Lookup ID', right_on = 'Code')
FinalAttachment.to_csv(os.path.join('Reference Files', 'Attachment_Crosswalk_Full.csv'), index = False)

FinalAttachment = FinalAttachment.drop(['Attachment Lookup ID', 'Code'], axis = 1)
FinalAttachment.to_csv(os.path.join('Reference Files', 'Attachment_Crosswalk.csv'), index = False)

'''Get Pose Crosswalk'''

poseid = []
lookupid = []
rarity = []

poses = gameplay_json['victoryPoses']

for i in range(len(poses)):
    poseid.append(poses[i]['typeID'])
    lookupid.append(poses[i]['name'].upper())
    rarity.append(poses[i]['dropRarity'])
    
PoseDf = pd.DataFrame(list(zip(poseid, lookupid, rarity)), columns = ['Pose ID', 'Pose Lookup ID', 'Rarity'])

FinalPose = pd.merge(PoseDf, CodeDf, how = 'left', left_on = 'Pose Lookup ID', right_on = 'Code')
FinalPose.to_csv(os.path.join('Reference Files', 'Pose_Crosswalk_Full.csv'), index = False)

FinalPose = FinalPose.drop(['Pose Lookup ID', 'Code'], axis = 1)
FinalPose.to_csv(os.path.join('Reference Files', 'Pose_Crosswalk.csv'), index = False)

'''Get Mount Crosswalk'''

mountid = []
lookupid = []
rarity = []

mounts = gameplay_json['mounts']

for i in range(len(mounts)):
    mountid.append(mounts[i]['typeID'])
    lookupid.append(mounts[i]['name'].upper())
    rarity.append(mounts[i]['dropRarity'])
    
MountDf = pd.DataFrame(list(zip(mountid, lookupid, rarity)), columns = ['Mount ID', 'Mount Lookup ID', 'Rarity'])

FinalMount = pd.merge(MountDf, CodeDf, how = 'left', left_on = 'Mount Lookup ID', right_on = 'Code')
FinalMount.to_csv(os.path.join('Reference Files', 'Mount_Crosswalk_Full.csv'), index = False)

FinalMount = FinalMount.drop(['Mount Lookup ID', 'Code'], axis = 1)
FinalMount.to_csv(os.path.join('Reference Files', 'Mount_Crosswalk.csv'), index = False)

'''Get Battlerites Crosswalk'''

champid = []
battleriteid = []
lookupid = []
battleritetype = []

champs = gameplay_json['characters']

for i in range(len(champs)):
    
    battlerites = champs[i]['battlerites']
    
    for j in range(len(battlerites)):
        
        champid.append(champs[i]['typeID'])
        battleriteid.append(battlerites[j]['typeID'])
        lookupid.append(battlerites[j]['name'].upper())
        battleritetype.append(battlerites[j]['type'])
        
BattleriteDf = pd.DataFrame(list(zip(champid, battleriteid, lookupid, battleritetype)),
                           columns = ['Champion ID', 'Battlerite ID', 'Battlerite Lookup ID', 'Battlerite Type'])

FinalBattlerite = pd.merge(BattleriteDf, CodeDf, how = 'left', left_on = 'Battlerite Lookup ID', right_on = 'Code')
FinalBattlerite.to_csv(os.path.join('Reference Files', 'Battlerite_Crosswalk_Full.csv'), index = False)

FinalBattlerite = FinalBattlerite.drop(['Champion ID', 'Battlerite Lookup ID', 'Code'], axis = 1)
FinalBattlerite.to_csv(os.path.join('Reference Files', 'Battlerite_Crosswalk.csv'), index = False)
        
'''Reference file for energy usage and names'''

champid = []
exid = []
lookupid = []

champs = gameplay_json['characters']

for i in range(len(champs)):
    
    exid.append(champs[i]['ability6']['typeID'])
    lookupid.append(champs[i]['ability6']['name'].upper())
    champid.append(champs[i]['typeID'])
    
    try: 
        exid.append(champs[i]['ability1']['EXAbility']['typeID'])
        lookupid.append(champs[i]['ability1']['EXAbility']['name'].upper())
        champid.append(champs[i]['typeID'])
        
    except:
        pass
    
    try: 
        exid.append(champs[i]['ability2']['EXAbility']['typeID'])
        lookupid.append(champs[i]['ability2']['EXAbility']['name'].upper())
        champid.append(champs[i]['typeID'])
        
    except:
        pass
    
    try: 
        exid.append(champs[i]['ability3']['EXAbility']['typeID'])
        lookupid.append(champs[i]['ability3']['EXAbility']['name'].upper())
        champid.append(champs[i]['typeID'])
        
    except:
        pass
    
    try: 
        exid.append(champs[i]['ability4']['EXAbility']['typeID'])
        lookupid.append(champs[i]['ability4']['EXAbility']['name'].upper())
        champid.append(champs[i]['typeID'])
        
    except:
        pass
    
    try: 
        exid.append(champs[i]['ability5']['EXAbility']['typeID'])
        lookupid.append(champs[i]['ability5']['EXAbility']['name'].upper())
        champid.append(champs[i]['typeID'])
        
    except:
        pass
    
EnergyAbilities = pd.DataFrame(list(zip(champid, exid, lookupid)),
                              columns = ['Champion ID', 'Ability ID', 'Lookup ID'])
#Get Champion name text
EnergyAbilities = pd.merge(EnergyAbilities, FinalChamps, on = 'Champion ID', how = 'left')
EnergyAbilities = EnergyAbilities.drop(['Role', 'Icon File Name', 'Champion ID'], axis = 1)
EnergyAbilities = EnergyAbilities.rename(columns = {'Value':'Champion'})

#Get Ability name text
FinalEnergyAbilities = pd.merge(EnergyAbilities, CodeDf, how = 'left', left_on = 'Lookup ID', right_on = 'Code')
FinalEnergyAbilities = FinalEnergyAbilities.rename(columns = {'Value':'Ability'})
FinalEnergyAbilities['Type'] = ['R', 'EX1', 'EX2']*28

FinalEnergyAbilities.to_csv(os.path.join('Reference Files', 'EnergyAbilities_Crosswalk_Full.csv'), index = False)

FinalEnergyAbilities = FinalEnergyAbilities.drop(['Code', 'Lookup ID'], axis = 1)

FinalEnergyAbilities.to_csv(os.path.join('Reference Files', 'EnergyAbilities_Crosswalk.csv'), index = False)
