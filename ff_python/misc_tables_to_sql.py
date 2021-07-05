#%%
import pandas as pd
import pickle
import re

def no_names(col):
    for ii in col:
        try:
            player_dict[ii]
        except:
            print(f'{ii}')

#%% Create pckl dictionary
pl = pd.read_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/player_table_2020.csv')

pl.set_index('full_name', inplace=True)
pl = pl[['player_id']]
pl_dict = pl['player_id'].to_dict()

#%% Write to dictionary
with open('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/player_dict_2020.pkl', 'wb') as f:
    pickle.dump(pl_dict,f)

#%% 
player_file = open('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/player_dict_2020.pkl', 'rb')
player_dict = pickle.load(player_file)

#%%
d = pd.read_excel('C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to17_combined.xlsx')
d.rename(columns = {'proj_player':'player_id', 'proj_week':'week'}, inplace = True)
#%% ######################
#### WR Matchup
##########################
d = d[['proj_player','proj_week','vs_cb_pos', 'vs_cb_tar', 'vs_cb_c', 'vs_cb_ypr', 'vs_cb_fpt' ,'vs_cb_shad', 'vs_cb_matchup']]
d.dropna(inplace= True)

# %%
c = pd.merge(d,players,left_on = ['proj_player'], right_on = ['full_name'])
c['season'] = 2020

# Reordering columns
id = c['player_id']
c.drop(['full_name','proj_player', 'player_id'], axis = 1, inplace = True)
c.insert(loc=0, column='player_id', value=id)

# %% Write out to csv
c.to_csv('C:/Users/mattw/Desktop/2020_wr_matchup.csv', index=False, header=True)

#%% ######################
#### Red Zone Passing
##########################
# These are cumulative stats, so just using the last week's data
rz_pass = d[['proj_player', 'proj_week','passing_twenty_comp','passing_twenty_att','passing_twenty_comp_per','passing_twenty_yds',
        'passing_twenty_td','passing_twenty_int','passing_ten_comp','passing_ten_att','passing_ten_comp_per',
        'passing_ten_yds','passing_ten_td','passing_ten_int']]
rz_pass.dropna(inplace= True)

# Converting to player_id
rz_pass['proj_player'] = rz_pass.apply(lambda row: player_dict[row.proj_player], axis = 1)
rz_pass['season'] = 2020

# Renaming columns
rz_pass.rename(columns = lambda x: re.sub("passing_", "", x), inplace = True)

# Write to csv
rz_pass.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_rz_passing.csv', index=False, header=True)

#%% ######################
#### Red Zone Rushing
##########################
# These are cumulative stats, so just using the last week's data
rz_players = d[['player_id', 'week']]
rz_rush = d.loc[:,d.columns.str.startswith('rushing_')]
rz_rush = pd.concat([rz_players, rz_rush], axis = 1)
rz_rush.dropna(inplace= True)

# Converting to player_id
rz_rush['player_id'] = rz_rush.apply(lambda row: player_dict[row.player_id], axis = 1)
rz_rush['season'] = 2020

# Renaming columns
rz_rush.rename(columns = lambda x: re.sub("rushing_", "", x), inplace = True)

# Write to csv
rz_rush.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_rz_rushing.csv', index=False, header=True)
# %%
#%% ######################
#### Red Zone Receiving
##########################
# These are cumulative stats, so just using the last week's data
rz_players = d[['player_id', 'week']]
rz_rec = d.loc[:,d.columns.str.startswith('receiving_')]
rz_rec = pd.concat([rz_players, rz_rec], axis = 1)
rz_rec.dropna(inplace= True)

# Converting to player_id
rz_rec['player_id'] = rz_rec.apply(lambda row: player_dict[row.player_id], axis = 1)
rz_rec['season'] = 2020

# Renaming columns
rz_rec.rename(columns = lambda x: re.sub("receiving_", "", x), inplace = True)

# Write to csv
rz_rec.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_rz_receiving.csv', index=False, header=True)
# %%
