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

#%% Load player dictionary
player_file = open('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/player_dict_2020.pkl', 'rb')
player_dict = pickle.load(player_file)

#%% Creating team dictionary
tm = pd.read_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/team_mysql.csv')

# Pro Football Reference Dict
pfr = tm[['pfr_abbreviation','team_id']]
pfr.set_index('pfr_abbreviation', inplace = True)
pfr = pfr['team_id'].to_dict()

#%% Load team dictionary


#%%
d = pd.read_excel('C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to17_combined.xlsx')
# Removing 'Jr' from names
d['proj_player'] = d.apply(lambda row: re.sub(" Jr", "", row.proj_player), axis = 1)

d.rename(columns = {'proj_player':'player_id', 'proj_week':'week'}, inplace = True)
players = d[['player_id', 'week']]
# ==============================================================================================
# ==============================================================================================
# ==============================================================================================

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
rz_pass = d.loc[:,d.columns.str.startswith('passing_')]
rz_pass = pd.concat([players, rz_pass], axis = 1)
rz_pass = rz_pass[rz_pass['passing_twenty_att'] >= 1]

# Converting to player_id
rz_pass['player_id'] = rz_pass.apply(lambda row: player_dict[row.player_id], axis = 1)
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
rz_rush = rz_rush[rz_rush['rushing_twenty_att'] >= 0]

# Converting to player_id
rz_rush['player_id'] = rz_rush.apply(lambda row: player_dict[row.player_id], axis = 1)
rz_rush['season'] = 2020

# Renaming columns
rz_rush.rename(columns = lambda x: re.sub("rushing_", "", x), inplace = True)

# Write to csv
rz_rush.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_rz_rushing.csv', index=False, header=True)

#%% ######################
#### Red Zone Receiving
##########################
# These are cumulative stats, so just using the last week's data
rz_players = d[['player_id', 'week']]
rz_rec = d.loc[:,d.columns.str.startswith('receiving_')]
rz_rec = pd.concat([rz_players, rz_rec], axis = 1)
rz_rec = rz_rec[rz_rec['receiving_twenty_tgt'] >= 0]

# Converting to player_id
rz_rec['player_id'] = rz_rec.apply(lambda row: player_dict[row.player_id], axis = 1)
rz_rec['season'] = 2020

# Renaming columns
rz_rec.rename(columns = lambda x: re.sub("receiving_", "", x), inplace = True)

# Write to csv
rz_rec.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_rz_receiving.csv', index=False, header=True)
#%% ######################
#### YTD Receiving Data
##########################

# These are cumulative stats, so just using the last week's data
ytd_rec = d.loc[:,d.columns.str.startswith('ytd_rec_')]
ytd_rec = pd.concat([players, ytd_rec], axis = 1)
ytd_rec = ytd_rec[ytd_rec['ytd_rec_target'] > 0]

# Converting to player_id
ytd_rec['player_id'] = ytd_rec.apply(lambda row: player_dict[row.player_id], axis = 1)
ytd_rec['season'] = 2020

# Renaming columns
ytd_rec.rename(columns = lambda x: re.sub("ytd_rec_", "", x), inplace = True)
ytd_rec.drop(['tm','pos'], axis = 1, inplace = True)

# Write to csv
ytd_rec.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_ytd_rec.csv', index=False, header=True)

#%% ######################
#### YTD Rushing Data
##########################

# These are cumulative stats, so just using the last week's data
ytd_rush = d.loc[:,d.columns.str.startswith('ytd_rush_')]
ytd_rush = pd.concat([players, ytd_rush], axis = 1)
ytd_rush = ytd_rush[ytd_rush['ytd_rush_att'] > 0]

# Converting to player_id
ytd_rush['player_id'] = ytd_rush.apply(lambda row: player_dict[row.player_id], axis = 1)
ytd_rush['season'] = 2020

# Renaming columns
ytd_rush.rename(columns = lambda x: re.sub("ytd_rush_", "", x), inplace = True)

# Write to csv
ytd_rush.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_ytd_rush.csv', index=False, header=True)

#%% ######################
#### YTD Passing Data
##########################

# These are cumulative stats, so just using the last week's data
ytd_pass = d.loc[:,d.columns.str.startswith('ytd_pass_')]
ytd_pass = pd.concat([players, ytd_pass], axis = 1)
ytd_pass = ytd_pass[ytd_pass['ytd_pass_comp_per'] > 0]

# Converting to player_id
ytd_pass['player_id'] = ytd_pass.apply(lambda row: player_dict[row.player_id], axis = 1)
ytd_pass['season'] = 2020

# Renaming columns
ytd_pass.rename(columns = lambda x: re.sub("ytd_pass_", "", x), inplace = True)
ytd_pass.drop('g', axis = 1, inplace = True)

# Write to csv
ytd_pass.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_ytd_pass.csv', index=False, header=True)

#%% ######################
#### FD Salary Information
##########################

# These are cumulative stats, so just using the last week's data
fd = d[['player_id', 'week', 'fd_sal', 'projected_own',
        'cash_odds','gpp_odds','implied_own','fd_lev']]
fd = fd[fd['fd_sal'] > 0]

# Removing fringe players
fringe = ['John Wolford','LaVante Bellamy','Mason Rudolph','Steven Mitchellell',
          'Scottie Phillips','Steven Mitchellell','Jonathan Ward','Isaiah Coulter',
          'Jonathan Williams','Keith Smith','Kendall Hinton','KJ Osborn']
fd = fd[~fd.player_id.isin(fringe)]

# Converting to player_id
fd['player_id'] = fd.apply(lambda row: player_dict[row.player_id], axis = 1)
fd['season'] = 2020

# Write to csv
fd.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_fd_info.csv', index=False, header=True)


#%% ######################
#### Total Team Defense (just using tables as presented by PFR; easiest to debug)
#### https://www.pro-football-reference.com/years/2020/opp.htm
##########################

# Getting each team's data
def_tot = d.loc[:,d.columns.str.startswith('def_tot_')]
opponent = d['proj_opp']
week = d['week']
def_tot = pd.concat([opponent, week, def_tot], axis = 1).drop_duplicates()
def_tot['season'] = 2020

# Converting to player_id
def_tot['proj_opp'] = def_tot.apply(lambda row: pfr[row.proj_opp], axis = 1)

# Renaming columns
def_tot.rename(columns = lambda x: re.sub("def_tot_", "", x), inplace = True)
def_tot.rename(columns = {'proj_opp':'team_id'}, inplace = True)

# Write to csv
def_tot.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_def_tot.csv', index=False, header=True)

#%% ######################
#### Team Passing Defense
#### https://www.pro-football-reference.com/years/2020/opp.htm
##########################

# Getting each team's data
def_pass = d.loc[:,d.columns.str.startswith('def_pass_')]
opponent = d['proj_opp']
week = d['week']
def_pass = pd.concat([opponent, week, def_pass], axis = 1).drop_duplicates()
def_pass['season'] = 2020

# Converting to player_id
def_pass['proj_opp'] = def_pass.apply(lambda row: pfr[row.proj_opp], axis = 1)

# Renaming columns
def_pass.rename(columns = lambda x: re.sub("def_pass_", "", x), inplace = True)
def_pass.rename(columns = {'proj_opp':'team_id'}, inplace = True)

# Write to csv
def_pass.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_def_pass.csv', index=False, header=True)

#%% ######################
#### Team Conversion Defense
#### https://www.pro-football-reference.com/years/2020/opp.htm
##########################

# Getting each team's data
def_conv = d.loc[:,['def_third_d_per','def_fourth_d_per','def_red_zone_td','def_red_zone_pct']]
opponent = d['proj_opp']
week = d['week']
def_conv = pd.concat([opponent, week, def_conv], axis = 1).drop_duplicates()
def_conv['season'] = 2020

# Converting to player_id
def_conv['proj_opp'] = def_conv.apply(lambda row: pfr[row.proj_opp], axis = 1)

# Renaming columns
def_conv.rename(columns = lambda x: re.sub("def_pass_", "", x), inplace = True)
def_conv.rename(columns = {'proj_opp':'team_id'}, inplace = True)

# Write to csv
def_conv.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_def_conv.csv', index=False, header=True)

#%% ########################
#### Previous Week Receiving
############################

# Getting each team's data
prev_wk_rec = d.loc[:,d.columns.str.startswith('prev_wk_rec_')]
player = d['player_id']
week = d['week']-1 # Correct for the data being for last week
prev_wk_rec = pd.concat([player, week, prev_wk_rec], axis = 1).drop_duplicates().dropna()
prev_wk_rec['season'] = 2020

# == Converting to player_id == #
prev_wk_rec['player_id'] = prev_wk_rec.apply(lambda row: player_dict[row.player_id], axis = 1)

# Renaming columns
prev_wk_rec.rename(columns = lambda x: re.sub("prev_wk_rec_", "", x), inplace = True)

# Write to csv
prev_wk_rec.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_wk_rec.csv', index=False, header=True)


#%% ########################
#### Previous Week Passing
############################

# Getting each team's data
prev_wk_pass = d.loc[:,d.columns.str.startswith('prev_wk_pass_')]
player = d['player_id']
week = d['week']-1 # Correct for the data being for last week
prev_wk_pass = pd.concat([player, week, prev_wk_pass], axis = 1).drop_duplicates().dropna().drop(['prev_wk_pass_yds.1'], axis = 1)
prev_wk_pass['season'] = 2020

# == Converting to player_id == #
prev_wk_pass['player_id'] = prev_wk_pass.apply(lambda row: player_dict[row.player_id], axis = 1)

# Renaming columns
prev_wk_pass.rename(columns = lambda x: re.sub("prev_wk_pass_", "", x), inplace = True)

# Write to csv
prev_wk_pass.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_wk_pass.csv', index=False, header=True)

# %%#%% ########################
#### Previous Week Rushing
############################

# Getting each team's data
prev_wk_rush = d.loc[:,d.columns.str.startswith('prev_wk_rush_')]
player = d['player_id']
week = d['week']-1 # Correct for the data being for last week
prev_wk_rush = pd.concat([player, week, prev_wk_rush], axis = 1).drop_duplicates().dropna()
prev_wk_rush['season'] = 2020

# == Converting to player_id == #
prev_wk_rush['player_id'] = prev_wk_rush.apply(lambda row: player_dict[row.player_id], axis = 1)

# Renaming columns
prev_wk_rush.rename(columns = lambda x: re.sub("prev_wk_rush_", "", x), inplace = True)

# Write to csv
prev_wk_rush.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_wk_rush.csv', index=False, header=True)

# %%
## ===== Proj stats ===== ##
# = Passing = #
proj_passing = d.loc[:,['player_id','week','proj_ffpts', 'proj_comp', 'proj_pass_att', 'proj_pass_yds', 'proj_pass_td', 'proj_int']]
proj_passing = proj_passing[proj_passing['proj_comp'] > 10]
proj_passing = proj_passing[proj_passing['player_id'] != 'Robert Griffin III']
proj_passing['player_id'] = proj_passing.apply(lambda row: player_dict[row.player_id], axis = 1)
proj_passing.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_fff_projected_passing.csv', index=False, header=True)

# = Rushing = #
proj_rush = d.loc[:,['player_id','week','proj_ffpts', 'proj_rush_att', 'proj_rush_yds', 'proj_rush_td']]
proj_rush = proj_rush[proj_rush['proj_ffpts'] > 5]
proj_rush = proj_rush[proj_rush['proj_rush_att'] > 5]
proj_rush = proj_rush[proj_rush['player_id'] != 'Robert Griffin III']
proj_rush = proj_rush[proj_rush['player_id'] != 'Kendall Hinton']
proj_rush = proj_rush[proj_rush['player_id'] != 'Ty Montgomery']
proj_rush['player_id'] = proj_rush.apply(lambda row: player_dict[row.player_id], axis = 1)
proj_rush.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_fff_projected_rushing.csv', index=False, header=True)

# = Receiving = #
proj_rec = d.loc[:,['player_id','week','proj_ffpts', 'proj_rec', 'proj_rec_yds', 'proj_rec_td']]
proj_rec = proj_rec[proj_rec['proj_ffpts'] > 5]
proj_rec = proj_rec[proj_rec['proj_rec'] > 2]
proj_rec = proj_rec[proj_rec['player_id'] != 'Ty Montgomery']
proj_rec = proj_rec[proj_rec['player_id'] != 'Rashaad Penny']
proj_rec['player_id'] = proj_rec.apply(lambda row: player_dict[row.player_id], axis = 1)
proj_rec.to_csv('C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/2020_fff_projected_receiving.csv', index=False, header=True)








# %%
