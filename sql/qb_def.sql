select p.full_name, 
	   wk.opp,
       round(t1.att::numeric/t1.g::numeric,2) as att,
       round(t1.yds::numeric/t1.g::numeric,2) as yds,
       round(t1.td::numeric/t1.g::numeric,1) as td,
       t2.fantasy_per_gm_fdpt as fd_pts,
       -- RZ Defense
	   t3.rztd as rz_td,
       t3.rzpct as rz_td_pct,
       -- DVOA
	   t4.total_dvoa,
       t4.pass_dvoa,
       round(total_dvoa::numeric - pass_dvoa::numeric,2) as difference,
       t5.pass_rank,
       t5.pass_adjustedsack_rate as adj_sack_rate,
       t6.rate,
       t6.ny_per_a as net_yds_per_att,
       t7.pass_adjustedsack_rate as oline_adjsack_rate,
	   wk.week, wk.season
from weekly_games wk
join players p on wk.player_id = p.player_id
join pfr_pass_def t1 on wk.opp_id = t1.tm and wk.week = t1.week and wk.season = t1.season
join pfr_fantasy_pts_against_qb t2 on wk.opp_id = t2.tm and wk.week = t2.week and wk.season = t2.season
join pfr_conv_def t3 on wk.opp_id = t3.tm and wk.week = t3.week and wk.season = t3.season
join fo_team_defense t4 on wk.opp_id = t4.team and wk.week = t4.week and wk.season = t4.season
join fo_dline t5 on wk.opp_id = t5.team and wk.week = t5.week and wk.season = t5.season
join pfr_pass_def t6 on wk.opp_id = t6.tm and wk.week = t6.week and wk.season = t6.season
join fo_oline t7 on wk.team_id = t7.team and wk.week = t7.week and wk.season = t7.season
where p.position_id = 1;