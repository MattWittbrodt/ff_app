select p.full_name, 
	   wk.opp,
       --  Def Performance against QB
       (t1.rushing_att + t1.receiving_tgt)/t1.g as total_touches,
       t1.fantasy_per_gm_fdpt,
       t1.rushing_att/t1.g as rush_att,
       t1.rushing_yds/t1.g as rush_yds,
       round(t1.rushing_td::numeric/t1.g::numeric,1) as rush_tds,
       round(t1.receiving_tgt::numeric/t1.g::numeric,1) as targets,
       t1.receiving_yds/t1.g as receiving_yards,
       -- DVOA Metrics
       t2.total_dvoa,
       t2.rush_dvoa,
       round(t2.rush_dvoa::numeric + t3.rush_dvoa::numeric,1) as dvoa_advantage,
       round(t2.rush_dvoa::numeric - t2.total_dvoa::numeric,1) as dvoa_difference,
       -- D Line
       t4.run_powersuccess,
       t5.run_powersuccess - t4.run_powersuccess as power_success_diff,
       round(t4.run_adj_lineyards::numeric - t5.run_adj_lineyards::numeric,1) as dline_net_adj_line_yards, 
       round((t4.run_adj_lineyards::numeric - t5.run_adj_lineyards::numeric) - (t5.run_adj_lineyards::numeric - t4.run_adj_lineyards::numeric),2) as net_adj_line_yd_diff_vs_off
from weekly_games wk
join players p
	on wk.player_id = p.player_id
left join pfr_fantasy_pts_against_rb t1
	on wk.opp_id = t1.tm
left join fo_team_defense t2
	on wk.opp_id = t2.team
left join fo_team_offense t3
	on wk.team_id = t3.team
left join fo_dline t4
	on wk.opp_id = t4.team
left join fo_oline t5
	on wk.team_id = t5.team
where p.position_id = 2;