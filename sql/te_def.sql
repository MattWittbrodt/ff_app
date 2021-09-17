select p.full_name, 
	   wk.opp,
       --  Points Against to TE
       t1.fantasy_per_gm_fdpt,
	   round((t1.receiving_tgt::numeric/t1.g::numeric),1) as tgt,
	   round((t1.receiving_yds::numeric/t1.g::numeric),1) as yds,
	   round(t1.receiving_td::numeric/t1.g::numeric,1) as td,
       -- RZ Defense
	   t2.rztd,
	   t2.rzpct,
	   -- Defense Efficiency
	   t3.any_per_a,
	   t3.y_per_g,
	   t4.tot_y_per_p,
	   -- DVOA
	   t5.total_dvoa,
	   t5.pass_dvoa,
       round(t5.pass_dvoa::numeric + t6.pass_dvoa::numeric,1) as dvoa_advantage,
	   round(t5.pass_dvoa::numeric - t5.total_dvoa::numeric,1) as dvoa_difference,
       -- D Line
       t7.pass_rank,
	   t7.pass_sacks,
	   t7.pass_adjustedsack_rate,
	   round(t8.pass_adjustedsack_rate::numeric - t7.pass_adjustedsack_rate::numeric,1) as sack_rate_diff,
	   wk.week,wk.season
from weekly_games wk
join players p on wk.player_id = p.player_id
join pfr_fantasy_pts_against_te t1 on wk.opp_id = t1.tm and wk.week = t1.week and wk.season = t1.season
join pfr_conv_def t2 on wk.opp_id = t2.tm and wk.week = t2.week and wk.season = t2.season
join pfr_pass_def t3 on wk.team_id = t3.tm and wk.week = t3.week and wk.season = t3.season
join pfr_team_def t4 on wk.opp_id = t4.tm and wk.week = t4.week and wk.season = t4.season
join fo_team_defense t5 on wk.opp_id = t5.team and wk.week = t5.week and wk.season = t5.season
join fo_team_offense t6 on wk.team_id = t6.team and wk.week = t6.week and wk.season = t6.season
join fo_dline t7 on wk.opp_id = t7.team and wk.week = t7.week and wk.season = t7.season
join fo_oline t8 on wk.team_id = t8.team and wk.week = t8.week and wk.season = t8.season
where p.position_id = 4;