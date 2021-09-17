 SELECT p.full_name,
    wk.opp,
	-- Per Game Stats
    t1.yds_per_gm,
    t1.td,
	-- Advanced Stats
    round(t3.iay::numeric / t3.g::numeric,1) AS iay,
    round(t4.dyar::numeric / t3.g::numeric,1) AS pass_dyar,
    t5.ontgt_per,
    round((t4.eyds::numeric - t4.yards::numeric) / t3.g::numeric,1) AS eyd_diff,
    t5.bad_per AS bad_throw_per,
    t6.prss_per AS pressure_per,
	-- QB Rushing -- ended here
    t7.dyar as rush_dyar,
    round(t7.yards::numeric / t3.g::numeric,1) AS rush_yds_gm,
    round((t7.eyds::numeric - t7.yards::numeric) / t3.g::numeric,1) AS rush_eyd_diff,
    t8.inside20_att,
    t8.inside20_td,
    t9.fd_sal,
    wk.implied_total,
	wk.week, wk.season
   FROM weekly_games wk
     LEFT JOIN players p ON wk.player_id = p.player_id
     LEFT JOIN pfr_ytd_passing t1 ON wk.player_id = t1.player_id and wk.week = t1.week and wk.season = t1.season
     LEFT JOIN pfr_adv_passing_airyards t3 ON wk.player_id = t3.player and wk.week = t3.week and wk.season = t3.season
     LEFT JOIN fo_qb_passing t4 ON wk.player_id = t4.player and wk.week = t4.week and wk.season = t4.season 
     LEFT JOIN pfr_adv_passing_accuracy t5 ON wk.player_id = t5.player and wk.week = t5.week and wk.season = t5.season
     LEFT JOIN pfr_adv_passing_pressure t6 ON wk.player_id = t6.player and wk.week = t6.week and wk.season = t6.season
     LEFT JOIN fo_qb_rushing t7 ON wk.player_id = t7.player and wk.week = t7.week and wk.season = t7.season
     LEFT JOIN pfr_redzone_passing t8 ON wk.player_id = t8.player and wk.week = t8.week and wk.season = t8.season
     LEFT JOIN fanduel_salary t9 ON wk.player_id = t9.player_id and wk.week = t9.week and wk.season = t9.season
  WHERE p.position_id = 1;