 SELECT p.full_name,
    wk.opp,
    t1.target,
    t1.yds_per_gm,
    t1.td,
    round(t2.dyar::numeric / t1.g::numeric, 2) AS dyar,
    t2.dvoa,
    round((t2.eyds::numeric - t2.yards::numeric) / t1.g::numeric, 2) AS eyds_diff,
    t3.adot,
    round(t3.adot::numeric * t1.target::numeric, 2) AS air_yards,
    round(t1.yds_per_gm::numeric / (t3.adot::numeric * t1.target::numeric), 2) AS racr,
    round(t3.tgt::numeric / t4.att::numeric * 100::numeric, 2) AS target_per,
    round(t3.adot::numeric * t3.tgt::numeric / t4.air_yards::numeric * 100::numeric, 2) AS air_yard_per,
    round(t3.tgt::numeric / t4.att::numeric * 100::numeric * 1.5 + t3.adot::numeric * t3.tgt::numeric / t4.air_yards::numeric * 100::numeric * 0.7, 2) AS wopr,
    t5.inside20_tgt,
    t5.inside20_pertgt,
    t6.vs_cb_tar,
    t6.vs_cb_fpt,
    t6.vs_cb_shad,
    t6.vs_cb_matchup,
    t7.fd_sal,
    round(t1.target::numeric / (t7.fd_sal::numeric / 1000::numeric), 2) AS target_per_1k,
    wk.implied_total,
    wk.week,
    wk.season
   FROM weekly_games wk
     LEFT JOIN players p ON wk.player_id = p.player_id
     LEFT JOIN pfr_ytd_receiving t1 ON wk.player_id = t1.player_id and wk.week = t1.week and wk.season = t1.season
     LEFT JOIN fo_wr t2 ON wk.player_id = t2.player and wk.week = t2.week and wk.season = t2.season
     LEFT JOIN pfr_adv_receiving t3 ON wk.player_id = t3.player and wk.week = t3.week and wk.season = t3.season
     JOIN team_airyards t4 ON wk.team_id = t4.team_id
     LEFT JOIN pfr_redzone_receiving t5 ON wk.player_id = t5.player and wk.week = t5.week and wk.season = t5.season
     LEFT JOIN fff_wr_matchup t6 ON wk.player_id = t6.player_id and wk.week = t6.week and wk.season = t6.season
     LEFT JOIN fanduel_salary t7 ON wk.player_id = t7.player_id and wk.week = t7.week and wk.season = t7.season
  WHERE p.position_id = 3;