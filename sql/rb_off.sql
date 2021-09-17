 SELECT p.full_name,
    wk.opp,
    round(t1.att::numeric + t2.target::numeric, 2) AS total_touches,
    round(t3.inside10_att::numeric / t2.g::numeric + t2.target::numeric, 2) AS high_value_touches,
    round((t3.inside10_att::numeric / t2.g::numeric + t2.target::numeric) / (t1.att::numeric + t2.target::numeric), 2) AS high_value_percent,
    t1.att,
    t1.td,
    t1.yds,
    round(t4.dyar::numeric / t1.g::numeric, 2) AS dyar,
    t4.dvoa,
    round((t4.eyds::numeric - t4.yards::numeric) / t2.g::numeric, 2) AS rush_diff,
    t4.suc_rate,
    t5.target,
    t5.yds AS rec_yds,
    t6.dyar AS rec_dyar,
    t6.dvoa AS rec_dvoa,
    round((t6.eyds::numeric - t6.yards::numeric) / t2.g::numeric, 2) AS rec_diff,
    t7.inside10_tgt,
    t8.inside10_att,
    t7.inside10_td + t8.inside10_td,
    t8.inside10_perrush,
    wk.line,
    t9.fd_sal,
    round((t1.att::numeric + t2.target::numeric) / (t9.fd_sal::numeric / 1000::numeric), 2) AS touches_per_1k,
    round((t3.inside10_att::numeric / t2.g::numeric + t2.target::numeric) / (t9.fd_sal::numeric / 1000::numeric), 2) AS high_value_per_1k,
    wk.week,
    wk.season
   FROM weekly_games wk
     JOIN players p ON wk.player_id = p.player_id
     LEFT JOIN pfr_ytd_rushing t1 ON wk.player_id = t1.player_id AND wk.week = t1.week AND wk.season = t1.season
     LEFT JOIN pfr_ytd_receiving t2 ON wk.player_id = t2.player_id AND wk.week = t2.week AND wk.season = t2.season
     LEFT JOIN pfr_redzone_rushing t3 ON wk.player_id = t3.player AND wk.week = t3.week AND wk.season = t3.season
     LEFT JOIN fo_rb_rushing t4 ON wk.player_id = t4.player AND wk.week = t4.week AND wk.season = t4.season
     LEFT JOIN pfr_ytd_receiving t5 ON wk.player_id = t5.player_id AND wk.week = t5.week AND wk.season = t5.season
     LEFT JOIN fo_rb_receiving t6 ON wk.player_id = t6.player AND wk.week = t6.week AND wk.season = t6.season
     LEFT JOIN pfr_redzone_receiving t7 ON wk.player_id = t7.player AND wk.week = t7.week AND wk.season = t7.season
     LEFT JOIN pfr_redzone_rushing t8 ON wk.player_id = t8.player AND wk.week = t8.week AND wk.season = t8.season
     JOIN fanduel_salary t9 ON wk.player_id = t9.player_id AND wk.week = t9.week AND wk.season = t9.season
  WHERE p.position_id = 2;