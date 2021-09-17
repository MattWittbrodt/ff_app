 SELECT fd.player_id,
    fd.fd_sal,
    fd.week,
    fd.fd_sal - lag(fd.fd_sal, 1) OVER (PARTITION BY fd.player_id ORDER BY fd.week) AS price_change
   FROM fanduel_salary fd;

