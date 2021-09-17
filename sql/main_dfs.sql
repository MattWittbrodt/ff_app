select p.full_name, 
	pos.short_name as position,
	wk.team,
	wk.opp,
	pts.proj_ffpts,
	round(pts.proj_ffpts::numeric / (fd.fd_sal::numeric / 1000),2) as pt_1k,
	fd.fd_lev,
	afpa.afpa,
	afpa.afpa_rk,
	fd.fd_sal,
	fdsalary_change.price_change,
	fd.projected_own,
	fd.cash_odds,
	fd.gpp_odds,
	wk.line,
	wk.total,
	wk.implied_total,
	wk.week, wk.season
from weekly_games wk 
	join players p on wk.player_id = p.player_id
	join positions pos on p.position_id = pos.position_id
	left join fff_projected_pts pts on wk.player_id = pts.player_id and wk.week = pts.week
	left join fanduel_salary fd on wk.player_id = fd.player_id
	left join fff_afpa afpa on wk.opp_id = afpa.team_id and p.position_id = afpa.position_id
	left join teams t on wk.team_id = t.team_id 
	left join teams t2 on wk.opp_id = t2.team_id
	left join fdsalary_change on wk.player_id = fdsalary_change.player_id 
where wk.week = 17 and pts.week = 17 and fd.week = 17 and afpa.week = 17 and fdsalary_change.week = 17

