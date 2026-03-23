* * * biomass_out
set
dbclass Dedicated biomass supply classes
dbtrans Dedicated biomass transport mode
dbmap(dbtrans,r,r) Transport mode mapping
fuelused fuel used
biosce
;

$gdxin database\setpar_%n%.gdx
$load dbclass,dbtrans,dbmap,fuelused,biosce
$gdxin

parameter
dbref_r(r,t) Reference dedicated biomass consumption (EJ) (regional)
dbref(t) Reference dedicated biomass consumption (EJ) (system)
dbelas_r(r,t) Supply elasticity for dedicated biomass (regional)
dbelas(t) Supply elasticity for dedicated biomass (system)
dbcost_r(dbclass,r,t) Cost of dedicated biomass by supply class (EUR per MWh th) (regional)
dbcost(dbclass,t) Cost of dedicated biomass by supply class (EUR per MWh th) (system)
dbcost_rr(dbclass,dbtrans,r,r,t) Trade cost of dedicated biomass by supply class (EUR per MWh th) (regional)
dbfueluse_rr(fuelused,dbclass,dbtrans,r,r,t) Trade fuel used (MWh per MWh)(regional)
dblim_r(dbclass,r,t) Class size of dedicated biomass supply function (regional)
dblim(dbclass,t) Class size of dedicated biomass supply function (system)
dblimagg_r(dbclass,r,t) Aggregated biomass supply (TWh) (regional)
dblimagg(dbclass,t) Aggregated biomass supply (TWh) (system)
dbind_int(biosce,r,t)
dbtra_int(biosce,r,t)
dboth_int(biosce,r,t)
dbhea_int(biosce,r,t)
dbrem_int(biosce,r,t)
dbind(r,t)
dbtra(r,t)
dboth(r,t)
dbhea(r,t)
dbrem(r,t)
;

$gdxin database\setpar_%n%.gdx
$load dbref_r
$load dbref
$load dbelas_r
$load dbelas
$load dbcost_r
$load dbcost
$load dbcost_rr
$load dbfueluse_rr
$load dblim_r
$load dblim
$load dblimagg_r
$load dblimagg
$load dbind_int = dbind
$load dbtra_int = dbtra
$load dboth_int = dboth
$load dbhea_int = dbhea
$load dbrem_int = dbrem
$gdxin

$if      set biotrend   dbind(r,t) = dbind_int("trend",r,t) ;
$if      set biotrend   dbtra(r,t) = dbtra_int("trend",r,t) ;
$if      set biotrend   dboth(r,t) = dboth_int("trend",r,t) ;
$if      set biotrend   dbhea(r,t) = dbhea_int("trend",r,t) ;
$if      set biotrend   dbrem(r,t) = dbrem_int("trend",r,t) ;

$if not  set biotrend   dbind(r,t) = dbind_int("notrend",r,t) ;
$if not  set biotrend   dbtra(r,t) = dbtra_int("notrend",r,t) ;
$if not  set biotrend   dboth(r,t) = dboth_int("notrend",r,t) ;
$if not  set biotrend   dbhea(r,t) = dbhea_int("notrend",r,t) ;
$if not  set biotrend   dbrem(r,t) = dbrem_int("notrend",r,t) ;