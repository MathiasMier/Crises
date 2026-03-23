* * * Demand module
parameter
Electricity1_rpt(t,r,*)
;

$if      set priceref $gdxin elastic\bauprice_newcap_priceref.gdx
$if      set priceref $load Electricity1_rpt
$if      set priceref $gdxin

parameter
p0dtwh(r,t) EUR per MWh
d0dtwh(r,t) MWh
pelasdtwh(r,t) Long-run demand elasticity
cb_1dtwh(r,t) A
cb_2dtwh(r,t) B
;

$if not  set priceref p0dtwh(r,t) = 40 ;
$if not  set priceref d0dtwh(r,t) = daref(r,t) ;

$if      set priceref p0dtwh(r,t) = Electricity1_rpt(t,r,"price-avg") ;
$if      set priceref d0dtwh(r,t) = Electricity1_rpt(t,r,"elec-demand-ave") * 1e+3 ;

pelasdtwh(r,t) = - 0.05 ;
$if      set nogroups $if      set pelasdtwh001 pelasdtwh(r,t) = -0.01 ;
$if      set nogroups $if      set pelasdtwh005 pelasdtwh(r,t) = -0.05 ;
$if      set nogroups $if      set pelasdtwh010 pelasdtwh(r,t) = -0.10 ;

*cb_1dtwh(r,t)$pelasdtwh(r,t) = round(p0dtwh(r,t) * (1 - 1 / pelasdtwh(r,t)),4) ;
*cb_2dtwh(r,t)$(pelasdtwh(r,t) and d0dtwh(r,t) > 0) = round(1 / pelasdtwh(r,t) * p0dtwh(r,t) / d0dtwh(r,t),4)

cb_1dtwh(r,t)$(pelasdtwh(r,t) and d0dtwh(r,"2022") > 0)       = round(p0dtwh(r,"2022")   - 1 / pelasdtwh(r,t) * p0dtwh(r,"2022") / d0dtwh(r,"2022") * d0dtwh(r,t),4) ;
cb_2dtwh(r,t)$(pelasdtwh(r,t) and d0dtwh(r,"2022") > 0)       = round(1 / pelasdtwh(r,t) * p0dtwh(r,"2022") / d0dtwh(r,"2022"),4) ;

parameter
weight_short
weight_long
;

weight_short = 0.5 ;
weight_long  = 0.5 ;

$if      set onlyshort   weight_short = 1.0 ;
$if      set onlyshort   weight_long  = 0.0 ;

$if      set onlylong    weight_short = 0.0 ;
$if      set onlylong    weight_long  = 1.0 ;

$if      set nosurplusfrac surplus_fact(r,t) = 1 ;
$if      set nosurplusadj  surplus_adj(r,t)  = 1 ;

* * * Demand modules
set
bse elastic lost loas segmenta /bs1,bs2,bs3,bs4,bs5,bs6/
;

parameter
bse_share(bse,r,t) elastic lost load share
;

bse_share(bse,r,t) = 0.05 ;
bse_share("bs6",r,t) = 0.75 ;

$if      set nogroups bse_share(bse,r,t) = 0 ;
$if      set nogroups bse_share("bs6",r,t) = 1 ;

parameter
vollelas(bse,r,t)
;

vollelas("bs1",r,t) = 500 ;
vollelas("bs2",r,t) = 1000 ;
vollelas("bs3",r,t) = 1500 ;
vollelas("bs4",r,t) = 2000 ;
vollelas("bs5",r,t) = 2500 ;
vollelas("bs6",r,t) = 3000 ;

$if not  set days   $include modules_precal\euregen2024_pre_elastic_segments_v1
$if      set days   $include modules_precal\euregen2024_pre_elastic_days_v1