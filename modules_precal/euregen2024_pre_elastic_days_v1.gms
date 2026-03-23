* * * Demand module
parameter
price_d(sd,hd,r,t)
pelas_d(bse,sd,hd,r,t)              Price elasticity at reference point (a negative value)
p0_d(bse,sd,hd,r,t)              Reference average price (EUR per MWh)
d0_d(bse,sd,hd,r,t)              Reference annual average price in euro per MWh
cb_1_d(bse,sd,hd,r,t)               Consumer benefit linear coefficient
cb_2_d(bse,sd,hd,r,t)               Consumer benefit quadratic coefficient
;

$if      set priceref $gdxin elastic\bauprice_newcap_priceref_days.gdx
$if      set priceref $load price_d
$if      set priceref $gdxin

pelas_d("bs6",sd,hd,r,t) = -0.005 ;
pelas_d("bs5",sd,hd,r,t) = -0.01 ;
pelas_d("bs4",sd,hd,r,t) = -0.015 ;
pelas_d("bs3",sd,hd,r,t) = -0.02 ;
pelas_d("bs2",sd,hd,r,t) = -0.03 ;
pelas_d("bs1",sd,hd,r,t) = -0.05 ;

$if      set nogroups                       pelas_d(bse,sd,hd,r,t) = -0.01 ;
$if      set nogroups $if      set pelas001 pelas_d("bs6",sd,hd,r,t) = -0.01 ;
$if      set nogroups $if      set pelas002 pelas_d("bs6",sd,hd,r,t) = -0.02 ;
$if      set nogroups $if      set pelas003 pelas_d("bs6",sd,hd,r,t) = -0.03 ;
$if      set nogroups $if      set pelas004 pelas_d("bs6",sd,hd,r,t) = -0.04 ;
$if      set nogroups $if      set pelas005 pelas_d("bs6",sd,hd,r,t) = -0.05 ;

$if not  set priceref p0_d(bse,sd,hd,r,t) = 40 ;
$if not  set priceref d0_d(bse,sd,hd,r,t) = load_d(sd,hd,r,t) * bse_share(bse,r,t) * 1e+3 ;

$if      set priceref p0_D(bse,sd,hd,r,t) = Electricity1_rpt(t,r,"price-avg") ;
$if      set priceref d0_d(bse,sd,hd,r,t) = load_d(sd,hd,r,t) * bse_share(bse,r,t) * 1e+3 ;

cb_1_d(bse,sd,hd,r,t)$(pelas_d(bse,sd,hd,r,t) and d0_d(bse,sd,hd,r,"2022") > 0) = round(p0_d(bse,sd,hd,r,"2022") - 1 / pelas_d(bse,sd,hd,r,t) * p0_d(bse,sd,hd,r,"2022") / d0_d(bse,sd,hd,r,"2022") * d0_d(bse,sd,hd,r,t),4) ;
cb_2_d(bse,sd,hd,r,t)$(pelas_d(bse,sd,hd,r,t) and d0_d(bse,sd,hd,r,"2022") > 0) = round(1 / pelas_d(bse,sd,hd,r,t) * p0_d(bse,sd,hd,r,"2022") / d0_d(bse,sd,hd,r,"2022"),4) ;

Parameter
surplus_short(r,t)
surplus_long(r,t)
surplus_fact(r,t)
surplus_share(r,t)
demand_share(r,t)
surplus_adj(r,t)
load_max_d(bse,sd,hd,r,t)
voll_d(sd,hd,r,t)
;

surplus_short(r,t) = sum(sd, days(sd) * sum(hd, sum(bse, cb_1_d(bse,sd,hd,r,t) * d0_d(bse,sd,hd,r,t) + 0.5 * cb_2_d(bse,sd,hd,r,t) * d0_d(bse,sd,hd,r,t) * d0_d(bse,sd,hd,r,t)))) * 1e-6 ;

surplus_long(r,t)                                                                               = 8760 * (                   cb_1dtwh(r,t)   * d0dtwh(r,t)   + 0.5 * cb_2dtwh(r,t) * d0dtwh(r,t)     * d0dtwh(r,t))    * 1e-6 ;
surplus_fact(r,t)$surplus_long(r,t)                                                             = round(surplus_short(r,t) / surplus_long(r,t),4) ;
surplus_share(r,t)$sum(rr, surplus_short(rr,t) + surplus_fact(rr,t) * surplus_long(rr,t))       = (surplus_short(r,t) + surplus_fact(r,t) * surplus_long(r,t)) / sum(rr, surplus_short(rr,t) + surplus_fact(rr,t) * surplus_long(rr,t)) ;
demand_share(r,t)$sum(rr, daref(rr,t))                                                          = daref(r,t) / sum(rr, daref(rr,t)) ;
surplus_adj(r,t)$surplus_share(r,t)                                                             = round(demand_share(r,t) / surplus_share(r,t),4) ;
load_max_d(bse,sd,hd,r,t)$cb_2_d(bse,sd,hd,r,t)                                                 = - round(cb_1_d(bse,sd,hd,r,t) / cb_2_d(bse,sd,hd,r,t) * 1e-3,4) ;
voll_d(sd,hd,r,t)   = 3000 ;