parameter
price(s,r,t)
Electricity1_rpt(t,r,*)
pelas(bse,s,r,t)              Price elasticity at reference point (a negative value)
p0(bse,s,r,t)              Reference average price (EUR per MWh)
d0(bse,s,r,t)              Reference annual average price in euro per MWh
cb_1(bse,s,r,t)               Consumer benefit linear coefficient
cb_2(bse,s,r,t)               Consumer benefit quadratic coefficient
;

$if      set priceref $gdxin elastic\bauprice_newcap_priceref.gdx
$if      set priceref $load price
$if      set priceref $gdxin

pelas("bs6",s,r,t) = -0.005 ;
pelas("bs5",s,r,t) = -0.01 ;
pelas("bs4",s,r,t) = -0.015 ;
pelas("bs3",s,r,t) = -0.02 ;
pelas("bs2",s,r,t) = -0.03 ;
pelas("bs1",s,r,t) = -0.05 ;

$if      set nogroups pelas(bse,s,r,t) = -0.01 ;

$if      set nogroups $if      set pelas001 pelas("bs6",s,r,t) = -0.01 ;
$if      set nogroups $if      set pelas002 pelas("bs6",s,r,t) = -0.02 ;
$if      set nogroups $if      set pelas003 pelas("bs6",s,r,t) = -0.03 ;
$if      set nogroups $if      set pelas004 pelas("bs6",s,r,t) = -0.04 ;
$if      set nogroups $if      set pelas005 pelas("bs6",s,r,t) = -0.05 ;

$if not  set priceref p0(bse,s,r,t) = 40 ;
$if not  set priceref d0(bse,s,r,t) = load(s,r,t) * bse_share(bse,r,t) * 1e+3 ;

$if      set priceref p0(bse,s,r,t) = Electricity1_rpt(t,r,"price-avg") ;
$if      set priceref d0(bse,s,r,t) = load(s,r,t) * bse_share(bse,r,t) * 1e+3 ;

*cb_1(bse,s,r,t)$pelas(bse,s,r,t) = round(p0(bse,s,r,t) * (1 - 1 / pelas(bse,s,r,t)),4) ;
*cb_2(bse,s,r,t)$pelas(bse,s,r,t) = round(1 / pelas(bse,s,r,t) * p0(bse,s,r,t) / d0(bse,s,r,t),4) ;

cb_1(bse,s,r,t)$(pelas(bse,s,r,t) and d0(bse,s,r,"2022") > 0) = round(p0(bse,s,r,"2022") - 1 / pelas(bse,s,r,t) * p0(bse,s,r,"2022") / d0(bse,s,r,"2022") * d0(bse,s,r,t),4) ;
cb_2(bse,s,r,t)$(pelas(bse,s,r,t) and d0(bse,s,r,"2022") > 0) = round(1 / pelas(bse,s,r,t) * p0(bse,s,r,"2022") / d0(bse,s,r,"2022"),4) ;


Parameter
surplus_short(r,t)
surplus_long(r,t)
surplus_fact(r,t)
surplus_share(r,t)
demand_share(r,t)
surplus_adj(r,t)
load_max(bse,s,r,t)
voll_s(s,r,t)
;

surplus_short(r,t) = sum(s, hours(s) * sum(bse, cb_1(bse,s,r,t) * d0(bse,s,r,t) + 0.5 * cb_2(bse,s,r,t) * d0(bse,s,r,t) * d0(bse,s,r,t))) * 1e-6 ;

surplus_long(r,t)                                                                               = 8760 * (                   cb_1dtwh(r,t)   * d0dtwh(r,t)   + 0.5 * cb_2dtwh(r,t) * d0dtwh(r,t)     * d0dtwh(r,t))    * 1e-6 ;
surplus_fact(r,t)$surplus_long(r,t)                                                             = round(surplus_short(r,t) / surplus_long(r,t),4) ;
surplus_share(r,t)$sum(rr, surplus_short(rr,t) + surplus_fact(rr,t) * surplus_long(rr,t))       = (surplus_short(r,t) + surplus_fact(r,t) * surplus_long(r,t)) / sum(rr, surplus_short(rr,t) + surplus_fact(rr,t) * surplus_long(rr,t)) ;
demand_share(r,t)$sum(rr, daref(rr,t))                                                          = daref(r,t) / sum(rr, daref(rr,t)) ;
surplus_adj(r,t)$surplus_share(r,t)                                                             = round(demand_share(r,t) / surplus_share(r,t),4) ;
load_max(bse,s,r,t)$cb_2(bse,s,r,t)                                                             = - round(cb_1(bse,s,r,t) / cb_2(bse,s,r,t) * 1e-3,4) ;
voll_s(s,r,t)       = 3000 ;
