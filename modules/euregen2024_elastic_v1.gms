set
bse elastic lost loas segments
;

$gdxin precal\precal_%n%.gdx
$load bse
$gdxin

parameter
price(s,r,t)
Electricity1_rpt(t,r,*)

bse_share(bse,r,t) elastic lost load share

pelas(bse,s,r,t)              Price elasticity at reference point (a negative value)
p0(bse,s,r,t)              Reference average price (EUR per MWh)
d0(bse,s,r,t)              Reference annual average price in euro per MWh
pelas_d(bse,sd,hd,r,t)              Price elasticity at reference point (a negative value)
p0_d(bse,sd,hd,r,t)              Reference average price (EUR per MWh)
d0_d(bse,sd,hd,r,t)              Reference annual average price in euro per MWh
cb_1(bse,s,r,t)               Consumer benefit linear coefficient
cb_2(bse,s,r,t)               Consumer benefit quadratic coefficient
cb_1_d(bse,sd,hd,r,t)               Consumer benefit linear coefficient
cb_2_d(bse,sd,hd,r,t)               Consumer benefit quadratic coefficient

p0dtwh(r,t) EUR per MWh
d0dtwh(r,t) MWh
pelasdtwh(r,t) Long-run demand elasticity
cb_1dtwh(r,t) A
cb_2dtwh(r,t) B

surplus_short(r,t)
surplus_long(r,t)
surplus_fact(r,t)
surplus_share(r,t)
demand_share(r,t)
surplus_adj(r,t)
load_max(bse,s,r,t)
load_max_d(bse,sd,hd,r,t)
;

$if      set priceref $gdxin $gdxin precal\precal_%n%.gdx
$if      set priceref $load price, Electricity1_rpt
$if      set priceref $gdxin

$gdxin precal\precal_%n%.gdx
$load bse_share
$load pelas
$load p0
$load d0
$load pelas_d
$load p0_d
$load d0_d
$load cb_1
$load cb_2
$load cb_1_d
$load cb_2_d
$load p0dtwh
$load d0dtwh
$load pelasdtwh
$load cb_1dtwh
$load cb_2dtwh
$load surplus_short
$load surplus_long
$load surplus_fact
$load surplus_share
$load demand_share
$load surplus_adj
$load load_max
$load load_max_d
$gdxin

parameter 
vollelas(bse,r,t)
;

$gdxin precal\precal_%n%.gdx
$load vollelas
$gdxin

positive variable
* Welfare
WELFARE_short(r,t)      WELFARE (positive value) (million EUR)
WELFARE_long(r,t)       WELFARE (positive value) (million EUR)
* Demand
BSELAS(bse,s,r,t)       Lost load (backstop demand option) (GW)
DTWH(r,t)               Annual load for elastic demand module (TWh)
DS(bse,s,r,t)           Segment load for elastic demand module (GW)
* Generation
;

equation
weldef                           Objection function -- definition of welfare
weldef_short(r,t)                Objection function -- definition of welfare
weldef_long(r,t)                 Objection function -- definition of welfare
dtwhdef(r,t)
;

parameter
weight_short
weight_long
;

$gdxin precal\precal_%n%.gdx
$load weight_short,weight_long 
$gdxin

weldef..
        WELFARE =e= 
$if not  set days       - sum(toptimize(t), dfact(t) * sum(r,  weight_short * surplus_adj(r,t) * sum(s, hours(s) * sum(bse,  DS(bse,s,r,t)        * 1e+3 * cb_1(bse,s,r,t) + 0.5 * DS(bse,s,r,t)    * DS(bse,s,r,t)    * 1e+6  * cb_2(bse,s,r,t))) * 1e-6
$if      set days       - sum(toptimize(t), dfact(t) * sum(r,  weight_short * surplus_adj(r,t) * sum(sd, days(sd) * sum(hd, sum(bse,  DS_D(bse,sd,hd,r,t)        * 1e+3 * cb_1_d(bse,sd,hd,r,t) + 0.5 * DS_D(bse,sd,hd,r,t)    * DS_D(bse,sd,hd,r,t)    * 1e+6  * cb_2_d(bse,sd,hd,r,t)))) * 1e-6
                        + weight_long  * surplus_adj(r,t) * surplus_fact(r,t) * 8760 * (DTWH(r,t) / 8760     * 1e+6 * cb_1dtwh(r,t)   + 0.5 * DTWH(r,t) / 8760 * DTWH(r,t) / 8760 * 1e+12 * cb_2dtwh(r,t))    * 1e-6 ))
                        + SURPLUS ;

dtwhdef(r,toptimize(t))..                             DTWH(r,t) =e=
$if not  set days   1e-3 * sum(s, hours(s) * sum(bse, DS(bse,s,r,t) - BSELAS(bse,s,r,t))) ;
$if      set days   1e-3 * sum(sd, days(sd) * sum(hd, sum(bse, DS_D(bse,sd,hd,r,t) - BSELAS_D(bse,sd,hd,r,t)))) ;
,t)))) ;
