* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * Timeseries and calibration
set
i                                Generation technology
j                                Storage technologies
h                                Hours /1*8760/
m                                Months /1*12/
hm(h,m)                          Map between hours and months for assembling availability factors
nonvj(j)
;

$gdxin precal\precal_%n%
$load i,j,hm,nonvj
$gdxin

* * * Fundamentals
set
v                                Vintages of generation capacity
oldv(v)                          Existing vintages
newv(v)                          New vintages
$if      set static     $if not  set hours  t      Model time periods /%year%/
$if not  set static                         t      Model time periods
$if      set hours                          t      Model time periods
tv(t,v)                          Time period in which vintage v is installed
r                                Model regions
rwindoff(r)
rnonuc(r)
rnuc(r)
rnolig(r)
rlig(r)
rnocoa(r)
rcoa(r)
rfr(r)
rge(r)
rbr(r)
reu(r)
toptimize(t)
topt2023(t)
topt2024_2030(t)
topt2024(t)
topt2030(t)
topt2030plus(t)
t2023(t)
topt2035plus(t)
tcri(t)
told(t)
;

$if not  set hours  $gdxin precal\precal_%n%.gdx
$if      set hours  $gdxin precal\precal_%year%_%n%.gdx
$load v, oldv, newv
$if not  set static $if not set hours $load t
$if      set static $if     set hours $load t
$load tv, r
$load rwindoff, rnonuc, rnuc, rnolig, rlig, rnocoa, rcoa, rfr, rge, rbr, reu
$load toptimize, topt2023, topt2024_2030, topt2024, topt2030, topt2030plus, t2023, topt2035plus, tcri, told
$gdxIn

$ontext
set
t2020(t)
t2021(t)
t2022(t)
t2023(t)
;

t2020(t)$(sameas(t,"2020")) = YES ;
t2021(t)$(sameas(t,"2021")) = YES ;
t2022(t)$(sameas(t,"2022")) = YES ;
t2023(t)$(sameas(t,"2023")) = YES ;
$offtext

alias(r,rr);
alias(t,tt);
alias(v,vv);

set
l
superirnw
quantiles
irnw_mapq(i,quantiles)
superirnw_map(i,superirnw)
superirnw_mapq(i,quantiles,superirnw)
;

$gdxin precal\precal_%n%.gdx
$load l, superirnw, quantiles, superirnw_map, superirnw_mapq, irnw_mapq
$gdxin

parameter
* More general
daref(r,t)                              Reference annual demand by region over time (TWh)
daref_sec(r,t,l)
lossave(r,t)
irnwlimUP_quantiles(i,r,quantiles)      Upper limit per quantile
;

$gdxin precal\precal_%n%.gdx
$load daref
$load daref_sec
$load lossave
$load irnwlimUP_quantiles
$gdxIn

* * * Generation technology
set
new(i)                           New generation technology
exi(i)                           Existing technologies (or capacities) in base year - EXISTING BLOCKS
ccs(i)                           CCS generation technologies (or capacities) - CCS BLOCKS
conv(i)                          Conventional generation technologies
irnw(i)                          Intermittent renewable generation technologies
notirnw(i)
lig(i)                           Lignite technologies
oil(i)
coa(i)                           Coal technologies
gas(i)                           Gas technologies
bio(i)                           Biomass technologies
sol(i)                           Solar technologies
hyd(i)
roof(i)                          Roof-top technologies
open(i)                          Open-field technologies
wind(i)                          Wind technologies
windon(i)                        Wind onshore technologies 
windoff(i)                       Wind offshore technologies 
rnw(i)                           Renewable technologies
lowcarb(i)                       Low-carbon technologies
nuc(i)                           Nuclear technologies
type                             Generation type
idef(i,type)                     Map between technology and type
iidef(i,type)                    Map between technology and type
chp(i)                           CHP technologies
nochp(i)                         Non-CHP technologies
mapchp(i,i)                      Mapping between technologies for CHP transformation
ivr(i,v,r)
ivrt(i,v,r,t)
ivrt_chp(i,v,r,t)
ivrt_nochp(i,v,r,t)
ivrt_capacity(i,v,r,t)
irnw_nohydro(i)
;

$if not  set hours  $gdxin precal\precal_%n%.gdx
$if      set hours  $gdxin precal\precal_%year%_%n%.gdx
$load new, exi, ccs, irnw, notirnw, conv, sol, roof, open, hyd, wind, windon, windoff, rnw, lowcarb, nuc, coa, lig, type, idef, gas, bio, oil, chp, nochp, mapchp
$load ivr, ivrt, ivrt_chp, ivrt_nochp, ivrt_capacity, irnw_nohydro
$gdxin

iidef(i,type) = idef(i,type) ;
alias(i,ii) ;

parameter
capt(i,v,r,t)                    Capacity installed by region in period t(GW)
invlimUP(i,r,t)                  Upper bounds on investment based on potential (cumulative since last time period) (GW)
invlimLO(i,r,t)                  Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
invlimUP_eu(i,t)                 Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
invlife(i,v,r)                     Capacity lifetime
invdepr(i,v,r)                     Investment depreciation
capcost(i,v,r)                   Capacity cost (investment) by region
fomcost(i,v,r)                   Fixed OM cost
vomcost(i,v,r)                   Variable OM cost
effrate(i,v,r)                   Efficiency
co2captured(i,v,r)               CCS capture rate
emit(i,v,r)                      Emission factor
reliability(i,v,r)               Reliability factor by region and technology
capcred(i,v,r)                   Capacity credit by region and technology
mindisp(i,v,r)                   Min load by region and technology
sclim(r)                         Upper bound on geologic storage of carbon (GtCO2)
sclim_eu                         Upper bound on geologic storage of carbon (GtCO2)
biolim(r,t)                      Upper bounds by region on biomass use (MWh)
biolim_eu(t)                     Upper bounds by region on biomass use (MWh)
deccost(i,v,r)
;

$gdxin precal\precal_%n%.gdx
$load capt, invlimUP, invlimLO, invlimUP_eu, invlife, invdepr, capcost, fomcost, vomcost, effrate, co2captured, emit, reliability, capcred, mindisp
$load sclim, sclim_eu, biolim, biolim_eu, deccost
$gdxin
     
* * * Storage technology
set
newj(j)                          New storage technology
exij(j)                          Existing storage technology
jres(j)
nvjres(j)
jpump(j)
nvj(j)
jvrt(j,v,r,t)
;

$gdxin precal\precal_%n%.gdx
$load newj, exij, jres, nvjres, jpump, nvj, jvrt
$gdxin

parameter
gcapt(j,v,r,t)                   Storage capacity by region (GW)
ghours(j,v,r)                    Hours of storage (room size relative to door size)
chrgpen(j,v,r)                   Charge efficiency penalty for storage by region (< 1)
dchrgpen(j,v,r)                  Discharge efficiency penalty for storage by region (< 1)
dischrg(j,v,r)                   Automatic storage discharge by region (in percent) (< 1)
gcapcost(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gcapcostc(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gcapcostd(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gcapcostr(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gfomcost(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gfomcostc(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gfomcostd(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gfomcostr(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gvomcost(j,v,r)                  Variable OM cost for storage by region (EUR per MWh)
greliability(j,v,r)              Storage reliability factor by region and technology
gcapcred(j,v,r)                  Storage capacity credit by region and technology
ginvlife(j,v,r)                    Storage investment life for new capacity additions (years)
ginvdepr(j,v,r)                    Storage investment life for new capacity additions (years)
ginvlimLO(j,r,t)                 Storage investment lower bound (GW)
ginvlimUP(j,r,t)                 Storage investment upper bound (GW)
ginvlimUP_eu(j,t)                Storage investment upper bound (GW)
himport(r,t)
;

$gdxin precal\precal_%n%.gdx
$load gcapt, ghours, chrgpen, dchrgpen, dischrg, gcapcost, gfomcost, gvomcost, greliability, gcapcred, ginvlife, ginvdepr, ginvlimLO, ginvlimUP, ginvlimUP_eu
$gdxin

chrgpen("Storage_LT",newv(v),r) = dchrgpen("Storage_LT",v,r) ;
dchrgpen("Storage_LT",newv(v),r) = effrate("Gas_CCGT",v,r) ;
gcapcost("Storage_LT","2020",r) = round(750 + 850 + 300 + 561   + 0.62 * ghours("Storage_LT","2020",r) + 8.5,4)  ;
gcapcost("Storage_LT","2050",r) = round(300 + 850 + 150 + 280.5 + 0.31 * ghours("Storage_LT","2020",r) + 4.25,4) ;
gfomcost("Storage_LT","2020",r) = round(13  + 21.25 + (561   + 0.62 * ghours("Storage_LT","2020",r) + 8.5 ) * 0.02,4) ;
gfomcost("Storage_LT","2050",r) = round(9   + 21.25 + (280.5 + 0.31 * ghours("Storage_LT","2020",r) + 4.25) * 0.02,4) ;

gcapcostc("Storage_LT","2020",r) = round(750 + 561 ,4)  ;
gcapcostc("Storage_LT","2050",r) = round(300 + 280.5,4) ;
gfomcostc("Storage_LT","2020",r) = round(13 + 561   * 0.02,4) ;
gfomcostc("Storage_LT","2050",r) = round(9  + 280.5 * 0.02,4) ;

gcapcostd("Storage_LT","2020",r) = round(850 + 300 + 8.5 ,4)  ;
gcapcostd("Storage_LT","2050",r) = round(850 + 150 + 4.25,4) ;
gfomcostd("Storage_LT","2020",r) = round(21.25 + 8.5 * 0.02 ,4) ;
gfomcostd("Storage_LT","2050",r) = round(21.25 + 4.25 * 0.02,4) ;

gcapcostr("Storage_LT","2020",r) = round(0.62,4) ;
gcapcostr("Storage_LT","2050",r) = round(0.31,4) ;
gfomcostr("Storage_LT","2020",r) = round(0.62 * 0.02,4) ;
gfomcostr("Storage_LT","2050",r) = round(0.31 * 0.02,4) ;

*gcapcost("Storage_LT","2020",r) = round(  0 + 850 + 300 + 0 + 0.62 * ghours("Storage_LT","2024",r) + 0,4)  ;
*gcapcost("Storage_LT","2050",r) = round(  0 + 850 + 150 + 0 + 0.31 * ghours("Storage_LT","2050",r) + 0,4) ;
*gfomcost("Storage_LT","2020",r) = round(  0 + 21.25 + (0    + 0.62 * ghours("Storage_LT","2024",r) + 0) * 0.02,4) ;
*gfomcost("Storage_LT","2050",r) = round(  0 + 21.25 + (0    + 0.31 * ghours("Storage_LT","2050",r) + 0) * 0.02,4) ;

gvomcost("Storage_LT",v,r)      = 2 ;

gcapcost("Storage_LT",newv(v),r) = round(gcapcost("Storage_LT","2020",r) + (gcapcost("Storage_LT","2050",r) - gcapcost("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;
gfomcost("Storage_LT",newv(v),r) = round(gfomcost("Storage_LT","2020",r) + (gfomcost("Storage_LT","2050",r) - gfomcost("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;
gvomcost("Storage_LT",newv(v),r) = round(gvomcost("Storage_LT","2020",r) + (gvomcost("Storage_LT","2050",r) - gvomcost("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;

gcapcostc("Storage_LT",newv(v),r) = round(gcapcostc("Storage_LT","2020",r) + (gcapcostc("Storage_LT","2050",r) - gcapcostc("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;
gfomcostc("Storage_LT",newv(v),r) = round(gfomcostc("Storage_LT","2020",r) + (gfomcostc("Storage_LT","2050",r) - gfomcostc("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;

gcapcostd("Storage_LT",newv(v),r) = round(gcapcostd("Storage_LT","2020",r) + (gcapcostd("Storage_LT","2050",r) - gcapcostd("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;
gfomcostd("Storage_LT",newv(v),r) = round(gfomcostd("Storage_LT","2020",r) + (gfomcostd("Storage_LT","2050",r) - gfomcostd("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;

gcapcostr("Storage_LT",newv(v),r) = round(gcapcostr("Storage_LT","2020",r) + (gcapcostr("Storage_LT","2050",r) - gcapcostr("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;
gfomcostr("Storage_LT",newv(v),r) = round(gfomcostr("Storage_LT","2020",r) + (gfomcostr("Storage_LT","2050",r) - gfomcostr("Storage_LT","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;

gcapcost("Storage_ST","2020",r) = 1042 ;
gcapcost("Storage_ST","2050",r) = 255 ;
gcapcost("Storage_ST",newv(v),r) = round(gcapcost("Storage_ST","2020",r) + (gcapcost("Storage_ST","2050",r) - gcapcost("Storage_ST","2020",r)) * (v.val - 2020) / (2050 - 2020),4) ;

gcapcost("Storage_ST",oldv(v),r) = 0 ;
gfomcost("Storage_ST",oldv(v),r) = 0 ;
gvomcost("Storage_ST",oldv(v),r) = 0 ;

gcapcost("Storage_LT",oldv(v),r) = 0 ;
gfomcost("Storage_LT",oldv(v),r) = 0 ;
gvomcost("Storage_LT",oldv(v),r) = 0 ;

himport(r,"2020") = 178 ;
himport(r,"2050") = 85 ;
*himport(r,"2050") = 100 ;
himport(r,toptimize(t)) = round(himport(r,"2020") + (himport(r,"2050") - himport(r,"2020")) * (t.val - 2020) / (2050 - 2020),4) ;

* * * Transmission technology
set
k                               Transmission technologies
tmap(k,r,r)          Regions eligible for transmission exchange by technology
tmapopt(k,r,r,t)
t2022(t)
t2022plus(t)
tmapopt_invup(k,r,rr,t)
tmapopt_invlo(k,r,rr,t)
tvrt(k,v,r,t)
;

$gdxin precal\precal_%n%.gdx
$load k, tmap, tmapopt, t2022, t2022plus, tmapopt_invup, tmapopt_invlo, tvrt
$gdxin

parameter
tcap(k,r,r)                     Transmission capacity from region X to region Y (GW)
tcapcost(k,r,r)                 Transmission investment cost ($ per kW)
tfomcost(k,r,r)                 Fixed O&M cost of new transmision capacity (euro per kW-year)
tvomcost(k,r,r)                 Variable O&M cost of new transmision capacity (euro per MWh)
trnspen(k,r,r)                  Transmission loss penalty
tinvlimUP(k,r,r,t)              Upper bound on total transmission capacity from region X to region Y (GW)
tinvlimLO(k,r,r,t)              Lower bound on total transmission capacity from region X to region Y (GW)
tinvlimUP_eu(k,t)               Lower bound on total transmission capacity from region X to region Y (GW)
tcapcred(k,r,r)                 Capacity credit for transmisson by region and technology
tinvlife(k)                     Capacity lifetime (years)
tinvdepr(k)                     Investment depreciation (years)
;

$gdxin precal\precal_%n%.gdx
$load tcap, tcapcost, tfomcost, tvomcost, trnspen, tinvlimUP, tinvlimLO, tinvlimUP_eu, tcapcred, tinvlife, tinvdepr
$gdxin

* * * Discounting
parameter
nyrs(t)                         Number of years since last time step
drate                           Annual discount rate
dfact(t)                        Discount factor for time period t (reflects number of years) for both
annuity(i,v)                     Annuity factor for generation capacity
gannuity(j,v)                   Annuity factor for storage capacity
tannuity(k)                     Annuity factor for transmission capacity
;

$gdxin precal\precal_%n%.gdx
$load nyrs, drate, dfact, annuity, gannuity, tannuity
$gdxin

* * * Lifetime and depreciation
parameter
lifetime(i,v,r,t)               Lifetime coefficient for existing and new capacity
deprtime(i,v,r,t)               Depreciation coefficient for existing and new capacity
glifetime(j,v,r,t)              Lifetime coefficient for existing and new capacity
gdeprtime(j,v,r,t)              Depreciation coefficient for existing and new capacity
tlifetime(k,v,r,t)              Depreciation coefficient for existing and new capacity
tdeprtime(k,v,r,t)              Fraction of discounted annualized payment stream contained in remaining model time horizon
endeffect(i,v,r,t)              Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
gendeffect(j,v,r,t)             Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tendeffect(k,v,r,t)             Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
modeldepr(i,v,r,t)              Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
gmodeldepr(j,v,r,t)             Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tmodeldepr(k,v,r,t)             Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
modeldepr_nodisc(i,v,r,t)       Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
gmodeldepr_nodisc(j,v,r,t)      Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tmodeldepr_nodisc(k,v,r,t)      Fraction of (non-)discounted annualized payment stream contained in remaining model time horizon (depreciation)
tmyopicLO
tmyopicUP
;

$gdxin precal\precal_%n%.gdx
$load lifetime, deprtime
$load glifetime, gdeprtime
$load tlifetime, tdeprtime
$load modeldepr, modeldepr_nodisc
$load gmodeldepr, gmodeldepr_nodisc
$load tmodeldepr, tmodeldepr_nodisc
$load endeffect, gendeffect, tendeffect
$load tmyopicLO,tmyopicUP
;

* * * Investor module
set
inv
drateopt
;

$gdxin precal\precal_%n%.gdx
$load inv, drateopt
$gdxin

parameter
share(inv,i,r)
gshare(inv,j,r)
tshare(inv,k,r)
zeta(inv,i,v,r)
gzeta(inv,j,v,r)
tzeta(inv,k,v,r)
;

$gdxin precal\precal_%n%.gdx
$load share
$load gshare
$load tshare
$load zeta
$load gzeta
$load tzeta
$gdxin 


* * * Prices
set
fuel                             Fuel
xfueli(fuel,i)                   Map fuel technology
;

$gdxin precal\precal_%n%.gdx
$load fuel, xfueli
$gdxin

parameter
pfuel(fuel,r,t)                         Fuel price (EUR er MWh)
pfadd(fuel,r,t)                         Absolute fuel price adders (EUR per MWh)
pfadd_rel(fuel,r,t)                     Relative fuel price adders (value between 0 (no adding) and x)
pco2_r(r,t)                             CO2 price by region (EUR per tCO2)
pco2(t)                                 CO2 price (EUR per tCO2)
biocost(r,t)                            Bioenegy cost (EUR per MWh)
biocost_eu(t)                           Bioenegy cost (EUR per MWh)
ccscost(r,t)                            CCS CO2 transportation cost (EUR per tCO2)
ccscost_eu(t)                           CCS CO2 transportation cost (EUR per tCO2)
;

$gdxin precal\precal_%n%.gdx
$load pfuel, pfadd, pco2_r, pco2, biocost, biocost_eu, ccscost, ccscost_eu, pfadd_rel
$gdxin

* * * Policy
set
pol_sc                   Defines a policy scenario
co2cap_sc                Defines a carbon cap scenario
rnwtgt_sc                Defines a renewable energy share (absolute value) target scenario
irnwtgt_sc               Defines a intermittent renewable energy share (absolute value) target scenario
coalexit_sc              Defines a coalexit scenario
nucexit_sc               Defines a nuclear exit scenario
gasexit_sc
;

$gdxin precal\precal_%n%.gdx
$load pol_sc, co2cap_sc, rnwtgt_sc, irnwtgt_sc, coalexit_sc, nucexit_sc, gasexit_sc
$gdxin

parameter
* Model parameter
co2p(t)                          Carbon price (EUR per t) (system)
co2cap_r(r,t)                    Carbon emissions cap (GtCO2) (regions)
co2cap(t)                        Carbon emissions cap (GtCO2) (system)
rnwtgt_r(r,t)                    Renewable energy share target (regions)
rnwtgt(t)                        Renewable energy share target (system)
irnwtgt_r(r,t)                   Intermittent renewable energy share target (regions)
irnwtgt(t)                       Intermittent renewable energy share target (system)
coallim_r(r,t)                   Policy constraint on hard coal phase out (regions)
coallim(t)                       Policy constraint on hard coal phase out (system)
lignlim_r(r,t)                   Policy constraint on lignite phase out (regions)
lignlim(t)                       Policy constraint on lignite phase out (system)
nuclim_r(r,t)                    Policy constraint on nuclear phase out (regions)
nuclim(t)                        Policy constraint on nuclear phase out (system)
gaslim(r,t)                      Natural gas bugdets (TWh)
gaslim_eu(t)                     Natural gas bugdets (TWh)
;

$gdxin precal\precal_%n%.gdx
$load co2p
$load co2cap_r
$load co2cap
$load rnwtgt_r
$load rnwtgt
$load irnwtgt_r
$load irnwtgt
$load coallim_r
$load coallim
$load lignlim_r
$load lignlim
$load nuclim_r
$load nuclim
$load gaslim
$load gaslim_eu
$gdxin

* * * market_out
set
ngclass          Natural gas supply classes
dbclass          Dedicated biomass supply classes
;

$gdxin precal\precal_%n%.gdx
$load ngclass, dbclass
$gdxin

parameter
ngref_r(r,t)                     Reference natural gas consumption (EJ) (regional)
ngref(t)                         Reference natural gas consumption (EJ) (system)
ngelas_r(r,t)                    Supply elasticity for natural gas (regional)
ngelas(t)                        Supply elasticity for natural gas (system)
ngcost_r(ngclass,r,t)            Cost of natural gas by supply class (EUR per MWh th) (regional)
ngcost(ngclass,t)                Cost of natural gas by supply class (EUR per MWh th) (system)
nglim_r(ngclass,r,t)             Class size of natural gas supply function (regional)
nglim(ngclass,t)                 Class size of natural gas supply function (system)
dbref_r(r,t)                     Reference dedicated biomass consumption (EJ) (regional)
dbref(t)                         Reference dedicated biomass consumption (EJ) (system)
dbelas_r(r,t)                    Supply elasticity for dedicated biomass (regional)
dbelas(t)                        Supply elasticity for dedicated biomass (system)
dbcost_r(dbclass,r,t)            Cost of dedicated biomass by supply class (EUR per MWh th) (regional)
dbcost(dbclass,t)                Cost of dedicated biomass by supply class (EUR per MWh th) (system)
dblim_r(dbclass,r,t)             Class size of dedicated biomass supply function (regional)
dblim(dbclass,t)                 Class size of dedicated biomass supply function (system)
;

$gdxin precal\precal_%n%.gdx
$load ngref, ngref_r, ngelas, ngelas_r, ngcost, ngcost_r, nglim, nglim_r
$load dbref, dbref_r, dbelas, dbelas_r, dbcost, dbcost_r, dblim, dblim_r
$gdxin

* * * Demand module
$if set elastic         $include modules\euregen2024_elastic_v1.gms
* * * Energy efficiency module
* * * Social cost module
$if set socialcost      $include modules\euregen2024_socialcost_v1.gms
* * * Subsidies and taxes (negative subsidies are taxes)
$if set subsidiestaxes  $include modules\euregen2024_subsidiestaxes_v1.gms

Parameter
ghoursnv(j,r,t)
* Merging all vintages
dischrgnv(j,r)
chrgpennv(j,r)
dchrgpennv(j,r)
* Endbalance
storage_endbalance(j,v,r,t)
storage_endbalancenv(j,r,t)
;

$gdxin precal\precal_%n%.gdx
$load dischrgnv,storage_endbalance,storage_endbalancenv
$gdxin

ghoursnv(nvj(j),r,topt2023(t))$(sum(oldv(v), gcapt(j,v,r,t)) > 0)       = sum(oldv(v), gcapt(j,v,r,t) * ghours(j,v,r)) / sum(oldv(v), gcapt(j,v,r,t)) ;
chrgpennv(nvj(j),r)$(sum(oldv(v), gcapt(j,v,r,"2023")) > 0)     = sum(oldv(v), gcapt(j,v,r,"2023") * chrgpen(j,v,r)) / sum(oldv(v), gcapt(j,v,r,"2023")) ;
dchrgpennv(nvj(j),r)$(sum(oldv(v), gcapt(j,v,r,"2023")) > 0)   = sum(oldv(v), gcapt(j,v,r,"2023") * dchrgpen(j,v,r)) / sum(oldv(v), gcapt(j,v,r,"2023")) ;
ghoursnv(nvj(j),r,topt2024(t))   = ghoursnv(j,r,"2023") ;


* * * Dispatch cost
parameter
discost(i,v,r,t)         Dispatch cost (EUR per MWh el)
discost_int(i,v,r,t)         Dispatch cost (EUR per MWh el)
voll(r,t) 
;

$gdxin precal\precal_%n%.gdx
$load discost, discost_int=discost
*,voll
$gdxin

$if      set nochp  discost(chp(i),v,r,t) = discost_int(i,v,r,t) + 60 ;

voll(r,t) = 3000 ;

$if not  set hours  $include    modules\euregen2024_callmin_v1.gms

* * * Declare Model
positive variable
* Generation
XTWH(i,v,r,t)           Annual generation for sparsity purposes (TWh)
XTWHIRNW(r,t)           Annual generation for sparsity purposes (TWh)
XC(i,v,r,t)             Installed generation capacity (GW)
IX(i,r,t)               New vintage investment (total GW to be added from t-1 to t) (GW)
* Storage
GC(j,v,r,t)             Energy storage charge-discharge capacity (GW)
GCC(j,v,r,t)            Energy storage charge capacity (GW)
GCD(j,v,r,t)            Energy storage discharge capacity (GW)
GCR(j,v,r,t)            Energy storage charge-discharge capacity (GWh)
GCNV(j,r,t)             Energy storage charge-discharge capacity (GW)
IG(j,r,t)               Investment in storage charge-discharge capacity (GW)
IGC(j,r,t)              Investment in storage charge capacity (GW)
IGD(j,r,t)              Investment in storage discharge capacity (GW)
IGR(j,r,t)              Investment in storage reservoir capacity (GW)
* Transmission
TC(k,r,r,t)             New Trade flow capacity (GW)
IT(k,r,r,t)             Investment in transmission (total GW to be added from t-1 to t)
* Market and policy variables
DBS(dbclass,t)          Dedicated biomass supply by class (MWh)
DBSR(dbclass,r,t)       Dedicated biomass supply by class by region (MWh)
NGS(ngclass,t)          Total supply of natural gas by class (MWh)
NGSR(ngclass,r,t)       Total supply of natural gas by class by region (MWh)
* Potential variables
BC_r(r,t)               Annual flow of biomass used (TWh)
BC(t)                   Annual flow of biomass used (TWh)
GASC_r(r,t)             Annual flow of natural gas used (TWh)
GASC(t)                 Annual flow of natural gas used (TWh)
SC_r(r,t)               Annual flow of geologically stored carbon (MtCO2)
SC(t)                   Annual flow of geologically stored carbon (MtCO2)
$ontext
* * Hydrogen
* Hydrogen generation
PX(s,p,v,r,t)           Hydrogen generation dispatch by segment (GWh)
PXC(p,v,r,t)            Capacity of hydrogen generation process (GW)
PXCS(s,p,v,r,t)         Copies in S (GW)
IP(p,r,t)               Investment into hydrogen generatin capacity (GWh)
* Hydrogen burning
BX(s,b,v,r,t)           Hydrogen burning dispatch by segment (GWh)
BXC(b,v,r,t)            Capacity of hydrogen burning process (GW)
BXCS(s,b,v,r,t)         Copies in S (GW)
IB(b,r,t)               Investment into hydrogen burning capacity (GWh)
* Storage
GP(s,gp,v,r,t)          Hydrogen storage charge (GW)
GPD(s,gp,v,r,t)         Hydrogen storage discharge (GW)
GPC(gp,v,r,t)           Hydrogen storage charge-discharge capacity (GW)
GPCS(s,gp,v,r,t)        Copies in S (GW)
GPB(s,gp,v,r,t)         Hydrogen storage accumulated balance (TWh)
IGP(gp,r,t)             Investment in hydrogen storage charge-discharge capacity (GW)
* Transport
EP(s,kp,r,r,t)          Bilateral hydrogen trade flows by load segment (GW)
TPC(kp,r,r,t)           New hydrogen trade flow capacity (GW)
TPCS(s,kp,r,r,t)        Copies in S (GW)
ITP(kp,r,r,t)           Investment in hydrogen transmission (total GW to be added from t-1 to t)
$offtext
;

variable
WELFARE                         WELFARE (negative value) (million EUR)
SURPLUS                         Cost (positive) (million EUR)
* Hydrogen
*HIMPORT(s,r,t)
*HIMPORTANN(r,t)
;

equation
objdef                           Objection function -- definition of cost
* Generation investment equations
invest_new(i,v,r,t)              Accumulation of annual investment flows
invest_old(i,v,r,t)              Accumulation of annual investment flows
exlife(i,v,r,t)                  Existing capacity including conversions
exlife2023(i,v,r,t)              Existing capacity in 2020 is fix
exlife_chp(i,v,r,t)
exlife2023_chp(i,v,r,t)          Existing capacity in 2020 is fix
exlife2030_chp(i,v,r,t)
exlife_bio(i,v,r,t)              Existing capacity of bioenergy is fix to avoid decomm (no subsidy implemented yet)
exlife2023_bio(i,v,r,t)          Existing capacity in 2020 is fix
exlife2030_bio(i,v,r,t)          Existing capacity of bioenergy is fix to avoid decomm (no subsidy implemented yet)
newlife(i,v,r,t)                 New vintages are subject to lifetime constraint
retire(i,v,r,t)                  Monotonicity constraint on installed capacity
investlimUP(i,r,t)               Upper limits on investment (region)
investlimLO(i,r,t)               Lower limits on investment (region)
investlimUP_eu(i,t)              Upper limits on investment (system)
investlimUP_irnw(i,r,t,quantiles) Upper limits on investment for intermittent renewables per quantile (region)
* Storage investment equations
ginvest_new(j,v,r,t)             Investment in storage charge-discharge capacity
ginvest_old(j,v,r,t)             Investment in storage charge-discharge capacity
gexlife(j,v,r,t)                 Existing storage capacity
gexlifenv2023(j,r,t)                 Existing storage capacity
gexlifenv(j,r,t)                 Existing storage capacity
gexlife2023(j,v,r,t)             Existing capacity in 2020 is fix
gexlife_pump(j,v,r,t)            Existing capacity of pumphydro is fix to avoid decomm (for the myopic runs)
gexlife_res(j,v,r,t)             Existing capacity of reservoir hydro is fix to avoid decomm (for the myopic runs)
gnewlife(j,v,r,t)                Storage wew vintages are subject to lifetime constraint
gretire(j,v,r,t)                 Monotonicity constraint on installed storage capacity
gretirenv(j,r,t)                 Monotonicity constraint on installed storage capacity
ginvestlimUP(j,r,t)              Upper limits on storage investment (region)
ginvestlimLO(j,r,t)              Lower limits on storage investment (region)
ginvestlimUP_eu(j,t)             Upper limits on storage investment (EU)
* Variable energy-to-power-ratios
ginvestc_new(j,v,r,t)            Investment in storage charge capacity
ginvestd_new(j,v,r,t)            Investment in storage discharge capacity
ginvestr_new(j,v,r,t)            Investment in storage reservoir capacity
gnewlifec(j,v,r,t)               Storage wew vintages are subject to lifetime constraint
gnewlifed(j,v,r,t)               Storage wew vintages are subject to lifetime constraint
gnewlifer(j,v,r,t)               Storage wew vintages are subject to lifetime constraint
gretirec(j,v,r,t)                Monotonicity constraint on installed storage capacity
gretired(j,v,r,t)                Monotonicity constraint on installed storage capacity
gretirer(j,v,r,t)                Monotonicity constraint on installed storage capacity
* Transmission equations
tinvestexi(k,r,r,t)              Accumulation of annual transmission investment flows
tinvestnew(k,r,r,t)              Accumulation of annual transmission investment flows
tinvestlimUP(k,r,r,t)            Upper limit on total transmission capacity (between regions)
tinvestlimLO(k,r,r,t)            Lower limit on total transmission capacity (between regions)
tinvestlimUP_eu(k,t)             Upper limit on per period transmission investment (EU)
* Market equations
biomarket(t)                     System-wide market for bioenergy or supply equal demand for bioenergy
biomarket_r(r,t)                 Supply equal demand for bioenergy (regional)
gasmarket(t)                     System-wide market for natural gas or supply equal demand for natural gas
gasmarket_r(r,t)                 Supply equal demand for natural gas (regional)
* Policy equations
rnwtgtmarket(t)                  Target market for renewables (system)
rnwtgtmarket_r(r,t)              Target market for renewables (regional)
irnwtgtmarket(t)                 Target market for intermittent renewables (system)
irnwtgtmarket_r(r,t)             Target market for interimittet renewables (regional)
coalphaseout(t)                  Policy constraint on hard coal phase out
coalphaseout_r(r,t)              Policy constraint on hard coal phase out (regions)
lignphaseout(t)                  Policy constraint on lignite phase out
lignphaseout_r(r,t)              Policy constraint on lignite phase out (regions)
nucphaseout(t)                   Policy constraint on nuclear phase out
nucphaseout_r(r,t)               Policy constraint on nuclear phase out (regions)
* Limit equations
bioflow(t)                       Annual flow of biomass
bioflow_r(r,t)                   Annual flow of biomass (per region)
cumbio(t)                        Limits on cumulative use of bioenergy (per region)
cumbio_r(r,t)                    Limits on cumulative use of bioenergy (per region)
gasflow(t)                       Annual flow of gasmass
gasflow_r(r,t)                   Annual flow of gasmass (per region)
cumgas(t)                        Limits on cumulative use of gasenergy (per region)
cumgas_r(r,t)                    Limits on cumulative use of gasenergy (per region)
ccsflow(t)                       Annual flow of captured carbon for geologic storage
ccsflow_r(r,t)                   Annual flow of captured carbon for geologic storage
cumccs                           Limits on cumulative geologic storage of carbon
cumccs_r(r)                      Limits on cumulative geologic storage of carbon
* Structual equations
xtwhdef(i,v,r,t)                 Calculate XTWH from X
xtwhirnwdef(r,t)                 Calculate XTWHIRNW from XIRNW
$ontext
* * Hydrogen
demand_hydrogen(s,r,t)
* Hydrogen generation
pcapacity(s,p,v,r,t)              Generation capacity constraint on dispatch
pinvest(p,v,r,t)                  Accumulation of annual investment flows
pnewlife(p,v,r,t)                 New vintages are subject to lifetime constraint
pretire(p,v,r,t)                  Monotonicity constraint on installed capacity
* Hydrogen burning
bcapacity(s,b,v,r,t)              Generation capacity constraint on dispatch
binvest(b,v,r,t)                  Accumulation of annual investment flows
bnewlife(b,v,r,t)                 New vintages are subject to lifetime constraint
bretire(b,v,r,t)                  Monotonicity constraint on installed capacity
* Hydrogen storage
pchargelim(s,gp,v,r,t)            Charge cannot exceed capacity
pdischargelim(s,gp,v,r,t)         Discharge cannot exceed capacity
preservoirlim(s,gp,v,r,t)         Storage reservoir limit
pstoragebal(s,gp,v,r,t)           Storage balance accumulation 
pstoragebal0(s,gp,v,r,t)          Storage balance accumulation begin ending
pstoragebal_endbalance(gp,v,r,t)  Storage balance accumulation begin ending
pstoragebalann(gp,v,r,t)          Storage annual balance accumulation 
gpinvest(gp,v,r,t)                Investment in storage reservoir capacity
gpnewlife(gp,v,r,t)               Storage new vintages are subject to lifetime constraint
gpretire(gp,v,r,t)                Monotonicity constraint on installed storage capacity
gpinvestlimUP(gp,r,t)             Upper limits on storage investment (region)
gpinvestlimLO(gp,r,t)             Lower limits on storage investment (region)
gpinvestlimUP_eu(gp,t)            Upper limits on storage investment (EU)
* Hydrogen transport
tpcapacity(s,kp,r,r,t)            Hydrogen transmission capacity constraint on trade
tpinvestexi(kp,r,r,t)             Accumulation of annual transmission investment flows
tpinvestnew(kp,r,r,t)             Accumulation of annual transmission investment flows
tpinvestlimUP(kp,r,r,t)           Upper limit on total transmission capacity (between regions)
tpinvestlimLO(kp,r,r,t)           Lower limit on total transmission capacity (between regions)
tpinvestlimUP_eu(kp,t)            Upper limit on per period transmission investment (EU)
$offtext
;

$if set ramping     $include modules\euregen2024_ramping_v1.gms              
$if set lbdreg      $include modules\euregen2024_lbdreg_v1.gms
$if set lbdeur      $include modules\euregen2024_lbdeur_v1.gms
$if set lbdreg      $include modules\euregen2024_lbsreg_v1.gms
$if set lbdeur      $include modules\euregen2024_lbseur_v1.gms
$if set lbdflhreg   $include modules\euregen2024_lbsflhreg_v1.gms
$if set lbdflheur   $include modules\euregen2024_lbsflheur_v1.gms

set
ivt(i,v,t)              Active vintage-capacity blocks aggregated to European metric
ivttv(i,v,t)
ivrttv(i,v,r,t)
jvrttv(j,v,r,t)
tvrttv(k,v,r,t)
notir_lea(i,r)
notkir_lea(i,r)
ivrt_nottv(i,v,r,t)
jvrt_nottv(j,v,r,t)
v2022_2023(v)
;

$if not  set hours  $gdxin precal\precal_%n%.gdx
$if      set hours  $gdxin precal\precal_%year%_%n%.gdx
$load ivt, ivttv, ivrttv, jvrttv, tvrttv, notir_lea, notkir_lea, ivrt_nottv, jvrt_nottv, v2022_2023
$gdxin


$ontext
* * * Spillover
parameter
lag time period lag for LBD spillover
klag time period lag for LBS spillover
spill(r,r) LBD spillover between region pair (1 = perfect spillover -> European LBD)
kspill(r,r) LBS spillover between region pair (1 = perfect spillover --> European LBS)
spilllag(r,r,lag) LBD spillover between region pair (1 = perfect spillover -> European LBD) in dependency of lag
kspilllag(r,r,klag) LBS spillover between region pair (1 = perfect spillover -> European LBS) in dependency of lag
;
$offtext

$ontext
parameter\
hdemandannr(r,t)
hdemand(s,r,t)
;

demand_hydrogen_nostorage(r,t)$(toptimize(t))..
    hdemandann(r,t) =e= sum(s, hours(s) * (sum(pvrt(p,v,r,t), PX(s,p,v,r,t)))) + HIMPORTANN(r,t) ; 
demand_hydrogen(s,r,t)$(toptimize(t))..
    hours(s) * hdemands(s,r,t) =e= hours(s) * (sum(pvrt(p,v,r,t), PX(s,p,v,r,t))
* Discharge less charge
$if      set hydrogenstorage                 + sum(gpvrt(gp,v,r,t), GPD(s,gp,v,r,t) * gpdchrgpen(gp,v) - GP(s,gp,v,r,t))
* Imports within Europe
$if      set hydrogentrans                   + sum(tprt(kp,v,rr,r,t), EP(s,kp,rr,r,t))
* Exports export within Europe
$if      set hydrogentrans                   + sum(tprt(kp,v,r,rr,t), EP(s,kp,r,rr,t) / tppen(kp,r,rr,t))
* Net-Import with outside Europe                                    
                                            + HIMPORT(s,r,t)) ;
$offtext
* * * * * Generation equations               
* * * Investment flows accumulate as new vintage capacity
invest_new(ivrttv(i,newv(v),r,t))..
         XC(i,v,r,t) =e= IX(i,r,t) ;
invest_old(ivrttv(i,v2022_2023(v),r,t))..
         XC(i,v,r,t) =e= IX(i,r,t) ;
*         XC(i,v,r,t) =e= capt(i,v,r,t) ;
* * * Existing vintages have fixed lifetime
* Cannot be decommissioned in 2020
exlife2023(ivrt_nochp(i,oldv(v),r,topt2023(t)))$(not bio(i))..
$if not  set static  XC(i,v,r,t) =l= capt(i,v,r,t) * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;      
* Standard exlife constraint
exlife(ivrt_nochp(i,oldv(v),r,topt2024(t)))$(not bio(i))..
$if not  set static  XC(i,v,r,t) =l= capt(i,v,r,"2023") * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;     
* CHP plants (without Bio-CHP) cannot be decommissioned before lifetime until 2030
exlife2023_chp(ivrt(chp(i),oldv(v),r,topt2023(t)))..
$if not  set static  XC(i,v,r,t) =e= capt(i,v,r,t) * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;       
exlife2030_chp(ivrt(chp(i),oldv(v),r,topt2024_2030(t)))..
$if not  set static  XC(i,v,r,t) =e= capt(i,v,r,"2023") * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;
exlife_chp(ivrt(chp(i),oldv(v),r,topt2030plus(t)))..
$if not  set static  XC(i,v,r,t) =l= capt(i,v,r,"2023") * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;
* Bioenergy or Bio-CHP cannot be decommissioned before lifetime until 2030
exlife2023_bio(ivrt_nochp(bio(i),oldv(v),r,topt2023(t)))..
$if not  set static  XC(i,v,r,t) =e= capt(i,v,r,"2023") * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;
exlife2030_bio(ivrt_nochp(bio(i),oldv(v),r,topt2024_2030(t)))..
$if not  set static  XC(i,v,r,t) =e= capt(i,v,r,"2023") * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;
exlife_bio(ivrt_nochp(bio(i),oldv(v),r,topt2030plus(t)))..
$if not  set static  XC(i,v,r,t) =l= capt(i,v,r,"2023") * lifetime(i,v,r,t) ;
$if      set static  XC(i,v,r,t) =g= 0 ;
* * * New vintages have a lifetime profile for enforced retirement
newlife(ivrt_nottv(i,newv(v),r,t))$(not sameas(v,"2050"))..
        XC(i,v,r,t) =l= lifetime(i,v,r,t) * sum(tv(tt,v), IX(i,r,tt)) ;
* * * All vintages must be monotonically decreasing (except 2050)
* For myopic runs the tstatic not has to be here because otherwise step-in-step-out of capacities is possible
retire(ivrt(i,v,r,topt2024(t)))$(not sameas(v,"2050"))..
        XC(i,v,r,t+1) =l= XC(i,v,r,t);
* * * Upper and lower limits on investments based on current pipeline or other regional constraints
* Upper limit
investlimUP(i,r,topt2024(t))$(invlimUP(i,r,t) < inf and invlimUP(i,r,t) > 0)..
         IX(i,r,t) =l= invlimUP(i,r,t) ;
* Lower limit
investlimLO(i,r,topt2024(t))$(invlimLO(i,r,t) > 0)..
         IX(i,r,t) =g= invlimLO(i,r,t) ;       
* Upper limit for whole system (in general deactivated)
investlimUP_eu(i,topt2024(t))$(invlimUP_eu(i,t) > 0 and invlimUP_eu(i,t) < inf)..
         sum(r, IX(i,r,t)) =l= invlimUP_eu(i,t) ;
* IRNW expansion limit
investlimUP_irnw(irnw_nohydro(new(i)),r,topt2024(t),quantiles)$(irnwlimUP_quantiles(i,r,quantiles))..       
        sum(irnw_mapq(i,quantiles)$(not sameas(quantiles,"qexi")), sum(v, XC(i,v,r,t))) 
        =l= irnwlimUP_quantiles(i,r,quantiles) - sum(irnw_mapq(i,quantiles)$(sameas(quantiles,"qexi")), sum(v, XC(i,v,r,t))) / 5 ;

* * * * * Transmission equations
* Accumulation of transmission capacity investments
tinvestexi(tmapopt(k,r,rr,t2022(t)))..
         TC(k,r,rr,t) =l= tcap(k,r,rr) + IT(k,r,rr,t) ;
tinvestnew(tmapopt(k,r,rr,t2022plus(t)))..
         TC(k,r,rr,t) =l= IT(k,r,rr,t) + TC(k,r,rr,t-1) ;
* Upper limit
tinvestlimUP(tmapopt_invup(k,r,rr,t))..
         TC(k,r,rr,t) =l= tinvlimUP(k,r,rr,t) ;
* Lower limit
tinvestlimLO(tmapopt_invlo(k,r,rr,t))..
         TC(k,r,rr,t) =g= tinvlimLO(k,r,rr,t) ;
* Upper limit for whole system (in general deactivated)
tinvestlimUP_eu(k,toptimize(t))$(tinvlimUP_eu(k,t) < inf)..
         sum((r,rr), TC(k,r,rr,t)) =l= tinvlimUP_eu(k,t) ;

* * * * * Storage equations  
* * * Allow accumulation of storage charge capacity investments
ginvest_new(
$if not  set storagebalnv   jvrttv(j,newv(v),r,t))..
$if      set storagebalnv   jvrttv(nonvj(j),newv(v),r,t))..
         GC(j,v,r,t) =e= IG(j,r,t) ;
  
Set
ghyd(j) /Storage_LT/
gbat(j) /Storage_ST/
;
       
ginvestc_new(jvrttv(ghyd(j),newv(v),r,t))..
         GCC(j,v,r,t) =e= IGC(j,r,t) ;
ginvestd_new(jvrttv(ghyd(j),newv(v),r,t))..
         GCD(j,v,r,t) =e= IGD(j,r,t) ;
ginvestr_new(jvrttv(ghyd(j),newv(v),r,t))..
         GCR(j,v,r,t) =e= IGR(j,r,t) ;

ginvest_old(
$if not  set storagebalnv   jvrttv(j,v2022_2023(v),r,t))..
$if      set storagebalnv   jvrttv(nonvj(j),v2022_2023(v),r,t))..
         GC(j,v,r,t) =e= IG(j,r,t) ;        
* * * Existing storage vintages have fixed lifetime
* MM (todo): Think about implementing different constraints for myopic runs since it might be that endogenous decommissioning needs to get disablted anyway
* No decomissioning in 2015
gexlife2023(
$if not  set storagebalnv   jvrt(j,oldv(v),r,topt2023(t)))..
$if      set storagebalnv   jvrt(nonvj(j),oldv(v),r,topt2023(t)))..
         GC(j,v,r,t) =l= gcapt(j,v,r,t) * glifetime(j,v,r,t) ;
* Decommissioning of old capacity is not possible (pumpstorage and reservoir)
gexlife(
$if not  set storagebalnv   jvrt(j,oldv(v),r,topt2024(t)))..
$if      set storagebalnv   jvrt(nonvj(j),oldv(v),r,topt2024(t)))..
$if not  set static  GC(j,v,r,t) =e= gcapt(j,v,r,"2023") * glifetime(j,v,r,t) ;
$if      set static  GC(j,v,r,t) =g= 0 ;
* Avoid decommissioning of pump storage capacity
gexlife_pump(jvrt(jpump(j),oldv(v),r,topt2024(t)))..
$if not  set static  GC(j,v,r,t) =e= gcapt(j,v,r,"2023") * glifetime(j,v,r,t) ;
$if      set static  GC(j,v,r,t) =g= 0 ;
gexlife_res(jvrt(jres(j),oldv(v),r,topt2024(t)))..
$if not  set static  GC(j,v,r,t) =e= gcapt(j,v,r,"2023") * glifetime(j,v,r,t) ;
$if      set static  GC(j,v,r,t) =g= 0 ;
* * * New storage vintages have a lifetime profile for enforced retirement
gnewlife(
$if not  set storagebalnv   jvrt(j,newv(v),r,t))$(not tv(t,v) and not sameas(v,"2050"))..
$if      set storagebalnv   jvrt(nonvj(j),newv(v),r,t))$(not tv(t,v) and not sameas(v,"2050"))..
        GC(j,v,r,t) =l= glifetime(j,v,r,t) * sum(tv(tt,v), IG(j,r,tt)) ;
        
gnewlifec(jvrt(ghyd(j),newv(v),r,t))$(not tv(t,v) and not sameas(v,"2050"))..
        GCC(j,v,r,t) =l= glifetime(j,v,r,t) * sum(tv(tt,v), IGC(j,r,tt)) ;
gnewlifed(jvrt(ghyd(j),newv(v),r,t))$(not tv(t,v) and not sameas(v,"2050"))..
        GCD(j,v,r,t) =l= glifetime(j,v,r,t) * sum(tv(tt,v), IGD(j,r,tt)) ;
gnewlifer(jvrt(ghyd(j),newv(v),r,t))$(not tv(t,v) and not sameas(v,"2050"))..
        GCR(j,v,r,t) =l= glifetime(j,v,r,t) * sum(tv(tt,v), IGR(j,r,tt)) ;

* * * All storage vintages must be monotonically decreasing (except 2050)
gretire(
$if not  set storagebalnv   jvrt(j,v,r,t))$(not sameas(v,"2050"))..
$if      set storagebalnv   jvrt(nonvj(j),v,r,t))$(not sameas(v,"2050"))..
        GC(j,v,r,t+1) =l= GC(j,v,r,t) ;

gretirec(jvrt(ghyd(j),v,r,t))$(not sameas(v,"2050"))..
        GCC(j,v,r,t+1) =l= GCC(j,v,r,t) ;
gretired(jvrt(ghyd(j),v,r,t))$(not sameas(v,"2050"))..
        GCD(j,v,r,t+1) =l= GCD(j,v,r,t) ;
gretirer(jvrt(ghyd(j),v,r,t))$(not sameas(v,"2050"))..
        GCR(j,v,r,t+1) =l= GCR(j,v,r,t) ;

* No vintage investment accumulation
gexlifenv2023(nvj(j),r,topt2023(t))..
         GCNV(j,r,t) =e= sum(jvrt(j,oldv(v),r,t), gcapt(j,v,r,t) * glifetime(j,v,r,t)) ;   
gexlifenv(nvj(j),r,topt2024(t))..
$if not  set static  GCNV(j,r,t) =e= sum(jvrt(j,oldv(v),r,t), gcapt(j,v,r,"2023") * glifetime(j,v,r,t)) ;   
$if      set static  GCNV(j,r,t) =g= 0 ;
gretirenv(nvj(j),r,topt2024(t))$(not sameas(t,"2050"))..
        GCNV(j,r,t+1) =l= GCNV(j,r,t) ;

* * * Upper and lower limits
* Upper limit
ginvestlimUP(newj(j),r,t)$(t.val ge 2024 and toptimize(t) and ginvlimUP(j,r,t) > 0 and ginvlimUP(j,r,t) < inf)..
         IG(j,r,t) =l= ginvlimUP(j,r,t) ;
* Lower limit
ginvestlimLO(newj(j),r,t)$(t.val ge 2024 and toptimize(t) and ginvlimLO(j,r,t) > 0)..
         IG(j,r,t) =g= ginvlimLO(j,r,t) ;
* Upper limits for whole system (again mostly inactive)
ginvestlimUP_eu(newj(j),t)$(t.val ge 2024 and ginvlimUP_eu(j,t) > 0 and ginvlimUP_eu(j,t) < inf)..
         sum(r, IG(j,r,t)) =l= ginvlimUP_eu(j,t) ;

* * * Bioenergy market
* for whole system (allows for trade and the marginal is then the "price")
biomarket(t)$(sum(dbclass, dblim(dbclass,t)) and toptimize(t))..
               sum(dbclass, DBS(dbclass,t))     =e= sum(ivrt(bio(i),v,r,t), round(1 / effrate(i,v,r),4) * XTWH(i,v,r,t)) ;
* for each region (does not allow for system-wide trade)
biomarket_r(r,t)$(sum(dbclass, dblim_r(dbclass,r,t)) and toptimize(t))..
               sum(dbclass, DBSR(dbclass,r,t))  =e= sum(ivrt(bio(i),v,r,t), round(1 / effrate(i,v,r),4) * XTWH(i,v,r,t)) ; 
* * * Natural gas market
* for whole system (allows for trade and the marginal is then the "price")
gasmarket(t)$(sum(ngclass, nglim(ngclass,t)) and toptimize(t))..
               sum(ngclass, NGS(ngclass,t))     =e= sum(ivrt(gas(i),v,r,t), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;
* for each region (does not allow for system-wide trade)
gasmarket_r(r,t)$(sum(ngclass, nglim_r(ngclass,r,t)) and toptimize(t))..
               sum(ngclass, NGSR(ngclass,r,t))  =e= sum(ivrt(gas(i),v,r,t), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;

* * * Renewable energy share market
* for whole system (allows for trade and the marginal is then the "price")
rnwtgtmarket(t)$(rnwtgt(t) and toptimize(t))..
        sum(r, sum(ivrt(rnw(i),v,r,t), XTWH(i,v,r,t)))  =g=  rnwtgt(t)     * sum(r, sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ) ;
* for each region (does not allow for system-wide trade)
rnwtgtmarket_r(r,t)$(rnwtgt_r(r,t) and toptimize(t))..
               sum(ivrt(rnw(i),v,r,t), XTWH(i,v,r,t))   =g=  rnwtgt_r(r,t) *        sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ;

* * * Intermittent renewable energy share market
* for whole system (allows for trade and the marginal is then the "price")
irnwtgtmarket(t)$(irnwtgt(t) and toptimize(t))..
        sum(r, sum(ivrt(irnw(i),v,r,t), XTWH(i,v,r,t))) =g= irnwtgt(t)     * sum(r, sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ) ;
* for each region (does not allow for system-wide trade)
irnwtgtmarket_r(r,t)$(irnwtgt_r(r,t) and toptimize(t))..
               sum(ivrt(irnw(i),v,r,t), XTWH(i,v,r,t))  =g= irnwtgt_r(r,t) *        sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) ;

* * * Bioenergy potential
* for whole system (allows for trade and the marginal is then the "price")
bioflow(t)$(biolim_eu(t) > 0 and biolim_eu(t) < inf and toptimize(t))..         BC(t)           =e= sum(ivrt(bio(i),v,r,t), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;
cumbio(t)$(biolim_eu(t) > 0 and biolim_eu(t) < inf and toptimize(t))..          BC(t)           =l= biolim_eu(t) ;
* for each region (does not allow for system-wide trade)
bioflow_r(r,t)$(biolim(r,t) > 0 and biolim(r,t) < inf and toptimize(t))..       BC_r(r,t)       =e= sum(ivrt(bio(i),v,r,t)$(not sameas(i,"Bio_CHP")), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;
cumbio_r(r,t)$(biolim(r,t) > 0 and biolim(r,t) < inf and toptimize(t))..        BC_r(r,t)       =l= biolim(r,t) ;        
* * * Gas budget equations
* for whole system (allows for trade and the marginal is then the "price")
gasflow(t)$(gaslim_eu(t) > 0 and gaslim_eu(t) < inf and toptimize(t))..         GASC(t)         =e= sum(ivrt(gas(i),v,r,t), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;
cumgas(t)$(gaslim_eu(t) > 0 and gaslim_eu(t) < inf and toptimize(t))..          GASC(t)         =l= gaslim_eu(t) ;
* for each region (does not allow for system-wide trade)
gasflow_r(r,t)$(gaslim(r,t) > 0 and gaslim(r,t) < inf and toptimize(t))..       GASC_r(r,t)     =e= sum(ivrt(gas(i),v,r,t), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;
cumgas_r(r,t)$(gaslim(r,t) > 0 and gaslim(r,t) < inf and toptimize(t))..        GASC_r(r,t)     =l= gaslim(r,t) ;      
* * * Geologic storage of carbon
* for whole system (system-wide constraints allow for trade and the marginal is then the "price")
ccsflow(t)$(toptimize(t))..                        SC(t) =e=  sum(r, sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWH(i,v,r,t))) * 1e-3 ;
cumccs..                                         sum(t, nyrs(t) * SC(t)) =l= sclim_eu ;
* for each region (does not allow for system-wide trade)
ccsflow_r(r,t)$(toptimize(t))..                    SC_r(r,t)  =e= sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWH(i,v,r,t)) * 1e-3 ;
cumccs_r(r)..                                    sum(t, nyrs(t) * SC_r(r,t)) =l= sclim(r) ;

$if      set hours                      $include    modules\euregen2024_hours_v1.gms
$if not  set days   $if not  set hours  $include    modules\euregen2024_segments_v4.gms
$if      set days                       $include    modules\euregen2024_days_v6.gms

* * * Structural equations to aid solver
xtwhdef(ivrt(notirnw(i),v,r,toptimize(t)))..           XTWH(i,v,r,t) =e=
$if not  set days   1e-3 * sum(s, X(s,i,v,r,t) * hours(s)) ;
$if      set days   1e-3 * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))) ;
xtwhirnwdef(r,toptimize(t))..                         XTWHIRNW(r,t) =e=
$if not  set days   1e-3 * sum(s, XIRNW(s,r,t) * hours(s)) ;
$if      set days   1e-3 * sum(sd, days(sd) * sum(hd, XIRNW_D(sd,hd,r,t))) ;

* * * Calibration equations
equation
* Import and export
impUP(r,t)
impLO(r,t)
expUP(r,t)
expLO(r,t)
expUP_all(t)
expLO_all(t)
* Coal and lignite usage
gen2030(r,t)
gen2029(r,t)
genUP_oil(r,t)
genLO_oil(r,t)
genUP_ngas(r,t)
genLO_ngas(r,t)
genUP_coal(r,t)
genLO_coal(r,t)
genUP_lign(r,t)
genLO_lign(r,t)
genUP_biom(r,t)
genLO_biom(r,t)
genUP_nucl(r,t)
genLO_nucl(r,t)
genUP_irnw(r,t)
genLO_irnw(r,t)
genUP_hydr(r,t)
genLO_hydr(r,t)
genUP_wind(r,t)
genLO_wind(r,t)
genUP_sola(r,t)
genLO_sola(r,t)
genUP_resv(r,t)
genLO_resv(r,t)
genUP_pump(r,t)
genLO_pump(r,t)
genUP_resv_all(t)
genLO_resv_all(t)
genUP_pump_all(t)
genLO_pump_all(t)
* Lignite fuel
genUP_lignfuel(r,t)
* CHP
genUP_oil_chp(r,t)
genLO_oil_chp(r,t)
genUP_coal_chp(r,t)
genLO_coal_chp(r,t)
genUP_lign_chp(r,t)
genLO_lign_chp(r,t)
genUP_biom_chp(r,t)
genLO_biom_chp(r,t)
genUP_ngas_chp(r,t)
genLO_ngas_chp(r,t)
* force CHP when CHP is not modeled
genLO_oil_forcechp(r,t)
genLO_coal_forcechp(r,t)
genLO_lign_forcechp(r,t)
genLO_biom_forcechp(r,t)
genLO_ngas_forcechp(r,t)
genUP_oil_forcechp(r,t)
genUP_coal_forcechp(r,t)
genUP_lign_forcechp(r,t)
genUP_biom_forcechp(r,t)
genUP_ngas_forcechp(r,t)
* NOCHP
genUP_oil_nochp(r,t)
genLO_oil_nochp(r,t)
genUP_coal_nochp(r,t)
genLO_coal_nochp(r,t)
genUP_lign_nochp(r,t)
genLO_lign_nochp(r,t)
genUP_biom_nochp(r,t)
genLO_biom_nochp(r,t)
genUP_ngas_nochp(r,t)
genLO_ngas_nochp(r,t)
;

impUP(r,topt2023(t))$(imp(r,t) > 0)..
$if not  set days   sum(rr,  sum((s,tmap(k,rr,r)), hours(s) * E(s,k,rr,r,t))) =l= imp(r,t) * 1.03 ;
$if      set days   sum(rr,  sum(sd, days(sd) * sum(tmap(k,rr,r), sum(hd, E_D(sd,hd,k,rr,r,t))))) =l= imp(r,t) * 1.03 ;

expUP(r,topt2023(t))$(expo(r,t) > 0)..
$if not  set days   sum(rr, sum((s,k)$tmap(k,rr,r), hours(s) * E(s,k,r,rr,t))) =l= expo(r,t) * 1.03 ;
$if      set days   sum(rr, sum(sd, days(sd) * sum(k$tmap(k,rr,r), sum(hd, E_D(sd,hd,k,r,rr,t))))) =l= expo(r,t) * 1.03 ;

impLO(r,topt2023(t))$(imp(r,t) > 0)..
$if not  set days   sum(rr,  sum((s,k)$tmap(k,rr,r), hours(s) * E(s,k,rr,r,t))) =g= imp(r,t) * 0.97 ;
$if      set days   sum(rr,  sum(sd, days(sd) * sum(k$tmap(k,rr,r), sum(hd, E_D(sd,hd,k,rr,r,t))))) =g= imp(r,t) * 0.97 ;

expLO(r,topt2023(t))$(expo(r,t) > 0)..
$if not  set days   sum(rr, sum((s,k)$tmap(k,rr,r), hours(s) * E(s,k,r,rr,t))) =g= expo(r,t) * 0.97 ;
$if      set days   sum(rr, sum(sd, days(sd) * sum(k$tmap(k,rr,r), sum(hd, E_D(sd,hd,k,r,rr,t))))) =g= expo(r,t) * 0.97 ;

expUP_all(topt2023(t))$(sum(r, expo(r,t)) > 0)..
$if not  set days   sum(r, sum(rr, sum((s,k)$tmap(k,rr,r), hours(s) * E(s,k,r,rr,t)))) * 1e-3 =l= sum(r, expo(r,t)) * 1.03 ;
$if      set days   sum(r, sum(rr, sum(sd, days(sd) * sum(k$tmap(k,rr,r), sum(hd, E_D(sd,hd,k,r,rr,t)))))) * 1e-3 =l= sum(r, expo(r,t)) * 1.03 ;

expLO_all(topt2023(t))$(sum(r, expo(r,t)) > 0)..
$if not  set days   sum(r, sum(rr, sum((s,k)$tmap(k,rr,r), hours(s) * E(s,k,r,rr,t)))) * 1e-3 =g= sum(r, expo(r,t)) * 0.97 ;
$if      set days   sum(r, sum(rr, sum(sd, days(sd) * sum(k$tmap(k,rr,r), sum(hd, E_D(sd,hd,k,r,rr,t)))))) * 1e-3 =g= sum(r, expo(r,t)) * 0.97 ;

* CHP generation is fixed until 2030
genUP_oil_chp(r,topt2030(t))..
    sum(ivrt_chp(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_oil(r,t) * 1.01 ;
genLO_oil_chp(r,topt2030(t))..
    sum(ivrt_chp(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_oil(r,t) * 0.99 ;

genUP_coal_chp(r,topt2030(t))..
    sum(ivrt_chp(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_coal(r,t) * 1.01 ;
genLO_coal_chp(r,topt2030(t))..
    sum(ivrt_chp(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_coal(r,t) * 0.99 ;
    
genUP_lign_chp(r,topt2030(t))..
    sum(ivrt_chp(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_lign(r,t) * 1.01 ;
genLO_lign_chp(r,topt2030(t))..
    sum(ivrt_chp(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_lign(r,t) * 0.99 ;
 
genUP_biom_chp(r,topt2030(t))..
    sum(ivrt_chp(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_biom(r,t) * 1.01 ;
genLO_biom_chp(r,topt2030(t))..
    sum(ivrt_chp(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_biom(r,t) * 0.99 ;   

genUP_ngas_chp(r,topt2030(t))..
    sum(ivrt_chp(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_ngas(r,t) * 1.01 ;
genLO_ngas_chp(r,topt2030(t))..
    sum(ivrt_chp(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_ngas(r,t) * 0.99 ;   

* NOCHP generation is only calibrated for until 2023
genUP_oil_nochp(r,topt2023(t))..
    sum(ivrt_nochp(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_nochp_oil(r,t) * 1.01 ;
genLO_oil_nochp(r,topt2023(t))..
    sum(ivrt_nochp(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_nochp_oil(r,t) * 0.99 ;

genUP_coal_nochp(r,topt2023(t))..
    sum(ivrt_nochp(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_nochp_coal(r,t) * 1.01 ;
genLO_coal_nochp(r,topt2023(t))..
    sum(ivrt_nochp(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_nochp_coal(r,t) * 0.99 ;
    
genUP_lign_nochp(r,topt2023(t))..
    sum(ivrt_nochp(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_nochp_lign(r,t) * 1.01 ;
genLO_lign_nochp(r,topt2023(t))..
    sum(ivrt_nochp(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_nochp_lign(r,t) * 0.99 ;
 
genUP_biom_nochp(r,topt2030(t))..
    sum(ivrt_nochp(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_nochp_biom(r,t) * 1.01 ;
genLO_biom_nochp(r,topt2030(t))..
    sum(ivrt_nochp(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_nochp_biom(r,t) * 0.99 ;   
    
genUP_ngas_nochp(r,topt2023(t))..
    sum(ivrt_nochp(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_nochp_ngas(r,t) * 1.01 ;
genLO_ngas_nochp(r,topt2023(t))..
    sum(ivrt_nochp(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_nochp_ngas(r,t) * 0.99 ;
    
* Generation from all technologies is only calibrated for until 2023 (when not chp)
genUP_oil(r,topt2023(t))..
    sum(ivrt(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= (gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) * 1.01 ;
genLO_oil(r,topt2030(t))$(gen_min2_chp_oil(r,t) > 0)..
    sum(ivrt(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_oil(r,t) * 0.99 ;

genUP_ngas(r,topt2023(t))..
    sum(ivrt(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= (gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) * 1.01 ;
genLO_ngas(r,topt2030(t))$(gen_min2_chp_ngas(r,t) > 0)..
    sum(ivrt(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_ngas(r,t) * 0.99 ;

genUP_lign(r,topt2023(t))..
    sum(ivrt(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= (gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) * 1.01 ;
genLO_lign(r,topt2030(t))$(gen_min2_chp_lign(r,t) > 0)..
    sum(ivrt(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_lign(r,t) * 0.99 ;
    
genUP_coal(r,topt2023(t))..
    sum(ivrt(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= (gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) * 1.01 ;
genLO_coal(r,topt2030(t))$(gen_min2_chp_coal(r,t) > 0)..
    sum(ivrt(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_coal(r,t) * 0.99 ;
    
genUP_biom(r,topt2023(t))..
    sum(ivrt(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= (gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) * 1.01 ;
genLO_biom(r,topt2030(t))$(gen_min2_chp_biom(r,t) > 0)..
    sum(ivrt(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_biom(r,t) * 0.99 ;
    
genUP_nucl(r,topt2023(t))$(gen_min2_nucl(r,t) > 0)..
    sum(ivrt(nuc(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_nucl(r,t) * 1.00 ;
genLO_nucl(r,topt2023(t))$(gen_min2_nucl(r,t) > 0)..
    sum(ivrt(nuc(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_nucl(r,t) * 0.99 ;
    
* Force CHP generation from 2024 to 2030
genLO_oil_forcechp(r,topt2024(t))$(gen_min2_chp_oil(r,t) > 0)..
    sum(ivrt(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_oil(r,t) * 0.99 ;
genLO_coal_forcechp(r,topt2024(t))$(gen_min2_chp_coal(r,t) > 0)..
    sum(ivrt(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_coal(r,t) * 0.99 ;
genLO_lign_forcechp(r,topt2024(t))$(gen_min2_chp_lign(r,t) > 0)..
    sum(ivrt(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_lign(r,t) * 0.99 ;
genLO_biom_forcechp(r,topt2024(t))$(gen_min2_chp_biom(r,t) > 0)..
    sum(ivrt(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_biom(r,t) * 0.99 ;   
genLO_ngas_forcechp(r,topt2024(t))$(gen_min2_chp_ngas(r,t) > 0)..
    sum(ivrt(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =g= gen_min2_chp_ngas(r,t) * 0.99 ;   

genUP_oil_forcechp(r,topt2024(t))$(gen_min2_chp_oil(r,t) > 0)..
    sum(ivrt(oil(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_oil(r,t) * 1.01 ;
genUP_coal_forcechp(r,topt2024(t))$(gen_min2_chp_coal(r,t) > 0)..
    sum(ivrt(coa(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_coal(r,t) * 1.01 ;
genUP_lign_forcechp(r,topt2024(t))$(gen_min2_chp_lign(r,t) > 0)..
    sum(ivrt(lig(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_lign(r,t) * 1.01 ;
genUP_biom_forcechp(r,topt2024(t))$(gen_min2_chp_biom(r,t) > 0)..
    sum(ivrt(bio(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_biom(r,t) * 1.01 ;   
genUP_ngas_forcechp(r,topt2024(t))$(gen_min2_chp_ngas(r,t) > 0)..
    sum(ivrt(gas(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_min2_chp_ngas(r,t) * 1.01 ;     
* Lignite usage is restricted total maximum production in recent years
genUP_lignfuel(r,topt2024(t))..
    sum(ivrt(lig(i),v,r,t), XTWH(i,v,r,t)) =l= max(gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t),smax(t2023(tt), gen_min2_lign(r,tt))) * 1.00 ;

* Generation from irnw and storage technologies is more complicated
genUP_irnw(r,topt2023(t))$(gen_min2_hydr(r,t) + gen_min2_wind(r,t) + gen_min2_sola(r,t) > 0)..
    XTWHIRNW(r,t) =l= (gen_min2_hydr(r,t) + gen_min2_wind(r,t) + gen_min2_sola(r,t)) * 1.05 ;
genLO_irnw(r,topt2023(t))$(gen_min2_hydr(r,t) + gen_min2_wind(r,t) + gen_min2_sola(r,t) > 0)..
    XTWHIRNW(r,t) =g= (gen_min2_hydr(r,t) + gen_min2_wind(r,t) + gen_min2_sola(r,t)) * 0.95 ;
   
genUP_pump(r,topt2023(t))$(gen_pump(r,t) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =l= gen_pump(r,t) * 1.01 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r)))) * 1e-3 =l= gen_pump(r,t) * 1.01 ;
$if not  set days   $if      set storagebalnv        sum(jpump(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r))) * 1e-3 =l= gen_pump(r,t) * 1.01 ;
$if      set days   $if      set storagebalnv        sum(jpump(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =l= gen_pump(r,t) * 1.01 ;
genLO_pump(r,topt2023(t))$(gen_pump(r,t) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =g= gen_pump(r,t) * 0.50 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r)))) * 1e-3 =g= gen_pump(r,t) * 0.50 ;
$if not  set days   $if      set storagebalnv        sum(jpump(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r))) * 1e-3 =g= gen_pump(r,t) * 0.50 ;
$if      set days   $if      set storagebalnv        sum(jpump(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =g= gen_pump(r,t) * 0.50 ;
    
genUP_resv(r,toptimize(t))$(gen_resv(r,t) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =l= gen_resv(r,t) * 1.01 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r)))) * 1e-3 =l= gen_resv(r,t) * 1.01 ;
$if not  set days   $if      set storagebalnv        sum(jres(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r))) * 1e-3 =l= gen_resv(r,t) * 1.01 ;
$if      set days   $if      set storagebalnv        sum(jres(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =l= gen_resv(r,t) * 1.01 ;
genLO_resv(r,topt2023(t))$(gen_resv(r,t) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =g= gen_resv(r,t) * 0.95 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,r,t) * dchrgpen(j,v,r)))) * 1e-3 =g= gen_resv(r,t) * 0.95 ;
$if not  set days   $if      set storagebalnv        sum(jres(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r))) * 1e-3 =g= gen_resv(r,t) * 0.95 ;
$if      set days   $if      set storagebalnv        sum(jres(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =g= gen_resv(r,t) * 0.95 ;
   
genUP_pump_all(topt2023(t))$(sum(r, gen_pump(r,t)) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =l= sum(r, gen_pump(r,t)) * 1.01 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r)))) * 1e-3 =l= sum(r, gen_pump(r,t)) * 1.01 ;
$if not  set days   $if      set storagebalnv        sum(r, sum(jpump(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =l= sum(r, gen_pump(r,t)) * 1.01 ;
$if      set days   $if      set storagebalnv        sum(r, sum(jpump(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r))))) * 1e-3 =l= sum(r, gen_pump(r,t)) * 1.01 ;
genLO_pump_all(topt2023(t))$(sum(r, gen_pump(r,t)) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =g= sum(r, gen_pump(r,t)) * 0.95 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jpump(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r)))) * 1e-3 =g= sum(r, gen_pump(r,t)) * 0.95 ;
$if not  set days   $if      set storagebalnv        sum(r, sum(jpump(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =g= sum(r, gen_pump(r,t)) * 0.95 ;
$if      set days   $if      set storagebalnv        sum(r, sum(jpump(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r))))) * 1e-3 =g= sum(r, gen_pump(r,t)) * 0.95 ;
    
genUP_resv_all(topt2023(t))$(sum(r, gen_resv(r,t)) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =l= sum(r, gen_resv(r,t)) * 1.03 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r)))) * 1e-3 =l= sum(r, gen_resv(r,t)) * 1.03 ;
$if not  set days   $if      set storagebalnv        sum(r, sum(jres(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =l= sum(r, gen_resv(r,t)) * 1.03 ;
$if      set days   $if      set storagebalnv        sum(r, sum(jres(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r))))) * 1e-3 =l= sum(r, gen_resv(r,t)) * 1.03 ;
genLO_resv_all(topt2023(t))$(sum(r, gen_resv(r,t)) > 0)..
$if not  set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(s, hours(s) * GD(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-3 =g= sum(r, gen_resv(r,t)) * 0.97 ;
$if      set days   $if not  set storagebalnv        sum(jvrt(jres(j),v,r,t), sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,r,t) * dchrgpen(j,v,r)))) * 1e-3 =g= sum(r, gen_resv(r,t)) * 0.97 ;
$if not  set days   $if      set storagebalnv        sum(r, sum(jres(j), sum(s, hours(s) * GDNV(s,j,r,t) * dchrgpennv(j,r)))) * 1e-3 =g= sum(r, gen_resv(r,t)) * 0.97 ;
$if      set days   $if      set storagebalnv        sum(r, sum(jres(j), sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r))))) * 1e-3 =g= sum(r, gen_resv(r,t)) * 0.97 ;

* * * Targets equations
* French nuclear
parameter
frnuc_gen_share(r,t)
;

$gdxin precal\precal_%n%.gdx
$load frnuc_gen_share
$gdxIn

equation
frnuctarget(t)
;

frnuctarget(topt2035plus(t))..
    sum(ivrt(nuc(i),v,rfr(r),t), XTWH(i,v,r,t)) =g= round(frnuc_gen_share("France",t) * daref("France",t) * (1 + lossave("France",t)),4) ; 

VARIABLE
EC(t)               Annual flow of CO2 emissions (MtCO2)
ECEU(t)             Annual flow of CO2 emissions (MtCO2) in European Union (plus Norway and Switzerland and Northern Ireland)
ECUK(t)             Annual flow of CO2 emissions (MtCO2) in UK (not Northern Ireland)
EC_r(r,t)           Annual flow of CO2 emissions (MtCO2)
;

EQUATION
co2flow(t)              Annual flow of CO2 emissions (regional) (Mt)
co2flow_r(r,t)          Annual flow of CO2 emissions (regional) (Mt)
euco2flow(t)            Annual flow of CO2 emissions (system) (Mt)
ukco2flow(t)            Annual flow of CO2 emissions (regional) (Mt)
;

co2flow(toptimize(t))..               EC(t)     =e= sum(r, sum(ivrt(i,v,r,t), emit(i,v,r) * XTWH(i,v,r,t))) ;
co2flow_r(reu(r),toptimize(t))..      EC_r(r,t) =e= sum(ivrt(i,v,r,t), emit(i,v,r) * XTWH(i,v,r,t)) ;
euco2flow(toptimize(t))..             ECEU(t)   =e= sum(reu(r), sum(ivrt(i,v,r,t), emit(i,v,r) * XTWH(i,v,r,t))) ;
ukco2flow(toptimize(t))..             ECUK(t)   =e= sum(rbr(r), sum(ivrt(i,v,r,t), emit(i,v,r) * XTWH(i,v,r,t))) ;

$if      set targets    $include    modules\euregen2024_targets_v1.gms

* Loading routine for parameters
$if      set co2mark    $include    modules\euregen2024_co2cons_v1.gms
$if      set co2iter    $include    modules\euregen2024_co2cons_v1.gms
$if      set co2mips    $include    modules\euregen2024_co2cons_v1.gms
* Loading of necessary equations
$if      set co2mark    $include    modules\euregen2024_co2mark_v1.gms
$if      set co2iter    $include    modules\euregen2024_co2iter_v1.gms
$if      set co2mips    $include    modules\euregen2024_co2mips_v1.gms
   
$if      set uselimits                          $include    modules\euregen2024_uselimits_v2.gms
$if not  set static     $if      set co2price   $include    modules\euregen2024_co2price_v1.gms
$if      set static     $if  not set hours      $include    modules\euregen2024_static_v1.gms
$if      set static     $if      set hours      $include    modules\euregen2024_hours_static_v1.gms
$if      set invlimits                          $include    modules\euregen2024_invlimit_v1.gms
    
* * * Objective function definition
objdef..
*        Surplus is defined in billion EUR
         SURPLUS =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
* European learning investment cost without region sum (old)
$if      set lbdeurcon  $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(i_lea(i)),                          sum(ivttv(i,v,t),  CAPEXEUR_CON(i,v) * 1e+6 *  endeffecteur(i,v,t)))  +
$if      set lbdeurnlp  $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(i_lea(i)),                          sum(ivttv(i,v,t),  CAPEXEUR_NLP(i,v) * 1e+6 *  endeffecteur(i,v,t)))  +
$if      set lbdeur     $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(i_lea(i)),                          sum(ivttv(i,v,t),  CAPEXEUR_MIP(i,v) * 1e+6 *  endeffecteur(i,v,t)))  +
$if      set lbseur     $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(ki_lea(i)),                         sum(ivttv(i,v,t), KCAPEXEUR_MIP(i,t) * 1e+6 *  endeffecteur(i,v,t)))  +
* European learning investment cost without region sum (new)
$if      set lbdeurcon  $if      set mixed                                           sum(new(i_lea(i)),  sum(inv, rshare(inv,i) * sum(ivttv(i,v,t),  CAPEXEUR_CON(i,v) * 1e+6 *       rzeta(inv,i,v)))) +
$if      set lbdeurnlp  $if      set mixed                                           sum(new(i_lea(i)),  sum(inv, rshare(inv,i) * sum(ivttv(i,v,t),  CAPEXEUR_NLP(i,v) * 1e+6 *       rzeta(inv,i,v)))) +
$if      set lbdeur     $if      set mixed                                           sum(new(i_lea(i)),  sum(inv, rshare(inv,i) * sum(ivttv(i,v,t), KCAPEXEUR_MIP(i,v) * 1e+6 *       rzeta(inv,i,v)))) +
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost (new)
* Mixed investor investment cost including discounting
$if not  set etc        $if      set mixed      sum(new(i),                 sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t),   (capcost(i,v,r) + deccost(i,v,r)) *  zeta(inv,i,v,r)))) +
$if      set storage    $if      set mixed      sum(newj(j),                sum(inv, gshare(inv,j,r) * IG(j,r,t)    * sum(jvrttv(j,v,r,t),  gcapcost(j,v,r)  * gzeta(inv,j,v,r)))) +

$if      set storage    $if      set hydrogensimple $if      set mixed      sum(newj(j), sum(inv, gshare(inv,j,r) * IGC(j,r,t)    * sum(jvrttv(j,v,r,t),  gcapcostc(j,v,r)  * gzeta(inv,j,v,r)))) +
$if      set storage    $if      set hydrogensimple $if      set mixed      sum(newj(j), sum(inv, gshare(inv,j,r) * IGD(j,r,t)    * sum(jvrttv(j,v,r,t),  gcapcostd(j,v,r)  * gzeta(inv,j,v,r)))) +
$if      set storage    $if      set hydrogensimple $if      set mixed      sum(newj(j), sum(inv, gshare(inv,j,r) * IGR(j,r,t)    * sum(jvrttv(j,v,r,t),  gcapcostr(j,v,r)  * gzeta(inv,j,v,r)))) +


$if      set trans      $if      set mixed      sum(tmap(k,r,rr),           sum(inv, tshare(inv,k,r) * IT(k,r,rr,t) * sum(tvrttv(k,v,r,t),  tcapcost(k,r,rr) * tzeta(inv,k,v,r)))) +
$if      set lbs        $if      set mixed      sum(new(notkir_lea(i,r)),   sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t),   (capcost(i,v,r) + deccost(i,v,r)) *  zeta(inv,i,v,r)))) +
$if      set lbs        $if      set mixed      sum(new(kir_lea(i,r)),      sum(inv,  share(inv,i,r)                * sum(ivrttv(i,v,r,t), KCAPEX_MIP(i,v,r) *  zeta(inv,i,v,r)))) +
                !! end investment cost (new)
*               DISCOUNTING                
                !! begin discounting
                dfact(t) * (               
*               INVESTMENT COST
*               Old, excluding discouting via normal annui, and ccost)                
                !! begin investment cost (old)
*               We need nyrs because investments happens once only but production nyrs-times)
                1 / nyrs(t)  * (1 
*                                   Normal investor consider total investment cost in the period of investment
$if not  set etc        $if      set normal    + sum(new(i),                    IX(i,r,t)    * sum(ivrttv(i,v,r,t),    (capcost(i,v,r) + deccost(i,v,r))  *  endeffect(i,v,r,t)))
$if      set storage    $if      set normal    + sum(newj(j),                   IG(j,r,t)    * sum(jvrttv(j,v,r,t),   gcapcost(j,v,r)  * gendeffect(j,v,r,t)))
$if      set trans      $if      set normal    + sum(tmap(k,r,rr),              IT(k,r,rr,t) * sum(tvrttv(k,v,r,t),   tcapcost(k,r,rr) * tendeffect(k,v,r,t)))
$if      set lbs        $if      set normal    + sum(new(notkir_lea(i,r)),      IX(i,r,t)    * sum(ivrttv(i,v,r,t),    capcost(i,v,r)  *  endeffect(i,v,r,t)))
$if      set lbs        $if      set normal    + sum(new(kir_lea(i,r),                         sum(ivrttv(i,v,r,t), KCAPEX_MIP(i,v,r)  * kendeffect(i,r,t)  ))

*                                   Investment costs follow from annuities (%) of borrowed capital (EUR/kW * GW)
$if                              set annui     + sum(new(i),       sum(ivrttv(i,v,r,tt)$(tt.val le t.val),             IX(i,r,tt)   *  (capcost(i,v,r) + deccost(i,v,r)) *  deprtime(i,v,r,tt) *  annuity(i,v)  * nyrs(t)))
$if      set storage    $if      set annui     + sum(newj(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val),             IG(j,r,tt)   * gcapcost(j,v,r)  * gdeprtime(j,v,r,tt) * gannuity(j,v)  * nyrs(t)))
$if      set trans      $if      set annui     + sum(tmap(k,r,rr), sum(tvrttv(k,v,r,tt)$(tt.val le t.val),             IT(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * tannuity(k)    * nyrs(t)))
*                                   Investment costs follow from WACC (%) of capital stock (EUR/kW * GW)
$if                              set ccost     + sum(new(i),       sum(ivrttv(i,v,r,tt)$(tt.val le t.val),             IX(i,r,tt)   *  (capcost(i,v,r) + deccost(i,v,r))  *  deprtime(i,v,r,tt) * drate * nyrs(t)))
$if      set storage    $if      set ccost     + sum(newj(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val),             IG(j,r,tt)   * gcapcost(j,v,r)  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set trans      $if      set ccost     + sum(tmap(k,r,rr), sum(tvrttv(k,v,r,tt)$(tt.val le t.val),             IT(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * drate * nyrs(t)))
* Regional learning investment cost wit region sum
$if      set lbd        $if      set normal    + sum(new(notir_lea(i,r)), IX(i,r,t)    * sum(ivrttv(i,v,r,t),    (capcost(i,v,r) + deccost(i,v,r))  *  endeffect(i,v,r,t)))
$if      set lbdcon     $if      set normal    + sum(new(ir_lea(i,r)), sum(ivrttv(i,v,r,t),  CAPEX_CON(i,v,r)        *  endeffect(i,v,r,t) ))
$if      set lbdnlp     $if      set normal    + sum(new(ir_lea(i,r)), sum(ivrttv(i,v,r,t),  CAPEX_NLP(i,v,r)        *  endeffect(i,v,r,t) ))
$if      set lbdmip     $if      set normal    + sum(new(ir_lea(i,r)), sum(ivrttv(i,v,r,t),  CAPEX_MIP(i,v,r) * 1e+6 *  endeffect(i,v,r,t) ))


               )
                !! end investment cost (old, excluding discount via investment cost factor)
*               DISPATCH COST
*               Are measured in ?/MWh and generation in GWh, so that we need to correct by 1e-3
                !! begin dispatch cost (regional)
* * Segments
*                       Dispatch cost (EUR/MWh) for generation (GWh)
$if not  set days       $if not  set static     + 1e-3 * sum(ivrt(i,v,r,t),            discost(i,v,r,t) * sum(s, hours(s) * X(s,i,v,r,t)))
$if not  set days       $if      set static     + 1e-3 * sum(ivrt(i,v,r,t),            sum(s, hours(s) * X(s,i,v,r,t) * discost_static(s,i,v,r,t)))
*                       Ramping cost (EUR/MWh) for ramping up and down (GWh) plus efficiency losses when not operating optimal (assumed to be linear and at mean for new vintages to avoid non-linearities)
$if not  set days       $if      set ramcost    + 1e-3 * sum(ivrt(i,v,r,t),            ramcost(i,v,r)   * sum(s, hours(s) * (RPNEG(s,i,v,r,t) + RPPOS(s,i,v,r,t))))
*                       Start-up cost (EUR/MW) for starting power plant after offtime 
$if not  set days       $if      set stacost    + 1e-3 * sum(ivrt(i,v,r,t),            stacost(i,v,r)   * sum(s, hours(s) * NUMONOFF(s,i,v,r,t)))
*                       Variable operation and maintenance cost (EUR/MWh) for storage charge and discharge (GWh)
$if not  set days       $if      set storage    $if not  set storagebalnv   + 1e-3 * sum(jvrt(j,v,r,t),            gvomcost(j,v,r)  * sum(s, hours(s) * GD(s,j,v,r,t)))
$if not  set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(jvrt(nonvj(j),v,r,t),     gvomcost(j,v,r)  * sum(s, hours(s) * GD(s,j,v,r,t)))
$if not  set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(nvj(j),                   sum(tv(t,v), gvomcost(j,v,r)  * sum(s, hours(s) * GDNV(s,j,r,t))))
*                       Variable operation and maintenance cost (EUR/MWh) for exports only (GWh)
$if not  set days       $if      set trans      + 1e-3 * sum((k,rr)$tmap(k,r,rr),      tvomcost(k,r,rr) * sum(s, hours(s) * E(s,k,r,rr,t)))
* * Days
*                       Dispatch cost (EUR/MWh) for generation (GWh)
$if      set days       $if not  set static     + 1e-3 * sum(ivrt(i,v,r,t),            discost(i,v,r,t) * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set days       $if      set static     + 1e-3 * sum(ivrt(i,v,r,t),            sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t) * discost_static_d(sd,i,v,r,t))))
*                       Ramping cost (EUR/MWh) for ramping up and down (GWh) plus efficiency losses when not operating optimal (assumed to be linear and at mean for new vintages to avoid non-linearities)
$if      set days       $if      set ramcost    + 1e-3 * sum(ivrt(i,v,r,t),            ramcost(i,v,r)   * sum(sd, days(sd) * sum(hd, (RPNEG_D(sd,hd,i,v,r,t) + RPPOS_D(sd,hd,i,v,r,t)))))
*                       Start-up cost (EUR/MW) for starting power plant after offtime 
$if      set days       $if      set stacost    + 1e-3 * sum(ivrt(i,v,r,t),            stacost(i,v,r)   * sum(sd, days(sd) * sum(hd, NUMONOFF_D(sd,hd,i,v,r,t))))
*                       Variable operation and maintenance cost (EUR/MWh) for storage charge and discharge (GWh)
$if      set days       $if      set storage    $if not  set storagebalnv   + 1e-3 * sum(jvrt(j,v,r,t),            gvomcost(j,v,r)  * sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t))))
$if      set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(jvrt(nonvj(j),v,r,t),     gvomcost(j,v,r)  * sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t))))
$if      set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(nvj(j),                   sum(tv(t,v), gvomcost(j,v,r)  * sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t)))))
$if      set storage    $if      set hydrogenimport                         +        sum(ghyd(j),                  himport(r,t) * HIMP(j,r,t))
*                       Variable operation and maintenance cost (EUR/MWh) for exports only (GWh)
$if      set days       $if      set trans      + 1e-3 * sum((k,rr)$tmap(k,r,rr),      tvomcost(k,r,rr) * sum(sd, days(sd) * sum(hd, E_D(sd,hd,k,r,rr,t))))
* * Both
*                        Cost of biomass fuel supply (we add "true" cost of receiving biomass by accouting for a step-wise biomass supply function; dispatch cost follow from pfuel)
$if      set biomark_r  + 1e-3 * sum(dbclass, DBSR(dbclass,r,t) * (dbcost_r(dbclass,r,t) - pfuel("Bioenergy",r,t)))
*                        Cost of natural gas fuel supply (we add "true" cost of receiving natural gas by accouting for a step-wise natural gas supply function; dispatch cost follow from pfuel)
$if      set gasmark_r  + 1e-3 * sum(ngclass, NGSR(ngclass,r,t) * (ngcost_r(ngclass,r,t) - pfuel("Gas",r,t)))
                !! end dispatch cost (regional)              
*               POLICY COST
*               Are from the perspective of the investor/firm/generator whose costs decrease when receiving a subsidy but increase by taxing (this is not a welfare optimum)
                !! begin policy cost
*                       Production subsidy (also for old vintage capacity in the moment, one can play around with newv(v))
*                       MM (todo): Think about introducing subsidy by vintage level to reflect feed-in tariff structures
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(irnw(i),v,r,t),             irnwsub(r,t)            * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(rnw(i),v,r,t),              rnwsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(sol(i),v,r,t),              solsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(wind(i),v,r,t),             windsub(r,t)            * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(nuc(i),v,r,t),              nucsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(lowcarb(i),v,r,t),          lowcarbsub(r,t)         * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(irnw(i),v,r,t),             irnwsub(r,t)            * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(rnw(i),v,r,t),              rnwsub(r,t)             * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(sol(i),v,r,t),              solsub(r,t)             * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(wind(i),v,r,t),             windsub(r,t)            * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(nuc(i),v,r,t),              nucsub(r,t)             * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(lowcarb(i),v,r,t),          lowcarbsub(r,t)         * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       Capacity subsidy (paid for new vintage capacity only)
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(irnw(i),newv(v),r,t),       irnwsub_cap(r,t)        * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(rnw(i),newv(v),r,t),        rnwsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(sol(i),newv(v),r,t),        solsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(wind(i),newv(v),r,t),       windsub_cap(r,t)        * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(nuc(i),newv(v),r,t),        nucsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(lowcarb(i),newv(v),r,t),    lowcarbsub_cap(r,t)     * sum(tv(t,v), IX(i,r,t) ))
                !! end policy cost
*               SOCIAL COST
               !! begin social cost
*                       Cost (EUR/MWh) of lost load/backstop (GWh)
$if not  set days                               + 1e-3 * voll(r,t) * sum(s, BS(s,r,t) * hours(s))
$if not  set days       $if     set elastic     + 1e-3 * sum(bse, vollelas(bse,r,t) * sum(s, BSELAS(bse,s,r,t) * hours(s)))
$if      set days                               + 1e-3 * voll(r,t) * sum(sd, days(sd) * sum(hd, BS_D(sd,hd,r,t)))
$if      set days       $if     set elastic     + 1e-3 * sum(bse, vollelas(bse,r,t) * sum(sd, days(sd) * sum(hd, BSELAS_D(bse,sd,hd,r,t))))
*                       Public acceptance cost (EUR/MWh) for incremental nuclear generation (GWh)
*                       MM (todo): Think about public acceptance cost for nuclear capacity (and also other capacities)
$if not  set days       $if      set scn        + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scn_emit(i,v,r,t)) * sum(s, X(s,i,v,r,t) * hours(s) ))
*                       Social cost of air pollution (EUR per MWh) from generation (GWh)
$if not  set days       $if      set socialcost + round( dfact_scap(t) / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(s, X(s,i,v,r,t) * hours(s) ))
*                       Social cost of carbon (EUR per MWh) from generation (GWh)
$if not  set days       $if      set socialcost + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(s, X(s,i,v,r,t) * hours(s) ))
*                       Public acceptance cost (EUR/MWh) for incremental nuclear generation (GWh)
*                       MM (todo): Think about public acceptance cost for nuclear capacity (and also other capacities                       
$if      set days       $if      set scn        + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scn_emit(i,v,r,t)) * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       Social cost of air pollution (EUR per MWh) from generation (GWh)
$if      set days       $if      set socialcost + round( dfact_scap(t) / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       Social cost of carbon (EUR per MWh) from generation (GWh)
$if      set days       $if      set socialcost + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       MM (todo): Introduce wind turbine public cost from visibility and noise (metric was already implemented once from Christoph) here (wait for the calibration from the Master thesis of Patrick)
                !! end social cost
*               FIXED COST
                !! begin fixed cost
*                       Fixed operation and maintenance cost (EUR/kW) for generation capacity (GW)
                        + sum(ivrt(i,v,r,t),       XC(i,v,r,t)  *  fomcost(i,v,r))
*                       Fixed operation and maintenance cost (EUR/kW) for storage capacity (GW)
$if      set storage    $if not  set storagebalnv   + sum(jvrt(j,v,r,t),              GC(j,v,r,t)  * gfomcost(j,v,r))
$if      set storage    $if      set storagebalnv   + sum(jvrt(nonvj(j),v,r,t),       GC(j,v,r,t)  * gfomcost(j,v,r))
$if      set storage    $if      set hydrogensimple + sum(jvrt(ghyd(j),v,r,t),        GCC(j,v,r,t) * gfomcostc(j,v,r) + GCD(j,v,r,t) * gfomcostd(j,v,r) + GCR(j,v,r,t) * gfomcostr(j,v,r))
$if      set storage    $if      set storagebalnv   + sum(nvj(j), sum(tv(t,v),        GCNV(j,r,t)  * gfomcost(j,v,r)))


*                       Fixed operation and maintenance cost (EUR/kW) for transmission capacity (GW)
$if      set trans      + sum((k,rr)$tmap(k,r,rr), TC(k,r,rr,t) * tfomcost(k,r,rr))
                !! end fixed cost
                )
                !! end discounting
                )
                !! end region sum
*               DISPATCH COST (system-level)
                !! begin dispatch cost (system)
*               Cost of biomass and natural gas fuel supply come truely from system-wide prices
$if      set biomark    + 1e-3 * sum(dbclass, DBS(dbclass,t) * (dbcost(dbclass,t) - round(sum(r, pfuel("Bioenergy",r,t))/sum(r, 1),4)))
$if      set gasmark    + 1e-3 * sum(ngclass, NGS(ngclass,t) * (ngcost(ngclass,t) - round(sum(r, pfuel("Gas",r,t))/sum(r, 1),4)))
                !! end dispatch cost (system)
* RD investment cost
                !! begin research cost
$if      set flh     $if      set surplusbudget  + dfact(t) * sum(ir_flh(i,r), IFLH(i,r,t))
$if      set flheur  $if      set surplusbudget  + dfact(t) * sum(i_flh(i),    IFLHEUR(i,t))
$if      set flheur  $if      set surplusbudget  + dfact(t) * sum(i_flh(i),    IFLHEUR(i,t))
                )
                !! end research cost
                !! end time period sum
;


* * * Model fixes
IX.FX(i,r,t)$(t.val ge 2024 and not new(i)) = 0 ;
IX.FX(i,r,t)$(t.val le 2021) = 0 ;
$if      set trans       IT.FX(k,r,rr,t)$(t.val le 2021) = 0 ;
$if      set storage     IG.FX(j,r,t)$(t.val le 2021) = 0 ;
$if      set storage     IG.FX(nonvj(j),r,topt2023(t)) = 0 ;
$if      set storage     IGC.FX(nonvj(j),r,topt2023(t)) = 0 ;
$if      set storage     IGD.FX(nonvj(j),r,topt2023(t)) = 0 ;
$if      set storage     IGR.FX(nonvj(j),r,topt2023(t)) = 0 ;
IX.FX(nuc(i),r,topt2023(t))$(not sum(tv(t,v), capt(i,v,r,t)) > 0) = 0 ;
IX.FX(nuc(i),r,topt2023(t))$(sum(tv(t,v), capt(i,v,r,t)) > 0) = sum(tv(t,v), capt(i,v,r,t)) ;
$if not  set nopipeline  IX.FX(nuc(i),r,t)$(t.val ge 2024 and t.val le 2030 and not invlimLO(i,r,t) > 0) = 0 ;
$if not  set nopipeline  IX.UP(nuc(i),r,t)$(t.val ge 2024 and t.val le 2030 and     invlimLO(i,r,t) > 0) = invlimLO(i,r,t) ;
IX.FX(ccs(i),r,topt2030(t)) = 0 ;
IX.FX(gas(i),r,topt2024_2030(t)) = 0 ;

$if      set noconvccs      IX.FX("Gas_CCS",r,t) = 0 ;
$if      set noconvccs      IX.FX("Coal_CCS",r,t) = 0 ;
$if      set noconvccs      IX.FX("Lignite_CCS",r,t) = 0 ;

$if      set flheur IX.FX("WindOn_q70",r,t) = 0 ;
$if      set flheur IX.FX("WindOn_q50",r,t) = 0 ;
$if      set flheur IX.FX("WindOn_q30",r,t) = 0 ;
$if      set flheur IX.FX("WindOn_q10",r,t) = 0 ;
$if      set flheur IX.FX("WindOff_q70",r,t) = 0 ;
$if      set flheur IX.FX("WindOff_q50",r,t) = 0 ;
$if      set flheur IX.FX("WindOff_q30",r,t) = 0 ;
$if      set flheur IX.FX("WindOff_q10",r,t) = 0 ;

$if      set flh  IX.FX("WindOn_q70",r,t) = 0 ;
$if      set flh  IX.FX("WindOn_q50",r,t) = 0 ;
$if      set flh  IX.FX("WindOn_q30",r,t) = 0 ;
$if      set flh  IX.FX("WindOn_q10",r,t) = 0 ;
$if      set flh  IX.FX("WindOff_q70",r,t) = 0 ;
$if      set flh  IX.FX("WindOff_q50",r,t) = 0 ;
$if      set flh  IX.FX("WindOff_q30",r,t) = 0 ;
$if      set flh  IX.FX("WindOff_q10",r,t) = 0 ;

$if      set simpmip IX.FX("RoofPV_q90",r,t)$(t.val le 2050) = 0 ;
$if      set simpmip IX.FX("RoofPV_q70",r,t)$(t.val le 2050) = 0 ;
$if      set simpmip IX.FX("RoofPV_q50",r,t)$(t.val le 2050) = 0 ;
$if      set simpmip IX.FX("RoofPV_q30",r,t)$(t.val le 2050) = 0 ;
$if      set simpmip IX.FX("RoofPV_q10",r,t)$(t.val le 2050) = 0 ;

$if      set simpmip IX.FX("OpenPV_q70",r,t) = 0 ;
$if      set simpmip IX.FX("OpenPV_q50",r,t) = 0 ;
$if      set simpmip IX.FX("OpenPV_q30",r,t) = 0 ;
$if      set simpmip IX.FX("OpenPV_q10",r,t) = 0 ;

$if      set simpmip IX.FX("WindOn_q70",r,t) = 0 ;
$if      set simpmip IX.FX("WindOn_q50",r,t) = 0 ;
$if      set simpmip IX.FX("WindOn_q30",r,t) = 0 ;
$if      set simpmip IX.FX("WindOn_q10",r,t) = 0 ;

$if      set simpmip IX.FX("WindOff_q70",r,t) = 0 ;
$if      set simpmip IX.FX("WindOff_q50",r,t) = 0 ;
$if      set simpmip IX.FX("WindOff_q30",r,t) = 0 ;
$if      set simpmip IX.FX("WindOff_q10",r,t) = 0 ;

* * Segments
* No transmission when not set transmission
$if not  set trans       IT.FX(k,r,rr,t) = 0 ;
$if not  set trans       E.FX(s,k,r,rr,t) = 0 ;
$if not  set trans       TC.FX(k,r,rr,t) = 0 ;
* No storage when not set storage
$if not  set storage     IG.FX(j,r,t) = 0 ;
$if not  set storage     $if not set days   G.FX(s,j,v,r,t) = 0 ;
$if not  set storage     $if not set days   GB.FX(s,j,v,r,t) = 0 ;
$if not  set storage     $if not set days   GBNV.FX(s,j,r,t) = 0 ;
$if not  set storage                        GC.FX(j,v,r,t) = 0 ;
$if not  set storage                        GCC.FX(j,v,r,t) = 0 ;
$if not  set storage                        GCD.FX(j,v,r,t) = 0 ;
$if not  set storage                        GCR.FX(j,v,r,t) = 0 ;
$if not  set storage     $if not set days   GD.FX(s,j,v,r,t) = 0 ;
$if not  set storage     $if not set days   GNV.FX(s,j,r,t) = 0 ;
$if not  set storage     $if not set days   GDNV.FX(s,j,r,t) = 0 ;
* Remove backstop unless explicitly allowed
$if not  set lostload    $if not set days   BS.FX(s,r,t) = 0 ;
$if not  set lostload    $if not set days   BSELAS.FX(bse,s,r,t) = 0 ;
* * Days
* No transmission when not set transmission
$if not  set trans       $if     set days   BE_D.FX(sd,hd,k,r,rr,t) = 0 ;
* No storage when not set storage
$if not  set storage     $if     set days   BG_D.FX(sd,hd,j,v,r,t) = 0 ;
$if not  set storage     $if     set days   BGB_D.FX(sd,hd,j,v,r,t) = 0 ;
$if not  set storage     $if     set days   BGBNV_D.FX(sd,hd,j,r,t) = 0 ;
$if not  set storage     $if     set days   BGBNV_DD.FX(sd,j,r,t) = 0 ;
$if not  set storage     $if     set days   BGD_D.FX(sd,hd,j,v,r,t) = 0 ;
* Remove backstop unless explicitly allowed
$if not  set lostload    $if     set days   BS_D.FX(sd,hd,r,t) = 0 ;
$if not  set lostload    $if     set days   BBSELAS_D.FX(bse,sd,hd,r,t) = 0 ;

*BSELAS.FX(bse,s,r,t) = 0 ;
*BSELAS_D.FX(bse,sd,hd,r,t) = 0 ;
$if      set elastic     $if not set days   DS.UP(bse,s,r,t) = 1.25 * bse_share(bse,r,t) * load(s,r,t) ;
$if      set elastic     $if not set days   DS.LO(bse,s,r,t) = 0.75 * bse_share(bse,r,t) * load(s,r,t) ;
$if      set elastic     $if     set days   DS_D.UP(bse,sd,hd,r,t) = 1.25 * bse_share(bse,r,t) * load_d(sd,hd,r,t) ;
$if      set elastic     $if     set days   DS_D.LO(bse,sd,hd,r,t) = 0.75 * bse_share(bse,r,t) * load_d(sd,hd,r,t) ;
$if      set lostloadnew $if not set days   BS.FX(s,r,t) = 0 ;storab
$if      set lostloadnew $if not set days   BSELAS.UP(bse,s,r,t) = bse_share(bse,r,t) * load(s,r,t) ;
$if      set elastic     $if not set days   BSELAS.FX(bse,s,r,t) = 0 ;
$if      set lostloadnew $if     set days   BS_D.FX(sd,hd,r,t) = 0 ;
$if      set lostloadnew $if     set days   BSELAS_D.UP(bse,sd,hd,r,t) = bse_share(bse,r,t) * load_d(sd,hd,r,t) ;
$if      set elastic     $if     set days   BSELAS_D.FX(bse,sd,hd,r,t) = 0 ;
* Upper bound on dedicated biomass supply (depends on biomass carbon price scenario)
$if      set biomark_r   DBSR.UP(dbclass,r,t)    = dblim_r(dbclass,r,t) ;
$if      set biomark     DBS.UP(dbclass,t)       = dblim(dbclass,t) ;
$if      set gasmark_r   NGSR.UP(ngclass,r,t)    = nglim_r(ngclass,r,t) ;
$if      set gasmark     NGS.UP(ngclass,t)       = nglim(ngclass,t) ;

$if      set hydrogensimple                         IG.FX(ghyd(j),r,toptimize(t)) = 0 ;
$if      set hydrogensimple                         GC.FX(ghyd(j),v,r,toptimize(t)) = 0 ;
$if      set hydrogensimple $if      set segments   GCS.FX(s,ghyd(j),v,r,toptimize(t)) = 0 ;
$if      set hydrogenimport                         HIMP.FX(j,r,toptimize(t))$(not ghyd(j)) = 0 ;
$if not  set hydrogenimport                         HIMP.FX(j,r,toptimize(t)) = 0 ;
$if      set hydrogensimple                         HRES.FX(j,r,toptimize(t))$(not ghyd(j)) = 0 ;
$if not  set hydrogensimple                         HRES.FX(j,r,toptimize(t)) = 0 ;

* * * * * Model declaration and solution
model euregen /
objdef
$if      set elastic                                weldef
*$if      set elastic                                weldef_short
*$if      set elastic                                weldef_long
* * Demand equations
$if      set hydrogensimple                         demand_hydrogen
$if      set hydrogenimport                         demand_hydrogenimport
* Segments
$if not  set days                                   demand
$if not  set days   $if      set rsa                demand_rsa
* Days
$if      set days                                   demand_d
$if      set days   $if      set rsa                demand_rsa_d
* * Generation
* Segments
*$if      set static                                 capacity_all
$if not  set days                                   capacity
$if not  set days   $if      set mergeirnw          capacity_irnw
$if not  set days   $if      set chp                capacity_chp
$if not  set days   $if      set minmax             capacity_nucmin_old
$if not  set days   $if      set minmax             capacity_nucmax_old
$if not  set days                                   capacity_nuc_new
$if not  set days   $if not  set minmax             capacity_nuc_old
$if not  set days   $if      set minmax             capacity_biomin_old
$if not  set days   $if      set minmax             capacity_biomax_old
$if not  set days                                   capacity_bio_new
$if not  set days   $if not  set minmax             capacity_bio_old
* Days
*$if      set days                                   capacity_all_d
$if      set days                                   capacity_d
$if      set days   $if      set mergeirnw          capacity_irnw_d
$if      set days   $if      set chp                capacity_chp_d
$if      set days   $if      set minmax             capacity_nucmin_old_d
$if      set days   $if      set minmax             capacity_nucmax_old_d
$if      set days                                   capacity_nuc_new_d
$if      set days   $if not  set minmax             capacity_nuc_old_d
$if      set days   $if      set minmax             capacity_biomin_old_d
$if      set days   $if      set minmax             capacity_biomax_old_d
$if      set days                                   capacity_bio_new_d
$if      set days   $if not  set minmax             capacity_bio_old_d
* Investment
$if not  set static                           invest_new
$if not  set static                           invest_old
$if not  set static                           exlife2023
$if not  set static                           exlife
$if not  set static $if      set chp                                 exlife2023_chp
$if not  set static $if      set chp                                 exlife2030_chp
$if not  set static $if      set chp                                 exlife_chp
$if not  set static $if      set biosub                              exlife2023_bio
$if not  set static $if      set biosub                              exlife2030_bio
$if not  set static $if      set biosub                              exlife_bio
$if not  set static                                                 newlife
$if not  set static $if not  set myopic                              retire
$if not  set static                                                 investlimUP
$if not  set static $if not  set nopipeline                          investlimLO
$if not  set static $if      set limeu                               investlimUP_eu
$if not  set static                                                 investlimUP_irnw
* * Storage
* Segments
*$if not  set days   $if      set storage                             dischargelim_hydrogen
*$if not  set days   $if      set storage                             dischargelim_fuelcell
$if not  set days   $if      set storage                                                            chargelim
$if not  set days   $if      set storage                                                            dischargelim
$if not  set days   $if      set storage                              $if      set hydrogensimple   chargelimc
$if not  set days   $if      set storage                              $if      set hydrogensimple   dischargelimd
*$if not  set days   $if      set storage  $if not  set storagebalnv                                 dischargelim_min
$if not  set days   $if      set storage  $if not  set storagebalnv                                 dischargelim_max
$if not  set days   $if      set storage  $if not  set storagebalnv                                 reservoirlim
$if not  set days   $if      set storage  $if      set storagebalnv                                 reservoirlimnonv
$if not  set days   $if      set storage  $if not  set storagebalnv   $if      set hydrogensimple   reservoirlimr
$if not  set days   $if      set storage  $if      set storagebalnv   $if      set hydrogensimple   reservoirlimrnonv
$if not  set days   $if      set storage  $if      set storagebalnv                                 chargelimnv
$if not  set days   $if      set storage  $if      set storagebalnv                                 dischargelimnv
*$if not  set days   $if      set storage  $if      set storagebalnv                                 dischargelimnv_min
$if not  set days   $if      set storage  $if      set storagebalnv                                 dischargelimnv_max
$if not  set days   $if      set storage  $if      set storagebalnv                                 reservoirlimnv
$if not  set days   $if      set storage  $if not  set storagebalnv                                 storagebal
$if not  set days   $if      set storage  $if not  set storagebalnv                                 storagebal_ann
$if not  set days   $if      set storage  $if not  set storagebalnv    $if      set storagefixstart storagebal_firsts
$if not  set days   $if      set storage  $if not  set storagebalnv    $if      set storagefixstart storagebal_lasts
$if not  set days   $if      set storage  $if not  set storagebalnv    $if not  set storagefixstart storagebal_endbalance
$if not  set days   $if      set storage  $if      set storagebalnv                                 storagebalnv
$if not  set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_ann
$if not  set days   $if      set storage  $if      set storagebalnv    $if      set storagefixstart storagebalnv_firsts
$if not  set days   $if      set storage  $if      set storagebalnv    $if      set storagefixstart storagebalnv_lasts
$if not  set days   $if      set storage  $if      set storagebalnv    $if not  set storagefixstart storagebalnv_endbalance
$if not  set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old
$if not  set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_ann
$if not  set days   $if      set storage  $if      set storagebalnv    $if      set storagefixstart storagebalnv_old_firsts
$if not  set days   $if      set storage  $if      set storagebalnv    $if      set storagefixstart storagebalnv_old_lasts
$if not  set days   $if      set storage  $if      set storagebalnv    $if not  set storagefixstart storagebalnv_old_endbalance
* Days
$if      set days   $if      set storage                                                            chargelim_d
$if      set days   $if      set storage                                                            dischargelim_d
$if      set days   $if      set storage                                                            chargelimc_d
$if      set days   $if      set storage                                                            dischargelimd_d
*$if      set days   $if      set storage  $if not  set storagebalnv                                 dischargelim_min_d
$if      set days   $if      set storage  $if not  set storagebalnv                                 dischargelim_max_d
$if      set days   $if      set storage  $if not  set storagebalnv                                 reservoirlim_d
$if      set days   $if      set storage  $if      set storagebalnv                                 reservoirlimnonv_d
$if      set days   $if      set storage  $if not  set storagebalnv                                 reservoirlim_dd
$if      set days   $if      set storage  $if      set storagebalnv                                 reservoirlimnonv_dd
$if      set days   $if      set storage  $if not  set storagebalnv                                 reservoirlimr_d
$if      set days   $if      set storage  $if      set storagebalnv                                 reservoirlimrnonv_d
$if      set days   $if      set storage  $if not  set storagebalnv                                 reservoirlimr_dd
$if      set days   $if      set storage  $if      set storagebalnv                                 reservoirlimrnonv_dd
$if      set days   $if      set storage  $if      set storagebalnv                                 chargelimnv_d
$if      set days   $if      set storage  $if      set storagebalnv                                 dischargelimnv_d
*$if      set days   $if      set storage  $if      set storagebalnv                                 dischargelimnv_min_d
$if      set days   $if      set storage  $if      set storagebalnv                                 dischargelimnv_max_d
$if      set days   $if      set storage  $if      set storagebalnv                                 reservoirlimnv_d
$if      set days   $if      set storage  $if      set storagebalnv                                 reservoirlimnv_dd

$if      set days   $if      set storage  $if not  set storagebalnv                                 storagebal_sdone_hdone
$if      set days   $if      set storage  $if not  set storagebalnv                                 storagebal_sdtwo_hdone
$if      set days   $if      set storage  $if not  set storagebalnv                                 storagebal_hdtwo
$if      set days   $if      set storage  $if not  set storagebalnv                                 storagebal_sdone
$if      set days   $if      set storage  $if not  set storagebalnv                                 storagebal_sdtwo
$if      set days   $if      set storage  $if not  set storagebalnv                                 storagebal_ann_d

$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_sdone_hdone
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_sdtwo_hdone
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_hdtwo
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_sdone
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_sdtwo
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_ann_d

$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_sdone_hdone
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_sdtwo_hdone
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_hdtwo
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_sdone
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_sdtwo
$if      set days   $if      set storage  $if      set storagebalnv                                 storagebalnv_old_ann_d
* Further equations
$if not  set static $if      set storage                                                            ginvest_new
$if not  set static $if      set storage                                                            ginvest_old
$if not  set static $if      set storage                                                            gexlife
$if not  set static $if      set storage  $if      set storagebalnv                                  gexlifenv
$if not  set static $if      set storage                                                            gexlife2023
$if not  set static $if      set storage  $if      set storagebalnv                                  gexlifenv2023
*$if not  set static $if      set storage                                                            gexlife_pump
*$if not  set static $if      set storage                                                            gexlife_res
$if not  set static $if      set storage                                                            gnewlife
$if not  set static $if      set storage                                                            gretire
$if not  set static $if      set storage  $if      set storagebalnv                                 gretirenv
$if not  set static $if      set storage                                                            ginvestlimUP
$if not  set static $if      set storage                                                            ginvestlimLO
$if not  set static $if      set storage                                                            ginvestlimUP_eu
* No fixed energy to power ratio
$if not  set static $if      set storage $if      set hydrogensimple                                ginvestc_new
$if not  set static $if      set storage $if      set hydrogensimple                                ginvestd_new
$if not  set static $if      set storage $if      set hydrogensimple                                ginvestr_new
$if not  set static $if      set storage $if      set hydrogensimple                                gnewlifec
$if not  set static $if      set storage $if      set hydrogensimple                                gnewlifed
$if not  set static $if      set storage $if      set hydrogensimple                                gnewlifer
$if not  set static $if      set storage $if      set hydrogensimple                                gretirec
$if not  set static $if      set storage $if      set hydrogensimple                                gretired
$if not  set static $if      set storage $if      set hydrogensimple                                gretirer
* Transmission
$if      set trans  $if not  set days                                                    tcapacity
$if      set trans  $if      set days                                                    tcapacity_d
$if not  set static $if not  set static $if      set trans                               tinvestexi
$if not  set static $if not  set static $if      set trans                               tinvestnew
$if not  set static $if not  set static $if      set trans                               tinvestlimUP
$if not  set static $if not  set static $if      set trans                               tinvestlimLO
$if not  set static $if not  set static $if      set trans     $if      set tlimeu       tinvestlimUP_eu
* * * Static model
$if      set static                              exlife_static
$if      set static                              gexlife_static
$if      set static                              gexlifenv_static
$if      set static                              tinvestexi_static
*$if      set static                              euets_static
*$if      set static                              ukets_static
*$if      set static                              euco2flow
*$if      set static                              ukco2flow
* Minimum dispatch
$if      set techmin                             capacity_techmin
$if      set techmin   $if      set stacost      eq_numonoffup
$if      set techmin   $if      set stacost      eq_numonofflo
* Ramping
$if      set ramping                             capacity_ramratepos
$if      set ramping                             capacity_ramrateneg
$if      set ramping                             capacity_rampdown
$if      set ramping                             capacity_rampup
* Biomass and naturgal gas markets
$if      set biomark                             biomarket
$if      set biomark_r                           biomarket_r
$if      set gasmark                             gasmarket
$if      set gasmark_r                           gasmarket_r
* Biomass, naturgal gas, and CCS markets (or limits)
$if      set biolim                              bioflow
$if      set biolim                              cumbio
$if      set biolim_r                            bioflow_r
$if      set biolim_r                            cumbio_r
$if      set gaslim                              gasflow
$if      set gaslim                              cumgas
$if      set gaslim_r                            gasflow_r
$if      set gaslim_r                            cumgas_r
$if not  set static     $if      set sclim       cumccs
$if      set sclim                               ccsflow
$if not  set static     $if      set sclim_r     cumccs_r
$if      set sclim_r                             ccsflow_r
* CO2 price exogenously given
$if      set co2price                            euco2flow
$if      set co2price                            ukco2flow
* CO2 market without MSR dynamic and w/o banking
$if      set co2marktotal                        co2flow
$if      set co2marktotal                        co2market
$if      set euetsbreak                          co2flow_r
$if      set euetsbreak                          co2market_r
$if      set euetsbreak                          tnac_r
$if      set co2mark                             euco2flow
$if      set co2mark                             euets
$if      set co2mark   $if      set banking      eutnac
$if      set co2mark                             ukco2flow
$if      set co2mark                             ukets
$if      set co2mark   $if      set banking      uktnac
* CO2 market with MSR dynamics via iterative modeling (only shortrun)
$if      set co2iter                             euco2flow
$if      set co2iter                             it_euets
$if      set co2iter   $if      set banking      it_tnac
$if      set co2iter                             ukco2flow
$if      set co2iter                	         ukets
$if      set co2iter   $if      set banking      uktnac
* CO2 market with MSR dynamics as simple as possible (only shortrun)
$if      set co2mips                                euco2flow
$if      set co2mips                                eqs_euets
$if      set co2mips                                eqs_msr
$if      set co2mips                                eqs_msrin2023
$if      set co2mips                                eqs_msrout2023
$if      set co2mips                                eqs_msrout_max
$if      set co2mips                                eqs_msrout_fin
$if      set co2mips                                eqs_msrout_bin
$if      set co2mips                                eqs_msroutup
$if      set co2mips                                eqs_msroutlo
$if      set co2mips    $if      set msrinout       eqs_msrinout
$if      set co2mips                                eqs_tnac
$if      set co2mips                                eqs_tnac_bin
$if      set co2mips                                eqs_cancel2023
$if      set co2mips                                eqs_cancel2045
$if      set co2mips                                eqs_cancel_fin
$if      set co2mips                                eqs_cancel_bin

$if      set co2mips    $if      set euetsold       eqs_msrin_old
$if      set co2mips    $if      set euetsold       eqs_msrout_old
$if      set co2mips    $if      set euetsold       eqs_cancel_old
$if      set co2mips    $if      set euetsold       eqs_cancelup_old             
$if      set co2mips    $if      set euetsold       eqs_cancello_old
$if      set co2mips    $if      set euetsold       eqs_tnacup_old
$if      set co2mips    $if      set euetsold       eqs_tnaclo_old

$if      set co2mips    $if      set euetsmsrin     eqs_msrin_new
$if      set co2mips    $if      set euetsmsrin     eqs_msrout_new
$if      set co2mips    $if      set euetsmsrin     eqs_cancel_old
$if      set co2mips    $if      set euetsmsrin     eqs_cancelup_old             
$if      set co2mips    $if      set euetsmsrin     eqs_cancello_old
$if      set co2mips    $if      set euetsmsrin     eqs_tnacup_old             
$if      set co2mips    $if      set euetsmsrin     eqs_tnaclo_old

$if      set co2mips    $if      set euetscancel    eqs_msrin_old
$if      set co2mips    $if      set euetscancel    eqs_msrout_old
$if      set co2mips    $if      set euetscancel    eqs_cancel_new
$if      set co2mips    $if      set euetscancel    eqs_cancelup_new             
$if      set co2mips    $if      set euetscancel    eqs_cancello_new
$if      set co2mips    $if      set euetscancel    eqs_tnacup_new             
$if      set co2mips    $if      set euetscancel    eqs_tnaclo_new

$if      set co2mips    $if      set euetsnew       eqs_msrin_new
$if      set co2mips    $if      set euetsnew       eqs_msrout_new
$if      set co2mips    $if      set euetsnew       eqs_cancel_new
$if      set co2mips    $if      set euetsnew       eqs_cancelup_new             
$if      set co2mips    $if      set euetsnew       eqs_cancello_new
$if      set co2mips    $if      set euetsnew       eqs_tnacup_new             
$if      set co2mips    $if      set euetsnew       eqs_tnaclo_new

$if      set co2mips                                ukco2flow
$if      set co2mips                                ukets
$if      set co2mips    $if      set banking        uktnac

* Calibration of CO2 markets (2022 and steering)
$if      set real2022                               eqs_co2real2022
$if      set co2mono                                eqs_co2monoUP
* Policy
$if      set rnwtarget                              rnwtgtmarket
$if      set rnwtarget_r                            rnwtgtmarket_r
$if      set irnwtarget                             irnwtgtmarket
$if      set irnwtarget_r                           irnwtgtmarket_r
$if      set coalexit                               coalphaseout
$if      set coalexit_r                             coalphaseout_r
$if      set lignexit                               lignphaseout
$if      set lignexit_r                             lignphaseout_r
$if      set nucexit                                nucphaseout
$if      set nucexit_r                              nucphaseout_r
$if      set frnuctarget                            frnuctarget
$if      set resmarket                                          resmarket_con
$if      set resmarket  $if not  set gentargetsdemand            resmarket_gen
$if      set capmarket                                          capmarket
$if      set engmarket                                          engmarket
*$if not  set resmarket  $if      set euetsbreak                                          resmarket_con
*$if not  set resmarket  $if      set euetsbreak   $if not  set gentargetsdemand           resmarket_gen
*$if not  set capmarket  $if      set euetsbreak                                          capmarket
*$if not  set engmarket  $if      set euetsbreak                                          engmarket

* * * Structural equations to aid solver
xtwhdef
$if      set mergeirnw                                                  xtwhirnwdef
* Segments
$if not  set days                                                       copyxc
$if not  set days   $if      set storage                                copygc
$if not  set days   $if      set storage    $if      set storagebalnv   copygcnv
$if not  set days   $if      set trans                                  copytc
* Days (structural equations deactivated)
*$if      set days                                                       copyxc_sd
*$if      set days   $if      set storage                                copygc_sd
*$if      set days   $if      set storage    $if      set storagebalnv   copygcnv_sd
*$if      set days   $if      set trans                                  copytc_sd
*$if      set days                                                       copyxc_d
*$if      set days   $if      set storage                                copygc_d
*$if      set days   $if      set storage    $if      set storagebalnv   copygcnv_d
*$if      set days   $if      set trans                                  copytc_d

* * * Calibration
$if      set invlimits                               investlimit_windon
$if      set invlimits                               investlimit_windoff
$if      set invlimits                               investlimit_sol
* When not selecting CHP calibration (for all conventionals)
$if      set calibration_conv    genUP_oil
$if      set calibration_conv    genUP_coal
$if      set calibration_conv    genUP_lign
$if      set calibration_conv    genUP_biom
$if      set calibration_conv    genUP_nucl
$if      set calibration_conv    genLO_oil
$if      set calibration_conv    genLO_coal
$if      set calibration_conv    genLO_lign
$if      set calibration_conv    genLO_biom
$if      set calibration_conv    genLO_ngas
* CHP calibration
$if      set calibration_chp     genUP_oil_chp
$if      set calibration_chp     genLO_oil_chp
$if      set calibration_chp     genUP_coal_chp
$if      set calibration_chp     genLO_coal_chp
$if      set calibration_chp     genUP_lign_chp
$if      set calibration_chp     genLO_lign_chp
$if      set calibration_chp     genUP_biom_chp
$if      set calibration_chp     genLO_biom_chp
$if      set calibration_chp     genUP_ngas_chp
$if      set calibration_chp     genLO_ngas_chp
* No CHP calibration
$if      set calibration_nochp   genUP_oil_nochp
*$if      set calibration_nochp   genLO_oil_nochp
$if      set calibration_nochp   genUP_coal_nochp
*$if      set calibration_nochp   genLO_coal_nochp
$if      set calibration_nochp   genUP_lign_nochp
*$if      set calibration_nochp   genLO_lign_nochp
$if      set calibration_nochp   genUP_biom_nochp
*$if      set calibration_nochp   genLO_biom_nochp
*$if      set calibration_nochp   genUP_ngas_nochp
*$if      set calibration_nochp   genLO_ngas_nochp
$if      set calibration_nochp   genUP_nucl
*$if      set calibration_nochp   genLO_nucl
* Additional calibrations
$if      set calibrationlignfuel                                                    genUP_lignfuel
* IRNW
$if      set calibration_irnw       $if      set mergeirnw                          genUP_irnw
$if      set calibration_irnw       $if      set mergeirnw                          genLO_irnw
* Storage
$if      set calibration_pump       $if      set storage                            genUP_pump
$if      set calibration_resv       $if      set storage                            genUP_resv
*$if      set calibration_pump       $if      set storage                            genLO_pump
*$if      set calibration_resv       $if      set storage                            genLO_resv
* Transmission
*$if      set calibration2023tranUP  $if      set transmission                       impUP
*$if      set calibration2023tranLO  $if      set transmission                       impLO
$if      set calibration2023tranUP  $if      set transmission                       expUP
$if      set calibration2023tranLO  $if      set transmission                       expLO
$if      set calibration2023tranUP_all  $if      set transmission                   expUP_all
$if      set calibration2023tranLO_all  $if      set transmission                   expLO_all

* * * Regional Learning-by-Doing
$if      set doing       $if      set recall       $if not  set eur                    acc_q_recall2020
$if      set doing       $if      set recall       $if not  set eur  $if       set leg acc_q_leg_recall2020
$if      set doing       $if      set recall       $if not  set eur                    acc_q_recall
$if      set doing       $if      set recall       $if not  set eur  $if       set leg acc_q_leg_recall
$if      set doing       $if      set continuous   $if not  set eur                    acc_q_continuous2020
$if      set doing       $if      set continuous   $if not  set eur  $if       set leg acc_q_leg_continuous2020
$if      set doing       $if      set continuous   $if not  set eur                    acc_q_continuous
$if      set doing       $if      set continuous   $if not  set eur  $if       set leg acc_q_leg_continuous
$if      set doing       $if      set discrete     $if not  set eur                    acc_q_discrete2020
$if      set doing       $if      set discrete     $if not  set eur  $if       set leg acc_q_leg_discrete2020
$if      set doing       $if      set discrete     $if not  set eur                    acc_q_discrete
$if      set doing       $if      set discrete     $if not  set eur  $if       set leg acc_q_leg_discrete
$if      set doing       $if      set constant     $if not  set eur                    capex_constant
$if      set doing       $if      set nonlinear    $if not  set eur                    capex_nonlinear
$if      set doing       $if      set nonlinear    $if not  set eur                    acc_capex_nonlinear
$if      set doing       $if      set nonlinear    $if not  set eur  $if       set leg acc_capex_leg_nonlinear
$if      set doing       $if      set mip          $if not  set eur                    capex_mixedip
$if      set doing       $if      set mip          $if not  set eur                    acc_capex_mixedip
$if      set doing       $if      set mip          $if not  set eur  $if       set leg acc_capex_leg_mixedip
$if      set doing       $if      set mip          $if not  set eur                    rho_mixedip
$if      set doing       $if      set mip          $if not  set eur  $if       set leg rho_leg_mixedip
$if      set doing       $if      set mip          $if not  set eur                    qlsLO_mixedip
$if      set doing       $if      set mip          $if not  set eur  $if       set leg qlsLO_leg_mixedip
$if      set doing       $if      set mip          $if not  set eur                    qlsUP_mixedip
$if      set doing       $if      set mip          $if not  set eur  $if       set leg qlsUP_leg_mixedip
$if      set doing       $if      set mip          $if not  set eur                    acc_qls_mixedip
$if      set doing       $if      set mip          $if not  set eur  $if       set leg acc_qls_leg_mixedip
* Max constraints
$if      set doing       $if      set mip          $if not  set eur                    acc_q_max
$if      set doing       $if      set mip          $if not  set eur  $if       set leg acc_q_leg_max
* Monotonicity constraints
$if      set doing       $if      set recall       $if not  set eur                    acc_q_mono
$if      set doing       $if      set recall       $if not  set eur  $if       set leg acc_q_leg_mono
$if      set doing       $if      set recall       $if not  set eur                    rho_mixedip_mono
$if      set doing       $if      set recall       $if not  set eur  $if       set leg rho_leg_mixedip_mono
$if      set doing       $if      set recall       $if not  set eur                    qls_mixedip_mono
$if      set doing       $if      set recall       $if not  set eur  $if       set leg qls_leg_mixedip_mono
* * * opean Learning-by-Doing
$if      set doing       $if      set recall       $if      set eur                    acc_q_recall2020
$if      set doing       $if      set recall       $if      set eur  $if       set leg acc_q_leg_recall2020
$if      set doing       $if      set recall       $if      set eur                    acc_q_recall
$if      set doing       $if      set recall       $if      set eur  $if       set leg acc_q_leg_recall
$if      set doing       $if      set continuous   $if      set eur                    acc_q_continuous2020
$if      set doing       $if      set continuous   $if      set eur  $if       set leg acc_q_leg_continuous2020
$if      set doing       $if      set continuous   $if      set eur                    acc_q_continuous
$if      set doing       $if      set continuous   $if      set eur  $if       set leg acc_q_leg_continuous
$if      set doing       $if      set discrete     $if      set eur                    acc_q_discrete2020
$if      set doing       $if      set discrete     $if      set eur  $if       set leg acc_q_leg_discrete2020
$if      set doing       $if      set discrete     $if      set eur                    acc_q_discrete
$if      set doing       $if      set discrete     $if      set eur  $if       set leg acc_q_leg_discrete
$if      set doing       $if      set constant     $if      set eur                    capex_constant
$if      set doing       $if      set nonlinear    $if      set eur                    capex_nonlinear
$if      set doing       $if      set nonlinear    $if      set eur                    acc_capex_nonlinear
$if      set doing       $if      set nonlinear    $if      set eur  $if       set leg acc_capex_leg_nonlinear
$if      set doing       $if      set mip          $if      set eur                    capex_mixedip
$if      set doing       $if      set mip          $if      set eur                    acc_capex_mixedip
$if      set doing       $if      set mip          $if      set eur  $if       set leg acc_capex_leg_mixedip
$if      set doing       $if      set mip          $if      set eur                    rho_mixedip
$if      set doing       $if      set mip          $if      set eur  $if       set leg rho_leg_mixedip
$if      set doing       $if      set mip          $if      set eur                    qlsLO_mixedip
$if      set doing       $if      set mip          $if      set eur  $if       set leg qlsLO_leg_mixedip
$if      set doing       $if      set mip          $if      set eur                    qlsUP_mixedip
$if      set doing       $if      set mip          $if      set eur  $if       set leg qlsUP_leg_mixedip
$if      set doing       $if      set mip          $if      set eur                    acc_qls_mixedip
$if      set doing       $if      set mip          $if      set eur  $if       set leg acc_qls_leg_mixedip
* Max constraints
$if      set doing       $if      set mip          $if      set eur                    acc_q_max
$if      set doing       $if      set mip          $if      set eur  $if       set leg acc_q_leg_max
* Monotonicity constraints
$if      set doing       $if      set recall       $if      set eur                    acc_q_mono
$if      set doing       $if      set recall       $if      set eur  $if       set leg acc_q_leg_mono
$if      set doing       $if      set recall       $if      set eur                    rho_mixedip_mono
$if      set doing       $if      set recall       $if      set eur  $if       set leg rho_leg_mixedip_mono
$if      set doing       $if      set recall       $if      set eur                    qls_mixedip_mono
$if      set doing       $if      set recall       $if      set eur  $if       set leg qls_leg_mixedip_mono
* * * Regional Learning-by-Searching
$if      set lbs                                    acc_k_continuous2020
$if      set lbs                                    acc_k_continuous
$if      set lbs                                    rho_mixedip_k
$if      set lbs                                    capex_mixedip_k2020
$if      set lbs                                    capex_mixedip_k
$if      set lbs                                    lbs_helper1
$if      set lbs                                    lbs_helper2
$if      set lbs                                    lbs_helper3
$if      set lbs                                    lbs_helper4
$if      set lbs                                    capcost_mixedip_k
$if      set lbs                                    klsLO_mixedip
$if      set lbs                                    klsUP_mixedip
$if      set lbs                                    acc_kls_mixedip
* Budget constraints
$if      set lbs        $if      set budget_irt      eq_lbs_rdbudget_irt        
$if      set lbs        $if      set budget_rt       eq_lbs_rdbudget_rt 
$if      set lbs        $if      set budget_it       eq_lbs_rdbudget_it 
$if      set lbs        $if      set budget_ir       eq_lbs_rdbudget_ir 
$if      set lbs        $if      set budget_t        eq_lbs_rdbudget_t 
$if      set lbs        $if      set budget_r        eq_lbs_rdbudget_r 
$if      set lbs        $if      set budget_i        eq_lbs_rdbudget_i 
$if      set lbs        $if      set budget          eq_lbs_rdbudget
* * * opean Learning-by-Searching
$if      set lbseur                                 acc_keur_continuous2020
$if      set lbseur                                 acc_keur_continuous
$if      set lbseur                                 rhoeur_mixedip_k
$if      set lbseur                                 capexeur_mixedip_k
$if      set lbseur                                 lbseur_helper1
$if      set lbseur                                 lbseur_helper2
$if      set lbseur                                 lbseur_helper3
$if      set lbseur                                 lbseur_helper4
$if      set lbseur                                 capcosteur_mixedip_k
$if      set lbseur                                 keurlsLO_mixedip
$if      set lbseur                                 keurlsUP_mixedip
$if      set lbseur                                 acc_keurls_mixedip
* Budget constraints
$if      set lbseur     $if      set budget_it       eq_lbseur_rdbudget_it 
$if      set lbseur     $if      set budget_i        eq_lbseur_rdbudget_i 
$if      set lbseur     $if      set budget_t        eq_lbseur_rdbudget_t 
$if      set lbseur     $if      set budget          eq_lbseur_rdbudget
* * * Regional FLHLearning-by-sarching
$if      set flh     acc_flh2020
$if      set flh     acc_flh
$if      set flh     rhoflh_mixedip
$if      set flh     flhlsLO_mixedip
$if      set flh     flhlsUP_mixedip
$if      set flh     acc_flhls_mixedip
$if      set flh     flh_helper1
$if      set flh     flh_helper2
$if      set flh     flh_helper3
$if      set flh     flh_helper4
$if      set flh     capacity_flholdv
$if      set flh     capacity_flh
* Budget constraints
$if      set flh        $if      set budget_irt      eq_flh_rdbudget_irt        
$if      set flh        $if      set budget_rt       eq_flh_rdbudget_rt 
$if      set flh        $if      set budget_it       eq_flh_rdbudget_it 
$if      set flh        $if      set budget_ir       eq_flh_rdbudget_ir 
$if      set flh        $if      set budget_t        eq_flh_rdbudget_t 
$if      set flh        $if      set budget_r        eq_flh_rdbudget_r 
$if      set flh        $if      set budget_i        eq_flh_rdbudget_i 
$if      set flh        $if      set budget          eq_flh_rdbudget
* * * European FLH Learning-by-sarching
$if      set flheur     acc_flheur2020
$if      set flheur     acc_flheur
$if      set flheur     rhoflheur_mixedip
$if      set flheur     flheurlsLO_mixedip
$if      set flheur     flheurlsUP_mixedip
$if      set flheur     acc_flheurls_mixedip
$if      set flheur     flheur_helper1
$if      set flheur     flheur_helper2
$if      set flheur     flheur_helper3
$if      set flheur     flheur_helper4
$if      set flheur     capacity_flheuroldv
$if      set flheur     capacity_flheur
* Budget constraints
$if      set flheur     $if      set budget_it      eq_flheur_rdbudget_it 
$if      set flheur     $if      set budget_i       eq_flheur_rdbudget_i 
$if      set flheur     $if      set budget_t       eq_flheur_rdbudget_t 
$if      set flheur     $if      set budget         eq_flheur_rdbudget
* * * Restriction constraints to help the solver into the right direction
$if      set nucrestrict                            nuclear_restriction
$if      set ccsrestrict                            ccs_restriction
$if      set windrestrict                           wind_restriction
$if      set gasrestrict                            gas_restriction
/;

* Intialize different CO2 markets to ensure report compiles even when the constraint is excluded
$if     set co2mark     co2market.M(t)                  = 0 ;
$if     set co2mark     co2market_r.M(r,t)              = 0 ;
$if     set co2mark     euets.M(t)                      = 0 ;
$if     set co2iter     it_euets.M(t)                   = 0 ;
$if     set co2mark     ukets.M(t)                      = 0 ;
$if     set co2iter     ukets.M(t)                      = 0 ;
$if     set co2mips     ukets.M(t)                      = 0 ;
$if     set co2mips     eqs_euets.M(t)                  = 0 ;
$if     set static      euets_static.M(t)               = 0 ;
$if     set static      ukets_static.M(t)               = 0 ;
$if     set targets     capmarket.M(techtarget,r,t)     = 0 ;
$if     set targets     engmarket.M(techtarget,r,t)     = 0 ;
$if     set targets     resmarket_gen.M(r,t)            = 0 ;
$if     set targets     resmarket_con.M(r,t)            = 0 ;
frnuctarget.M(t)                = 0 ;

$if not  set solver $set solver gurobi
*$if not  set solver $set solver cplex
option lp=%solver% ;
option qcp=%solver% ;
option mip=%solver% ;
option rmip=%solver% ;
option miqcp=%solver% ;
option rmiqcp=%solver% ;

euregen.optfile = 1;
euregen.holdfixed = 1;
*euregen.reslim = 7200;
euregen.reslim = 500000 ;
option solprint = on;

$if      set mip                          solve euregen using mip minimizing SURPLUS ;
$if      set miqcp                        solve euregen using miqcp minimizing SURPLUS ;
$if      set co2mips                      solve euregen using miqcp minimizing SURPLUS ;
$if      set techmin                      solve euregen using miqcp minimizing SURPLUS ;
$if      set elastic                      solve euregen using qcp minimizing WELFARE ;
$if not  set mip $if not  set miqcp $if not  set co2mips $if not  set techmin $if not  set elastic    solve euregen using lp minimizing SURPLUS ;

$if set co2iter $if set intermed    parameter
$if set co2iter $if set intermed    co2elec_out(t) 
$if set co2iter $if set intermed    ;
$if set co2iter $if set intermed    co2elec_out(t) = ECEU.L(t) + eps ;
$if set co2iter $if set intermed    execute_unload   'euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx',         co2elec_out, co2ele_int ;
$if set co2iter $if set intermed    execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\%s%.xlsx                           par=co2elec_out rng=co2elec_out!a1'
$if set co2iter $if set intermed    execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\%s%.xlsx                           par=co2ele_int  rng=co2ele_int!a1'
*Don't include report so that restart file can be used with modified report without re-running model