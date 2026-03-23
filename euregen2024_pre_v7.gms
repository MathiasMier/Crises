* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * Timeseries and calibration
set
i                                Generation technology
j                                Storage technologies /Storage_LT,Storage_ST,PumpStorage,Reservoir/
h                                Hours /1*8760/
m                                Months /1*12/
hm(h,m)                          Map between hours and months for assembling availability factors
;

$gdxin database\setpar_%n%.gdx
$load i, hm
$gdxin

* * * Fundamentals
set
v                                Vintages of generation capacity
oldv(v)                          Existing vintages
newv(v)                          New vintages
t                                Model time periods
tv(t,v)                          Time period in which vintage v is installed
r                                Model regions
;

$gdxin database\setpar_%n%.gdx
$load v, oldv, newv, t, tv, r
$gdxIn

alias(r,rr);
alias(t,tt);
alias(v,vv);

Set
$if not  set longrun    tmerge(t) /2030/
$if      set longrun    tmerge(t) /2024,2025,2026,2027,2028,2029,2030/
$if not  set longrun    vmerge(v) /2030/
$if      set longrun    vmerge(v) /2024,2025,2026,2027,2028,2029,2030/
$if not  set longrun    tmergesub(t) 
$if      set longrun    tmergesub(t) /2024,2025,2026,2027,2028,2029/
t2030(t)                          /2030/
v2030(v)                          /2030/
;

set
$if      set static                            toptimize(t)     Optimization periods /%year%/
$if not  set static     $if      set shortrun  toptimize(t)     Optimization periods /2022,2023,2024,2025,2026,2027,2028,2029,2030,2035,2040,2045,2050/
*$if not  set static     $if      set longrun   toptimize(t)     Optimization periods /2022,2025,2030,2035,2040,2045,2050/
$if not  set static     $if      set longrun   toptimize(t)     Optimization periods /2023,2030,2035,2040,2045,2050/

$if      set myopic2020 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2020/
$if      set myopic2025 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2025/
$if      set myopic2030 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2030/
$if      set myopic2035 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2035/
$if      set myopic2040 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2040/
$if      set myopic2045 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2045/
$if      set myopic2050 $if      set overlap1 toptimize(t)      Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2020,2025/
$if      set myopic2025 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2025,2030/
$if      set myopic2030 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2030,2035/
$if      set myopic2035 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2035,2040/
$if      set myopic2040 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2040,2045/
$if      set myopic2045 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if      set overlap2 toptimize(t)      Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2020,2025,2030/
$if      set myopic2025 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2025,2030,2035/
$if      set myopic2030 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2030,2035,2040/
$if      set myopic2035 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2035,2040,2045/
$if      set myopic2040 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if      set overlap3 toptimize(t)      Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2020,2025,2030,2035/
$if      set myopic2025 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2025,2030,2035,2040/
$if      set myopic2030 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2030,2035,2040,2045/
$if      set myopic2035 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if      set overlap4 toptimize(t)      Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040/
$if      set myopic2025 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045/
$if      set myopic2030 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2035,2040,2045,250/
$if      set myopic2040 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if      set overlap5 toptimize(t)      Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040,2045/
$if      set myopic2025 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045,2050/
$if      set myopic2030 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if      set overlap6 toptimize(t)      Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2020,2025,2030,2035,2040,2045,2050/
$if      set myopic2025 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2025,2030,2035,2040,2045,2050/
$if      set myopic2030 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2030,2035,2040,2045,2050/
$if      set myopic2035 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2035,2040,2045,2050/
$if      set myopic2040 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2040,2045,2050/
$if      set myopic2045 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2045,2050/
$if      set myopic2050 $if      set overlap7 toptimize(t)      Myopic (or static) optimization year /2050/
;


set
topt2023(t)
topt2024(t)
topt2024_2030(t)
t2023(t)
told(t) /2020,2021,2022,2023/
tcri(t) /2022,2023/
;

topt2023(t)$(toptimize(t) and t.val le 2023) = YES ;
topt2024(t)$(toptimize(t) and t.val ge 2024) = YES ;
topt2024_2030(t)$(toptimize(t) and t.val ge 2024 and t.val le 2030) = YES ;
t2023(t)$(t.val le 2023) = YES ;


set
l
superirnw
quantiles
irnw_mapq(i,quantiles)
superirnw_map(i,superirnw)
superirnw_mapq(i,quantiles,superirnw)
;

$gdxin database\setpar_%n%.gdx
$load l, superirnw, quantiles, superirnw_map, superirnw_mapq, irnw_mapq
$gdxin

* * * Generation technology
set
new(i)                           New generation technology
exi(i)                           Existing technologies (or capacities) in base year - EXISTING BLOCKS
ccs(i)                           CCS generation technologies (or capacities) - CCS BLOCKS
conv(i)                          Conventional generation technologies
irnw(i)                          Intermittent renewable generation technologies
lig(i)                           Lignite technologies
oil(i)
coa(i)                           Coal technologies
gas(i)                           Gas technologies
bio(i)                           Biomass technologies
sol(i)                           Solar technologies
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
;

$gdxin database\setpar_%n%.gdx
$load new, exi, ccs, irnw, conv, sol, roof, open, wind, windon, windoff, rnw, lowcarb, nuc, coa, lig, type, idef, gas, bio, chp, nochp, mapchp
$gdxin

oil("OilOther") = YES ;
oil("Oil_CHP") = YES ;

iidef(i,type) = idef(i,type) ;
alias(i,ii) ;

parameter
cap(i,v,r)                       Capacity installed by region (GW)
capt_int(i,v,r,t)                Capacity installed by region in period t(GW)
capt(i,v,r,t)                    Capacity installed by region in period t(GW)
invlimUP(i,r,t)                  Upper bounds on investment based on potential (cumulative since last time period) (GW)
invlimLO(i,r,t)                  Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
invlimUP_eu(i,t)                 Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
invlife(i,v,r)                   Capacity lifetime
invdepr(i,v,r)                   Investment depreciation
capcost_int(i,v,r)               Capacity cost (investment) by region
capcost(i,v,r)                   Capacity cost (investment) by region
fomcost_int(i,v,r)               Fixed OM cost
fomcost(i,v,r)                   Fixed OM cost
vomcost(i,v,r)                   Variable OM cost
effrate(i,v,r)                   Efficiency
effrate_int(i,v,r)
co2captured_int(i,v,r)           CCS capture rate
co2captured(i,v,r)               CCS capture rate
emit_int(i,v,r)                  Emission factor
emit(i,v,r)                      Emission factor
reliability(i,v,r)               Reliability factor by region and technology
capcred(i,v,r)                   Capacity credit by region and technology
mindisp(i,v,r)                   Min load by region and technology
sclim_int(r)                     Upper bound on geologic storage of carbon (GtCO2)
sclim_eu_int                     Upper bound on geologic storage of carbon (GtCO2)
sclim(r)                         Upper bound on geologic storage of carbon (GtCO2)
sclim_eu                         Upper bound on geologic storage of carbon (GtCO2)
biolim_r_int(r,t)                Upper bounds by region on biomass use (MWh)
biolim_int(t)                    Upper bounds by region on biomass use (MWh)
biolim_r(r,t)                    Upper bounds by region on biomass use (MWh)
biolim(t)                        Upper bounds by region on biomass use (MWh)
;

$gdxin database\setpar_%n%.gdx
$load capt_int=capt, invlimUP, invlimLO, invlimUP_eu, invlife, invdepr, capcost_int=capcost, fomcost_int=fomcost, vomcost, effrate_int=effrate, co2captured_int=co2captured, emit_int=emit, reliability, capcred, mindisp
$load sclim_int=sclim, sclim_eu_int=sclim_eu, biolim_r_int=biolim, biolim_int=biolim_eu
$gdxin

* * CO2 emission correction via effrate
set
ivr(i,v,r)
;

effrate(i,v,r) = effrate_int(i,v,r) ;
ivr(i,v,r)$effrate(i,v,r) = YES ;

effrate(ivr(nochp(i),oldv(v),r))$(not bio(i)) = round(min(effrate_int(i,"2024",r), effrate_int(i,v,r)),4) ;
effrate(ivr(chp(i),oldv(v),r))$(not bio(i)) = round(effrate_int(i,v,r),4) ;
effrate(ivr(chp(i),oldv(v),r))$(not bio(i) and sum(mapchp(i,nochp(ii)), effrate_int(ii,v,r)) > 0) = round(min(sum(mapchp(i,nochp(ii)), effrate_int(ii,v,r)),effrate_int(i,v,r)),4) ;

set
rwindoff(r)
;

rwindoff(r)$(capcost_int("Windoff_q90","2022",r) > 0) = YES ;

capcost(i,v,r) = capcost_int(i,v,r) ;
$if      set scwindon10        capcost(windon(i),v,r) = 1.1 * capcost_int(i,v,r) ;
$if      set scwindon20        capcost(windon(i),v,r) = 1.2 * capcost_int(i,v,r) ;
$if      set scwindon30        capcost(windon(i),v,r) = 1.3 * capcost_int(i,v,r) ;
$if      set scwindon40        capcost(windon(i),v,r) = 1.4 * capcost_int(i,v,r) ;
$if      set scwindon50        capcost(windon(i),v,r) = 1.5 * capcost_int(i,v,r) ;
$if      set scwindon60        capcost(windon(i),v,r) = 1.6 * capcost_int(i,v,r) ;
$if      set scwindon70        capcost(windon(i),v,r) = 1.7 * capcost_int(i,v,r) ;
$if      set scwindon80        capcost(windon(i),v,r) = 1.8 * capcost_int(i,v,r) ;
$if      set scwindon90        capcost(windon(i),v,r) = 1.9 * capcost_int(i,v,r) ;
$if      set scwindon100       capcost(windon(i),v,r) = 2.0 * capcost_int(i,v,r) ;

$if      set windoncap70       capcost(windon(i),"2050",r) = 0.7 * capcost_int(i,"2050",r) ;
$if      set windoncap80       capcost(windon(i),"2050",r) = 0.8 * capcost_int(i,"2050",r) ;
$if      set windoncap90       capcost(windon(i),"2050",r) = 0.9 * capcost_int(i,"2050",r) ;
$if      set windoncap110      capcost(windon(i),"2050",r) = 1.1 * capcost_int(i,"2050",r) ;
$if      set windoncap120      capcost(windon(i),"2050",r) = 1.2 * capcost_int(i,"2050",r) ;
$if      set windoncap130      capcost(windon(i),"2050",r) = 1.3 * capcost_int(i,"2050",r) ;
$if      set windoncap140      capcost(windon(i),"2050",r) = 1.4 * capcost_int(i,"2050",r) ;

$if      set windoninterpol    capcost(windon(i),newv(v),r)$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost(i,"2022",r) + (capcost(i,"2050",r) - capcost(i,"2022",r)) * (v.val - 2022) / (2050 - 2022) ;

$if      set windoffcap70      capcost(windoff(i),"2050",rwindoff(r)) = 0.7 * capcost_int(i,"2050",r) ;
$if      set windoffcap80      capcost(windoff(i),"2050",rwindoff(r)) = 0.8 * capcost_int(i,"2050",r) ;
$if      set windoffcap90      capcost(windoff(i),"2050",rwindoff(r)) = 0.9 * capcost_int(i,"2050",r) ;
$if      set windoffcap110     capcost(windoff(i),"2050",rwindoff(r)) = 1.1 * capcost_int(i,"2050",r) ;
$if      set windoffcap120     capcost(windoff(i),"2050",rwindoff(r)) = 1.2 * capcost_int(i,"2050",r) ;
$if      set windoffcap130     capcost(windoff(i),"2050",rwindoff(r)) = 1.3 * capcost_int(i,"2050",r) ;
$if      set windoffcap140     capcost(windoff(i),"2050",rwindoff(r)) = 1.4 * capcost_int(i,"2050",r) ;

$if      set windoffinterpol   capcost(windoff(i),newv(v),rwindoff(r))$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost(i,"2022",r) + (capcost(i,"2050",r) - capcost(i,"2022",r)) * (v.val - 2022) / (2050 - 2022) ;

$if      set solcap70       capcost(sol(i),"2050",r) = 0.7 * capcost_int(i,"2050",r) ;
$if      set solcap80       capcost(sol(i),"2050",r) = 0.8 * capcost_int(i,"2050",r) ;
$if      set solcap90       capcost(sol(i),"2050",r) = 0.9 * capcost_int(i,"2050",r) ;
$if      set solcap110      capcost(sol(i),"2050",r) = 1.1 * capcost_int(i,"2050",r) ;
$if      set solcap120      capcost(sol(i),"2050",r) = 1.2 * capcost_int(i,"2050",r) ;
$if      set solcap130      capcost(sol(i),"2050",r) = 1.3 * capcost_int(i,"2050",r) ;
$if      set solcap140      capcost(sol(i),"2050",r) = 1.4 * capcost_int(i,"2050",r) ;

$if      set solinterpol   capcost(sol(i),newv(v),r)$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost(i,"2022",r) + (capcost(i,"2050",r) - capcost(i,"2022",r)) * (v.val - 2022) / (2050 - 2022) ;

* Create CHP capacities when activated
capt_int(i,"2023",r,"2022") = 0 ;
capt(i,v,r,t)       = capt_int(i,v,r,t) ;

reliability(i,v,r)$(capcost(i,v,r) > 0 and sameas(i,"Geothermal")) = 0.85 ;

* Set existing capacities to 2021 levels to push for directed pipeline investment within the model (better reflects "true cost")
cap(i,v,r) = capt(i,v,r,"2021") ;
* Correct austria oil capacity
*capt(oil(i),v,r,"2023")$(sameas(r,"Austria") and v.val le 2022) = capt(i,v,r,"2022") ;
* Correcting nuclear fix cost
fomcost(i,v,r) = fomcost_int(i,v,r) ;
fomcost(irnw(i),"2022",r)$(fomcost_int(i,"2022",r) > 0 and fomcost_int(i,"2021",r) > 0 and fomcost_int(i,"2023",r) > 0) = fomcost_int(i,"2021",r)/2 + fomcost_int(i,"2023",r)/2 ;
fomcost(irnw(i),"2022",r)$(fomcost_int(i,"2022",r) > 0 and fomcost_int(i,"2021",r) = 0 and fomcost_int(i,"2023",r) > 0) = fomcost_int(i,"2023",r) ;
fomcost(irnw(i),"2022",r)$(fomcost_int(i,"2022",r) > 0 and fomcost_int(i,"2021",r) > 0 and fomcost_int(i,"2023",r) = 0) = fomcost_int(i,"2021",r) ;

set
rnonuc(r)
rnuc(r)
rnolig(r)
rlig(r)
rnocoa(r)
rcoa(r)
;

$if      set nucall  rnuc(r)   = YES ;
$if      set nucall  rnonuc(r) = NO ;
$if not  set nucall  rnonuc(r)$(sum(v$(v.val le 2023), capcost("Nuclear",v,r)) = 0) = YES ;
$if not  set nucall  rnuc(r)$(sum(v$(v.val le 2023), capcost("Nuclear",v,r))) = YES ;

rnolig(r)$(sum(v$(v.val le 2023), capcost("Lignite",v,r)) = 0) = YES ;
rlig(r)$(sum(v$(v.val le 2023), capcost("Lignite",v,r))) = YES ;

rnocoa(r)$(sum(v$(v.val le 2023), capcost("Coal",v,r)) = 0) = YES ;
rcoa(r)$(sum(v$(v.val le 2023), capcost("Coal",v,r))) = YES ;

$if not  set nucall  rnuc("Bulgaria") = YES ;
$if not  set nucall  rnuc("Poland") = YES ;
$if not  set nucall  rnuc("Germany") = NO ;
$if not  set nucall  rnonuc("Bulgaria") = NO ;
$if not  set nucall  rnonuc("Poland") = NO ;
$if not  set nucall  rnonuc("Germany") = YES ;

$if not  set nucall  capcost("Nuclear",newv(v),rnonuc(r)) = 0 ;
$if not  set nucall  capcost("Coal",newv(v),rnocoa(r)) = 0 ;
$if not  set nucall  capcost("Lignite",newv(v),rnolig(r)) = 0 ;

$if not  set nucall  fomcost("Nuclear",newv(v),rnonuc(r)) = 0 ;
$if not  set nucall  fomcost("Coal",newv(v),rnocoa(r)) = 0 ;
$if not  set nucall  fomcost("Lignite",newv(v),rnolig(r)) = 0 ;

$if not  set nucall  vomcost("Nuclear",newv(v),rnonuc(r)) = 0 ;
$if not  set nucall  vomcost("Coal",newv(v),rnocoa(r)) = 0 ;
$if not  set nucall  vomcost("Lignite",newv(v),rnolig(r)) = 0 ;

$if      set windoff10         fomcost(windoff(i),v,r) = 0.1 * fomcost_int(i,v,r) ;
$if      set windoff20         fomcost(windoff(i),v,r) = 0.2 * fomcost_int(i,v,r) ;
$if      set windoff30         fomcost(windoff(i),v,r) = 0.3 * fomcost_int(i,v,r) ;
$if      set windoff40         fomcost(windoff(i),v,r) = 0.4 * fomcost_int(i,v,r) ;
$if      set windoff50         fomcost(windoff(i),v,r) = 0.5 * fomcost_int(i,v,r) ;
$if      set windoff60         fomcost(windoff(i),v,r) = 0.6 * fomcost_int(i,v,r) ;
$if      set windoff70         fomcost(windoff(i),v,r) = 0.7 * fomcost_int(i,v,r) ; 
$if      set windoff80         fomcost(windoff(i),v,r) = 0.8 * fomcost_int(i,v,r) ;
$if      set windoff90         fomcost(windoff(i),v,r) = 0.9 * fomcost_int(i,v,r) ;

$if      set windoffcap        capcost(windoff(i),v,r)$(v.val ge 2025) = 0.9 * capcost_int(i,v,r) ;
$if      set windoffcap        capcost(windoff(i),v,r)$(v.val ge 2030) = 0.9 * capcost_int(i,v,r) ;
$if      set windoffcap        capcost(windoff(i),v,r)$(v.val ge 2035) = 0.85 * capcost_int(i,v,r) ;
$if      set windoffcap        capcost(windoff(i),v,r)$(v.val ge 2040) = 0.8 * capcost_int(i,v,r) ;
$if      set windoffcap        capcost(windoff(i),v,r)$(v.val ge 2045) = 0.75 * capcost_int(i,v,r) ;

$if      set nuclearcapreal    capcost("Nuclear","2020",rnuc(r)) = 8250 ;
$if      set nuclearcapreal    capcost("Nuclear","2021",rnuc(r)) = 8250 ;
$if      set nuclearcapreal    capcost("Nuclear","2022",rnuc(r)) = 8250 ;
$if      set nuclearcapreal    capcost("Nuclear","2023",rnuc(r)) = 8250 ;
$if      set nuclearcapreal    capcost("Nuclear","2024",rnuc(r)) = 8250 ;
$if      set nuclearcap2050    capcost("Nuclear","2050",rnuc(r)) = 5000 ;

$if      set nuclearcapreal    fomcost("Nuclear","2024",rnuc(r)) = 144 ;
$if      set nuclearcap2050    fomcost("Nuclear","2050",rnuc(r)) = 105 ;

$if      set nuclearfom25      fomcost("Nuclear","2050",rnuc(r)) = 0.25 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom50      fomcost("Nuclear","2050",rnuc(r)) = 0.5 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom75      fomcost("Nuclear","2050",rnuc(r)) = 0.75 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom125     fomcost("Nuclear","2050",rnuc(r)) = 1.25 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom150     fomcost("Nuclear","2050",rnuc(r)) = 1.5 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom175     fomcost("Nuclear","2050",rnuc(r)) = 1.75 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom200     fomcost("Nuclear","2050",rnuc(r)) = 2.0 * fomcost_int("Nuclear","2050",r) ;

$if      set nuclearcap25      capcost("Nuclear","2050",rnuc(r)) = 0.25 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap50      capcost("Nuclear","2050",rnuc(r)) = 0.5 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap75      capcost("Nuclear","2050",rnuc(r)) = 0.75 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap125     capcost("Nuclear","2050",rnuc(r)) = 1.25 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap150     capcost("Nuclear","2050",rnuc(r)) = 1.5 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap175     capcost("Nuclear","2050",rnuc(r)) = 1.75 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap200     capcost("Nuclear","2050",rnuc(r)) = 2.0 * capcost_int("Nuclear","2050",r) ;

$if      set frnuccapreal      capcost("Nuclear","2024",rnuc(r)) = 8250 ;
$if      set frnuclearcap2050  capcost("Nuclear","2050",rnuc(r)) = 5000 ;

$if      set frnucfom25        fomcost("Nuclear","2050","France") = 0.25 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom50        fomcost("Nuclear","2050","France") = 0.5 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom75        fomcost("Nuclear","2050","France") = 0.75 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom125       fomcost("Nuclear","2050","France") = 1.25 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom150       fomcost("Nuclear","2050","France") = 1.5 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom175       fomcost("Nuclear","2050","France") = 1.75 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom200       fomcost("Nuclear","2050","France") = 2.0 * fomcost_int("Nuclear","2050","France") ;

$if      set frnuccap25        capcost("Nuclear","2050","France") = 0.25 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap50        capcost("Nuclear","2050","France") = 0.5 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap75        capcost("Nuclear","2050","France") = 0.75 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap125       capcost("Nuclear","2050","France") = 1.25 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap150       capcost("Nuclear","2050","France") = 1.5 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap175       capcost("Nuclear","2050","France") = 1.75 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap200       capcost("Nuclear","2050","France") = 2.0 * capcost_int("Nuclear","2050","France") ;

*vomcost("Nuclear",newv(v),rnuc(r)) = 8 ;
$if      set nuclearinterpol   capcost("Nuclear",newv(v),rnuc(r))$(not sameas(v,"2024") or not sameas(v,"2050")) = capcost("Nuclear","2024",r) + (capcost("Nuclear","2050",r) - capcost("Nuclear","2024",r)) * (v.val - 2024) / (2050 - 2024) ;
$if      set nuclearinterpol   fomcost("Nuclear",newv(v),rnuc(r))$(not sameas(v,"2024") or not sameas(v,"2050")) = fomcost("Nuclear","2024",r) + (fomcost("Nuclear","2050",r) - fomcost("Nuclear","2024",r)) * (v.val - 2024) / (2050 - 2024) ;


parameter
deccost(i,v,r)
;

deccost("Nuclear",v,r)$(capcost("Nuclear",v,r) > 0) = 0 ;
$if      set deccost250    deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 250 ;
$if      set deccost500    deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 500 ;
$if      set deccost750    deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 500 ;
$if      set deccost1000   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1000 ;
$if      set deccost1250   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1250 ;
$if      set deccost1500   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1500 ;
$if      set deccost1750   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1500 ;
$if      set deccost2000   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 2000 ;

*$if      set nucall  capcost("Nuclear",newv(v),rnonuc)            = capcost("Nuclear",v,"France") ;
*$if      set nucall  deccost("Nuclear",newv(v),rnonuc)            = deccost("Nuclear",v,"France") ;
*$if      set nucall  fomcost("Nuclear",newv(v),rnonuc)            = fomcost("Nuclear",v,"France") ;
*$if      set nucall  vomcost("Nuclear",newv(v),rnonuc)            = vomcost("Nuclear",v,"France") ;
*$if      set nucall  reliability("Nuclear",newv(v),rnonuc)        = reliability("Nuclear",v,"France") ;
*$if      set nucall  effrate("Nuclear",newv(v),rnonuc)            = effrate("Nuclear",v,"France") ;
*$if      set nucall  capcred("Nuclear",newv(v),rnonuc)            = capcred("Nuclear",v,"France") ;
*$if      set nucall  invlimUP("Nuclear",rnonuc,t)$(t.val ge 2022) = invlimUP("Nuclear","France",t) ;

* Correcting SC limits because much of this potential is needed after 2050 when "sucking" technologies become competitive (to restore climate)
sclim_eu = 1   * sclim_eu_int ;
$if      set sclim10           sclim_eu = 0.1 * sclim_eu_int ;
$if      set sclim20           sclim_eu = 0.2 * sclim_eu_int ;
$if      set sclim50           sclim_eu = 0.5 * sclim_eu_int ;
$if      set sclim100          sclim_eu = 1.0 * sclim_eu_int ;
sclim(r) = 1   * sclim_int(r) ;
$if      set sclim10           sclim(r) = 0.1 * sclim_int(r) ;
$if      set sclim20           sclim(r) = 0.2 * sclim_int(r) ;
$if      set sclim50           sclim(r) = 0.5 * sclim_int(r) ;
$if      set sclim100          sclim(r) = 1.0 * sclim_int(r) ;

* Correcting biomass limits because not all biomass can go into the power plants (food, traffic, ...)
biolim(t) = 1 * biolim_int(t) ;
$if      set biolimhalf        biolim(t) = 0.5 * biolim_int(t) ;
$if      set biolimdouble      biolim(t) = 2 * biolim_int(t) ;
biolim_r(r,t) = 1 * biolim_r_int(r,t) ;
$if      set biolimhalf        biolim_r(r,t) = 0.5 * biolim_r_int(r,t) ;
$if      set biolimdouble      biolim_r(r,t) = 2 * biolim_r_int(r,t) ;
   
$if      set biomarket                          $include modules_precal\euregen2024_pre_biomass_v1.gms
$if      set biomarket_r                        $include modules_precal\euregen2024_pre_biomass_v1.gms  
* * * Storage technology
set
newj(j)                          New storage technology
exij(j)                          Existing storage technology
;

$gdxin database\setpar_%n%.gdx
$load newj, exij
$gdxin

parameter
gcapt(j,v,r,t)                   Storage capacity by region (GW)
ghours(j,v,r)                    Hours of storage (room size relative to door size)
chrgpen(j,v,r)                   Charge efficiency penalty for storage by region (< 1)
dchrgpen(j,v,r)                  Discharge efficiency penalty for storage by region (< 1)
dischrg(j,v,r)                   Automatic storage discharge by region (in percent) (< 1)
gcapcost_int(j,v,r)              Capital cost of storage charge-discharge capacity by region (EUR per MW)
gcapcost(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gfomcost(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gvomcost(j,v,r)                  Variable OM cost for storage by region (EUR per MWh)
greliability(j,v,r)              Storage reliability factor by region and technology
gcapcred(j,v,r)                  Storage capacity credit by region and technology
ginvlife(j,v,r)                    Storage investment life for new capacity additions (years)
ginvdepr(j,v,r)                    Storage investment life for new capacity additions (years)
ginvlimLO(j,r,t)                 Storage investment lower bound (GW)
ginvlimUP(j,r,t)                 Storage investment upper bound (GW)
ginvlimUP_eu(j,t)                Storage investment upper bound (GW)
;

parameter
gcapt(j,v,r,t)                   Storage capacity by region (GW)
ghours(j,v,r)                    Hours of storage (room size relative to door size)
chrgpen(j,v,r)                   Charge efficiency penalty for storage by region (< 1)
dchrgpen(j,v,r)                  Discharge efficiency penalty for storage by region (< 1)
dischrg(j,v,r)                   Automatic storage discharge by region (in percent) (< 1)
gcapcost(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gcapcostc(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW) Charging?
gcapcostd(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW) Discharge?
gcapcostr(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW) reservoir or storage medium?  Ist das die richtige Beschriftung oder einfach Copy paste?
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
hydtwh(r,t)
;

$gdxin database\setpar_%n%.gdx
$load gcapt, ghours, chrgpen, dchrgpen, dischrg, gcapcost_int=gcapcost, gfomcost, gvomcost, greliability, gcapcred, ginvlife, ginvdepr, ginvlimLO, ginvlimUP, ginvlimUP_eu
$gdxin

Set
jres(j)
nvjres(j)
jpump(j)
nvj(j) /Reservoir,PumpStorage/
;

jres(j)$(sameas(j,"Reservoir")) = YES ;
nvjres(j)$(sameas(j,"Reservoir") and nvj(j)) = YES ;
jpump(j)$(sameas(j,"PumpStorage")) = YES ;

gcapcost(j,v,r)                                          = 1   * gcapcost_int(j,v,r) ;
$if      set halfgcc     gcapcost(j,v,r)$(v.val ge 2020) = 0.5 * gcapcost_int(j,v,r) ;
$if      set doublegcc   gcapcost(j,v,r)$(v.val ge 2020) = 2   * gcapcost_int(j,v,r) ;
$if      set triplegcc   gcapcost(j,v,r)$(v.val ge 2020) = 3   * gcapcost_int(j,v,r) ;

chrgpen("Storage_LT",newv(v),r) = dchrgpen("Storage_LT",v,r) ;
dchrgpen("Storage_LT",newv(v),r) = effrate("Gas_CCGT",v,r) ;
gcapcost("Storage_LT","2020",r) = round(750 + 850 + 300 + 561   + 0.62 * ghours("Storage_LT","2020",r) + 8.5,4)  ;
gcapcost("Storage_LT","2050",r) = round(300 + 850 + 150 + 280.5 + 0.31 * ghours("Storage_LT","2020",r) + 4.25,4) ;
gfomcost("Storage_LT","2020",r) = round(13  + 21.25 + (561   + 0.62 * ghours("Storage_LT","2020",r) + 8.5 ) * 0.02,4) ;
gfomcost("Storage_LT","2050",r) = round(9   + 21.25 + (280.5 + 0.31 * ghours("Storage_LT","2020",r) + 4.25) * 0.02,4) ;
*Where do these numbers come from
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
himport(r,toptimize(t)) = round(himport(r,"2020") + (himport(r,"2050") - himport(r,"2020")) * (t.val - 2020) / (2050 - 2020),4) ;


Parameter
ghoursnv(j,r,t)
dischrgnv(j,r)
chrgpennv(j,r)
dchrgpennv(j,r)
;

*storage
ghoursnv(nvj(j),r,topt2023(t))$(sum(oldv(v), gcapt(j,v,r,t)) > 0)       = sum(oldv(v), gcapt(j,v,r,t) * ghours(j,v,r)) / sum(oldv(v), gcapt(j,v,r,t)) ;
*charge
chrgpennv(nvj(j),r)$(sum(oldv(v), gcapt(j,v,r,"2023")) > 0)     = sum(oldv(v), gcapt(j,v,r,"2023") * chrgpen(j,v,r)) / sum(oldv(v), gcapt(j,v,r,"2023")) ;
*discharge
dchrgpennv(nvj(j),r)$(sum(oldv(v), gcapt(j,v,r,"2023")) > 0)   = sum(oldv(v), gcapt(j,v,r,"2023") * dchrgpen(j,v,r)) / sum(oldv(v), gcapt(j,v,r,"2023")) ;
ghoursnv(nvj(j),r,topt2024(t))   = ghoursnv(j,r,"2023") ;

dischrgnv(j,r)  = smax(v, dischrg(j,v,r)) ;
*chrgpennv(j,r)  = smax(v, chrgpen(j,v,r)) ;
*dchrgpennv(j,r) = smax(v, dchrgpen(j,v,r)) ;

* * * Transmission technology
set
k                               Transmission technologies
tmap(k,r,r)          Regions eligible for transmission exchange by technology
;

$gdxin database\setpar_%n%.gdx
$load k, tmap
$gdxin

parameter
tcap(k,r,r)                     Transmission capacity from region X to region Y (GW)
tcapcost(k,r,r)                 Transmission investment cost ($ per kW)
tfomcost(k,r,r)                 Fixed O&M cost of new transmision capacity (euro per kW-year)
tvomcost(k,r,r)                 Variable O&M cost of new transmision capacity (euro per MWh)
trnspen(k,r,r)                  Transmission loss penalty
tinvlimUP_int(k,r,r,t)          Upper bound on total transmission capacity from region X to region Y (GW)
tinvlimUP(k,r,r,t)              Upper bound on total transmission capacity from region X to region Y (GW)
tinvlimLO(k,r,r,t)              Lower bound on total transmission capacity from region X to region Y (GW)
tinvlimUP_eu(k,t)               Lower bound on total transmission capacity from region X to region Y (GW)
tcapcred(k,r,r)                 Capacity credit for transmisson by region and technology
tinvlife(k)                     Capacity lifetime (years)
tinvdepr(k)                     Investment depreciation (years)
;

$gdxin database\setpar_%n%.gdx
$load tcap, tcapcost, tfomcost, tvomcost, trnspen, tinvlimUP_int=tinvlimUP, tinvlimLO, tinvlimUP_eu, tcapcred, tinvlife, tinvdepr
$gdxin

tinvlimUP(k,r,rr,t)                                           = 1   * tinvlimUP_int(k,r,rr,t) ;
$if      set flatntc     tinvlimUP(k,r,rr,t)$(t.val ge 2035)  =       tinvlimUP(k,r,rr,"2030") ;
$if      set flatntc     tinvlimLO(k,r,rr,t)$(t.val ge 2035)  =       min(tinvlimLO(k,r,rr,t),  tinvlimUP(k,r,rr,"2030")) ;
$if      set lowntc      tinvlimUP(k,r,rr,t)$(t.val ge 2035)  =       tinvlimLO(k,r,rr,t) ;
$if      set doublentc   tinvlimUP(k,r,rr,t)$(t.val ge 2035)  = 2   * tinvlimUP_int(k,r,rr,t) ;
$if      set triplentc   tinvlimUP(k,r,rr,t)$(t.val ge 2035)  = 3   * tinvlimUP_int(k,r,rr,t) ;

$if      set ntc000       tinvlimUP(k,r,rr,"2050") = 1.00 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc025       tinvlimUP(k,r,rr,"2050") = 1.25 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc050       tinvlimUP(k,r,rr,"2050") = 1.50 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc075       tinvlimUP(k,r,rr,"2050") = 1.75 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc100       tinvlimUP(k,r,rr,"2050") = 2.00 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc125       tinvlimUP(k,r,rr,"2050") = 2.25 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc150       tinvlimUP(k,r,rr,"2050") = 2.50 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc175       tinvlimUP(k,r,rr,"2050") = 2.75 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntc200       tinvlimUP(k,r,rr,"2050") = 3.00 * tinvlimUP_int(k,r,rr,"2035") ;
$if      set ntcsce       tinvlimUP(k,r,rr,t)$(t.val ge 2036 and tinvlimUP(k,r,rr,"2035") > 0) = tinvlimUP(k,r,rr,"2035") + (tinvlimUP(k,r,rr,"2050") - tinvlimUP(k,r,rr,"2035")) * (t.val - 2035) / (2050 - 2035) ;

tinvlimLO(k,r,rr,t)$(tinvlimLO(k,r,rr,t) < tcap(k,r,rr)) =  tcap(k,r,rr) ;
tinvlimUP(k,r,rr,t)$(tinvlimUP(k,r,rr,t) < tcap(k,r,rr)) =  tcap(k,r,rr) ;
tinvlimUP(k,r,rr,t)$(tinvlimUP(k,r,rr,t) < tinvlimLO(k,r,rr,t)) =  tcap(k,r,rr) ;

Set
tmapopt(k,r,r,t)
t2022(t)
t2022plus(t)
tmapopt_invup(k,r,rr,t)
tmapopt_invlo(k,r,rr,t)
;

tmapopt(k,r,rr,t)$(tmap(k,r,rr) and toptimize(t)) = YES ;
t2022(t)$(toptimize(t) and t.val le 2022) = YES ;
t2022plus(t)$(toptimize(t) and t.val ge 2023) = YES ;
tmapopt_invup(k,r,rr,t)$(tmap(k,r,rr) and toptimize(t) and tinvlimUP(k,r,rr,t) and t.val ge 2022) = YES ;
tmapopt_invlo(k,r,rr,t)$(tmap(k,r,rr) and toptimize(t) and tinvlimLO(k,r,rr,t) and t.val ge 2022) = YES ;

* * * Discounting
parameter
nyrs(t)                         Number of years since last time step
drate                           Annual discount rate
dfact(t)                        Discount factor for time period t (reflects number of years) for both
annuity(i,v)                     Annuity factor for generation capacity
gannuity(j,v)                   Annuity factor for storage capacity
tannuity(k)                     Annuity factor for transmission capacity
annuity_ir(i,v,r) Annuity factor for generation capacity
annuity_jr(j,v,r) Annuity factor for storage capacity
annuity_kr(k,v,r) Annuity factor for transmission capacity
;

$gdxin database\setpar_%n%.gdx
$load nyrs, drate, dfact, annuity, gannuity, tannuity, annuity_ir, annuity_jr, annuity_kr
$gdxin

$if      set longrun   nyrs("2030") = 7 ;
$if      set longrun   nyrs(t)$(not toptimize(t)) = 0 ;

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
;

$gdxin database\setpar_%n%.gdx
$load lifetime, deprtime
$load glifetime, gdeprtime
$load tlifetime, tdeprtime
$load modeldepr, modeldepr_nodisc
$load gmodeldepr, gmodeldepr_nodisc
$load tmodeldepr, tmodeldepr_nodisc
;

* * * Myopic modules
$gdxin database\setpar_%n%.gdx
$if not  set myopic $if not  set nodisc                           $load endeffect=modeldepr
$if not  set myopic $if      set nodisc                           $load endeffect=modeldepr_nodisc
$if not  set myopic $if not  set nodisc                           $load gendeffect=gmodeldepr
$if not  set myopic $if      set nodisc                           $load gendeffect=gmodeldepr_nodisc
$if not  set myopic $if not  set nodisc                           $load tendeffect=tmodeldepr
$if not  set myopic $if      set nodisc                           $load tendeffect=tmodeldepr_nodisc
$if      set myopic $if not  set nodisc   $if      set overlap1        $load endeffect=modeldepr_myopic
$if      set myopic $if      set nodisc       $if      set overlap1        $load endeffect=modeldepr_nodisc_myopic
$if      set myopic $if not  set nodisc   $if      set overlap2        $load endeffect=modeldepr_myopic_overlap2
$if      set myopic $if      set nodisc       $if      set overlap2        $load endeffect=modeldepr_nodisc_myopic_overlap2
$if      set myopic $if not  set nodisc   $if      set overlap3        $load endeffect=modeldepr_myopic_overlap3
$if      set myopic $if      set nodisc       $if      set overlap3        $load endeffect=modeldepr_nodisc_myopic_overlap3
$if      set myopic $if not  set nodisc   $if      set overlap4        $load endeffect=modeldepr_myopic_overlap4
$if      set myopic $if      set nodisc       $if      set overlap4        $load endeffect=modeldepr_nodisc_myopic_overlap4
$if      set myopic $if not  set nodisc   $if      set overlap5        $load endeffect=modeldepr_myopic_overlap5
$if      set myopic $if      set nodisc       $if      set overlap5        $load endeffect=modeldepr_nodisc_myopic_overlap5
$if      set myopic $if not  set nodisc   $if      set overlap6        $load endeffect=modeldepr_myopic_overlap6
$if      set myopic $if      set nodisc       $if      set overlap6        $load endeffect=modeldepr_nodisc_myopic_overlap6
$if      set myopic $if not  set nodisc   $if      set overlap7        $load endeffect=modeldepr_myopic_overlap7
$if      set myopic $if      set nodisc       $if      set overlap7        $load endeffect=modeldepr_nodisc_myopic_overlap7
$if      set myopic $if not  set nodisc   $if      set overlap1        $load gendeffect=gmodeldepr_myopic
$if      set myopic $if      set nodisc       $if      set overlap1        $load gendeffect=gmodeldepr_nodisc_myopic
$if      set myopic $if not  set nodisc   $if      set overlap2        $load gendeffect=gmodeldepr_myopic_overlap2
$if      set myopic $if      set nodisc       $if      set overlap2        $load gendeffect=gmodeldepr_nodisc_myopic_overlap2
$if      set myopic $if not  set nodisc   $if      set overlap3        $load gendeffect=gmodeldepr_myopic_overlap3
$if      set myopic $if      set nodisc       $if      set overlap3        $load gendeffect=gmodeldepr_nodisc_myopic_overlap3
$if      set myopic $if      set nodisc       $if      set overlap3        $load gendeffect=gmodeldepr_nodisc_myopic_overlap3
$if      set myopic $if not  set nodisc   $if      set overlap4        $load gendeffect=gmodeldepr_myopic_overlap4
$if      set myopic $if      set nodisc       $if      set overlap4        $load gendeffect=gmodeldepr_nodisc_myopic_overlap4
$if      set myopic $if not  set nodisc   $if      set overlap5        $load gendeffect=gmodeldepr_myopic_overlap5
$if      set myopic $if      set nodisc       $if      set overlap5        $load gendeffect=gmodeldepr_nodisc_myopic_overlap5
$if      set myopic $if not  set nodisc   $if      set overlap6        $load gendeffect=gmodeldepr_myopic_overlap6
$if      set myopic $if      set nodisc       $if      set overlap6        $load gendeffect=gmodeldepr_nodisc_myopic_overlap6
$if      set myopic $if not  set nodisc   $if      set overlap7        $load gendeffect=gmodeldepr_myopic_overlap7
$if      set myopic $if      set nodisc       $if      set overlap7        $load gendeffect=gmodeldepr_nodisc_myopic_overlap7
$if      set myopic $if not  set nodisc   $if      set overlap1        $load tendeffect=tmodeldepr_myopic
$if      set myopic $if      set nodisc       $if      set overlap1        $load tendeffect=tmodeldepr_nodisc_myopic
$if      set myopic $if not  set nodisc   $if      set overlap2        $load tendeffect=tmodeldepr_myopic_overlap2
$if      set myopic $if      set nodisc       $if      set overlap2        $load tendeffect=tmodeldepr_nodisc_myopic_overlap2
$if      set myopic $if not  set nodisc   $if      set overlap3        $load tendeffect=tmodeldepr_myopic_overlap3
$if      set myopic $if      set nodisc       $if      set overlap3        $load tendeffect=tmodeldepr_nodisc_myopic_overlap3
$if      set myopic $if not  set nodisc   $if      set overlap4        $load tendeffect=tmodeldepr_myopic_overlap4
$if      set myopic $if      set nodisc       $if      set overlap4        $load tendeffect=tmodeldepr_nodisc_myopic_overlap4
$if      set myopic $if not  set nodisc   $if      set overlap5        $load tendeffect=tmodeldepr_myopic_overlap5
$if      set myopic $if      set nodisc       $if      set overlap5        $load tendeffect=tmodeldepr_nodisc_myopic_overlap5
$if      set myopic $if not  set nodisc   $if      set overlap6        $load tendeffect=tmodeldepr_myopic_overlap6
$if      set myopic $if      set nodisc       $if      set overlap6        $load tendeffect=tmodeldepr_nodisc_myopic_overlap6
$if      set myopic $if not  set nodisc   $if      set overlap7        $load tendeffect=tmodeldepr_myopic_overlap7
$if      set myopic $if      set nodisc       $if      set overlap7        $load tendeffect=tmodeldepr_nodisc_myopic_overlap7
$gdxin

* Define correspoinding parameters for myopic and overlapping myopic runs
parameter
$if not  set myopic                      tmyopicLO         Myopic (or static) optimization year /2020/
$if not  set myopic                      tmyopicUP         Myopic (or static) optimization year /2500/

$if      set myopic2020                  tmyopicLO         Myopic (or static) optimization year /2020/
$if      set myopic2025                  tmyopicLO         Myopic (or static) optimization year /2025/
$if      set myopic2030                  tmyopicLO         Myopic (or static) optimization year /2030/
$if      set myopic2035                  tmyopicLO         Myopic (or static) optimization year /2035/
$if      set myopic2040                  tmyopicLO         Myopic (or static) optimization year /2040/
$if      set myopic2045                  tmyopicLO         Myopic (or static) optimization year /2045/
$if      set myopic2050                  tmyopicLO         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2020/
$if      set myopic2025 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2025/
$if      set myopic2030 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2035 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2040 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2045 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2050 $if      set overlap1 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2025/
$if      set myopic2025 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2030 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2035 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2040 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2045 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap2 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2030/
$if      set myopic2025 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2030 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2035 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2040 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap3 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2035/
$if      set myopic2025 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2030 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2035 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap4 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2040/
$if      set myopic2025 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2030 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap5 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2045/
$if      set myopic2025 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2030 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap6 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2025 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2030 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap7 tmyopicUP         Myopic (or static) optimization year /2050/

$if      set myopic2020 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2025 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2030 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2035 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2040 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2045 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
$if      set myopic2050 $if      set overlap8 tmyopicUP         Myopic (or static) optimization year /2050/
;

$ontext
parameter
$if      set static                             capt_static(i,v,r,t)
$if      set static                             gcapt_static(j,v,r,t)
$if      set static                             tcapt_static(k,r,r,t)
$if not  set myopic $if not  set static      dummy
$if      set myopic $if      set myopic2020     dd_myopic_int(r,t)
$if      set myopic $if      set myopic2020     bs_myopic_int(s,r,t)
$if      set myopic $if      set myopic2020     x_myopic_int(s,i,v,r,t)
$if      set myopic $if      set myopic2020     xc_myopic_int(i,v,r,t)
$if      set myopic $if      set myopic2020     ix_myopic_int(i,r,t)
$if      set myopic $if      set myopic2020     xcs_myopic_int(s,i,v,r,t)
$if      set myopic $if      set myopic2020     xtwh_myopic_int(i,v,r,t)
$if      set myopic $if      set myopic2020     e_myopic_int(s,k,r,r,t)
$if      set myopic $if      set myopic2020     it_myopic_int(k,r,r,t)
$if      set myopic $if      set myopic2020     g_myopic_int(s,j,v,r,t)
$if      set myopic $if      set myopic2020     gd_myopic_int(s,j,v,r,t)
$if      set myopic $if      set myopic2020     gc_myopic_int(j,v,r,t)
$if      set myopic $if      set myopic2020     ig_myopic_int(j,r,t)
$if      set myopic $if      set myopic2020     gb_myopic_int(s,j,v,r,t)
$if      set myopic $if      set myopic2020     sc_myopic_int(r,t)
$if      set myopic $if      set myopic2020     da_myopic_int(r,t)
$if      set myopic $if      set myopic2020     tc_myopic_int(k,r,r,t)
$if      set myopic                             dd_myopic(r,t)
$if      set myopic                             bs_myopic(s,r,t)
$if      set myopic                             x_myopic(s,i,v,r,t)
$if      set myopic                             xc_myopic(i,v,r,t)
$if      set myopic                             ix_myopic(i,r,t)
$if      set myopic                             xcs_myopic(s,i,v,r,t)
$if      set myopic                             xtwh_myopic(i,v,r,t)
$if      set myopic                             e_myopic(s,k,r,r,t)
$if      set myopic                             it_myopic(k,r,r,t)
$if      set myopic                             g_myopic(s,j,v,r,t)
$if      set myopic                             gd_myopic(s,j,v,r,t)
$if      set myopic                             gc_myopic(j,v,r,t)
$if      set myopic                             ig_myopic(j,r,t)
$if      set myopic                             gb_myopic(s,j,v,r,t)
$if      set myopic                             sc_myopic(r,t)
$if      set myopic                             da_myopic(r,t)
$if      set myopic                             tc_myopic(k,r,r,t)
;

$if not  set myopic                             dummy = 0 ;
$if      set myopic $if      set myopic2020     d_myopic(r,t) = 0 ;
$if      set myopic $if      set myopic2020     bs_myopic(s,r,t) = 0 ;
$if      set myopic $if      set myopic2020     x_myopic(s,i,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     xc_myopic(i,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     ix_myopic(i,r,t) = 0 ;
$if      set myopic $if      set myopic2020     xcs_myopic(s,i,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     xtwh_myopic(i,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     e_myopic(s,k,r,rr,t) = 0 ;
$if      set myopic $if      set myopic2020     it_myopic(k,r,rr,t) = 0 ;
$if      set myopic $if      set myopic2020     g_myopic(s,j,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     gd_myopic(s,j,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     gc_myopic(j,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     ig_myopic(j,r,t) = 0 ;
$if      set myopic $if      set myopic2020     gb_myopic(s,j,v,r,t) = 0 ;
$if      set myopic $if      set myopic2020     sc_myopic(r,t) = 0 ;
$if      set myopic $if      set myopic2020     da_myopic(r,t) = 0 ;
$if      set myopic $if      set myopic2020     tc_myopic(k,r,rr,t) = 0 ;
* Load variables (need to fix because there are new variables)
$if      set myopic $if not  set myopic2020     $gdxin limits\%e%_limits.gdx
$if      set myopic $if not  set myopic2020     $load dd_myopic=dd_myopic_int, bs_myopic=bs_myopic_int, x_myopic=x_myopic_int, xc_myopic=xc_myopic_int, ix_myopic=ix_myopic_int, xcs_myopic=xcs_myopic_int, xtwh_myopic=xtwh_myopic_int, e_myopic=e_myopic_int
$if      set myopic $if not  set myopic2020     $load it_myopic=it_myopic_int,
$if      set myopic $if not  set myopic2020     $load g_myopic=g_myopic_int, gd_myopic=gd_myopic_int, gc_myopic=gc_myopic_int, ig_myopic=ig_myopic_int
$if      set myopic $if not  set myopic2020     $load gb_myopic=gb_myopic_int, sc_myopic=sc_myopic_int, da_myopic=da_myopic_int, tc_myopic=tc_myopic_int
$if      set myopic $if not  set myopic2020     $gdxin
* Myopic model fixes
$if      set myopic      BS.FX(s,r,t)$(t.val < tmyopicLO)                 = bs_myopic(s,r,t) ;
$if      set myopic      X.FX(s,i,v,r,t)$(t.val < tmyopicLO)              = x_myopic(s,i,v,r,t) ;
$if      set myopic      XC.FX(i,v,r,t)$(t.val < tmyopicLO)               = xc_myopic(i,v,r,t) ;
$if      set myopic      IX.FX(i,r,t)$(t.val < tmyopicLO)                 = ix_myopic(i,r,t) ;
$if      set myopic      XCS.FX(s,i,v,r,t)$(t.val < tmyopicLO)            = xcs_myopic(s,i,v,r,t) ;
$if      set myopic      XTWH.FX(i,v,r,t)$(t.val < tmyopicLO)             = xtwh_myopic(i,v,r,t) ;
$if      set myopic      E.FX(s,k,r,rr,t)$(t.val < tmyopicLO)             = e_myopic(s,k,r,rr,t) ;
$if      set myopic      IT.FX(k,r,rr,t)$(t.val < tmyopicLO)              = it_myopic(k,r,rr,t) ;
$if      set myopic      G.FX(s,j,v,r,t)$(t.val < tmyopicLO)              = g_myopic(s,j,v,r,t) ;
$if      set myopic      GD.FX(s,j,v,r,t)$(t.val < tmyopicLO)             = gd_myopic(s,j,v,r,t) ;
$if      set myopic      GC.FX(j,v,r,t)$(t.val < tmyopicLO)               = gc_myopic(j,v,r,t) ;
$if      set myopic      IG.FX(j,r,t)$(t.val < tmyopicLO)                 = ig_myopic(j,r,t) ;
$if      set myopic      GB.FX(s,j,v,r,t)$(t.val < tmyopicLO)             = gb_myopic(s,j,v,r,t) ;
$if      set myopic      SC.FX(r,t)$(t.val < tmyopicLO)                   = sc_myopic(r,t) ;
$if      set myopic      TC.FX(k,r,rr,t)$(t.val < tmyopicLO)              = tc_myopic(k,r,rr,t) ;
$if      set myopic      BS.FX(s,r,t)$(t.val > tmyopicUP)                 = eps ;
$if      set myopic      X.FX(s,i,v,r,t)$(t.val > tmyopicUP)              = eps ;
$if      set myopic      XC.FX(i,v,r,t)$(t.val > tmyopicUP)               = eps ;
$if      set myopic      IX.FX(i,r,t)$(t.val > tmyopicUP)                 = eps ;
$if      set myopic      XCS.FX(s,i,v,r,t)$(t.val > tmyopicUP)            = eps ;
$if      set myopic      XTWH.FX(i,v,r,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      E.FX(s,k,r,rr,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      IT.FX(k,r,rr,t)$(t.val > tmyopicUP)              = eps ;
$if      set myopic      G.FX(s,j,v,r,t)$(t.val > tmyopicUP)              = eps ;
$if      set myopic      GD.FX(s,j,v,r,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      GC.FX(j,v,r,t)$(t.val > tmyopicUP)               = eps ;
$if      set myopic      IG.FX(j,r,t)$(t.val > tmyopicUP)                 = eps ;
$if      set myopic      GB.FX(s,j,v,r,t)$(t.val > tmyopicUP)             = eps ;
$if      set myopic      SC.FX(r,t)$(t.val > tmyopicUP)                   = eps ;
$if      set myopic      TC.FX(k,r,rr,t)$(t.val > tmyopicUP)              = eps ;
$offtext

* * * Investor modules
set
inv
drateopt
;

$gdxin database\setpar_%n%.gdx
$load inv, drateopt
$gdxin

parameter
share_invr(inv,r)
shareinv_invir(inv,i,r)  
shareinv_invjr(inv,j,r)  
shareinv_invkr(inv,k,r)    
dfact_int2(drateopt,t)
deprtime_invi(inv,i,v,r,t)                                
deprtime_invj(inv,j,v,r,t) 
deprtime_invk(inv,k,v,r,t)
lifetime_invi(inv,i,v,r,t)                                
lifetime_invj(inv,j,v,r,t) 
lifetime_invk(inv,k,v,r,t)
zeta_invir(drateopt,inv,i,v,r)
zeta_invjr(drateopt,inv,j,v,r)
zeta_invkr(drateopt,inv,k,v,r)

zetastar_invir(drateopt,inv,i,v,r)
zetastar_invjr(drateopt,inv,j,v,r)
zetastar_invkr(drateopt,inv,k,v,r)
zetastar_invir_nodisc(drateopt,inv,i,v,r)
zetastar_invjr_nodisc(drateopt,inv,j,v,r)
zetastar_invkr_nodisc(drateopt,inv,k,v,r)

zeta_invir_rpt(inv,i,v,r)
zeta_invjr_rpt(inv,j,v,r)
zeta_invkr_rpt(inv,k,v,r)
zeta_invir_nodisc_rpt(inv,i,v,r)
zeta_invjr_nodisc_rpt(inv,j,v,r)
zeta_invkr_nodisc_rpt(inv,k,v,r)

drate_opt(drateopt)
;

$gdxin database\setpar_%n%.gdx
$load drate_opt
$load share_invr
$load shareinv_invir  
$load shareinv_invjr  
$load shareinv_invkr  
$load dfact_int2
$load deprtime_invi    
$load deprtime_invj 
$load deprtime_invk
$load lifetime_invi    
$load lifetime_invj 
$load lifetime_invk
$load zetastar_invir, zetastar_invjr zetastar_invkr, zetastar_invir_nodisc, zetastar_invjr_nodisc, zetastar_invkr_nodisc
$if not  set myopic     $if not  set nodisc                         $load   zeta_invir  =   zetastar_invir
$if not  set myopic     $if not  set nodisc                         $load   zeta_invjr  =   zetastar_invjr
$if not  set myopic     $if not  set nodisc                         $load   zeta_invkr  =   zetastar_invkr
$if not  set myopic     $if      set nodisc                         $load   zeta_invir  =   zetastar_invir_nodisc
$if not  set myopic     $if      set nodisc                         $load   zeta_invjr  =   zetastar_invjr_nodisc
$if not  set myopic     $if      set nodisc                         $load   zeta_invkr  =   zetastar_invkr_nodisc
$if      set myopic     $if not  set nodisc     $if      set noverlap    $load   zeta_invir  =   zetastar_invir_noverlap
$if      set myopic     $if not  set nodisc     $if      set noverlap    $load   zeta_invjr  =   zetastar_invjr_noverlap
$if      set myopic     $if not  set nodisc     $if      set noverlap    $load   zeta_invkr  =   zetastar_invkr_noverlap
$if      set myopic     $if not  set nodisc     $if      set overlap1    $load   zeta_invir  =   zetastar_invir_overlap1
$if      set myopic     $if not  set nodisc     $if      set overlap1    $load   zeta_invjr  =   zetastar_invjr_overlap1
$if      set myopic     $if not  set nodisc     $if      set overlap1    $load   zeta_invkr  =   zetastar_invkr_overlap1
$if      set myopic     $if not  set nodisc     $if      set overlap2    $load   zeta_invir  =   zetastar_invir_overlap2
$if      set myopic     $if not  set nodisc     $if      set overlap2    $load   zeta_invjr  =   zetastar_invjr_overlap2
$if      set myopic     $if not  set nodisc     $if      set overlap2    $load   zeta_invkr  =   zetastar_invkr_overlap2
$if      set myopic     $if not  set nodisc     $if      set overlap3    $load   zeta_invir  =   zetastar_invir_overlap3
$if      set myopic     $if not  set nodisc     $if      set overlap3    $load   zeta_invjr  =   zetastar_invjr_overlap3
$if      set myopic     $if not  set nodisc     $if      set overlap3    $load   zeta_invkr  =   zetastar_invkr_overlap3
$if      set myopic     $if not  set nodisc     $if      set overlap4    $load   zeta_invir  =   zetastar_invir_overlap4
$if      set myopic     $if not  set nodisc     $if      set overlap4    $load   zeta_invjr  =   zetastar_invjr_overlap4
$if      set myopic     $if not  set nodisc     $if      set overlap4    $load   zeta_invkr  =   zetastar_invkr_overlap4
$if      set myopic     $if not  set nodisc     $if      set overlap5    $load   zeta_invir  =   zetastar_invir_overlap5
$if      set myopic     $if not  set nodisc     $if      set overlap5    $load   zeta_invjr  =   zetastar_invjr_overlap5
$if      set myopic     $if not  set nodisc     $if      set overlap5    $load   zeta_invkr  =   zetastar_invkr_overlap5
$if      set myopic     $if not  set nodisc     $if      set overlap6    $load   zeta_invir  =   zetastar_invir_overlap6
$if      set myopic     $if not  set nodisc     $if      set overlap6    $load   zeta_invjr  =   zetastar_invjr_overlap6
$if      set myopic     $if not  set nodisc     $if      set overlap6    $load   zeta_invkr  =   zetastar_invkr_overlap6
$if      set myopic     $if not  set nodisc     $if      set overlap7    $load   zeta_invir  =   zetastar_invir_overlap7
$if      set myopic     $if not  set nodisc     $if      set overlap7    $load   zeta_invjr  =   zetastar_invjr_overlap7
$if      set myopic     $if not  set nodisc     $if      set overlap7    $load   zeta_invkr  =   zetastar_invkr_overlap7
$if      set myopic     $if      set nodisc     $if      set noverlap    $load   zeta_invir  =   zetastar_invir_nodisc_noverlap
$if      set myopic     $if      set nodisc     $if      set noverlap    $load   zeta_invjr  =   zetastar_invjr_nodisc_noverlap
$if      set myopic     $if      set nodisc     $if      set noverlap    $load   zeta_invkr  =   zetastar_invkr_nodisc_noverlap
$if      set myopic     $if      set nodisc     $if      set overlap1    $load   zeta_invir  =   zetastar_invir_nodisc_overlap1
$if      set myopic     $if      set nodisc     $if      set overlap1    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap1
$if      set myopic     $if      set nodisc     $if      set overlap1    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap1
$if      set myopic     $if      set nodisc     $if      set overlap2    $load   zeta_invir  =   zetastar_invir_nodisc_overlap2
$if      set myopic     $if      set nodisc     $if      set overlap2    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap2
$if      set myopic     $if      set nodisc     $if      set overlap2    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap2
$if      set myopic     $if      set nodisc     $if      set overlap3    $load   zeta_invir  =   zetastar_invir_nodisc_overlap3
$if      set myopic     $if      set nodisc     $if      set overlap3    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap3
$if      set myopic     $if      set nodisc     $if      set overlap3    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap3
$if      set myopic     $if      set nodisc     $if      set overlap4    $load   zeta_invir  =   zetastar_invir_nodisc_overlap4
$if      set myopic     $if      set nodisc     $if      set overlap4    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap4
$if      set myopic     $if      set nodisc     $if      set overlap4    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap4
$if      set myopic     $if      set nodisc     $if      set overlap5    $load   zeta_invir  =   zetastar_invir_nodisc_overlap5
$if      set myopic     $if      set nodisc     $if      set overlap5    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap5
$if      set myopic     $if      set nodisc     $if      set overlap5    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap5
$if      set myopic     $if      set nodisc     $if      set overlap6    $load   zeta_invir  =   zetastar_invir_nodisc_overlap6
$if      set myopic     $if      set nodisc     $if      set overlap6    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap6
$if      set myopic     $if      set nodisc     $if      set overlap6    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap6
$if      set myopic     $if      set nodisc     $if      set overlap7    $load   zeta_invir  =   zetastar_invir_nodisc_overlap7
$if      set myopic     $if      set nodisc     $if      set overlap7    $load   zeta_invjr  =   zetastar_invjr_nodisc_overlap7
$if      set myopic     $if      set nodisc     $if      set overlap7    $load   zeta_invkr  =   zetastar_invkr_nodisc_overlap7
$gdxin

parameter
dfact_int3(drateopt,t)
;

dfact_int3(drateopt,t) = dfact_int2(drateopt,t) ;

$if      set longrun    dfact_int2(drateopt,"2030") = sum(tmerge(t), dfact_int3(drateopt,t)) ;

parameter
share(inv,i,r)
gshare(inv,j,r)
tshare(inv,k,r)
zeta(inv,i,v,r)
gzeta(inv,j,v,r)
tzeta(inv,k,v,r)
;

*$if      set nucall  lifetime_invi(inv,"Nuclear",newv(v),rnonuc,t)$(t.val ge 2022) = lifetime_invi(inv,"Nuclear",v,"France",t) ;

* Shares according to options
$if      set opt1   share(inv,i,r)  = share_invr(inv,r) ;
$if      set opt3   share(inv,i,r)  = shareinv_invir(inv,i,r) ;
$if      set opt1   gshare(inv,j,r) = share_invr(inv,r) ;
$if      set opt3   gshare(inv,j,r) = shareinv_invjr(inv,j,r) ;
$if      set opt1   tshare(inv,k,r) = share_invr(inv,r) ;
$if      set opt3   tshare(inv,k,r) = shareinv_invkr(inv,k,r) ;
* Lifetimes according to options (overriding the old option)
$if      set opt1   lifetime(i,v,r,t)      = sum(inv, lifetime_invi(inv,i,v,r,t) * share_invr(inv,r)) ;
$if      set opt1   glifetime(j,v,r,t)     = sum(inv, lifetime_invj(inv,j,v,r,t) * share_invr(inv,r)) ;
$if      set opt1   tlifetime(k,v,r,t)     = sum(inv, lifetime_invk(inv,k,v,r,t) * share_invr(inv,r)) ;
$if      set opt3   lifetime(i,v,r,t)      = sum(inv, lifetime_invi(inv,i,v,r,t) * shareinv_invir(inv,i,r)) ;
$if      set opt3   glifetime(j,v,r,t)     = sum(inv, lifetime_invj(inv,j,v,r,t) * shareinv_invjr(inv,j,r)) ;
$if      set opt3   tlifetime(k,v,r,t)     = sum(inv, lifetime_invk(inv,k,v,r,t) * shareinv_invkr(inv,k,r)) ;

lifetime("nuclear","1990","Germany","2022") = 1 ;
lifetime("nuclear","1990","Germany","2023") = 1 ;
$if set gernucnormal    lifetime("nuclear","1990","Germany","2023") = 1 ;
lifetime(i,v,r,t)$(v.val > t.val) = 0 ;

* Investment cost factor according to options
$if      set opt1   zeta(inv,i,v,r)  = zeta_invir("opt1",inv,i,v,r) ;
$if      set opt3   zeta(inv,i,v,r)  = zeta_invir("opt3",inv,i,v,r) ;
$if      set opt1   gzeta(inv,j,v,r) = zeta_invjr("opt1",inv,j,v,r) ;
$if      set opt3   gzeta(inv,j,v,r) = zeta_invjr("opt3",inv,j,v,r) ;
$if      set opt1   tzeta(inv,k,v,r) = zeta_invkr("opt1",inv,k,v,r) ;
$if      set opt3   tzeta(inv,k,v,r) = zeta_invkr("opt3",inv,k,v,r) ;

$if      set longrun    $if      set opt1   zeta(inv,i,v2030(v),r)  = sum(vmerge(vv), zeta_invir("opt1",inv,i,vv,r))/sum(tv(t,v), nyrs(t)) ;
$if      set longrun    $if      set opt3   zeta(inv,i,v2030(v),r)  = sum(vmerge(vv), zeta_invir("opt3",inv,i,vv,r))/sum(tv(t,v), nyrs(t)) ;
$if      set longrun    $if      set opt1   gzeta(inv,j,v2030(v),r) = sum(vmerge(vv), zeta_invjr("opt1",inv,j,vv,r))/sum(tv(t,v), nyrs(t)) ;
$if      set longrun    $if      set opt3   gzeta(inv,j,v2030(v),r) = sum(vmerge(vv), zeta_invjr("opt3",inv,j,vv,r))/sum(tv(t,v), nyrs(t)) ;
$if      set longrun    $if      set opt1   tzeta(inv,k,v2030(v),r) = sum(vmerge(vv), zeta_invkr("opt1",inv,k,vv,r))/sum(tv(t,v), nyrs(t)) ;
$if      set longrun    $if      set opt3   tzeta(inv,k,v2030(v),r) = sum(vmerge(vv), zeta_invkr("opt3",inv,k,vv,r))/sum(tv(t,v), nyrs(t)) ;
* Discount and financing rate
$if      set opt1   drate  = drate_opt("opt1") ;
$if      set opt3   drate  = drate_opt("opt3") ;
* For reporting routine of investment cost
$if      set opt1   zeta_invir_rpt(inv,i,v,r) = zetastar_invir("opt1",inv,i,v,r) ;
$if      set opt1   zeta_invjr_rpt(inv,j,v,r) = zetastar_invjr("opt1",inv,j,v,r) ;
$if      set opt1   zeta_invkr_rpt(inv,k,v,r) = zetastar_invkr("opt1",inv,k,v,r) ;
$if      set opt1   zeta_invir_nodisc_rpt(inv,i,v,r) = zetastar_invir_nodisc("opt1",inv,i,v,r) ;
$if      set opt1   zeta_invjr_nodisc_rpt(inv,j,v,r) = zetastar_invjr_nodisc("opt1",inv,j,v,r) ;
$if      set opt1   zeta_invjr_nodisc_rpt(inv,j,v,r) = zetastar_invjr_nodisc("opt1",inv,j,v,r) ;
$if      set opt1   zeta_invkr_nodisc_rpt(inv,k,v,r) = zetastar_invkr_nodisc("opt1",inv,k,v,r) ;
$if      set opt3   zeta_invir_rpt(inv,i,v,r) = zetastar_invir("opt3",inv,i,v,r) ;
$if      set opt3   zeta_invjr_rpt(inv,j,v,r) = zetastar_invjr("opt3",inv,j,v,r) ;
$if      set opt3   zeta_invkr_rpt(inv,k,v,r) = zetastar_invkr("opt3",inv,k,v,r) ;
$if      set opt3   zeta_invir_nodisc_rpt(inv,i,v,r) = zetastar_invir_nodisc("opt3",inv,i,v,r) ;
$if      set opt3   zeta_invjr_nodisc_rpt(inv,j,v,r) = zetastar_invjr_nodisc("opt3",inv,j,v,r) ;
$if      set opt3   zeta_invkr_nodisc_rpt(inv,k,v,r) = zetastar_invkr_nodisc("opt3",inv,k,v,r) ;

* Adjustment so that all countries can do nuclear
$if not  set nucall  lifetime("Nuclear",newv(v),rnonuc(r),t)$(t.val ge 2024) = 0 ;
$if not  set nucall  lifetime("Coal",newv(v),rnocoa(r),t)$(t.val ge 2024) = 0 ;
$if not  set nucall  lifetime("Lignite",newv(v),rnolig(r),t)$(t.val ge 2024) = 0 ;

* Adjustment of discount rate according to options
$if      set opt1   dfact(t) = dfact_int2("opt1",t) ;
$if      set opt3   dfact(t) = dfact_int2("opt3",t) ;
* No discouting option
$if      set nodisc dfact(t) = nyrs(t) ;
* Germany nuclear extension and Steckbetrieb (adjustments are necessary here and not above)
$if      set extension      $if not  set shortrun    lifetime("Nuclear","1990","Germany","2025")                              = 0.2 + round((1.41 + 1.31 + 1.335)/(1.41 + 1.41 + 1.31 + 1.335), 8) * 0.8 ;
$if      set extension      $if not  set shortrun    lifetime("Nuclear","1990","Germany","2030")                              = round((1.41 + 1.31 + 1.335)/(1.41 + 1.41 + 1.31 + 1.335), 8) ;
$if      set streckbetrieb  $if not  set shortrun    lifetime("Nuclear","1990","Germany","2025")                              = 0.2 + round((1.41 + 1.31 + 1.335)/(1.41 + 1.41 + 1.31 + 1.335), 8) * 0.4 ;
$if      set streckbetrieb  $if not  set shortrun    lifetime("Nuclear","1990","Germany","2030")                     	     = 0 ;
$if      set extension      $if      set shortrun    lifetime("Nuclear","1990","Germany",t)$(t.val ge 2023 and t.val le 2029) = lifetime("Nuclear","1990","Germany","2022") ;
$if      set streckbetrieb  $if      set shortrun    lifetime("Nuclear","1990","Germany","2023")                              = lifetime("Nuclear","1990","Germany","2022") ;
$if      set strext         $if      set shortrun    lifetime("Nuclear","1990","Germany",t)$(t.val ge 2023 and t.val le 2030) = lifetime("Nuclear","1990","Germany","2022") ;


$if      set hydroexp  capcost("Hydro",newv(v),r)            = capcost("Hydro","2020",r) ;
$if      set hydroexp  deccost("Hydro",newv(v),r)            = deccost("Hydro","2020",r) ;
$if      set hydroexp  fomcost("Hydro",newv(v),r)            = fomcost("Hydro","2020",r) ;
$if      set hydroexp  vomcost("Hydro",newv(v),r)            = vomcost("Hydro","2020",r) ;
$if      set hydroexp  reliability("Hydro",newv(v),r)        = reliability("Hydro","2020",r) ;
$if      set hydroexp  effrate("Hydro",newv(v),r)            = effrate("Hydro","2020",r) ;
$if      set hydroexp  capcred("Hydro",newv(v),r)            = capcred("Hydro","2020",r) ;
$if      set hydroexp  lifetime_invi(inv,"Hydro",newv(v),r,t)$(t.val ge 2022) = lifetime_invi(inv,"Nuclear",v,"France",t) ;
$if      set hydroexp  zeta(inv,"Hydro",newv(v),r) = zeta(inv,"Nuclear",v,"France") ;
$if      set hydroexp  $if      set opt1   zeta_invir_rpt(inv,i,v,r)$(sameas(i,"Hydro"))            = zetastar_invir("opt1",inv,"Nuclear",v,"France") ;
$if      set hydroexp  $if      set opt1   zeta_invir_nodisc_rpt(inv,i,v,r)$(sameas(i,"Hydro"))     = zetastar_invir_nodisc("opt1",inv,"Nuclear",v,"France") ;
$if      set hydroexp  $if      set opt3   zeta_invir_rpt(inv,i,v,r)$(sameas(i,"Hydro"))            = zetastar_invir("opt3",inv,"Nuclear",v,"France") ;
$if      set hydroexp  $if      set opt3   zeta_invir_nodisc_rpt(inv,i,v,r)$(sameas(i,"Hydro"))     = zetastar_invir_nodisc("opt3",inv,"Nuclear",v,"France") ;

* * * Prices
set
fuel                             Fuel
xfueli(fuel,i)                   Map fuel technology
price_sc                         Price adder scenarios
;

$gdxin database\setpar_%n%.gdx
$load fuel, xfueli, price_sc
$gdxin

* Correction of emission factors (calibration issue)
* Check CCS in database emit
Parameter
emitfuel(fuel)
capturerate
;

*emitfuel("Lignite") = 398.7/1000 ;
emitfuel("Lignite") = 364.32/1000 ;
*emitfuel("Coal") = 338.2/1000 ;
emitfuel("Coal") = 340.56/1000 ;
*emitfuel("Gas") = 200.08/1000 ;
emitfuel("Gas") = 201.96/1000 ;
*emitfuel("Oil") = 266.5/1000 ;
emitfuel("Oil") = 263.88/1000 ;
emitfuel("Bioenergy") = 367.6/1000 ;

capturerate = 0.9 ;

emit_int(ivr(i,v,r)) = round(sum(xfueli(fuel,i), emitfuel(fuel)) / effrate(i,v,r),4) ;

co2captured(ivr(ccs(i),v,r)) = round(sum(xfueli(fuel,i), capturerate * emitfuel(fuel)) / effrate(i,v,r),4) ;
emit(ivr(i,v,r))  = emit_int(i,v,r) - co2captured(i,v,r) ;
emit(ivr(i,v,r))$(not ccs(i))  = emit_int(i,v,r) - co2captured(i,v,r) ;
emit(ivr(chp(i),v,r))$(sum(mapchp(i,nochp(ii)), emit_int(i,v,r)) > 0) = sum(mapchp(i,nochp(ii)), emit_int(ii,v,r)) ;

effrate(ivr(i,v,r))$(not bio(i)) = effrate_int(i,v,r) ;



* Correcting biomass emissions factors according to biomass neutral treatment (socially and politically questionable)    
$if      set bioneutral        emit("Bioenergy",v,r) = 0 ;
$if      set bioneutral        emit("Bio_CHP",v,r)   = 0 ;
$if      set bioneutral        emit("Bio_CCS",v,r)   = - co2captured("Bio_CCS",v,r) ;
$if      set bio66             emit("Bioenergy",v,r) = round(emit("Bioenergy",v,r) * 0.3333, 4) ;
$if      set bio66             emit("Bio_CHP",v,r)   = round(emit("Bio_CHP",v,r) * 0.3333, 4) ;
$if      set bio66             emit("Bio_CCS",v,r)   = round(- co2captured("Bio_CCS",v,r) * 0.6667, 4) ;

parameter
pfuel(fuel,r,t)                         Fuel price (EUR er MWh)
pfadd(fuel,r,t)                         Absolute fuel price adders (EUR per MWh)
pfadd_rel_int(price_sc,fuel,r,t)        Relative fuel price adders (value between 0 (no adding) and x)
pfadd_rel(fuel,r,t)                     Relative fuel price adders (value between 0 (no adding) and x)
pco2_r(r,t)                             CO2 price by region (EUR per tCO2)
pco2_int(t)                             CO2 price (EUR per tCO2) standard
pco2(t)                                 CO2 price (EUR per tCO2)
biocost(r,t)                            Bioenegy cost (EUR per MWh)
biocost_eu(t)                           Bioenegy cost (EUR per MWh)
ccscost(r,t)                            CCS CO2 transportation cost (EUR per tCO2)
ccscost_eu(t)                           CCS CO2 transportation cost (EUR per tCO2)
;

$gdxin database\setpar_%n%.gdx
$load pfuel, pfadd, pco2_r, pco2_int=pco2, biocost, biocost_eu, ccscost, ccscost_eu, pfadd_rel_int=pfadd_rel
$gdxin

* Fuel price corrections
pfuel("gas",r,"2020") = 17.59 ;
pfuel("gas",r,"2021") = 46.89 ;
pfuel("gas",r,"2022") = 132.2 ;
$if      set shortrun   pfuel("gas",r,"2023") = 41.42 ;
$if      set shortrun   pfuel("gas",r,"2024") = 28.39 ;
pfuel("gas",r,t)$(t.val ge 2025) = 28.39 ;

$if      set gas_baup   pfuel("gas",r,t)$(t.val ge 2021) = pfuel("gas",r,"2020") ;

$if      set gas_reco   pfuel("gas",r,"2030") = pfuel("gas",r,"2020") ;
$if      set shortrun   $if      set gas_reco   pfuel("gas",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("gas",r,"2024") + (pfuel("gas",r,"2030") - pfuel("gas",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set gas_reco   pfuel("gas",r,t)$(t.val ge 2031) = pfuel("gas",r,"2030") ;

$if      set gas_high   pfuel("gas",r,"2030") = pfuel("gas",r,"2020") * 1.5 ;
$if      set shortrun   $if      set gas_high   pfuel("gas",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("gas",r,"2024") + (pfuel("gas",r,"2030") - pfuel("gas",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set gas_high   pfuel("gas",r,t)$(t.val ge 2031) = pfuel("gas",r,"2030") ;

$if      set gas_long   pfuel("gas",r,"2030") = pfuel("gas",r,"2020") * 2 ;
$if      set shortrun   $if      set gas_long   pfuel("gas",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("gas",r,"2024") + (pfuel("gas",r,"2030") - pfuel("gas",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set gas_long   pfuel("gas",r,t)$(t.val ge 2031) = pfuel("gas",r,"2030") ;

pfuel("oil",r,"2020") = round(31.26 / 1.09,4) ;
pfuel("oil",r,"2021") = round(38.11 / 1.09,4) ;
pfuel("oil",r,"2022") = round(52.20 / 1.09,4) ;
$if      set shortrun   pfuel("oil",r,"2023") = round(44.10 / 1.09,4) ;
$if      set shortrun   pfuel("oil",r,"2024") = round(44.84 / 1.09,4) ;
pfuel("oil",r,t)$(t.val ge 2025) = round(44.84 / 1.09,4) ;

$if      set oil_baup   pfuel("oil",r,t)$(t.val ge 2021) = pfuel("oil",r,"2020") ;

$if      set oil_reco   pfuel("oil",r,"2030") = pfuel("oil",r,"2020") ;
$if      set shortrun   $if      set oil_reco   pfuel("oil",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("oil",r,"2024") + (pfuel("oil",r,"2030") - pfuel("oil",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set oil_reco   pfuel("oil",r,t)$(t.val ge 2031) = pfuel("oil",r,"2030") ;

$if      set oil_high   pfuel("oil",r,"2030") = pfuel("oil",r,"2020") * 1.5 ;
$if      set shortrun   $if      set oil_high   pfuel("oil",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("oil",r,"2024") + (pfuel("oil",r,"2030") - pfuel("oil",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set oil_high   pfuel("oil",r,t)$(t.val ge 2031) = pfuel("oil",r,"2030") ;

$if      set oil_long   pfuel("oil",r,"2030") = pfuel("oil",r,"2020") * 2 ;
$if      set shortrun   $if      set oil_long   pfuel("oil",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("oil",r,"2024") + (pfuel("oil",r,"2030") - pfuel("oil",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set oil_long   pfuel("oil",r,t)$(t.val ge 2031) = pfuel("oil",r,"2030") ;

pfuel("coal",r,"2020") = round(8.97 / 1.09,4) ;
pfuel("coal",r,"2021") = round(15.33 / 1.09,4) ;
pfuel("coal",r,"2022") = round(40.24 / 1.09,4) ;
$if      set shortrun   pfuel("coal",r,"2023") = round(19.70 / 1.09,4) ;
$if      set shortrun   pfuel("coal",r,"2024") = round(14.78 / 1.09,4) ;
pfuel("coal",r,t)$(t.val ge 2025) = round(14.78 / 1.09,4) ;

$if      set coal_baup   pfuel("coal",r,t)$(t.val ge 2021) = pfuel("coal",r,"2020") ;

$if      set coal_reco   pfuel("coal",r,"2030") = pfuel("coal",r,"2020") ;
$if      set shortrun   $if      set coal_reco   pfuel("coal",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("coal",r,"2024") + (pfuel("coal",r,"2030") - pfuel("coal",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set coal_reco   pfuel("coal",r,t)$(t.val ge 2031) = pfuel("coal",r,"2030") ;

$if      set coal_high   pfuel("coal",r,"2030") = pfuel("coal",r,"2020") * 1.5 ;
$if      set shortrun   $if      set coal_high   pfuel("coal",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("coal",r,"2024") + (pfuel("coal",r,"2030") - pfuel("coal",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set coal_high   pfuel("coal",r,t)$(t.val ge 2031) = pfuel("coal",r,"2030") ;

$if      set coal_long   pfuel("coal",r,"2030") = pfuel("coal",r,"2020") * 2 ;
$if      set shortrun   $if      set coal_long   pfuel("coal",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("coal",r,"2024") + (pfuel("coal",r,"2030") - pfuel("coal",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set coal_long   pfuel("coal",r,t)$(t.val ge 2031) = pfuel("coal",r,"2030") ;

pfuel("lignite",r,"2020") = 2 ;
pfuel("lignite",r,"2021") = 2 ;
pfuel("lignite",r,"2022") = 2 ;
$if      set shortrun   pfuel("lignite",r,"2023") = 2 ;
$if      set shortrun   pfuel("lignite",r,"2024") = 2 ;
pfuel("lignite",r,t)$(t.val ge 2025) =2 ;

pfuel("bioenergy",r,"2020") = 28.77 ;
pfuel("bioenergy",r,"2021") = 28.77 ;
pfuel("bioenergy",r,"2022") = 28.77 ;
$if      set shortrun   pfuel("bioenergy",r,"2023") = 28.77 ;
pfuel("bioenergy",r,"2030") = 30.32 ;
pfuel("bioenergy",r,"2050") = 34.89 ;
$if      set shortrun   pfuel("bioenergy",r,t)$(t.val ge 2024 and t.val le 2029) = pfuel("bioenergy",r,"2023") + (pfuel("bioenergy",r,"2030") - pfuel("bioenergy",r,"2023")) * (t.val - 2023) / (2030 - 2023) ;
$if      set longrun    pfuel("bioenergy",r,t)$(t.val ge 2023 and t.val le 2029) = pfuel("bioenergy",r,"2022") + (pfuel("bioenergy",r,"2030") - pfuel("bioenergy",r,"2022")) * (t.val - 2022) / (2030 - 2022) ;
pfuel("bioenergy",r,t)$(t.val ge 2031 and t.val le 2049) = pfuel("bioenergy",r,"2030") + (pfuel("bioenergy",r,"2050") - pfuel("bioenergy",r,"2030")) * (t.val - 2030) / (2050 - 2030) ;

$if      set bio_low  pfuel("bioenergy",r,"2050") = 34.89 * 0.5 ;
$if      set bio_low  pfuel("bioenergy",r,t)$(t.val ge 2031 and t.val le 2049) = pfuel("bioenergy",r,"2030") + (pfuel("bioenergy",r,"2050") - pfuel("bioenergy",r,"2030")) * (t.val - 2030) / (2050 - 2030) ;

$if      set bio_increase pfuel("bioenergy",r,"2050") = 34.89 * 2 ;
$if      set bio_increase pfuel("bioenergy",r,t)$(t.val ge 2031 and t.val le 2049) = pfuel("bioenergy",r,"2030") + (pfuel("bioenergy",r,"2050") - pfuel("bioenergy",r,"2030")) * (t.val - 2030) / (2050 - 2030) ;

* 1 lb. U3O8 (assuming 0.5% conversion loss gives 180,000,000 BTU = 52,... MWh) (prices from tradenomics) + conversion (7%), enrichment (24%), fuel fabrication (18%) = 49% * 4.6 USD/MWh (World nuclear associatiuon)
pfuel("uranium",r,"2020") = round(0.47 / 1.09,4) + round(0.49 * 4.6 / 1.09,4) ;
pfuel("uranium",r,"2021") = round(0.62 / 1.09,4) + round(0.49 * 4.6 / 1.09,4) ;
pfuel("uranium",r,"2022") = round(0.88 / 1.09,4) + round(0.49 * 4.6 / 1.09,4) ;
$if      set shortrun   pfuel("uranium",r,"2023") = round(1.05 / 1.09,4) + round(0.49 * 4.6 / 1.09,4) ;
$if      set shortrun   pfuel("uranium",r,"2024") = round(1.64 / 1.09,4) + round(0.49 * 4.6 / 1.09,4) ;
pfuel("uranium",r,t)$(t.val ge 2025) = round(1.64 / 1.09,4) + round(0.49 * 4.6 / 1.09,4) ; ;

$if      set uran_baup   pfuel("uranium",r,t)$(t.val ge 2021) = pfuel("uranium",r,"2020") ;

$if      set uran_reco   pfuel("uranium",r,"2030") = pfuel("uranium",r,"2020") ;
$if      set shortrun    $if      set uran_reco   pfuel("uranium",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("uranium",r,"2024") + (pfuel("uranium",r,"2030") - pfuel("uranium",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set uran_reco   pfuel("uranium",r,t)$(t.val ge 2031) = pfuel("uranium",r,"2030") ;

$if      set uran_high   pfuel("uranium",r,"2030") = round(28 / 52.752792628674 / 1.09,4) * 1.5 + round(0.49 * 4.6 / 1.09,4) ;
$if      set shortrun    $if      set uran_high   pfuel("uranium",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("uranium",r,"2024") + (pfuel("uranium",r,"2030") - pfuel("uranium",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set uran_high   pfuel("uranium",r,t)$(t.val ge 2031) = pfuel("uranium",r,"2030") ;

$if      set uran_long   pfuel("uranium",r,"2030") = round(28 / 52.752792628674 / 1.09,4) * 2 + round(0.49 * 4.6 / 1.09,4) ;
$if      set shortrun    $if      set uran_long   pfuel("uranium",r,t)$(t.val ge 2024 and t.val le 2030) = pfuel("uranium",r,"2024") + (pfuel("uranium",r,"2030") - pfuel("uranium",r,"2024")) * (t.val - 2024) / (2030 - 2024) ;
$if      set uran_long   pfuel("uranium",r,t)$(t.val ge 2031) = pfuel("uranium",r,"2030") ;

pco2(t)                                          = 1 * pco2_int(t) ;
$if      set flatpco2    pco2(t)$(t.val ge 2025) = 1 * pco2_int("2020") ;
$if      set doublepco2  pco2(t)$(t.val ge 2025) = 2 * pco2_int(t) ;
$if      set triplepco2  pco2(t)$(t.val ge 2025) = 3 * pco2_int(t) ;

* Price adjustments must happen here because discost follow before the rest of the "lag"-modules is defined
$if      set bauprice                           pfadd_rel(fuel,r,t) = pfadd_rel_int("bauprice",fuel,r,t) ;
$if      set high                               pfadd_rel(fuel,r,t) = pfadd_rel_int("high",fuel,r,t) ;
$if      set recovery                           pfadd_rel(fuel,r,t) = pfadd_rel_int("recovery",fuel,r,t) ;
$if      set long                               pfadd_rel(fuel,r,t) = pfadd_rel_int("long",fuel,r,t) ;

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

$gdxin database\setpar_%n%.gdx
$load pol_sc, co2cap_sc, rnwtgt_sc, irnwtgt_sc, coalexit_sc, nucexit_sc, gasexit_sc
$gdxin

parameter
* Interim parameter from database with scenario
co2p_int(pol_sc,t)               Carbon price (EUR per t) (system)
co2cap_r_int(co2cap_sc,r,t)      Carbon emissions cap (GtCO2) (regions)
co2cap_int(pol_sc,t)             Carbon emissions cap (GtCO2) (system)
rnwtgt_r_int(rnwtgt_sc,r,t)      Renewable energy share target (regions)
rnwtgt_int(rnwtgt_sc,t)          Renewable energy share target (system)
irnwtgt_r_int(irnwtgt_sc,r,t)    Intermittent renewable energy share target (regions)
irnwtgt_int(irnwtgt_sc,t)        Intermittent renewable energy share target (system)
coallim_r_int(coalexit_sc,r,t)   Policy constraint on hard coal phase out (regions)
coallim_int(coalexit_sc,t)       Policy constraint on hard coal phase out (system)
lignlim_r_int(coalexit_sc,r,t)   Policy constraint on lignite phase out (regions)
lignlim_int(coalexit_sc,t)       Policy constraint on lignite phase out (system)
nuclim_r_int(nucexit_sc,r,t)     Policy constraint on nuclear phase out (regions)
nuclim_int(nucexit_sc,t)         Policy constraint on nuclear phase out (system)
gaslim_int(gasexit_sc,r,t)       Natural gas bugdets (TWh)
gaslim_eu_int(gasexit_sc,t)      Natural gas bugdets (TWh)
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

$gdxin database\setpar_%n%.gdx
$load co2p_int=co2p, co2cap_int=co2cap, co2cap_r_int=co2cap_r, rnwtgt_r_int=rnwtgt_r, rnwtgt_int=rnwtgt, irnwtgt_r_int=irnwtgt_r, irnwtgt_int=irnwtgt
$load coallim_int=coallim, coallim_r_int=coallim_r, lignlim_int=lignlim, lignlim_r_int=lignlim_r, nuclim_int=nuclim, nuclim_r_int=nuclim_r,
$load  gaslim_int=gaslim_r, gaslim_eu_int=gaslim
$gdxin

* Scenario switches
$if      set onlyelec       co2p(t) = co2p_int("onlyelec",t) ;
$if      set onlyeleccrisis co2p(t) = co2p_int("onlyeleccrisis",t) ;
$if      set sceuets        co2p(t) = co2p_int("sceuets",t) ;
$if      set sceuetscrisis  co2p(t) = co2p_int("sceuetscrisis",t) ;

$if      set co2price    co2p(t) = round(255.982 * (dfact("2050")/nyrs("2050")) / (dfact(t)/nyrs(t)),4) ;

$if      set onlyelec       co2cap(t) = co2cap_int("onlyelec",t) ;
$if      set onlyeleccrisis co2cap(t) = co2cap_int("onlyeleccrisis",t) ;
$if      set sceuets        co2cap(t) = co2cap_int("sceuets",t) ;
$if      set sceuetscrisis  co2cap(t) = co2cap_int("sceuetscrisis",t) ;

$if      set ger         coallim_r(r,t) = coallim_r_int("ger",r,t) ;
$if      set ger-fast    coallim_r(r,t) = coallim_r_int("ger-fast",r,t) ;
$if      set ger-slow    coallim_r(r,t) = coallim_r_int("ger-slow",r,t) ;
$if      set eu          coallim_r(r,t) = coallim_r_int("eu",r,t) ;
$if      set eu-fast     coallim_r(r,t) = coallim_r_int("eu-fast",r,t) ;
$if      set eu-slow     coallim_r(r,t) = coallim_r_int("eu-flow",r,t) ;

$if      set ger         lignlim_r(r,t) = lignlim_r_int("ger",r,t) ;
$if      set ger-fast    lignlim_r(r,t) = lignlim_r_int("ger-fast",r,t) ;
$if      set ger-slow    lignlim_r(r,t) = lignlim_r_int("ger-slow",r,t) ;
$if      set eu          lignlim_r(r,t) = lignlim_r_int("eu",r,t) ;
$if      set eu-fast     lignlim_r(r,t) = lignlim_r_int("eu-fast",r,t) ;
$if      set eu-slow     lignlim_r(r,t) = lignlim_r_int("eu-flow",r,t) ;

$if      set ger         nuclim_r(r,t) = nuclim_r_int("ger",r,t) ;
$if      set ger-fast    nuclim_r(r,t) = nuclim_r_int("ger-fast",r,t) ;
$if      set ger-slow    nuclim_r(r,t) = nuclim_r_int("ger-slow",r,t) ;
$if      set eu          nuclim_r(r,t) = nuclim_r_int("eu",r,t) ;
$if      set eu-fast     nuclim_r(r,t) = nuclim_r_int("eu-fast",r,t) ;
$if      set eu-slow     nuclim_r(r,t) = nuclim_r_int("eu-flow",r,t) ;

$if      set aaa         co2cap_r(r,t) = co2cap_r_int("aaa",r,t) ;
$if      set bbb         co2cap_r(r,t) = co2cap_r_int("bbb",r,t) ;
$if      set ccc         co2cap_r(r,t) = co2cap_r_int("ccc",r,t) ;

$if      set aaa         rnwtgt_r(r,t) = rnwtgt_r_int("aaa",r,t) ;
$if      set bbb         rnwtgt_r(r,t) = rnwtgt_r_int("bbb",r,t) ;
$if      set ccc         rnwtgt_r(r,t) = rnwtgt_r_int("ccc",r,t) ;

$if      set aaa         irnwtgt_r(r,t) = irnwtgt_r_int("aaa",r,t) ;
$if      set bbb         irnwtgt_r(r,t) = irnwtgt_r_int("bbb",r,t) ;
$if      set ccc         irnwtgt_r(r,t) = irnwtgt_r_int("ccc",r,t) ;

$if      set ger         coallim(t) = coallim_int("ger",t) ;
$if      set ger-fast    coallim(t) = coallim_int("ger-fast",t) ;
$if      set ger-slow    coallim(t) = coallim_int("ger-slow",t) ;
$if      set eu          coallim(t) = coallim_int("eu",t) ;
$if      set eu-fast     coallim(t) = coallim_int("eu-fast",t) ;
$if      set eu-slow     coallim(t) = coallim_int("eu-flow",t) ;

$if      set ger         lignlim(t) = lignlim_int("ger",t) ;
$if      set ger-fast    lignlim(t) = lignlim_int("ger-fast",t) ;
$if      set ger-slow    lignlim(t) = lignlim_int("ger-slow",t) ;
$if      set eu          lignlim(t) = lignlim_int("eu",t) ;
$if      set eu-fast     lignlim(t) = lignlim_int("eu-fast",t) ;
$if      set eu-slow     lignlim(t) = lignlim_int("eu-flow",t) ;

$if      set ger         nuclim(t) = nuclim_int("ger",t) ;
$if      set ger-fast    nuclim(t) = nuclim_int("ger-fast",t) ;
$if      set ger-slow    nuclim(t) = nuclim_int("ger-slow",t) ;
$if      set eu          nuclim(t) = nuclim_int("eu",t) ;
$if      set eu-fast     nuclim(t) = nuclim_int("eu-fast",t) ;
$if      set eu-slow     nuclim(t) = nuclim_int("eu-flow",t) ;

$if      set aaa         rnwtgt(t) = rnwtgt_int("aaa",t) ;
$if      set bbb         rnwtgt(t) = rnwtgt_int("bbb",t) ;
$if      set ccc         rnwtgt(t) = rnwtgt_int("ccc",t) ;

$if      set aaa         irnwtgt(t) = irnwtgt_int("aaa",t) ;
$if      set bbb         irnwtgt(t) = irnwtgt_int("bbb",t) ;
$if      set ccc         irnwtgt(t) = irnwtgt_int("ccc",t) ;

$if      set bau         gaslim(r,t) = gaslim_int("bau",r,t) ;
$if      set tenperc     gaslim(r,t) = gaslim_int("tenperc",r,t) ;
$if      set fiftyperc   gaslim(r,t) = gaslim_int("fiftyperc",r,t) ;
$if      set nogas       gaslim(r,t) = gaslim_int("nogas",r,t) ;

$if      set bau         gaslim_eu(t) = gaslim_eu_int("bau",t) ;
$if      set tenperc     gaslim_eu(t) = gaslim_eu_int("tenperc",t) ;
$if      set fiftyperc   gaslim_eu(t) = gaslim_eu_int("fiftyperc",t) ;
$if      set nogas       gaslim_eu(t) = gaslim_eu_int("nogas",t) ;

$if      set gergaslimit gaslim("Germany","2023") = 75 ;
$if      set gergaslimit gaslim("Germany","2024") = 100 ;

* * * market_out
set
ngclass          Natural gas supply classes
;

$gdxin database\setpar_%n%.gdx
$load ngclass
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
;

$gdxin database\setpar_%n%.gdx
$load ngref, ngref_r, ngelas, ngelas_r, ngcost, ngcost_r, nglim, nglim_r
$gdxin

* * * Energy efficiency modules

$if      set socialcost $include    modules_precal\euregen2024_pre_socialcost_v1


* * * Subsidies and taxes (negative subsidies are taxes)
parameter
irnwsub(r,t)                     New irnw production subsidy (EUR per MWh)
rnwsub(r,t)                      New rnw production subsidy (EUR per MWh)
solsub(r,t)                      New solar PV production subsidy in (EUR per MWh)
windsub(r,t)                     New wind production subsidy (EUR per MWh)
nucsub(r,t)                      New nuclear production subsidy (EUR per MWh)
lowcarbsub(r,t)                  New rnw nuc and CCS production subsidy (EUR per MWh)
irnwsub_cap(r,t)                 New irnw capacity subsidy (EUR per kW)
rnwsub_cap(r,t)                  New rnw capacity subsidy (EUR per kW)
solsub_cap(r,t)                  New solar PV capacity subsidy in (EUR per kW)
windsub_cap(r,t)                 New wind capacity subsidy (EUR per kW)
nucsub_cap(r,t)                  New nuclear capacity subsidy (EUR per kW)
lowcarbsub_cap(r,t)              New rnw nuc and CCS capacity subsidy (EUR per kW)
;

* Set irnw subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set irnwsub     irnwsub(r,t) = 0;
$if      set irnwsub     irnwsub(r,t) = %irnwsub%;
* Set rnw subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set rnwsub      rnwsub(r,t) = 0;
$if      set rnwsub      rnwsub(r,t) = %rnwsub%;
* Set solar PV subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set solsub      solsub(r,t) = 0;
$if      set solsub      solsub(r,t) = %solsub%;
* Set wind subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set windsub     windsub(r,t) = 0;
$if      set windsub     windsub(r,t) = %windsub%;
* Set low-carb subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set nucsub      nucsub(r,t) = 0;
$if      set nucsub      nucsub(r,t) = %lowcarbsub%;
* Set low-carb subsidy at indicated rate if set (usual 50 euro per MWh ~ 5 cents per kWh)
$if not  set lowcarbsub  lowcarbsub(r,t) = 0;
$if      set lowcarbsub  lowcarbsub(r,t) = %lowcarbsub%;

* Set irnw subsidy at indicated rate if set
$if not  set irnwsub_cap     irnwsub_cap(r,t) = 0;
$if      set irnwsub_cap     irnwsub_cap(r,t) = %irnwsub_cap%;
* Set rnw subsidy at indicated rate if set
$if not  set rnwsub_cap      rnwsub_cap(r,t) = 0;
$if      set rnwsub_cap      rnwsub_cap(r,t) = %rnwsub_cap%;
* Set solar PV subsidy at indicated rate if set
$if not  set solsub_cap      solsub_cap(r,t) = 0;
$if      set solsub_cap      solsub_cap(r,t) = %solsub_cap%;
* Set wind subsidy at indicated rate if set
$if not  set windsub_cap     windsub_cap(r,t) = 0;
$if      set windsub_cap     windsub_cap(r,t) = %windsub_cap%;
* Set low-carb subsidy at indicated rate if set
$if not  set nucsub_cap      nucsub_cap(r,t) = 0;
$if      set nucsub_cap      nucsub_cap(r,t) = %lowcarbsub_cap%;
* Set low-carb subsidy at indicated rate if set
$if not  set lowcarbsub_cap  lowcarbsub_cap(r,t) = 0;
$if      set lowcarbsub_cap  lowcarbsub_cap(r,t) = %lowcarbsub_cap%;

* * * Sets of relevant generators
set
ivrt(i,v,r,t)           Active vintage-capacity blocks
jvrt(j,v,r,t)           Active storage vintage-capacity blocks
tvrt(k,v,r,t)  Active transmission vintage-capacity blocks
;

* * Generation technologies (ivrt)
ivrt(new(i),newv(v),r,t)$(lifetime(i,v,r,t))                    = YES ;
ivrt(i,oldv(v),r,t)$(capt(i,v,r,"2022") * lifetime(i,v,r,t))    = YES ;
ivrt(i,oldv(v),r,t)$(capt(i,v,r,"2023") * lifetime(i,v,r,t))    = YES ;
* Other technologies to exlcude                                                                         
$if      set nocoa       ivrt(coa(i),newv(v),r,t)          = NO ;
$if      set nooil       ivrt(oil(i),newv(v),r,t)          = NO ;
$if      set nolig       ivrt(lig(i),newv(v),r,t)          = NO ; 
$if      set noccs       ivrt(ccs(i),newv(v),r,t)          = NO ;
$if      set nonuc       ivrt(nuc(i),newv(v),r,t)          = NO ;
$if      set nogas       ivrt(gas(i),newv(v),r,t)          = NO ;
ivrt(i,v,r,t)$(not toptimize(t))                           = NO ;
$if      set longrun    ivrt(i,v,r,t)$(v.val ge 2024 and v.val le 2029)            = NO ;

* * Storage technologies (jvrt)
jvrt(newj(j),newv(v),r,t)$(glifetime(j,v,r,t))             = YES ;
jvrt(j,v,r,t)$(gcapt(j,v,r,"2022") * glifetime(j,v,r,t))   = YES ;
jvrt(j,v,r,t)$(gcapt(j,v,r,"2023") * glifetime(j,v,r,t))   = YES ;
$if not  set storage     jvrt(j,v,r,t)                     = NO ;
jvrt(j,v,r,t)$(not toptimize(t))                           = NO ;
$if      set longrun    jvrt(j,v,r,t)$(v.val ge 2024 and v.val le 2029)            = NO ;

* * Transmission (tvrt)
tvrt(k,v,r,t)$(sum(tmap(k,r,rr), tlifetime(k,v,r,t)))                                                                                   = YES ;
$if not  set trans       tvrt(k,v,r,t)                                                                                                  = NO ;                                                      
*tvrt(k,v,r,t)$(not toptimize(t))                  = NO ;

* * * Dispatch cost
parameter
discost(i,v,r,t)         Dispatch cost (EUR per MWh el)
;

* * * Define dispatch cost
discost(ivrt(i,v,r,t))$(effrate(i,v,r)) = 
*        Variable O&M costs
                           vomcost(i,v,r)
*        Fuel costs (including region-specific price delta) including regional adder relative                    
                         + round(sum(xfueli(fuel,i), pfuel(fuel,r,t) / effrate(i,v,r)), 8)
* Determine true average load of each vintages and calibrate for ramping cost here
* Determine true average load of each vintages and calibrate for ramping cost here
$if      set ramcost     * (1 + effloss(i,v,r) / 0.5 )
*        Regional adder absolute
*                         + (pfadd(fuel,r,t)$xfueli(fuel,i))$effrate(i,v,r)
*        CO2 price (includes benefits from negative emissions)
$if      set co2price    + emit(i,v,r) * co2p(t)
*        CCS costs (from capturing)
$if      set ccs         + round(co2captured(i,v,r) * ccscost(r,t), 8)
;

discost(ivrt(chp(i),v,r,t))$(effrate(i,v,r)) = 
*        Variable O&M costs
                           vomcost(i,v,r) 
*        Fuel costs (including region-specific price delta) including regional adder relative                    
                         + round(sum(xfueli(fuel,i), pfuel(fuel,r,t) / effrate(i,v,r)), 8)
* Determine true average load of each vintages and calibrate for ramping cost here
* Determine true average load of each vintages and calibrate for ramping cost here
$if      set ramcost     * (1 + effloss(i,v,r) / 0.5 )
*        Regional adder absolute
*                         + (pfadd(fuel,r,t)$xfueli(fuel,i))$effrate(i,v,r)
*        CO2 price (includes benefits from negative emissions)
$if      set co2price    + emit(i,v,r) * co2p(t)
*        CCS costs (from capturing)
$if      set ccs         + round(co2captured(i,v,r) * ccscost(r,t), 8)
;

parameter          
voll(r,t)       
afm(m,i,r,t)
afm_chp(m,i,r,t)
afm_nochp(m,i,r,t)
afmin_m(m,i,r,t)
afmax_m(m,i,r,t)
gafmin_m(m,j,r,t)
gafmax_m(m,j,r,t)
;

$onUndf
$gdxin database\setpar_%n%.gdx
$load afm, afm_chp, afm_nochp, afmin_m, afmax_m, gafmin_m, gafmax_m
$gdxin

voll(r,t) = 3000 ;

afm(m,i,r,t)$(t.val ge 2024)        = afm(m,i,r,"2023") ;
afm_nochp(m,i,r,t)$(t.val ge 2024)  = afm_nochp(m,i,r,"2023") ;
afm_chp(m,i,r,t)$(t.val ge 2024)    = afm_chp(m,i,r,"2023") ;
afmin_m(m,i,r,t)$(t.val ge 2024)    = afmin_m(m,i,r,"2023") ;
afmax_m(m,i,r,t)$(t.val ge 2024)    = afmax_m(m,i,r,"2023") ;
gafmin_m(m,j,r,t)$(t.val ge 2024)   = gafmin_m(m,j,r,"2023") ;
gafmax_m(m,j,r,t)$(t.val ge 2024)   = gafmax_m(m,j,r,"2023") ;

set
ivrt_nochp(i,v,r,t)
ivrt_chp(i,v,r,t)
;

ivrt_nochp(i,v,r,t)$(ivrt(i,v,r,t) and not chp(i)) = YES ;
ivrt_chp(i,v,r,t)$(ivrt(i,v,r,t) and chp(i)) = YES ;

* Set reliability of old vintage technologies to 1 (availability is only steered via availability)
reliability(ivr(i,oldv(v),r)) = 1 ;

set
rge(r)/Germany/
rfr(r)
rbr(r)
reu(r)
topt2035plus(t)
hyd(i) /Hydro/
;

rfr(r)$(sameas(r,"France")) = YES ;
rbr(r)$(sameas(r,"Britain")) = YES ;
reu(r)$(not sameas(r,"Britain")) = YES ;
topt2035plus(t)$(t.val ge 2035 and toptimize(t)) = YES ;

$include    modules_precal\euregen2024_pre_generation_v1.gms
$if not  set days   $include    modules_precal\euregen2024_pre_segments_v3.gms    
$if      set days   $include    modules_precal\euregen2024_pre_days_v4.gms

hydtwh("Austria","2050") = 25.9 ;
hydtwh("Belgium","2050") = 39.5 ;
hydtwh("Bulgaria","2050") = 14.6 ;
*hydtwh("Switzerland","2050") = 0 ;
hydtwh(r,"2050")$(sameas(r,"Switzerland")) = hydtwh("Germany","2050") * daref(r,"2050") / daref("Germany","2050") ;
hydtwh("Czech","2050") = 32 ;
hydtwh("Germany","2050") = 227 ;
hydtwh("Denmark","2050") = 13.5 ;
hydtwh("Spain","2050") = 97.1 ;
hydtwh("Estonia","2050") = 3.8 ;
hydtwh("Finland","2050") = 25.8 ;
hydtwh("France","2050") = 189 ;
hydtwh("Britain","2050") = 140 ;
hydtwh("Greece","2050") = 17.9 ;
hydtwh("Croatia","2050") = 6.6 ;
hydtwh("Hungary","2050") = 19.8 ;
hydtwh("Ireland","2050") = 11.8 ;
hydtwh("Italy","2050") = 117 ;
hydtwh("Lithuania","2050") = 5 ;
hydtwh("Luxembourg","2050") = 3.6 ;
hydtwh("Latvia","2050") = 3.7 ;
hydtwh("Netherlands","2050") = 51 ;
*hydtwh("Norway","2050") = 0 ;
hydtwh(r,"2050")$(sameas(r,"Norway")) = hydtwh("Germany","2050") * daref(r,"2050") / daref("Germany","2050") ;
hydtwh("Poland","2050") = 78.9 ;
hydtwh("Portugal","2050") = 17.7 ;
hydtwh("Romania","2050") = 25.7 ;
hydtwh("Slovakia","2050") = 12.8 ;
hydtwh("Slovenia","2050") = 5.2 ;
hydtwh("Sweden","2050") = 36.8 ;

$if      set hydoldnumbers  hydtwh("Germany","2050") = 197.2728 ;
$if      set hydoldnumbers  hydtwh(r,t)$(not sameas(r,"Germany")) = hydtwh("Germany","2050") * daref(r,"2050") / daref("Germany","2050") ;

hydtwh(r,toptimize(t))$(t.val ge 2027) = round(hydtwh(r,"2050") * (t.val - 2026) / (2050 - 2026),4) ;

* * * Minimum dispatch modules (minimum dispatch of all technologies, this condition is deactivitated if not "techmin=yes")
SET
$if not  set nucmin  techmini(i)         Technologies with techmin constraints /Nuclear,Coal,Coal_CCS,Lignite,Bioenergy,Bio_CCS,Gas_CCGT,Gas_OCGT,OilOther,Gas_ST/
$if      set nucmin  techmini(i)         Technologies with techmin constraints /Nuclear/
;

PARAMETER
stacost(i,v,r)      Start-up cost (EUR per MWh)
;

$gdxin database\setpar_%n%.gdx
$load stacost
$gdxin

$if      set nucmin  stacost(i,v,r)     = 0 ;
$if      set nucmin  stacost("Nuclear",v,r)$(v.val le 2015)  = 80 ;
$if      set nucmin  stacost("Nuclear",v,r)$(v.val ge 2020)  = 50 ;

* * * Ramping modules (ramping up and down ability for a set of technologies, this condition is deactiviated if not "ramping=yes")
SET
ram(i)          Technologies with ramping constraints 
;

$gdxin database\setpar_%n%.gdx
$load ram
$gdxin

PARAMETER
ramrate(i,v,r)  ramping quota (% per hour)
ramcost(i,v,r)  ramping cost (EUR per MW(h))
effloss(i,v,r)  efficiency loss from ramping (% points)
;

$gdxin database\setpar_%n%.gdx
$load ramrate,ramcost,effloss
$gdxin
              
set
ivrttv(i,v,r,t)
jvrttv(j,v,r,t)
tvrttv(k,v,r,t)
;

* ETC modules
ivrttv(ivrt(i,v,r,t))$(tv(t,v)) = YES ;
jvrttv(jvrt(j,v,r,t))$(tv(t,v)) = YES ;
tvrttv(tvrt(k,v,r,t))$(tv(t,v)) = YES ;

$if      set learning   $include modules_precal\euregen2024_pre_learning_v1

set
topt2030(t)
topt2030plus(t)
;

topt2030(t)$(toptimize(t) and t.val le 2030) = YES ;
topt2030plus(t)$(toptimize(t) and t.val ge 2031) = YES ;
              
set
ivrt_capacity(i,v,r,t)
irnw_nohydro(i)
biominmax(i)
;

ivrt_capacity(i,v,r,t)$(ivrt(i,v,r,t) and not nuc(i) and not irnw(i) and not sameas(i,"Bioenergy")
$if      set chp           and not chp(i)
$if      set flh           and not ir_flh(i,r)
$if      set flheur        and not i_flh(i)
$if      set mergeirnw     and not irnw(i)
                    ) = YES ;
irnw_nohydro(i)$(irnw(i) and not sameas(i,"Hydro")) = YES ;
biominmax(i)$(bio(i) and not chp(i) and not ccs(i)) = YES ;

set
ivrt_tv(i,v,r,t)
ivrt_nottv(i,v,r,t)
jvrt_tv(j,v,r,t)
jvrt_nottv(j,v,r,t)
v2022_2023(v)
newconv(i)
nonvj(j)
notirnw(i)
;

ivrt_tv(i,v,r,t)$(ivrt(i,v,r,t) and tv(t,v)) = YES ;
ivrt_nottv(i,v,r,t)$(ivrt(i,v,r,t) and not tv(t,v)) = YES ;
jvrt_tv(j,v,r,t)$(jvrt(j,v,r,t) and tv(t,v)) = YES ;
jvrt_nottv(j,v,r,t)$(jvrt(j,v,r,t) and not tv(t,v)) = YES ;
v2022_2023(oldv(v))$(v.val ge 2022) = YES ;
newconv(i)$(new(i) and conv(i) and chp(i)) = YES ;
nonvj(j)$(not nvj(j)) = YES ;
notirnw(i)$(not irnw(i)) = YES ;

$include modules_precal\euregen2024_pre_targets_v1
