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

set
$if      set static                            toptimize(t)     Optimization periods /%year%/
$if not  set static     $if      set shortrun  toptimize(t)     Optimization periods /2022,2023,2024,2025,2026,2027,2028,2029,2030,2035,2040,2045,2050/
$if not  set static     $if      set longrun   toptimize(t)     Optimization periods /2022,2025,2030,2035,2040,2045,2050/

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
invlife(i,v,r)                     Capacity lifetime
invdepr(i,v,r)                     Investment depreciation
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
biolim_int(r,t)                  Upper bounds by region on biomass use (MWh)
biolim_eu_int(t)                 Upper bounds by region on biomass use (MWh)
biolim(r,t)                      Upper bounds by region on biomass use (MWh)
biolim_eu(t)                     Upper bounds by region on biomass use (MWh)
;

$gdxin database\setpar_%n%.gdx
$load capt_int=capt, invlimUP, invlimLO, invlimUP_eu, invlife, invdepr, capcost_int=capcost, fomcost_int=fomcost, vomcost, effrate_int=effrate, co2captured_int=co2captured, emit_int=emit, reliability, capcred, mindisp
$load sclim_int=sclim, sclim_eu_int=sclim_eu, biolim_int=biolim, biolim_eu_int=biolim_eu
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
biolim_eu(t) = 1 * biolim_eu_int(t) ;
$if      set biolimhalf        biolim_eu(t) = 0.5 * biolim_eu_int(t) ;
$if      set biolimdouble      biolim_eu(t) = 2 * biolim_eu_int(t) ;
biolim(r,t) = 1 * biolim_int(r,t) ;
$if      set biolimhalf        biolim(r,t) = 0.5 * biolim_int(r,t) ;
$if      set biolimdouble      biolim(r,t) = 2 * biolim_int(r,t) ;
     
* * * Storage technology
set
newj(j)                          New storage technology
exij(j)                          Existing storage technology
;

$gdxin database\setpar_%n%.gdx
$load newj, exij
$gdxin

parameter
gcap(j,v,r)                      Storage capacity by region (GW)
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

gcap(j,v,r) = gcapt(j,v,r,"2021") ;

gcapcost(j,v,r)                                          = 1   * gcapcost_int(j,v,r) ;
$if      set halfgcc     gcapcost(j,v,r)$(v.val ge 2020) = 0.5 * gcapcost_int(j,v,r) ;
$if      set doublegcc   gcapcost(j,v,r)$(v.val ge 2020) = 2   * gcapcost_int(j,v,r) ;
$if      set triplegcc   gcapcost(j,v,r)$(v.val ge 2020) = 3   * gcapcost_int(j,v,r) ;

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
;

$gdxin database\setpar_%n%.gdx
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
;

$gdxin database\setpar_%n%.gdx
$load lifetime, deprtime
$load glifetime, gdeprtime
$load tlifetime, tdeprtime
$load modeldepr, modeldepr_nodisc
$load gmodeldepr, gmodeldepr_nodisc
$load tmodeldepr, tmodeldepr_nodisc
;

* * * Myopic module
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

* * * Investor module
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

;

$gdxin database\setpar_%n%.gdx
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
* Investment cost factor according to options
$if      set opt1   zeta(inv,i,v,r)  = zeta_invir("opt1",inv,i,v,r) ;
$if      set opt3   zeta(inv,i,v,r)  = zeta_invir("opt3",inv,i,v,r) ;
$if      set opt1   gzeta(inv,j,v,r) = zeta_invjr("opt1",inv,j,v,r) ;
$if      set opt3   gzeta(inv,j,v,r) = zeta_invjr("opt3",inv,j,v,r) ;
$if      set opt1   tzeta(inv,k,v,r) = zeta_invkr("opt1",inv,k,v,r) ;
$if      set opt3   tzeta(inv,k,v,r) = zeta_invkr("opt3",inv,k,v,r) ;
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

* Price adjustments must happen here because discost follow before the rest of the "lag"-module is defined
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
dbclass          Dedicated biomass supply classes
;

$gdxin database\setpar_%n%.gdx
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

$gdxin database\setpar_%n%.gdx
$load ngref, ngref_r, ngelas, ngelas_r, ngcost, ngcost_r, nglim, nglim_r
$load dbref, dbref_r, dbelas, dbelas_r, dbcost, dbcost_r, dblim, dblim_r
$gdxin

* * * Demand module
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
 
* * * Energy efficiency module

* * * Social cost module
set
ap                               Air pollutant
ap                               Air pollutant
impactap                         Impact of air pollutant
emfap_sc                         Scenario emission factor ;

$gdxin database\setpar_%n%.gdx
$load ap, impactap, emfap_sc
$gdxin

parameter
gdpgrowth(r,t)                                           GDP growth index
gdpdistri(r,t)                                           GDP distributional index
emfap_int(i,emfap_sc,ap,v)                               Emission factor air pollutant (t per MWh thermal)
emitap_int(i,emfap_sc,ap,v,r)                            Emission factor air pollutant (t per MWh electric)
emfap(i,ap,v)                                            Emission factor air pollutant (t per MWh thermal)
emitap(i,ap,v,r)                                         Emission factor air pollutant (t per MWh electric)
scap_int(ap,impactap,r,t)                                Social cost of air pollution (2015er EUR per t)
scap_emit_impactap_int(impactap,emfap_sc,i,v,r,t)        Social cost of air pollution by impact (2015er EUR per MWh electric)
scap_emit_ap_int(ap,emfap_sc,i,v,r,t)                    Social cost of air pollution by pollutant (2015er EUR per MWh electric)
scap_emit_int(emfap_sc,i,v,r,t)                          Social cost of air pollution (2015er EUR per MWh electric)
scap_i(ap,impactap,r,t)                                  Social cost of air pollution (2015er EUR per t)
scap_emit_impactap_i(impactap,i,v,r,t)                   Social cost of air pollution by impact (2015er EUR per MWh electric)
scap_emit_ap_i(ap,i,v,r,t)                               Social cost of air pollution by pollutant (2015er EUR per MWh electric)
scap_emit_i(i,v,r,t)                                     Social cost of air pollution (2015er EUR per MWh electric)
scapr(ap,impactap,t)                                     Social cost of air pollution (2015er EUR per t)
scapr_emit_impactap(impactap,i,v,t)                      Social cost of air pollution by impact (2015er EUR per MWh electric)
scapr_emit_ap(ap,i,v,t)                                  Social cost of air pollution by pollutant (2015er EUR per MWh electric)
scapr_emit(i,v,t)                                        Social cost of air pollution (2015er EUR per MWh electric)
scap(ap,impactap,r,t)                                    Social cost of air pollution (2015er EUR per t)
scap_emit_impactap(impactap,i,v,r,t)                     Social cost of air pollution by impact (2015er EUR per MWh electric)
scap_emit_ap(ap,i,v,r,t)                                 Social cost of air pollution by pollutant (2015er EUR per MWh electric)
scap_emit(i,v,r,t)                                       Social cost of air pollution (2015er EUR per MWh electric)
scc(t)                                                   Social cost of carbon (2015er EUR per t)
scc_emit(i,v,r,t)                                        Social cost of carbon (2015er EUR per MWh electric)
scc_int(t)                                               Social cost of carbon (2015er EUR per t)
scc_emit_int(i,v,r,t)                                    Social cost of carbon (2015er EUR per MWh electric)
drate_scap                                               Annual discount rate
dfact_scap(t)                                            Discount factor for time period t (reflects number of years) for both
drate_scc                                                Annual discount rate
dfact_scc(t)                                             Discount factor for time period t (reflects number of years) for both
;

$gdxin database\setpar_%n%.gdx
$load gdpgrowth, gdpdistri, emfap_int=emfap, emitap_int=emitap, scap_i=scap, scap_int=scap, scap_emit_impactap_int=scap_emit_impactap, scap_emit_ap_int=scap_emit_ap, scap_emit_int=scap_emit, scc_int=scc, scc_emit_int=scc_emit
$load drate_scap, dfact_scap, drate_scc, dfact_scc
$gdxin

$if      set emfaplow            emfap(i,ap,v) = emfap_int(i,"emfap_midlow",ap,v) ;
$if      set emfapmid            emfap(i,ap,v) = emfap_int(i,"emfap_mid",ap,v) ;
$if      set emfaphigh           emfap(i,ap,v) = emfap_int(i,"emfap_high",ap,v) ;

$if      set emfaplow            emitap(i,ap,v,r) = emitap_int(i,"emfap_midlow",ap,v,r) ;
$if      set emfapmid            emitap(i,ap,v,r) = emitap_int(i,"emfap_mid",ap,v,r) ;
$if      set emfaphigh           emitap(i,ap,v,r) = emitap_int(i,"emfap_high",ap,v,r) ;

$if      set emfaplow            scap_emit_impactap_i(impactap,i,v,r,t) = scap_emit_impactap_int(impactap,"emfap_midlow",i,v,r,t) ;
$if      set emfapmid            scap_emit_impactap_i(impactap,i,v,r,t) = scap_emit_impactap_int(impactap,"emfap_mid",i,v,r,t) ;
$if      set emfaphigh           scap_emit_impactap_i(impactap,i,v,r,t) = scap_emit_impactap_int(impactap,"emfap_high",i,v,r,t) ;

$if      set emfaplow            scap_emit_ap_i(ap,i,v,r,t) = scap_emit_ap_int(ap,"emfap_midlow",i,v,r,t) ;
$if      set emfapmid            scap_emit_ap_i(ap,i,v,r,t) = scap_emit_ap_int(ap,"emfap_mid",i,v,r,t) ;
$if      set emfaphigh           scap_emit_ap_i(ap,i,v,r,t) = scap_emit_ap_int(ap,"emfap_high",i,v,r,t) ;

$if      set emfaplow            scap_emit_i(i,v,r,t) = scap_emit_int("emfap_midlow",i,v,r,t) ;
$if      set emfapmid            scap_emit_i(i,v,r,t) = scap_emit_int("emfap_mid",i,v,r,t) ;
$if      set emfaphigh           scap_emit_i(i,v,r,t) = scap_emit_int("emfap_high",i,v,r,t) ;

$if      set scap1               scap(ap,impactap,r,t)                   = 0.25 * scap_i(ap,impactap,r,t) ;
$if      set scap1               scap_emit_impactap(impactap,i,v,r,t)    = 0.25 * scap_emit_impactap_i(impactap,i,v,r,t) ;
$if      set scap1               scap_emit_ap(ap,i,v,r,t)                = 0.25 * scap_emit_ap_i(ap,i,v,r,t) ;
$if      set scap1               scap_emit(i,v,r,t)                      = 0.25 * scap_emit_i(i,v,r,t) ;

$if      set scap2               scap(ap,impactap,r,t)                   = 0.5 * scap_i(ap,impactap,r,t) ;
$if      set scap2               scap_emit_impactap(impactap,i,v,r,t)    = 0.5 * scap_emit_impactap_i(impactap,i,v,r,t) ;
$if      set scap2               scap_emit_ap(ap,i,v,r,t)                = 0.5 * scap_emit_ap_i(ap,i,v,r,t) ;
$if      set scap2               scap_emit(i,v,r,t)                      = 0.5 * scap_emit_i(i,v,r,t) ;

$if      set scap3               scap(ap,impactap,r,t)                   = 1 * scap_i(ap,impactap,r,t) ;
$if      set scap3               scap_emit_impactap(impactap,i,v,r,t)    = 1 * scap_emit_impactap_i(impactap,i,v,r,t) ;
$if      set scap3               scap_emit_ap(ap,i,v,r,t)                = 1 * scap_emit_ap_i(ap,i,v,r,t) ;
$if      set scap3               scap_emit(i,v,r,t)                      = 1 * scap_emit_i(i,v,r,t) ;

$if      set scap4               scap(ap,impactap,r,t)                   = 2 * scap_i(ap,impactap,r,t) ;
$if      set scap4               scap_emit_impactap(impactap,i,v,r,t)    = 2 * scap_emit_impactap_i(impactap,i,v,r,t) ;
$if      set scap4               scap_emit_ap(ap,i,v,r,t)                = 2 * scap_emit_ap_i(ap,i,v,r,t) ;
$if      set scap4               scap_emit(i,v,r,t)                      = 2 * scap_emit_i(i,v,r,t) ;

$if      set scap5               scap(ap,impactap,r,t)                   = 4 * scap_i(ap,impactap,r,t) ;
$if      set scap5               scap_emit_impactap(impactap,i,v,r,t)    = 4 * scap_emit_impactap_i(impactap,i,v,r,t) ;
$if      set scap5               scap_emit_ap(ap,i,v,r,t)                = 4 * scap_emit_ap_i(ap,i,v,r,t) ;
$if      set scap5               scap_emit(i,v,r,t)                      = 4 * scap_emit_i(i,v,r,t) ;

$if      set scap6               scap(ap,impactap,r,t)                   = 8 * scap_i(ap,impactap,r,t) ;
$if      set scap6               scap_emit_impactap(impactap,i,v,r,t)    = 8 * scap_emit_impactap_i(impactap,i,v,r,t) ;
$if      set scap6               scap_emit_ap(ap,i,v,r,t)                = 8 * scap_emit_ap_i(ap,i,v,r,t) ;
$if      set scap6               scap_emit(i,v,r,t)                      = 8 * scap_emit_i(i,v,r,t) ;

$if      set distri              scap(ap,impactap,r,t)                   = scap_i(ap,impactap,r,t)                       * gdpdistri(r,t) ;
$if      set distri              scap_emit_impactap(impactap,i,v,r,t)    = scap_emit_impactap_i(impactap,i,v,r,t)        * gdpdistri(r,t) ;
$if      set distri              scap_emit_ap(ap,i,v,r,t)                = scap_emit_ap_i(ap,i,v,r,t)                    * gdpdistri(r,t) ;
$if      set distri              scap_emit(i,v,r,t)                      = scap_emit_i(i,v,r,t)                          * gdpdistri(r,t) ;

$if      set scapequal           scapr(ap,impactap,t)                    = round( sum(r, daref(r,t) * scap_i(ap,impactap,r,t))                  / sum(r, daref(r,t)),4) ;
$if      set scapequal           scapr_emit_impactap(impactap,i,v,t)     = round( sum(r, daref(r,t) * scap_emit_impactap_i(impactap,i,v,r,t))   / sum(r, daref(r,t)),4) ;
$if      set scapequal           scapr_emit_ap(ap,i,v,t)                 = round( sum(r, daref(r,t) * scap_emit_ap_i(ap,i,v,r,t))               / sum(r, daref(r,t)),4) ;
$if      set scapequal           scapr_emit(i,v,t)                       = round( sum(r, daref(r,t) * scap_emit_i(i,v,r,t))                     / sum(r, daref(r,t)),4) ;

$if      set scapequal           scap(ap,impactap,r,t)                   = scapr(ap,impactap,t) ;
$if      set scapequal           scap_emit_impactap(impactap,i,v,r,t)    = scapr_emit_impactap(impactap,i,v,t) ;
$if      set scapequal           scap_emit_ap(ap,i,v,r,t)                = scapr_emit_ap(ap,i,v,t) ;
$if      set scapequal           scap_emit(i,v,r,t)                      = scapr_emit(i,v,t) ;

$if      set scapequaldistri     scapr(ap,impactap,t)                    = sum(r, daref(r,t) * scap_i(ap,impactap,r,t))                  / sum(r, daref(r,t)),4) ;
$if      set scapequaldistri     scapr_emit_impactap(impactap,i,v,t)     = sum(r, daref(r,t) * scap_emit_impactap_i(impactap,i,v,r,t))   / sum(r, daref(r,t)),4) ;
$if      set scapequaldistri     scapr_emit_ap(ap,i,v,t)                 = sum(r, daref(r,t) * scap_emit_ap_i(ap,i,v,r,t))               / sum(r, daref(r,t)),4) ;
$if      set scapequaldistri     scapr_emit(i,v,t)                       = sum(r, daref(r,t) * scap_emit_i(i,v,r,t))                     / sum(r, daref(r,t)),4) ;

$if      set scapequaldistri     scap(ap,impactap,r,t)                   = scapr(ap,impactap,t)  * gdpdistri(r,t) ;
$if      set scapequaldistri     scap_emit_impactap(impactap,i,v,r,t)    = scapr_emit_impactap(impactap,i,v,t) * gdpdistri(r,t) ;
$if      set scapequaldistri     scap_emit_ap(ap,i,v,r,t)                = scapr_emit_ap(ap,i,v,t)* gdpdistri(r,t) ;
$if      set scapequaldistri     scap_emit(i,v,r,t)                      = scapr_emit(i,v,t) * gdpdistri(r,t) ;

$if      set nogdpgrowth         scap(ap,impactap,r,t)                   = round( scap_i(ap,impactap,r,t)                   / gdpgrowth(r,t),4) ;
$if      set nogdpgrowth         scap_emit_impactap(impactap,i,v,r,t)    = round( scap_emit_impactap_i(impactap,i,v,r,t)    / gdpgrowth(r,t),4) ;
$if      set nogdpgrowth         scap_emit_ap(ap,i,v,r,t)                = round( scap_emit_ap_i(ap,i,v,r,t)                / gdpgrowth(r,t),4) ;
$if      set nogdpgrowth         scap_emit(i,v,r,t)                      = round( scap_emit_i(i,v,r,t)                      / gdpgrowth(r,t),4) ;

$if      set scapequalnogdp      scapr(ap,impactap,t)                    = round( sum(r, daref(r,t) * scap_i(ap,impactap,r,t)                    / gdpgrowth(r,t) )   / sum(r, daref(r,t)),4) ;
$if      set scapequalnogdp      scapr_emit_impactap(impactap,i,v,t)     = round( sum(r, daref(r,t) * scap_emit_impactap_i(impactap,i,v,r,t)     / gdpgrowth(r,t) )   / sum(r, daref(r,t)),4) ;
$if      set scapequalnogdp      scapr_emit_ap(ap,i,v,t)                 = round( sum(r, daref(r,t) * scap_emit_ap_i(ap,i,v,r,t)                 / gdpgrowth(r,t) )   / sum(r, daref(r,t)),4) ;
$if      set scapequalnogdp      scapr_emit(i,v,t)                       = round( sum(r, daref(r,t) * scap_emit_i(i,v,r,t)                       / gdpgrowth(r,t) )   / sum(r, daref(r,t)),4) ;

$if      set scapequalnogdp      scap(ap,impactap,r,t)                   = scapr(ap,impactap,t) ;
$if      set scapequalnogdp      scap_emit_impactap(impactap,i,v,r,t)    = scapr_emit_impactap(impactap,i,v,t) ;
$if      set scapequalnogdp      scap_emit_ap(ap,i,v,r,t)                = scapr_emit_ap(ap,i,v,t) ;
$if      set scapequalnogdp      scap_emit(i,v,r,t)                      = scapr_emit(i,v,t) ;

$if      set scc1                scc(t)                                  = 0.25 * scc_int(t) ;
$if      set scc1                scc_emit(i,v,r,t)                       = 0.25 * scc_emit_int(i,v,r,t) ;
$if      set scc2                scc(t)                                  = 0.5 * scc_int(t) ;
$if      set scc2                scc_emit(i,v,r,t)                       = 0.5 * scc_emit_int(i,v,r,t) ;
$if      set scc3                scc(t)                                  = 1 * scc_int(t) ;
$if      set scc3                scc_emit(i,v,r,t)                       = 1 * scc_emit_int(i,v,r,t) ;
$if      set scc4                scc(t)                                  = 2 * scc_int(t) ;
$if      set scc4                scc_emit(i,v,r,t)                       = 2 * scc_emit_int(i,v,r,t) ;
$if      set scc5                scc(t)                                  = 4 * scc_int(t) ;
$if      set scc5                scc_emit(i,v,r,t)                       = 4 * scc_emit_int(i,v,r,t) ;
$if      set scc6                scc(t)                                  = 8 * scc_int(t) ;
$if      set scc6                scc_emit(i,v,r,t)                       = 8 * scc_emit_int(i,v,r,t) ;
* No SCC adjustments according to scenarios in first period
scc("2020") = scc_int("2020") ;
scc_emit(i,v,r,"2020") = scc_emit_int(i,v,r,"2020") ;
* No air pollution internalization in first period
dfact_scap("2020") = 0 ;
* Switches to avoid SCC and SCAP in objective function (depending on policy question changes are necessary here)
$if not  set scc    dfact_scc(t)$(t.val >= 2025) = 0 ;
$if not  set scc    dfact_scc(t)  = 0 ;
$if not  set scap   dfact_scap(t) = 0 ;


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
   
* * Storage technologies (jvrt)
jvrt(newj(j),newv(v),r,t)$(glifetime(j,v,r,t))             = YES ;
jvrt(j,v,r,t)$(gcapt(j,v,r,"2022") * glifetime(j,v,r,t))   = YES ;
jvrt(j,v,r,t)$(gcapt(j,v,r,"2023") * glifetime(j,v,r,t))   = YES ;
$if not  set storage     jvrt(j,v,r,t)                     = NO ;
jvrt(j,v,r,t)$(not toptimize(t))                           = NO ;

Parameter
ghoursnv(j,r)
* Merging all vintages
dischrgnv(j,r)
chrgpennv(j,r)
dchrgpennv(j,r)
* Endbalance
storage_endbalance(j,v,r,t)
storage_endbalancenv(j,r,t)
;

ghoursnv(j,r)$(sum(jvrt(j,oldv(v),r,t), gcapt(j,v,r,t)) > 0)  = sum(jvrt(j,oldv(v),r,t), ghours(j,v,r) * gcapt(j,v,r,t)) / sum(jvrt(j,oldv(v),r,t), gcapt(j,v,r,t)) ;
dischrgnv(j,r)  = smax(v, dischrg(j,v,r)) ;
chrgpennv(j,r)  = smax(v, chrgpen(j,v,r)) ;
dchrgpennv(j,r) = smax(v, dchrgpen(j,v,r)) ;

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

afm(m,i,r,t)$(t.val ge 2024)         = afm(m,i,r,"2023") ;
afm_nochp(m,i,r,t)$(t.val ge 2024)   = afm_nochp(m,i,r,"2023") ;
afm_chp(m,i,r,t)$(t.val ge 2024)     = afm_chp(m,i,r,"2023") ;
afmin_m(m,i,r,t)$(t.val ge 2024)    = afmin_m(m,i,r,"2023") ;
afmax_m(m,i,r,t)$(t.val ge 2024)    = afmax_m(m,i,r,"2023") ;
gafmin_m(m,j,r,t)$(t.val ge 2024)   = gafmin_m(m,j,r,"2023") ;
gafmax_m(m,j,r,t)$(t.val ge 2024)   = gafmax_m(m,j,r,"2023") ;

parameter
vollelas(bse,r,t)
;

vollelas("bs1",r,t) = 500 ;
vollelas("bs2",r,t) = 1000 ;
vollelas("bs3",r,t) = 1500 ;
vollelas("bs4",r,t) = 2000 ;
vollelas("bs5",r,t) = 2500 ;
vollelas("bs6",r,t) = 3000 ;

set
topt2023(t)
ivrt_nochp(i,v,r,t)
ivrt_chp(i,v,r,t)
topt2024_2030(t)
topt2024(t)
t2023(t)
told(t) /2020,2021,2022,2023/
tcri(t) /2022,2023/
;

topt2023(t)$(toptimize(t) and t.val le 2023) = YES ;
ivrt_nochp(i,v,r,t)$(ivrt(i,v,r,t) and not chp(i)) = YES ;
ivrt_chp(i,v,r,t)$(ivrt(i,v,r,t) and chp(i)) = YES ;
topt2024_2030(t)$(toptimize(t) and t.val ge 2024 and t.val le 2030) = YES ;
topt2024(t)$(toptimize(t) and t.val ge 2024) = YES ;
t2023(t)$(t.val le 2023) = YES ;

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

$if not  set days   $if not  set hours  $include    modules\euregen2024_pre_segments_v2.gms    
$if      set days                       $include    modules\euregen2024_pre_days_v3.gms
$if      set hours                      $include    modules\euregen2024_pre_hours_v2.gms
$if      set static $if not  set days   $include    modules\euregen2024_pre_static_v1.gms
$if      set static $if      set days   $include    modules\euregen2024_pre_static_days_v1.gms     

* * * Minimum dispatch module (minimum dispatch of all technologies, this condition is deactivitated if not "techmin=yes")
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

* * * Ramping module (ramping up and down ability for a set of technologies, this condition is deactiviated if not "ramping=yes")
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
              
* * * LBD: Learning-by-doing
set
i_del(i)         Technologies to exclude from the model
i_lea(i)         Technologies to learn
r_lea(r)         Regions to learn
ir_lea(i,r)      Technology region pair under learning
ls               Number of line segments
dpr              Depreciation method (of experience stock)
;

$gdxin database\setpar_%n%.gdx
$load i_del, i_lea, r_lea, ls, dpr
$gdxin

ir_lea(i,r) = YES$(i_lea(i) and r_lea(r)) ;

$if      set learning capcost(i,v,r)$(i_lea(i)) = capcost_int(i,v,r) ;
$if      set learning capcost(i_del(i),v,r) = 0 ;
$if      set learning lifetime(i_del(i),v,r,t) = NO ;
$if      set learning deprtime(i_del(i),v,r,t) = NO ;
$if      set learning capcost(i_del(i),v,r)    = 0 ;
$if      set learning vrsc(s,i_del(i),v,r)       = 0 ;
$if      set learning invlimLO(i_del(i),r,t)   = 0 ;
$if      set learning cap(i_del(i),v,r)        = 0 ;

parameter
ls_weight(ls)            Weight to determine breakpoints
delta_q_pa               Annual depreciation factor of the experience stock
delta_q                  Periodical depreciation factor of the experience stock
;

$gdxin database\setpar_%n%.gdx
$load ls_weight,delta_q_pa, delta_q
$gdxin

* * * Regional Learning-by-doing (LBD)
parameter
* Interim parameter
b_q_int(dpr,i,r) 
qFIRST_int(dpr,i,r)
qSTART_int(dpr,i,r) 
qLAST_int(dpr,i,r) 
qlsLO_int(dpr,i,r,ls) 
qlsUP_int(dpr,i,r,ls) 
capcost0_int(dpr,i,r) 
acc_capexFIRST_int(dpr,i,r) 
acc_capexSTART_int(dpr,i,r) 
acc_capexLAST_int(dpr,i,r) 
acc_capexLO_int(dpr,i,r,ls) 
acc_capexUP_int(dpr,i,r,ls) 
slope_mip_int(dpr,i,r,ls) 
* Used parameter
b_q(i,r)                 Learning rate from capacity (Q) expansion by technology and region
qFIRST(i,r)              First capacity stock unit (GW)
qSTART(i,r)              Initial capacity stock (GW)
qLAST(i,r)               Maximum capacity stock (GW)
qlsLO(i,r,ls)            Lower kink points of capacity stock (GW)
qlsUP(i,r,ls)            Upper kink points of capacity stock (GW)
capcost0(i,r)            Cost of the first unit installed (EUR per kW)
acc_capexFIRST(i,r)      First unit capacity stock accumulated cost (million)
acc_capexSTART(i,r)      Initial capacity stock accumulated cost (million)
acc_capexLAST(i,r)       Maximum capacity stock accumulated cost (million)
acc_capexLO(i,r,ls)      Lower kink points of capacity stock accumulated cost (million)
acc_capexUP(i,r,ls)      Upper kink points of capacity stock accumulated cost (million)
slope_mip(i,r,ls)        Slope of linear approximated function (EUR per kW)
;


$gdxin database\setpar_%n%.gdx
$load   b_q_int  =   b_q 
$load   qFIRST_int   =   qFIRST
$load   qSTART_int   =   qSTART 
$load   qLAST_int    =   qLAST 
$load   qlsLO_int    =   qlsLO 
$load   qlsUP_int    =   qlsUP 
$load   capcost0_int     =   capcost0 
$load   acc_capexFIRST_int   =   acc_capexFIRST 
$load   acc_capexSTART_int   =   acc_capexSTART 
$load   acc_capexLAST_int    =   acc_capexLAST 
$load   acc_capexLO_int  =   acc_capexLO 
$load   acc_capexUP_int  =   acc_capexUP 
$load   slope_mip_int    =   slope_mip 
$gdxin

b_q(i,r)   =   b_q_int("rec",i,r)     ;
qFIRST(i,r)    =   qFIRST_int("rec",i,r)  ;
qSTART(i,r)    =   qSTART_int("rec",i,r)  ;
qLAST(i,r)     =   qLAST_int("rec",i,r)   ;
qlsLO(i,r,ls)  =   qlsLO_int("rec",i,r,ls)    ;
qlsUP(i,r,ls)  =   qlsUP_int("rec",i,r,ls)    ;
capcost0(i,r)  =   capcost0_int("rec",i,r)    ;
acc_capexFIRST(i,r)    =   acc_capexFIRST_int("rec",i,r)  ;
acc_capexSTART(i,r)    =   acc_capexSTART_int("rec",i,r)  ;
acc_capexLAST(i,r)     =   acc_capexLAST_int("rec",i,r)   ;
acc_capexLO(i,r,ls)    =   acc_capexLO_int("rec",i,r,ls)  ;
acc_capexUP(i,r,ls)    =   acc_capexUP_int("rec",i,r,ls)  ;
slope_mip(i,r,ls)  =   slope_mip_int("rec",i,r,ls)    ;

$if      set continuous  b_q(i,r)   =   b_q_int("rec",i,r)     ;
$if      set continuous  qFIRST(i,r)    =   qFIRST_int("rec",i,r)  ;
$if      set continuous  qSTART(i,r)    =   qSTART_int("rec",i,r)  ;
$if      set continuous  qLAST(i,r)     =   qLAST_int("rec",i,r)   ;
$if      set continuous  qlsLO(i,r,ls)  =   qlsLO_int("rec",i,r,ls)    ;
$if      set continuous  qlsUP(i,r,ls)  =   qlsUP_int("rec",i,r,ls)    ;
$if      set continuous  capcost0(i,r)  =   capcost0_int("rec",i,r)    ;
$if      set continuous  acc_capexFIRST(i,r)    =   acc_capexFIRST_int("rec",i,r)  ;
$if      set continuous  acc_capexSTART(i,r)    =   acc_capexSTART_int("rec",i,r)  ;
$if      set continuous  acc_capexLAST(i,r)     =   acc_capexLAST_int("rec",i,r)   ;
$if      set continuous  acc_capexLO(i,r,ls)    =   acc_capexLO_int("rec",i,r,ls)  ;
$if      set continuous  acc_capexUP(i,r,ls)    =   acc_capexUP_int("rec",i,r,ls)  ;
$if      set continuous  slope_mip(i,r,ls)  =   slope_mip_int("rec",i,r,ls)    ;

$if      set discrete      b_q(i,r)   =   b_q_int("rec",i,r)     ;
$if      set discrete      qFIRST(i,r)    =   qFIRST_int("rec",i,r)  ;
$if      set discrete      qSTART(i,r)    =   qSTART_int("rec",i,r)  ;
$if      set discrete      qLAST(i,r)     =   qLAST_int("rec",i,r)   ;
$if      set discrete      qlsLO(i,r,ls)  =   qlsLO_int("rec",i,r,ls)    ;
$if      set discrete      qlsUP(i,r,ls)  =   qlsUP_int("rec",i,r,ls)    ;
$if      set discrete      capcost0(i,r)  =   capcost0_int("rec",i,r)    ;
$if      set discrete      acc_capexFIRST(i,r)    =   acc_capexFIRST_int("rec",i,r)  ;
$if      set discrete      acc_capexSTART(i,r)    =   acc_capexSTART_int("rec",i,r)  ;
$if      set discrete      acc_capexLAST(i,r)     =   acc_capexLAST_int("rec",i,r)   ;
$if      set discrete      acc_capexLO(i,r,ls)    =   acc_capexLO_int("rec",i,r,ls)  ;
$if      set discrete      acc_capexUP(i,r,ls)    =   acc_capexUP_int("rec",i,r,ls)  ;
$if      set discrete      slope_mip(i,r,ls)  =   slope_mip_int("rec",i,r,ls)    ;

* * * European Learning-by-doing
parameter
* Interim parameter
b_qeur_int(dpr,i) 
qeurFIRST_int(dpr,i)
qeurSTART_int(dpr,i) 
qeurLAST_int(dpr,i) 
qeurlsLO_int(dpr,i,ls) 
qeurlsUP_int(dpr,i,ls) 
capcosteur0_int(dpr,i) 
acc_capexeurFIRST_int(dpr,i) 
acc_capexeurSTART_int(dpr,i) 
acc_capexeurLAST_int(dpr,i) 
acc_capexeurLO_int(dpr,i,ls) 
acc_capexeurUP_int(dpr,i,ls) 
slopeeur_mip_int(dpr,i,ls) 
* Used parameter
b_qeur(i)                 Learning rate from capacity (Q) expansion by technology and region
qeurFIRST(i)              First capacity stock unit (GW)
qeurSTART(i)              Initial capacity stock (GW)
qeurLAST(i)               Maximum capacity stock (GW)
qeurlsLO(i,ls)            Lower kink points of capacity stock (GW)
qeurlsUP(i,ls)            Upper kink points of capacity stock (GW)
capcosteur0(i)            Cost of the first unit installed (EUR per kW)
acc_capexeurFIRST(i)      First unit capacity stock accumulated cost (million)
acc_capexeurSTART(i)      Initial capacity stock accumulated cost (million)
acc_capexeurLAST(i)       Maximum capacity stock accumulated cost (million)
acc_capexeurLO(i,ls)      Lower kink points of capacity stock accumulated cost (million)
acc_capexeurUP(i,ls)      Upper kink points of capacity stock accumulated cost (million)
slopeeur_mip(i,ls)        Slope of linear approximated function (EUR per kW)
;

$gdxin database\setpar_%n%.gdx
$load   b_qeur_int  =   b_qeur 
$load   qeurFIRST_int   =   qeurFIRST
$load   qeurSTART_int   =   qeurSTART 
$load   qeurLAST_int    =   qeurLAST 
$load   qeurlsLO_int    =   qeurlsLO 
$load   qeurlsUP_int    =   qeurlsUP 
$load   capcosteur0_int     =   capcosteur0 
$load   acc_capexeurFIRST_int   =   acc_capexeurFIRST 
$load   acc_capexeurSTART_int   =   acc_capexeurSTART 
$load   acc_capexeurLAST_int    =   acc_capexeurLAST 
$load   acc_capexeurLO_int  =   acc_capexeurLO 
$load   acc_capexeurUP_int  =   acc_capexeurUP 
$load   slopeeur_mip_int    =   slopeeur_mip 
$gdxin

b_qeur(i)   =   b_qeur_int("rec",i)     ;
qeurFIRST(i)    =   qeurFIRST_int("rec",i)  ;
qeurSTART(i)    =   qeurSTART_int("rec",i)  ;
qeurLAST(i)     =   qeurLAST_int("rec",i)   ;
qeurlsLO(i,ls)  =   qeurlsLO_int("rec",i,ls)    ;
qeurlsUP(i,ls)  =   qeurlsUP_int("rec",i,ls)    ;
capcosteur0(i)  =   capcosteur0_int("rec",i)    ;
acc_capexeurFIRST(i)    =   acc_capexeurFIRST_int("rec",i)  ;
acc_capexeurSTART(i)    =   acc_capexeurSTART_int("rec",i)  ;
acc_capexeurLAST(i)     =   acc_capexeurLAST_int("rec",i)   ;
acc_capexeurLO(i,ls)    =   acc_capexeurLO_int("rec",i,ls)  ;
acc_capexeurUP(i,ls)    =   acc_capexeurUP_int("rec",i,ls)  ;
slopeeur_mip(i,ls)  =   slopeeur_mip_int("rec",i,ls)    ;

$if      set continuous  b_qeur(i)   =   b_qeur_int("rec",i)     ;
$if      set continuous  qeurFIRST(i)    =   qeurFIRST_int("rec",i)  ;
$if      set continuous  qeurSTART(i)    =   qeurSTART_int("rec",i)  ;
$if      set continuous  qeurLAST(i)     =   qeurLAST_int("rec",i)   ;
$if      set continuous  qeurlsLO(i,ls)  =   qeurlsLO_int("rec",i,ls)    ;
$if      set continuous  qeurlsUP(i,ls)  =   qeurlsUP_int("rec",i,ls)    ;
$if      set continuous  capcosteur0(i)  =   capcosteur0_int("rec",i)    ;
$if      set continuous  acc_capexeurFIRST(i)    =   acc_capexeurFIRST_int("rec",i)  ;
$if      set continuous  acc_capexeurSTART(i)    =   acc_capexeurSTART_int("rec",i)  ;
$if      set continuous  acc_capexeurLAST(i)     =   acc_capexeurLAST_int("rec",i)   ;
$if      set continuous  acc_capexeurLO(i,ls)    =   acc_capexeurLO_int("rec",i,ls)  ;
$if      set continuous  acc_capexeurUP(i,ls)    =   acc_capexeurUP_int("rec",i,ls)  ;
$if      set continuous  slopeeur_mip(i,ls)  =   slopeeur_mip_int("rec",i,ls)    ;

$if      set discrete    b_qeur(i)   =   b_qeur_int("rec",i)     ;
$if      set discrete    qeurFIRST(i)    =   qeurFIRST_int("rec",i)  ;
$if      set discrete    qeurSTART(i)    =   qeurSTART_int("rec",i)  ;
$if      set discrete    qeurLAST(i)     =   qeurLAST_int("rec",i)   ;
$if      set discrete    qeurlsLO(i,ls)  =   qeurlsLO_int("rec",i,ls)    ;
$if      set discrete    qeurlsUP(i,ls)  =   qeurlsUP_int("rec",i,ls)    ;
$if      set discrete    capcosteur0(i)  =   capcosteur0_int("rec",i)    ;
$if      set discrete    acc_capexeurFIRST(i)    =   acc_capexeurFIRST_int("rec",i)  ;
$if      set discrete    acc_capexeurSTART(i)    =   acc_capexeurSTART_int("rec",i)  ;
$if      set discrete    acc_capexeurLAST(i)     =   acc_capexeurLAST_int("rec",i)   ;
$if      set discrete    acc_capexeurLO(i,ls)    =   acc_capexeurLO_int("rec",i,ls)  ;
$if      set discrete    acc_capexeurUP(i,ls)    =   acc_capexeurUP_int("rec",i,ls)  ;
$if      set discrete    slopeeur_mip(i,ls)  =   slopeeur_mip_int("rec",i,ls)    ;
  
* * * R&D: Learning-by-lbs
set
ki_del(i)         Technologies to exclude from the model
ki_lea(i)         Technologies to learn
kr_lea(r)         Regions to learn
kir_lea(i,r)      Technology region pair under learning
kls               Number of line segments
;

$gdxin database\setpar_%n%.gdx
$load ki_del, ki_lea, kr_lea, kls
$gdxin


parameter
kcapcost(i,v,r)
kls_weight(kls)          Weight to determine breakpoints
delta_k_pa               Annual depreciation factor of the experience stock
delta_k                  Periodical depreciation factor of the experience stock
;

$gdxin database\setpar_%n%.gdx
$load kls_weight,delta_k_pa, delta_k, kcapcost
$gdxin


* * * Regional Learning-by-lbs
parameter
b_k(i,r)                 Learning rate from capacity (Q) expansion by technology and region
kFIRST(i,r)              First capacity stock unit (GW)
kSTART(i,r)              Initial capacity stock (GW)
kLAST(i,r)               Maximum capacity stock (GW)
klsLO(i,r,kls)           Lower kink points of capacity stock (GW)
klsUP(i,r,kls)           Upper kink points of capacity stock (GW)
kcapcost0(i,r)           Cost of the first unit installed (EUR per kW)
kcapcostFIRST(i,r)       Cost of the first unit installed (EUR per kW)
kcapcostSTART(i,r)       Cost of the first unit to install in 2020
kcapcostLAST(i,r)        Cost of the last possible unit installed (EUR per kW)
kcapcostLO(i,r,kls)      Cost of the units at the breakpoints
kcapcostUP(i,r,kls)      Cost of the units at the breakpoints
kcapcostAV(i,r,kls)      Cost of the units at the breakpoints
kacc_capexFIRST(i,r)     First unit capacity stock accumulated cost (million)
kacc_capexSTART(i,r)     Initial capacity stock accumulated cost (million)
kacc_capexLAST(i,r)      Maximum capacity stock accumulated cost (million)
kacc_capexLO(i,r,kls)    Lower kink points of capacity stock accumulated cost (million)
kacc_capexUP(i,r,kls)    Upper kink points of capacity stock accumulated cost (million)
kslope_lin(i,r)          onstant slope of linear approximated function (EUR per kW)
kslope_mip(i,r,kls)      Slope of linear approximated function (EUR per kW)
ktest_slope(i,r,kls,*)   Difference to average (EUR per kW)
ktest_slope2(i,r,kls,*)  Average cost per line segment (EUR per kW)
rd_budget(i,r,t)         RD budget (million EUR)
kspillover(i,r,r)        Spillover from r to r
k_exo(i,r,v)
;

$gdxin database\setpar_%n%.gdx
$load b_k,kFIRST,kSTART,kLAST,klsLO,klsUP,kcapcost0,kcapcostFIRST,kcapcostSTART,kcapcostLAST,kcapcostLO,kcapcostUP,kcapcostAV,kacc_capexFIRST,kacc_capexSTART,kacc_capexLAST,kacc_capexLO,kacc_capexUP,kslope_lin,kslope_mip,ktest_slope,ktest_slope2,rd_budget
$load kspillover
$load k_exo
$gdxin

kir_lea(i,r) = YES$(kcapcost0(i,r) > 0) ;

$if      set lbs    capcost(i,v,r)$(ki_lea(i)) = kcapcost(i,v,r) ;
$if      set lbseur capcost(i,v,r)$(kir_lea(i,r)) = kcapcost(i,v,r) ;
$if      set lbsbenchmark   capcost(i,v,r)$(kir_lea(i,r)) = kcapcost(i,v,r) ;

parameter
tspillover(i,i)
;

tspillover("WindOn_q90","WindOff_q90") = 0 ;
tspillover("WindOff_q90","WindOn_q90") = 0 ;

$if      set fulltechspillover   tspillover("WindOn_q90","WindOff_q90") = 1 ;
$if      set fulltechspillover   tspillover("WindOff_q90","WindOn_q90") = 1 ;
$if      set halftechspillover   tspillover("WindOn_q90","WindOff_q90") = 0.5 ;
$if      set halftechspillover   tspillover("WindOff_q90","WindOn_q90") = 0.5 ;
$if      set quartechspillover   tspillover("WindOn_q90","WindOff_q90") = 0.25 ;
$if      set quartechspillover   tspillover("WindOff_q90","WindOn_q90") = 0.25 ;

parameter
kspillover_int(i,r,r)
;

kspillover_int(i,r,rr) = kspillover(i,r,rr) ;
$if      set spill2  kspillover(i,r,rr) = 2  * kspillover_int(i,r,rr) ;
$if      set spill5  kspillover(i,r,rr) = 5  * kspillover_int(i,r,rr) ;

* * * European Learning-by-lbs
parameter
b_keur(i)                Learning rate from capacity (Q) expansion by technology and region
keurFIRST(i)             First capacity stock unit (GW)
keurSTART(i)             Initial capacity stock (GW)
keurLAST(i)              Maximum capacity stock (GW)
keurlsLO(i,kls)          Lower kink points of capacity stock (GW)
keurlsUP(i,kls)          Upper kink points of capacity stock (GW)
kcapcosteur0(i)          Cost of the first unit installed (EUR per kW)
kcapcosteurFIRST(i)      Cost of the first unit installed (EUR per kW)
kcapcosteurSTART(i)      Cost of the first unit to install in 2020
kcapcosteurLAST(i)       Cost of the last possible unit installed (EUR per kW)
kcapcosteurLO(i,kls)     Cost of the units at the breakpoints
kcapcosteurUP(i,kls)     Cost of the units at the breakpoints
kcapcosteurAV(i,kls)     Cost of the units at the breakpoints
kacc_capexeurFIRST(i)    First unit capacity stock accumulated cost (million)
kacc_capexeurSTART(i)    Initial capacity stock accumulated cost (million)
kacc_capexeurLAST(i)     Maximum capacity stock accumulated cost (million)
kacc_capexeurLO(i,kls)   Lower kink points of capacity stock accumulated cost (million)
kacc_capexeurUP(i,kls)   Upper kink points of capacity stock accumulated cost (million)
kslopeeur_lin(i)         Constant slope of linear approximated function (EUR per kW)
kslopeeur_mip(i,kls)     Slope of linear approximated function (EUR per kW)
ktest_slopeeur(i,kls,*)  Difference to average (EUR per kW)
ktest_slopeeur2(i,kls,*) Average cost per line segment (EUR per kW)
rd_budgeteur(i,t)        RD budget (million EUR)
kcapcosteur(i,v)
;

$gdxin database\setpar_%n%.gdx
$load kcapcosteur, b_keur,keurFIRST,keurSTART,keurLAST,keurlsLO,keurlsUP,kcapcosteur0,kcapcosteurSTART,kcapcosteurLAST,kcapcosteurLO,kcapcosteurUP,kcapcosteurAV,kacc_capexeurFIRST,kacc_capexeurSTART,kacc_capexeurLAST,kacc_capexeurLO,kacc_capexeurUP,kslopeeur_lin,kslopeeur_mip,ktest_slopeeur,ktest_slopeeur2,rd_budgeteur
$gdxin

$if      set lbsnobenchmark capcost(i,v,r)$(kir_lea(i,r)) = kcapcosteur(i,v) ;

                 
* * * R&D budget allocation
equation
eq_lbs_rdbudget_irt(i,r,t) 
eq_lbs_rdbudget_rt(r,t)
eq_lbs_rdbudget_it(i,t)
eq_lbs_rdbudget_ir(i,r)
eq_lbs_rdbudget_t(t)
eq_lbs_rdbudget_r(r)
eq_lbs_rdbudget_i(i)
eq_lbs_rdbudget
eq_lbseur_rdbudget_it(i,t)
eq_lbseur_rdbudget_i(i)
eq_lbseur_rdbudget_t(t)
eq_lbseur_rdbudget
;

parameter
lbsrdbudget(i,r,v)
lbseurrdbudget(i,v)
lbsrdbudget_int(i,r,v)
lbseurrdbudget_int(i,v)
;

lbsrdbudget_int(i,r,v) = sum(tv(t,v), rd_budget(i,r,t)) ;
lbseurrdbudget_int(i,v) = sum(tv(t,v), rd_budgeteur(i,t)) ;

lbsrdbudget(i,r,v) = 1 * lbsrdbudget_int(i,r,v) ;
lbseurrdbudget(i,v) = 1 * lbseurrdbudget_int(i,v) ;

$if      set halfbudget      lbsrdbudget(i,r,v) = 0.5 * lbsrdbudget_int(i,r,v) ;
$if      set halfbudget      lbseurrdbudget(i,v) = 0.5 * lbseurrdbudget_int(i,v) ;

$if      set threeqbudget    lbsrdbudget(i,r,v) = 0.75 * lbsrdbudget_int(i,r,v) ;
$if      set threeqbudget    lbseurrdbudget(i,v) = 0.75 * lbseurrdbudget_int(i,v) ;

$if      set onefiftybudget  lbsrdbudget(i,r,v) = 1.5 * lbsrdbudget_int(i,r,v) ;
$if      set onefiftybudget  lbseurrdbudget(i,v) = 1.5 * lbseurrdbudget_int(i,v) ;

$if      set doublebudget    lbsrdbudget(i,r,v) = 2 * lbsrdbudget_int(i,r,v) ;
$if      set doublebudget    lbseurrdbudget(i,v) = 2 * lbseurrdbudget_int(i,v) ;

* * * FLH LBS
set
i_flh(i)
ir_flh(i,r)
flhls
;

$gdxin database\setpar_%n%.gdx
$load i_flh, ir_flh, flhls
$gdxin

* * Regional
parameter
bflh(i,r) learning rate (%)
flhstock(i,r,v) knowledge stock (million ?)
flhrdbudget(i,r,v) RD budget (million ?)
flhindex(i,r,v) Average FLH (index 2022 = 1)
flh(i,r,v) Average FLH (h per a)
;

$gdxin database\setpar_%n%.gdx
$load bflh,flhstock,flhrdbudget,flhindex,flh
$gdxin

parameter
$if not  set days   vrsc_nor(s,i,v,r)
$if      set days   vrsc_nor_d(sd,hd,i,v,r)
flh_check(i,v,r)
;

$if not  set days   vrsc_nor(s,i,v,r)$(ir_flh(i,r) and v.val ge 2023 and sum(ss, hours(ss) * vrsc(ss,i,v,r)) > 0) = vrsc(s,i,v,r) * sum(ss, hours(ss) * vrsc(ss,i,"2022",r)) / sum(ss, hours(ss) * vrsc(ss,i,v,r)) ;
$if      set days   vrsc_nor_d(sd,hd,i,v,r)$(ir_flh(i,r) and v.val ge 2023 and sum((sdd,hdd), days(sdd) * vrsc_d(sdd,hdd,i,v,r)) > 0) = vrsc_d(sd,hd,i,v,r) * sum((sdd,hdd), days(sdd) * vrsc_d(sdd,hdd,i,"2022",r)) / sum((sdd,hdd), days(sdd) * vrsc_d(sdd,hdd,i,v,r)) ;
$if not  set days   flh_check(i,v,r) = sum(s, hours(s) * vrsc_nor(s,i,v,r)) ;
$if      set days   flh_check(i,v,r) = sum((sd,hd), days(sd) * vrsc_nor_d(sd,hd,i,v,r)) ;

parameter
flhstockFIRST(i,r)
flhstockSTART(i,r)
flhstockLAST(i,r)
flhstockUP(i,r,flhls)
flhstockLO(i,r,flhls)
flhaccumFIRST(i,r)
flhaccumSTART(i,r)
flhaccumLAST(i,r)
flhaccumUP(i,r,flhls)
flhaccumLO(i,r,flhls)
flhindexFIRST(i,r)
flhindexSTART(i,r)
flhindexLAST(i,r)
flhindexUP(i,r,flhls)
flhindexLO(i,r,flhls)
flhindexSLOPE(i,r,flhls)
;

$gdxin database\setpar_%n%.gdx
$load flhstockFIRST
$load flhstockSTART
$load flhstockLAST
$load flhstockUP
$load flhstockLO
$load flhaccumFIRST
$load flhaccumSTART
$load flhaccumLAST
$load flhaccumUP
$load flhaccumLO
$load flhindexFIRST
$load flhindexSTART
$load flhindexLAST
$load flhindexUP
$load flhindexLO
$load flhindexSLOPE
$gdxin
               
* * European
parameter
bflheur(i) learning rate (%)
flheurindex_reg(i,r,v) regional correction (0 ... X)
flheurstock(i,v) knowledge stock (million ?)
flheurrdbudget(i,v) RD budget (million ?)
flheurindex(i,v) Average FLH (index 2022 = 1)
flheur(i,v) Average FLH (h per a)
;

$gdxin database\setpar_%n%.gdx
$load bflheur,flheurstock,flheurindex_reg,flheurrdbudget,flheurindex,flheur
$gdxin

parameter
$if not  set days   vrsceur_nor(s,i,v,r)
flheur_check(i,v,r)
;

$if not  set days   vrsceur_nor(s,i,v,r)$(ir_flh(i,r) and v.val ge 2023 and sum(ss, hours(ss) * vrsc(ss,i,v,r)) > 0) = vrsc(s,i,v,r) * flheur(i,"2022") / sum(ss, hours(ss) * vrsc(ss,i,v,r)) ;
$if not  set days   flheur_check(i,v,r) = sum(s, hours(s) * vrsceur_nor(s,i,v,r)) ;

parameter
flheurstockFIRST(i)
flheurstockSTART(i)
flheurstockLAST(i)
flheurstockUP(i,flhls)
flheurstockLO(i,flhls)
flheuraccumFIRST(i)
flheuraccumSTART(i)
flheuraccumLAST(i)
flheuraccumUP(i,flhls)
flheuraccumLO(i,flhls)
flheurindexFIRST(i)
flheurindexSTART(i)
flheurindexLAST(i)
flheurindexUP(i,flhls)
flheurindexLO(i,flhls)
flheurindexSLOPE(i,flhls)
;

$gdxin database\setpar_%n%.gdx
$load flheurstockFIRST
$load flheurstockSTART
$load flheurstockLAST
$load flheurstockUP
$load flheurstockLO
$load flheuraccumFIRST
$load flheuraccumSTART
$load flheuraccumLAST
$load flheuraccumUP
$load flheuraccumLO
$load flheurindexFIRST
$load flheurindexSTART
$load flheurindexLAST
$load flheurindexUP
$load flheurindexLO
$load flheurindexSLOPE
$gdxin

parameter
flhrdbudget_int(i,r,v)
flheurrdbudget_int(i,v)
;

flhrdbudget_int(i,r,v) = flhrdbudget(i,r,v) ;
flheurrdbudget_int(i,v) = flheurrdbudget(i,v) ;

$if      set halfbudget      flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 0.5 * flhrdbudget_int(i,r,v) ;
$if      set halfbudget      flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 0.5 * flheurrdbudget_int(i,v) ;

$if      set threeqbudget    flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 0.75 * flhrdbudget_int(i,r,v) ;
$if      set threeqbudget    flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 0.75 * flheurrdbudget_int(i,v) ;

$if      set onefiftybudget  flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 1.5 * flhrdbudget_int(i,r,v) ;
$if      set onefiftybudget  flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 1.5 * flheurrdbudget_int(i,v) ;

$if      set doublebudget    flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 2 * flhrdbudget_int(i,r,v) ;
$if      set doublebudget    flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 2 * flheurrdbudget_int(i,v) ;

set
ivt(i,v,t)              Active vintage-capacity blocks aggregated to European metric
ivttv(i,v,t)
ivrttv(i,v,r,t)
jvrttv(j,v,r,t)
tvrttv(k,v,r,t)
notir_lea(i,r)
notkir_lea(i,r)
;

parameter
deprtimeeur(i,v,t)      deprtime for European learning metric
endeffecteur(i,v,t)     endeffect for European learning metric
kendeffect(i,r,t)       endeffect for lbs
kendeffecteur(i,t)      endeffecteur for lbs
;

* ETC module
ivt(i,v,t) = YES$(ivrt(i,v,"Germany",t) and i_lea(i)) ;
ivttv(ivt(i,v,t))$(tv(t,v)) = YES ;
ivrttv(ivrt(i,v,r,t))$(tv(t,v)) = YES ;
jvrttv(jvrt(j,v,r,t))$(tv(t,v)) = YES ;
tvrttv(tvrt(k,v,r,t))$(tv(t,v)) = YES ;
notir_lea(i,r)$(not ir_lea(i,r)) = YES ;
notkir_lea(i,r)$(not kir_lea(i,r)) = YES ;

deprtimeeur(i,v,t)$(sum(r, 1$deprtime(i,v,r,t)) > 0) = sum(r, deprtime(i,v,r,t)) / sum(r, 1$deprtime(i,v,r,t)) ;
endeffecteur(i,v,t)$(sum(r, 1$endeffect(i,v,r,t)) > 0) = sum(r, endeffect(i,v,r,t)) / sum(r, 1$endeffect(i,v,r,t)) ;
kendeffect(i,r,t)$(kir_lea(i,r)) = sum(tv(t,v), endeffect(i,v,r,t)) ;
kendeffecteur(i,t)$(ki_lea(i) and sum(r, 1$kendeffect(i,r,t)) > 0) = sum(r, kendeffect(i,r,t)) / sum(r, 1$kendeffect(i,r,t)) ;

parameter
rshare(inv,i)
rzeta(inv,i,v)
;

rzeta(inv,i,v) = sum(r, share(inv,i,r) * zeta(inv,i,v,r) * daref(r,"2022")) / sum(r, daref(r,"2022")) ;
rshare(inv,i)  = sum(r, share(inv,i,r) * daref(r,"2022")) / sum(r, daref(r,"2022")) ;

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

* * * Targets equations
* French nuclear
parameter
frnuc_gen_share(r,t)
;

frnuc_gen_share(r,t) = 0 ;
$if      set frnuc10     frnuc_gen_share("France",t) = 0.1 ;
$if      set frnuc20     frnuc_gen_share("France",t) = 0.2 ;
$if      set frnuc30     frnuc_gen_share("France",t) = 0.3 ;
$if      set frnuc40     frnuc_gen_share("France",t) = 0.4 ;
$if      set frnuc50     frnuc_gen_share("France",t) = 0.5 ;
$if      set frnuc60     frnuc_gen_share("France",t) = 0.6 ;
$if      set frnuc70     frnuc_gen_share("France",t) = 0.7 ;
$if      set frnuc80     frnuc_gen_share("France",t) = 0.8 ;
$if      set frnuc90     frnuc_gen_share("France",t) = 0.9 ;
$if      set frnuc100    frnuc_gen_share("France",t) = 1.0 ;

* Renewable targets
set
techtarget
maptechtarget(techtarget,i)
mapsuperirnw(techtarget,superirnw)
;

*$onecho >temp\gdxxrw.rsp
*set=techtarget     rng=techtarget!a1    rdim=1 cdim=0 values=nodata
*set=maptechtarget  rng=maptechtarget!a1 rdim=2 cdim=0 values=nodata
*set=mapsuperirnw   rng=mapsuperirnw!a1  rdim=2 cdim=0 values=nodata
*$offecho

*$call 'gdxxrw i=restarget\restarget_%hor%.xlsx o=restarget\set_restarget_%hor%.gdx trace=3 log=temp\set_restarget_%hor%.log @temp\gdxxrw.rsp';

$gdxin restarget\set_restarget_%hor%
$load techtarget
$load maptechtarget
$load mapsuperirnw
$gdxIn

parameter
sha_gen_constant(r,t)
sha_gen_extra(r,t)
sha_con_constant(r,t)
sha_con_extra(r,t)
sha_gen_constant_int(r,t)
sha_gen_extra_int(r,t)
sha_con_constant_int(r,t)
sha_con_extra_int(r,t)
tot_con_constant(r,t)
tot_con_extra(r,t)
cap_constant(r,techtarget,t)
cap_constant_int(r,techtarget,t)
cap_extra(r,techtarget,t)
cap_extra_int(r,techtarget,t)
eng_constant(r,techtarget,t)
eng_constant_int(r,techtarget,t)
eng_extra(r,techtarget,t)
eng_extra_int(r,techtarget,t)
cap_constant_diff(r,techtarget)
cap_extra_diff(r,techtarget)
;

*$onecho >temp\gdxxrw.rsp
*par=sha_gen_constant     rng=sha_gen_constant!a1    rdim=1 cdim=1
*par=sha_gen_extra        rng=sha_gen_extra!a1       rdim=1 cdim=1
*par=sha_con_constant     rng=sha_con_constant!a1    rdim=1 cdim=1
*par=sha_con_extra        rng=sha_con_extra!a1       rdim=1 cdim=1
*par=cap_constant         rng=cap_constant!a1        rdim=2 cdim=1
*par=cap_extra            rng=cap_extra!a1           rdim=2 cdim=1
*par=eng_constant         rng=eng_constant!a1        rdim=2 cdim=1
*par=eng_extra            rng=eng_extra!a1           rdim=2 cdim=1
*$offecho

*$call 'gdxxrw i=restarget\restarget_%hor%.xlsx o=restarget\par_restarget_%hor%.gdx trace=3 log=temp\par_restarget_%hor%.log @temp\gdxxrw.rsp';

$gdxin restarget\par_restarget_%hor%
$load sha_gen_constant_int=sha_gen_constant
$load sha_gen_extra_int=sha_gen_extra
$load sha_con_constant_int=sha_con_constant
$load sha_con_extra_int=sha_con_extra
$load cap_constant_int=cap_constant
$load cap_extra_int=cap_extra
$load eng_constant_int=eng_constant
$load eng_extra_int=eng_extra
$gdxin

sha_gen_constant(r,t) = round(sha_gen_constant_int(r,t),4) ;
sha_gen_extra(r,t) = round(sha_gen_extra_int(r,t),4) ;
sha_con_constant(r,t) = round(sha_con_constant_int(r,t),4) ;
sha_con_extra(r,t)= round(sha_con_extra_int(r,t),4) ;

cap_constant(r,techtarget,t) = round(cap_constant_int(r,techtarget,t),4) ;
cap_extra(r,techtarget,t) = round(cap_extra_int(r,techtarget,t),4) ;
eng_constant(r,techtarget,t) = round(eng_constant_int(r,techtarget,t),4) ;
eng_extra(r,techtarget,t)= round(eng_extra_int(r,techtarget,t),4) ;

* Translate energy targets into capacity ones when merging irnw
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOn") and irnwflh_exi_h("WindOn_exi",r,t) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOn_exi",r,t)) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOff") and irnwflh_exi_h("WindOff_exi",r,t) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOff_exi",r,t)) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"SolarPV") and irnwflh_exi_h("OpenPV_exi",r,t) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / irnwflh_exi_h("OpenPV_exi",r,t)) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOn") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOn_q90",v,r))) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOff") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOff_q90",v,r))) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"SolarPV") and sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("OpenPV_q90",v,r))) ;

$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOn") and irnwflh_exi_h("WindOn_exi",r,t) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOn_exi",r,t)) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOff") and irnwflh_exi_h("WindOff_exi",r,t) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOff_exi",r,t)) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"SolarPV") and irnwflh_exi_h("OpenPV_exi",r,t) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / irnwflh_exi_h("OpenPV_exi",r,t)) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOn") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOn_q90",v,r))) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOff") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOff_q90",v,r))) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"SolarPV") and sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("OpenPV_q90",v,r))) ;

* Delay will not get compensated for
cap_constant_diff(r,techtarget) = max(0,cap_constant_int(r,techtarget,"2023") - sum(maptechtarget(techtarget,i), sum(oldv(v), capt(i,v,r,"2023")))) ;
cap_extra_diff(r,techtarget) = max(0,cap_extra_int(r,techtarget,"2023") - sum(maptechtarget(techtarget,i), sum(oldv(v), capt(i,v,r,"2023")))) ;

$if      set frnuctarget   $if      set constanttargets        sha_gen_constant("France",t)$(t.val ge 2035 and sha_gen_constant("France",t) + frnuc_gen_share("France",t) ge 1) = 1 - frnuc_gen_share("France",t) ;
$if      set frnuctarget   $if      set extratargets           sha_gen_extra("France",t)$(t.val ge 2035 and sha_gen_extra("France",t) + frnuc_gen_share("France",t) ge 1) = 1 - frnuc_gen_share("France",t) ;

tot_con_constant(r,t)$(sha_con_constant(r,t) > 0) = round(sha_con_constant(r,t) * daref(r,t), 4) ;
tot_con_extra(r,t)$(sha_con_extra(r,t) > 0)       = round(sha_con_extra(r,t) * daref(r,t), 4) ;

* Set generation targets equal to consumption targets (plus distribution losses) as approximation to simplify model
$if      set gentargetsdemand    tot_con_constant(r,t)$(sha_gen_constant(r,t) > 0) = round(sha_gen_constant(r,t) * daref(r,t) * (1 + lossave(r,t)),4) ;
$if      set gentargetsdemand    tot_con_extra(r,t)$(sha_gen_extra(r,t) > 0)       = round(sha_gen_extra(r,t) * daref(r,t) * (1 + lossave(r,t)),4) ;

* Correct capacity targets in line with resource potential (important for the simpmip metric with reduced potentials)
cap_constant(r,techtarget,t)$(t.val ge 2024 and cap_constant_int(r,techtarget,t) > 0 and (sameas(techtarget,"SolarPV") or sameas(techtarget,"WindOn") or sameas(techtarget,"WindOff")))
                  = min(round(cap_constant_int(r,techtarget,t)-cap_constant_diff(r,techtarget),4), sum(mapsuperirnw(techtarget,superirnw), sum(superirnw_mapq(i,quantiles,superirnw)$(not sameas(i,"qexi")), irnwlimUP_quantiles(i,r,quantiles)))) ;
cap_extra(r,techtarget,t)$(t.val ge 2024 and cap_extra_int(r,techtarget,t) > 0 and (sameas(techtarget,"SolarPV") or sameas(techtarget,"WindOn") or sameas(techtarget,"WindOff")))
                  = min(round(cap_extra_int(r,techtarget,t)-cap_extra_diff(r,techtarget),4), sum(mapsuperirnw(techtarget,superirnw), sum(superirnw_mapq(i,quantiles,superirnw)$(not sameas(i,"qexi")), irnwlimUP_quantiles(i,r,quantiles)))) ;

parameter
cap_constantann(r,techtarget,t)
cap_extraann(r,techtarget,t)
;

cap_constantann(r,techtarget,t)$(t.val ge 2024) = max(0,cap_constant(r,techtarget,t) - cap_constant(r,techtarget,t-1)) ;
cap_extraann(r,techtarget,t) = max(0,cap_extra(r,techtarget,t) - cap_extra(r,techtarget,t-1)) ;
                  
$if      set hydroexp  invlimUP("Hydro",r,t)$(toptimize(t))      = cap_extra(r,"Water",t) - cap_extra(r,"Water",t-1) ;
