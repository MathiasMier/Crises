* * * Electricity reporting
$ontext
set
sea                    Seasons of the year /w, s, m, strm, stra/
ssea(s,sea)            Map between segment and season
ssea(sd,sea)            Map between segment and season
;

* Generate map between segments and seasons
ssea(s,"w")$(sm(s,"1") or sm(s,"2") or sm(s,"12")) = YES ;
ssea(s,"s")$(sm(s,"6") or sm(s,"7") or sm(s,"8"))  = YES ;
ssea(s,"m")$(not ssea(s,"s") and not ssea(s,"w"))  = YES ;
ssea(s,"strm")$(sm(s,"1") or sm(s,"2") or sm(s,"3"))                = YES ;
ssea(s,"stra")$(sm(s,"1") or sm(s,"2") or sm(s,"3") or sm(s,"4"))   = YES ;

ssea(sd,"w")$(sdm(sd,"1") or sdm(sd,"2") or sdm(sd,"12")) = YES ;
ssea(sd,"s")$(sdm(sd,"6") or sdm(sd,"7") or sdm(sd,"8"))  = YES ;
ssea(sd,"m")$(not ssea(sd,"s") and not ssea(sd,"w"))  = YES ;
ssea(sd,"strm")$(sdm(sd,"1") or sdm(sd,"2") or sdm(sd,"3"))                = YES ;
ssea(sd,"stra")$(sdm(sd,"1") or sdm(sd,"2") or sdm(sd,"3") or sdm(sd,"4"))   = YES ;
$offtext

parameter
$if      set days   price_d(sd,hd,r,t)                     Electricity price (EUR per MWh)
$if not  set days   price(s,r,t)                      Electricity price (EUR per MWh)
Electricity1_rpt(t,r,*)           Electricity reporting vector (EUR per MWh and TWh)
Electricity1_total_rpt(t,*)       Electricity reporting vector (EUR per MWh and TWh)
Electricity1_GER_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
Electricity1_FR_rpt(t,*)          Electricity reporting vector (EUR per MWh and TWh)
Electricity2_rpt(t,r,*)           Electricity reporting vector (EUR per MWh and TWh)
Electricity2_total_rpt(t,*)       Electricity reporting vector (EUR per MWh and TWh)
Electricity2_GER_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
Electricity2_FR_rpt(t,*)          Electricity reporting vector (EUR per MWh and TWh)
Electricity3_rpt(t,r,*)           Electricity reporting vector (EUR per MWh and TWh)
Electricity3_total_rpt(t,*)       Electricity reporting vector (EUR per MWh and TWh)
Electricity3_GER_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
Electricity3_FR_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
discostco2(i,v,r,t)
;

$if not  set static discostco2(ivrt(i,v,r,t)) = discost(i,v,r,t) + emit(i,v,r) * co2prreg(r,t) ;
$if      set static discostco2(ivrt(i,v,r,t)) = sum(s, discost_hours(s,i,v,r,t)) / 8760 + emit(i,v,r) * co2price_model_ave(r,t) ;

$if not  set days   price(s,r,t)        = (demand.M(s,r,t) 
$if      set days   price_d(sd,hd,r,t)  = (demand_d.M(sd,hd,r,t)
                 ) / dfact(t) ;              
Parameter
darefcal(t,r)
dareforg(t,r)
bscal(t,r)
profitload(t,r)
profit(t,r,i)
vcost(t,r,i)
fcost(t,r,i)
darefres(t,r)
genera(t,r,i)

darefcal_total(t)
dareforg_total(t)
bscal_total(t)
profitload_total(t)
profit_total(t,i)
vcost_total(t,i)
fcost_total(t,i)
darefres_total(t)
genera_total(t,i)
;

$if not  set days   $if not  set elastic darefcal(t,r) = sum(s, load_res(s,r,t) * hours(s)) * 1e-3 ;
$if not  set days   $if      set elastic darefcal(t,r) = sum(s, sum(bse, DS.L(bse,s,r,t)) * hours(s)) ;
$if not  set days   dareforg(t,r) = sum(s, load_res(s,r,t) * hours(s)) * 1e-3 ;
$if not  set days   $if not  set elastic bscal(t,r) = sum(s, hours(s) * BS.L(s,r,t)) * 1e-3 ;
$if not  set days   $if      set elastic bscal(t,r) = sum(s, hours(s) * sum(bse, BSELAS.L(bse,s,r,t))) * 1e-3 ;
$if not  set days   $if not  set elastic profitload(t,r) = sum(s, price(s,r,t) * load_res(s,r,t) * hours(s)) * 1e-3 ;
$if not  set days   $if      set elastic profitload(t,r) = sum(s, price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3 ;
$if not  set days   profit(t,r,i) = sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)) * 1e-3 ;
$if not  set days   vcost(t,r,i) = sum(v, discostco2(i,v,r,t) * XTWHL(i,v,r,t)) ;
$if not  set days   fcost(t,r,i) = sum(v, XC.L(i,v,r,t) *  fomcost(i,v,r)) ;
$if not  set days   darefres(t,r) = sum(s, load_res(s,r,t) * hours(s)) * 1e-3 ;

$if      set days   $if not  set elastic darefcal(t,r) = sum((sd,hd), load_res_D(sd,hd,r,t) * days(sd)) * 1e-3 ;
$if      set days   $if      set elastic darefcal(t,r) = sum((sd,hd), sum(bse, DS_D.L(bse,s,r,t)) * days(sd)) ;
$if      set days   dareforg(t,r) = sum((sd,hd), load_res_D(sd,hd,r,t) * days(sd)) * 1e-3 ;
$if      set days   $if not  set elastic bscal(t,r) = sum((sd,hd), days(sd) * BS_D.L(sd,hd,r,t)) * 1e-3 ;
$if      set days   $if      set elastic bscal(t,r) = sum((sd,hd), days(sd) * sum(bse, BSELAS_D.L(bse,s,r,t))) * 1e-3 ;
$if      set days   $if not  set elastic profitload(t,r) = sum((sd,hd), price_D(sd,hd,r,t) * load_res_D(sd,hd,r,t) * days(sd)) * 1e-3 ;
$if      set days   $if      set elastic profitload(t,r) = sum((sd,hd), price_D(sd,hd,r,t) * sum(bse, DS_D.L(bse,s,r,t)) * days(sd)) * 1e-3 ;
$if      set days   profit(t,r,i) = sum((sd,hd), price_D(sd,hd,r,t) * sum(v, XL_D(sd,hd,i,v,r,t)) * days(sd)) * 1e-3 ;
$if      set days   vcost(t,r,i) = sum(v, discostco2(i,v,r,t) * XTWHL(i,v,r,t)) ;
$if      set days   fcost(t,r,i) = sum(v, XC.L(i,v,r,t) *  fomcost(i,v,r)) ;
$if      set days   darefres(t,r) = sum((sd,hd), load_res_D(sd,hd,r,t) * days(sd)) * 1e-3 ;

genera(t,r,i) = sum(v, XTWHL(i,v,r,t)) * 1e-3 ;

darefcal_total(t) = sum(r, darefcal(t,r)) ;
dareforg_total(t) = sum(r, dareforg(t,r)) ;
bscal_total(t) = sum(r, bscal(t,r)) ;
profitload_total(t) = sum(r, profitload(t,r)) ;
profit_total(t,i) = sum(r, profit(t,r,i)) ;
vcost_total(t,i) = sum(r, vcost(t,r,i)) ;
fcost_total(t,i) = sum(r, fcost(t,r,i)) ; 
darefres_total(t) = sum(r, darefres(t,r)) ;
genera_total(t,i) = sum(r, genera(t,r,i)) ;

Electricity1_rpt(t,r,"price-avg")$(darefcal(t,r) > 0) = profitload(t,r) / darefcal(t,r) + eps ;
$if not  set days   Electricity1_rpt(t,r,"price-max")                = smax(s, price(s,r,t)) + eps ;
$if not  set days   Electricity1_rpt(t,r,"price-min")                = smin(s, price(s,r,t)) + eps ;
$if      set days   Electricity1_rpt(t,r,"price-max")                = smax((sd,hd), price_d(sd,hd,r,t)) + eps ;
$if      set days   Electricity1_rpt(t,r,"price-min")                = smin((sd,hd), price_d(sd,hd,r,t)) + eps ;

Electricity1_rpt(t,r,"elec-demand")              = darefcal(t,r) + eps ;
Electricity1_rpt(t,r,"elec-demand-org")          = dareforg(t,r) + eps ;         

$ontext
Electricity2_rpt(t,r,"price-nuc")$(sum(nuc(i), genera(t,r,i)) > 0) = sum(nuc(i), profit(t,r,i)) / sum(nuc(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-sol")$(sum(sol(i), genera(t,r,i)) > 0) = sum(sol(i), profit(t,r,i)) / sum(sol(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-wind")$(sum(wind(i), genera(t,r,i)) > 0) = sum(wind(i), profit(t,r,i)) / sum(wind(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-windon")$(sum(windon(i), genera(t,r,i)) > 0) = sum(windon(i), profit(t,r,i)) / sum(windon(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-windoff")$(sum(windoff(i), genera(t,r,i)) > 0) = sum(windoff(i), profit(t,r,i)) / sum(windoff(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-bio")$(sum(bio(i), genera(t,r,i)) > 0) = sum(bio(i), profit(t,r,i)) / sum(bio(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-gas")$(sum(gas(i), genera(t,r,i)) > 0) = sum(gas(i), profit(t,r,i)) / sum(gas(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-coa")$(sum(coa(i), genera(t,r,i)) > 0) = sum(coa(i), profit(t,r,i)) / sum(coa(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-lig")$(sum(lig(i), genera(t,r,i)) > 0) = sum(lig(i), profit(t,r,i)) / sum(lig(i), genera(t,r,i)) + eps ;
Electricity2_rpt(t,r,"price-ccs")$(sum(ccs(i), genera(t,r,i)) > 0) = sum(ccs(i), profit(t,r,i)) / sum(ccs(i), genera(t,r,i)) + eps ;

Electricity3_rpt(t,r,"cost-nuc")$(sum(nuc(i), genera(t,r,i)) > 0) = sum(nuc(i), vcost(t,r,i) + fcost(t,r,i)) / sum(nuc(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-sol")$(sum(sol(i), genera(t,r,i)) > 0) = sum(sol(i), vcost(t,r,i) + fcost(t,r,i)) / sum(sol(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-wind")$(sum(wind(i), genera(t,r,i)) > 0) = sum(wind(i), vcost(t,r,i) + fcost(t,r,i)) / sum(wind(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-windon")$(sum(windon(i), genera(t,r,i)) > 0) = sum(windon(i), vcost(t,r,i) + fcost(t,r,i)) / sum(windon(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-windoff")$(sum(windoff(i), genera(t,r,i)) > 0) = sum(windoff(i), vcost(t,r,i) + fcost(t,r,i)) / sum(windoff(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-bio")$(sum(bio(i), genera(t,r,i)) > 0) = sum(bio(i), vcost(t,r,i) + fcost(t,r,i)) / sum(bio(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-gas")$(sum(gas(i), genera(t,r,i)) > 0) = sum(gas(i), vcost(t,r,i) + fcost(t,r,i)) / sum(gas(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-coa")$(sum(coa(i), genera(t,r,i)) > 0) = sum(coa(i), vcost(t,r,i) + fcost(t,r,i)) / sum(coa(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-lig")$(sum(lig(i), genera(t,r,i)) > 0) = sum(lig(i), vcost(t,r,i) + fcost(t,r,i)) / sum(lig(i), genera(t,r,i)) + eps ;
Electricity3_rpt(t,r,"cost-ccs")$(sum(ccs(i), genera(t,r,i)) > 0) = sum(ccs(i), vcost(t,r,i) + fcost(t,r,i)) / sum(ccs(i), genera(t,r,i)) + eps ;
$offText

Electricity1_total_rpt(t,"price-avg")$(sum(r, darefcal_total(t) > 0)) = profitload_total(t) / darefcal_total(t) + eps ;
$if not  set days   Electricity1_total_rpt(t,"price-max")                = smax(r, smax(s, price(s,r,t))) + eps ;
$if not  set days   Electricity1_total_rpt(t,"price-min")                = smin(r, smin(s, price(s,r,t))) + eps ;
$if      set days   Electricity1_total_rpt(t,"price-max")                = smax(r, smax((sd,hd), price_d(sd,hd,r,t))) + eps ;
$if      set days   Electricity1_total_rpt(t,"price-min")                = smin(r, smin((sd,hd), price_d(sd,hd,r,t))) + eps ;
Electricity1_total_rpt(t,"elec-demand")              = darefcal_total(t) + eps ;
Electricity1_total_rpt(t,"elec-demand-org")          = dareforg_total(t) + eps ;         

$ontext
Electricity2_total_rpt(t,"price-nuc")$(sum(nuc(i), genera_total(t,i)) > 0) = sum(nuc(i), profit_total(t,i)) / sum(nuc(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-sol")$(sum(sol(i), genera_total(t,i)) > 0) = sum(sol(i), profit_total(t,i)) / sum(sol(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-wind")$(sum(wind(i), genera_total(t,i)) > 0) = sum(wind(i), profit_total(t,i)) / sum(wind(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-windon")$(sum(windon(i), genera_total(t,i)) > 0) = sum(windon(i), profit_total(t,i)) / sum(windon(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-windoff")$(sum(windoff(i), genera_total(t,i)) > 0) = sum(windoff(i), profit_total(t,i)) / sum(windoff(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-bio")$(sum(bio(i), genera_total(t,i)) > 0) = sum(bio(i), profit_total(t,i)) / sum(bio(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-gas")$(sum(gas(i), genera_total(t,i)) > 0) = sum(gas(i), profit_total(t,i)) / sum(gas(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-coa")$(sum(coa(i), genera_total(t,i)) > 0) = sum(coa(i), profit_total(t,i)) / sum(coa(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-lig")$(sum(lig(i), genera_total(t,i)) > 0) = sum(lig(i), profit_total(t,i)) / sum(lig(i), genera_total(t,i)) + eps ;
Electricity2_total_rpt(t,"price-ccs")$(sum(ccs(i), genera_total(t,i)) > 0) = sum(ccs(i), profit_total(t,i)) / sum(ccs(i), genera_total(t,i)) + eps ;

Electricity3_total_rpt(t,"cost-nuc")$(sum(nuc(i), genera_total(t,i)) > 0) = sum(nuc(i), vcost_total(t,i) + fcost_total(t,i)) / sum(nuc(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-sol")$(sum(sol(i), genera_total(t,i)) > 0) = sum(sol(i), vcost_total(t,i) + fcost_total(t,i)) / sum(sol(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-wind")$(sum(wind(i), genera_total(t,i)) > 0) = sum(wind(i), vcost_total(t,i) + fcost_total(t,i)) / sum(wind(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-windon")$(sum(windon(i), genera_total(t,i)) > 0) = sum(windon(i), vcost_total(t,i) + fcost_total(t,i)) / sum(windon(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-windoff")$(sum(windoff(i), genera_total(t,i)) > 0) = sum(windoff(i), vcost_total(t,i) + fcost_total(t,i)) / sum(windoff(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-bio")$(sum(bio(i), genera_total(t,i)) > 0) = sum(bio(i), vcost_total(t,i) + fcost_total(t,i)) / sum(bio(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-gas")$(sum(gas(i), genera_total(t,i)) > 0) = sum(gas(i), vcost_total(t,i) + fcost_total(t,i)) / sum(gas(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-coa")$(sum(coa(i), genera_total(t,i)) > 0) = sum(coa(i), vcost_total(t,i) + fcost_total(t,i)) / sum(coa(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-lig")$(sum(lig(i), genera_total(t,i)) > 0) = sum(lig(i), vcost_total(t,i) + fcost_total(t,i)) / sum(lig(i), genera_total(t,i)) + eps ;
Electricity3_total_rpt(t,"cost-ccs")$(sum(ccs(i), genera_total(t,i)) > 0) = sum(ccs(i), vcost_total(t,i) + fcost_total(t,i)) / sum(ccs(i), genera_total(t,i)) + eps ;                                        
$offtext

$if set profits     $include    modules_rpt\euregen2024_profits_rpt_v1                                

$ontext
$if not  set static     Electricity2_total_rpt("2020","price-nuc") = eps ;
$if not  set static     Electricity3_total_rpt("2020","cost-nuc") = eps ;
$if not  set static     Electricity2_total_rpt("2021","price-nuc") = eps ;
$if not  set static     Electricity3_total_rpt("2021","cost-nuc") = eps ;
$offtext                   

Electricity1_GER_rpt(t,"price-avg") = Electricity1_rpt(t,"Germany","price-avg") + eps ;
Electricity1_GER_rpt(t,"price-max") = Electricity1_rpt(t,"Germany","price-max") + eps ;
Electricity1_GER_rpt(t,"price-min") = Electricity1_rpt(t,"Germany","price-min") + eps ;
Electricity1_GER_rpt(t,"elec-demand") = Electricity1_rpt(t,"Germany","elec-demand") + eps ;
Electricity1_GER_rpt(t,"elec-demand-org") = Electricity1_rpt(t,"Germany","elec-demand-org") + eps ;

$ontext
Electricity2_GER_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Germany","price-nuc") + eps ;
Electricity2_GER_rpt(t,"price-sol")              = Electricity2_rpt(t,"Germany","price-sol") + eps ;
Electricity2_GER_rpt(t,"price-wind")             = Electricity2_rpt(t,"Germany","price-wind") + eps ;
Electricity2_GER_rpt(t,"price-windon")           = Electricity2_rpt(t,"Germany","price-windon") + eps ;
Electricity2_GER_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Germany","price-windoff") + eps ;
Electricity2_GER_rpt(t,"price-bio")              = Electricity2_rpt(t,"Germany","price-bio") + eps ;
Electricity2_GER_rpt(t,"price-gas")              = Electricity2_rpt(t,"Germany","price-gas") + eps ;
Electricity2_GER_rpt(t,"price-coa")              = Electricity2_rpt(t,"Germany","price-coa") + eps ;
Electricity2_GER_rpt(t,"price-lig")              = Electricity2_rpt(t,"Germany","price-lig") + eps ;
Electricity2_GER_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Germany","price-ccs") + eps ;

Electricity3_GER_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Germany","cost-nuc") + eps ;
Electricity3_GER_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Germany","cost-sol") + eps ;
Electricity3_GER_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Germany","cost-wind") + eps ;
Electricity3_GER_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Germany","cost-windon") + eps ;
Electricity3_GER_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Germany","cost-windoff") + eps ;
Electricity3_GER_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Germany","cost-bio") + eps ;
Electricity3_GER_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Germany","cost-gas") + eps ;
Electricity3_GER_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Germany","cost-coa") + eps ;
Electricity3_GER_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Germany","cost-lig") + eps ;
Electricity3_GER_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Germany","cost-ccs") + eps ;
$offtext

Electricity1_FR_rpt(t,"price-avg") = Electricity1_rpt(t,"France","price-avg") + eps ;
Electricity1_FR_rpt(t,"price-max") = Electricity1_rpt(t,"France","price-max") + eps ;
Electricity1_FR_rpt(t,"price-min") = Electricity1_rpt(t,"France","price-min") + eps ;
Electricity1_FR_rpt(t,"elec-demand") = Electricity1_rpt(t,"France","elec-demand") + eps ;
Electricity1_FR_rpt(t,"elec-demand-org") = Electricity1_rpt(t,"France","elec-demand-org") + eps ;

$ontext
Electricity2_FR_rpt(t,"price-nuc")              = Electricity2_rpt(t,"France","price-nuc") + eps ;
Electricity2_FR_rpt(t,"price-sol")              = Electricity2_rpt(t,"France","price-sol") + eps ;
Electricity2_FR_rpt(t,"price-wind")             = Electricity2_rpt(t,"France","price-wind") + eps ;
Electricity2_FR_rpt(t,"price-windon")           = Electricity2_rpt(t,"France","price-windon") + eps ;
Electricity2_FR_rpt(t,"price-windoff")          = Electricity2_rpt(t,"France","price-windoff") + eps ;
Electricity2_FR_rpt(t,"price-bio")              = Electricity2_rpt(t,"France","price-bio") + eps ;
Electricity2_FR_rpt(t,"price-gas")              = Electricity2_rpt(t,"France","price-gas") + eps ;
Electricity2_FR_rpt(t,"price-coa")              = Electricity2_rpt(t,"France","price-coa") + eps ;
Electricity2_FR_rpt(t,"price-lig")              = Electricity2_rpt(t,"France","price-lig") + eps ;
Electricity2_FR_rpt(t,"price-ccs")              = Electricity2_rpt(t,"France","price-ccs") + eps ;

Electricity3_FR_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"France","cost-nuc") + eps ;
Electricity3_FR_rpt(t,"cost-sol")              = Electricity3_rpt(t,"France","cost-sol") + eps ;
Electricity3_FR_rpt(t,"cost-wind")             = Electricity3_rpt(t,"France","cost-wind") + eps ;
Electricity3_FR_rpt(t,"cost-windon")           = Electricity3_rpt(t,"France","cost-windon") + eps ;
Electricity3_FR_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"France","cost-windoff") + eps ;
Electricity3_FR_rpt(t,"cost-bio")              = Electricity3_rpt(t,"France","cost-bio") + eps ;
Electricity3_FR_rpt(t,"cost-gas")              = Electricity3_rpt(t,"France","cost-gas") + eps ;
Electricity3_FR_rpt(t,"cost-coa")              = Electricity3_rpt(t,"France","cost-coa") + eps ;
Electricity3_FR_rpt(t,"cost-lig")              = Electricity3_rpt(t,"France","cost-lig") + eps ;
Electricity3_FR_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"France","cost-ccs") + eps ;
$offtext

$ontext
parameter
Electricity1_UK_rpt(t,*)
Electricity2_UK_rpt(t,*)
Electricity3_UK_rpt(t,*)
;

Electricity1_UK_rpt(t,"price-avg")              = sum(r$sameas(r,"Britain"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
Electricity1_UK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
Electricity1_UK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Britain"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_UK_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Britain","price-nuc") + eps ;
Electricity2_UK_rpt(t,"price-sol")              = Electricity2_rpt(t,"Britain","price-sol") + eps ;
Electricity2_UK_rpt(t,"price-wind")             = Electricity2_rpt(t,"Britain","price-wind") + eps ;
Electricity2_UK_rpt(t,"price-windon")           = Electricity2_rpt(t,"Britain","price-windon") + eps ;
Electricity2_UK_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Britain","price-windoff") + eps ;
Electricity2_UK_rpt(t,"price-bio")              = Electricity2_rpt(t,"Britain","price-bio") + eps ;
Electricity2_UK_rpt(t,"price-gas")              = Electricity2_rpt(t,"Britain","price-gas") + eps ;
Electricity2_UK_rpt(t,"price-coa")              = Electricity2_rpt(t,"Britain","price-coa") + eps ;
Electricity2_UK_rpt(t,"price-lig")              = Electricity2_rpt(t,"Britain","price-lig") + eps ;
Electricity2_UK_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Britain","price-ccs") + eps ;

Electricity3_UK_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Britain","cost-nuc") + eps ;
Electricity3_UK_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Britain","cost-sol") + eps ;
Electricity3_UK_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Britain","cost-wind") + eps ;
Electricity3_UK_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Britain","cost-windon") + eps ;
Electricity3_UK_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Britain","cost-windoff") + eps ;
Electricity3_UK_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Britain","cost-bio") + eps ;
Electricity3_UK_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Britain","cost-gas") + eps ;
Electricity3_UK_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Britain","cost-coa") + eps ;
Electricity3_UK_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Britain","cost-lig") + eps ;
Electricity3_UK_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Britain","cost-ccs") + eps ;

parameter
Electricity1_DK_rpt(t,*)
Electricity2_DK_rpt(t,*)
Electricity3_DK_rpt(t,*)
;

Electricity1_DK_rpt(t,"price-avg")              = sum(r$sameas(r,"Denmark"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
Electricity1_DK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
Electricity1_DK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Denmark"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_DK_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Denmark","price-nuc") + eps ;
Electricity2_DK_rpt(t,"price-sol")              = Electricity2_rpt(t,"Denmark","price-sol") + eps ;
Electricity2_DK_rpt(t,"price-wind")             = Electricity2_rpt(t,"Denmark","price-wind") + eps ;
Electricity2_DK_rpt(t,"price-windon")           = Electricity2_rpt(t,"Denmark","price-windon") + eps ;
Electricity2_DK_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Denmark","price-windoff") + eps ;
Electricity2_DK_rpt(t,"price-bio")              = Electricity2_rpt(t,"Denmark","price-bio") + eps ;
Electricity2_DK_rpt(t,"price-gas")              = Electricity2_rpt(t,"Denmark","price-gas") + eps ;
Electricity2_DK_rpt(t,"price-coa")              = Electricity2_rpt(t,"Denmark","price-coa") + eps ;
Electricity2_DK_rpt(t,"price-lig")              = Electricity2_rpt(t,"Denmark","price-lig") + eps ;
Electricity2_DK_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Denmark","price-ccs") + eps ;

Electricity3_DK_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Denmark","cost-nuc") + eps ;
Electricity3_DK_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Denmark","cost-sol") + eps ;
Electricity3_DK_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Denmark","cost-wind") + eps ;
Electricity3_DK_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Denmark","cost-windon") + eps ;
Electricity3_DK_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Denmark","cost-windoff") + eps ;
Electricity3_DK_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Denmark","cost-bio") + eps ;
Electricity3_DK_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Denmark","cost-gas") + eps ;
Electricity3_DK_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Denmark","cost-coa") + eps ;
Electricity3_DK_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Denmark","cost-lig") + eps ;
Electricity3_DK_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Denmark","cost-ccs") + eps ;

parameter
Electricity1_NO_rpt(t,*)
Electricity2_NO_rpt(t,*)
Electricity3_NO_rpt(t,*)
;

Electricity1_NO_rpt(t,"price-avg")              = sum(r$sameas(r,"Norway"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
Electricity1_NO_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
Electricity1_NO_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Norway"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_NO_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Norway","price-nuc") + eps ;
Electricity2_NO_rpt(t,"price-sol")              = Electricity2_rpt(t,"Norway","price-sol") + eps ;
Electricity2_NO_rpt(t,"price-wind")             = Electricity2_rpt(t,"Norway","price-wind") + eps ;
Electricity2_NO_rpt(t,"price-windon")           = Electricity2_rpt(t,"Norway","price-windon") + eps ;
Electricity2_NO_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Norway","price-windoff") + eps ;
Electricity2_NO_rpt(t,"price-bio")              = Electricity2_rpt(t,"Norway","price-bio") + eps ;
Electricity2_NO_rpt(t,"price-gas")              = Electricity2_rpt(t,"Norway","price-gas") + eps ;
Electricity2_NO_rpt(t,"price-coa")              = Electricity2_rpt(t,"Norway","price-coa") + eps ;
Electricity2_NO_rpt(t,"price-lig")              = Electricity2_rpt(t,"Norway","price-lig") + eps ;
Electricity2_NO_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Norway","price-ccs") + eps ;

Electricity3_NO_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Norway","cost-nuc") + eps ;
Electricity3_NO_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Norway","cost-sol") + eps ;
Electricity3_NO_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Norway","cost-wind") + eps ;
Electricity3_NO_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Norway","cost-windon") + eps ;
Electricity3_NO_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Norway","cost-windoff") + eps ;
Electricity3_NO_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Norway","cost-bio") + eps ;
Electricity3_NO_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Norway","cost-gas") + eps ;
Electricity3_NO_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Norway","cost-coa") + eps ;
Electricity3_NO_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Norway","cost-lig") + eps ;
Electricity3_NO_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Norway","cost-ccs") + eps ;

parameter
Electricity1_PL_rpt(t,*)
Electricity2_PL_rpt(t,*)
Electricity3_PL_rpt(t,*)
;

Electricity1_PL_rpt(t,"price-avg")              = sum(r$sameas(r,"Poland"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
Electricity1_PL_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
Electricity1_PL_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Poland"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_PL_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Poland","price-nuc") + eps ;
Electricity2_PL_rpt(t,"price-sol")              = Electricity2_rpt(t,"Poland","price-sol") + eps ;
Electricity2_PL_rpt(t,"price-wind")             = Electricity2_rpt(t,"Poland","price-wind") + eps ;
Electricity2_PL_rpt(t,"price-windon")           = Electricity2_rpt(t,"Poland","price-windon") + eps ;
Electricity2_PL_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Poland","price-windoff") + eps ;
Electricity2_PL_rpt(t,"price-bio")              = Electricity2_rpt(t,"Poland","price-bio") + eps ;
Electricity2_PL_rpt(t,"price-gas")              = Electricity2_rpt(t,"Poland","price-gas") + eps ;
Electricity2_PL_rpt(t,"price-coa")              = Electricity2_rpt(t,"Poland","price-coa") + eps ;
Electricity2_PL_rpt(t,"price-lig")              = Electricity2_rpt(t,"Poland","price-lig") + eps ;
Electricity2_PL_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Poland","price-ccs") + eps ;

Electricity3_PL_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Poland","cost-nuc") + eps ;
Electricity3_PL_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Poland","cost-sol") + eps ;
Electricity3_PL_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Poland","cost-wind") + eps ;
Electricity3_PL_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Poland","cost-windon") + eps ;
Electricity3_PL_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Poland","cost-windoff") + eps ;
Electricity3_PL_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Poland","cost-bio") + eps ;
Electricity3_PL_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Poland","cost-gas") + eps ;
Electricity3_PL_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Poland","cost-coa") + eps ;
Electricity3_PL_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Poland","cost-lig") + eps ;
Electricity3_PL_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Poland","cost-ccs") + eps ;

parameter
Electricity1_ES_rpt(t,*)
Electricity2_ES_rpt(t,*)
Electricity3_ES_rpt(t,*)
;

Electricity1_ES_rpt(t,"price-avg")              = sum(r$sameas(r,"Spain"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
Electricity1_ES_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
Electricity1_ES_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Spain"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_ES_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Spain","price-nuc") + eps ;
Electricity2_ES_rpt(t,"price-sol")              = Electricity2_rpt(t,"Spain","price-sol") + eps ;
Electricity2_ES_rpt(t,"price-wind")             = Electricity2_rpt(t,"Spain","price-wind") + eps ;
Electricity2_ES_rpt(t,"price-windon")           = Electricity2_rpt(t,"Spain","price-windon") + eps ;
Electricity2_ES_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Spain","price-windoff") + eps ;
Electricity2_ES_rpt(t,"price-bio")              = Electricity2_rpt(t,"Spain","price-bio") + eps ;
Electricity2_ES_rpt(t,"price-gas")              = Electricity2_rpt(t,"Spain","price-gas") + eps ;
Electricity2_ES_rpt(t,"price-coa")              = Electricity2_rpt(t,"Spain","price-coa") + eps ;
Electricity2_ES_rpt(t,"price-lig")              = Electricity2_rpt(t,"Spain","price-lig") + eps ;
Electricity2_ES_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Spain","price-ccs") + eps ;

Electricity3_ES_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Spain","cost-nuc") + eps ;
Electricity3_ES_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Spain","cost-sol") + eps ;
Electricity3_ES_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Spain","cost-wind") + eps ;
Electricity3_ES_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Spain","cost-windon") + eps ;
Electricity3_ES_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Spain","cost-windoff") + eps ;
Electricity3_ES_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Spain","cost-bio") + eps ;
Electricity3_ES_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Spain","cost-gas") + eps ;
Electricity3_ES_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Spain","cost-coa") + eps ;
Electricity3_ES_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Spain","cost-lig") + eps ;
Electricity3_ES_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Spain","cost-ccs") + eps ;
$offtext
