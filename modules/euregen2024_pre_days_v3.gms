* * * Timeseries and calibration
set
d /1*365/
$if      set    fromsixd      sd /1,2,4,7,9,10/
$if      set    fromonefived     sd /4,11,12,13,15,21,22,23,25,26,28,29,33,34,37/
hd
srepdays(sd,d)
sdm(sd,m)
peak_d(sd,hd,r,t)
;

$gdxin database\setpar_%n%.gdx
$load hd,srepdays,sdm
$gdxin

alias(sd,sdd) ;
alias(hd,hdd) ;

parameter
days(sd)
numberdays
* Load
load_d(sd,hd,r,t)                            
load_d_int(sd,hd,r,t)                           
loadcorr_d(r,t)
loadmax_d(r,t)
dref(r,t)                               Indexed reference growth path
daref(r,t)                              Reference annual demand by region over time (TWh)
* Sectoral load                  
load_sec_d(sd,hd,r,t,l)
load_sec_d_int(sd,hd,r,t,l)
loadcorr_sec_d(r,t,l)
dref_sec(r,t,l)
daref_sec(r,t,l)
* Variable renewables of old vintages
vrsc_exi_d(sd,hd,i,r,t)              
vrsc_exi_d_int(sd,hd,i,r,t)            
vrsccorr_exi_d(i,r,t)          
* Variable renewables of new vintages
vrsc_d(sd,hd,i,v,r)                          
vrsc_d_int(sd,hd,i,v,r)                        
vrsccorr_d(i,v,r)
* Losses                   
loss_d(sd,hd,r,t)
loss_d_int(sd,hd,r,t)
losscorr_d(r,t)
lossave(r,t)
* Full-load hours
irnwflh_h(i,v,r)                     
irnwflh_d(i,v,r)
irnwflh_exi_h(i,r,t)                        Intermittent renewables full-load hours (hours)
irnwflh_exi_d(i,r,t)                        Intermittent renewables full-load hours (segments)
* Upper invest limit
irnwlimUP_quantiles(i,r,quantiles)      Upper limit per quantile
* Inflows                    
inflowtot_h_d(j,v,r,t)
inflow_d(sd,hd,j,v,r,t)
inflow_d_int(sd,hd,j,v,r,t)
inflowcorr_d(j,v,r,t)
inflowtot_d(j,v,r,t)
inflowtot_nv_h_d(j,r,t)
inflow_nv_d(sd,hd,j,r,t)
inflow_nv_d_int(sd,hd,j,r,t)
inflowcorr_nv_d(j,r,t)
inflowtot_nv_d(j,r,t)
;

$gdxin database\setpar_%n%.gdx
$load days,numberdays,daref,daref_sec
$load load_d_int = load 
$load loadcorr_d = loadcorr 
$load load_sec_d_int = load_sec
$load loadcorr_sec_d = loadcorr_sec
$load vrsc_exi_d_int = vrsc_exi 
$load vrsccorr_exi_d = vrsccorr_exi 
$load vrsc_d_int = vrsc 
$load vrsccorr_d = vrsccorr 
$load loss_d_int = loss
$load losscorr_d = losscorr, lossave, irnwlimUP_quantiles
$load irnwflh_h
$load irnwflh_d = irnwflh_sd
$load irnwflh_exi_h 
$load irnwflh_exi_d = irnwflh_exi_sd
$load inflowtot_h_d = inflowtot_h
$load inflow_d_int = inflow
$load inflowcorr_d = inflowcorr
$load inflowtot_d = inflowtot_sd
$load inflowtot_nv_h_d = inflowtot_nv_h
$load inflow_nv_d_int = inflow_nv
$load inflowcorr_nv_d = inflowcorr_nv
$load inflowtot_nv_d = inflowtot_nv_sd
$gdxin

numberdays = 15 ;

* Correct time series to match annual load and full-load hours of renewables
$if      set corr_full  load_d(sd,hd,r,t)$(loadcorr_d(r,t) > 0) = loadcorr_d(r,t) * load_d_int(sd,hd,r,t) ;
$if      set corr_full  load_sec_d(sd,hd,r,t,l)$(loadcorr_sec_d(r,t,l) > 0) = loadcorr_sec_d(r,t,l) * load_sec_d_int(sd,hd,r,t,l) ;
$if      set corr_full  vrsc_exi_d(sd,hd,i,r,t)$(vrsccorr_exi_d(i,r,t) > 0) = vrsccorr_exi_d(i,r,t) * vrsc_exi_d_int(sd,hd,i,r,t) ;
$if      set corr_full  vrsc_d(sd,hd,i,v,r)$(vrsccorr_d(i,v,r) > 0) = vrsccorr_d(i,v,r) * vrsc_d_int(sd,hd,i,v,r) ;
$if      set corr_full  loss_d(sd,hd,r,t) = losscorr_d(r,t) * loss_d_int(sd,hd,r,t) ;
$if      set corr_full  inflow_d(sd,hd,j,v,r,t) = inflowcorr_d(j,v,r,t) * inflow_d_int(sd,hd,j,v,r,t) ;
$if      set corr_full  inflow_nv_d(sd,hd,j,r,t) = inflowcorr_nv_d(j,r,t) * inflow_nv_d_int(sd,hd,j,r,t) ;
 
$if not  set corr_full  load_d(sd,hd,r,t) = load_d_int(sd,hd,r,t) ;
$if not  set corr_full  load_sec_d(sd,hd,r,t,l) = load_sec_d_int(sd,hd,r,t,l) ;
$if not  set corr_full  vrsc_exi_d(sd,hd,i,r,t) = vrsc_exi_d_int(sd,hd,i,r,t) ;
$if not  set corr_full  vrsc_d(sd,hd,i,v,r) = vrsc_d_int(sd,hd,i,v,r) ;
$if not  set corr_full  loss_d(sd,hd,r,t) = loss_d_int(sd,hd,r,t) ;
$if not  set corr_full  inflow_d(sd,hd,j,v,r,t) = inflow_d_int(sd,hd,j,v,r,t) ;
$if not  set corr_full  inflow_nv_d(sd,hd,j,r,t) = inflow_nv_d_int(sd,hd,j,r,t) ;

* Correct PumpStorage inflow to avoid trouble
inflow_d(sd,hd,j,v,r,t)$(not sameas(j,"Reservoir")) = 0 ;
inflow_nv_d(sd,hd,j,r,t)$(not sameas(j,"Reservoir")) = 0 ;
inflowtot_d(j,v,r,t)$(not sameas(j,"Reservoir")) = 0 ;
inflowtot_nv_d(j,r,t)$(not sameas(j,"Reservoir")) = 0 ;

inflow_d(sd,hd,jres(j),v,r,topt2024(t)) = inflow_d(sd,hd,j,v,r,"2020")/2  + inflow_d(sd,hd,j,v,r,"2021")/2 ;
inflow_nv_d(sd,hd,jres(j),r,topt2024(t)) = inflow_nv_d(sd,hd,j,r,"2020")/2  + inflow_nv_d(sd,hd,j,r,"2021")/2 ;
$if     set hydronormal inflow_nv_d(sd,hd,jres(j),r,topt2023(t)) = inflow_nv_d(sd,hd,j,r,"2020")/2  + inflow_nv_d(sd,hd,j,r,"2021")/2 ;
$if     set hydronormal inflow_d(sd,hd,jres(j),v,r,topt2023(t)) = inflow_d(sd,hd,j,v,r,"2020")/2  + inflow_d(sd,hd,j,v,r,"2021")/2 ;

parameter
load_d_int(sd,hd,r,t)
daref_int(r,t)
daref_org(r,t)
;

daref_org(r,t) = daref(r,t) ;
daref_int(r,t)$(t.val le 2021) = daref(r,t) ;
daref_int(r,t)$(t.val ge 2022 and t.val le 2050) = daref(r,"2021") + (daref(r,"2050") - daref(r,"2021")) * (t.val - 2021) / (2050 - 2021) ;

$if     set loadnormal    load_d_int(sd,hd,r,t) = load_d(sd,hd,r,t) ;
$if     set loadnormal    load_d(sd,hd,r,t) = load_d_int(sd,hd,r,t) * daref_int(r,t) / daref_org(r,t) ; 
$if     set loadnormal    daref(r,t) = sum((sd,hd), days(sd) * load_d(sd,hd,r,t)) * 1e-3 ;

loadmax_d(r,t) = smax((sd,hd), load_d(sd,hd,r,t)) ;
peak_d(sd,hd,r,t) = YES$(load_d(sd,hd,r,t) eq loadmax_d(r,t)) ;

* * * Demand module
parameter
price_d(sd,hd,r,t)
Electricity1_rpt(t,r,*)
pelas_d(bse,sd,hd,r,t)              Price elasticity at reference point (a negative value)
p0_d(bse,sd,hd,r,t)              Reference average price (EUR per MWh)
d0_d(bse,sd,hd,r,t)              Reference annual average price in euro per MWh
cb_1_d(bse,sd,hd,r,t)               Consumer benefit linear coefficient
cb_2_d(bse,sd,hd,r,t)               Consumer benefit quadratic coefficient
;

$if      set priceref $gdxin elastic\bauprice_newcap_priceref_days.gdx
$if      set priceref $load price_d, Electricity1_rpt
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

Parameter
surplus_short(r,t)
surplus_long(r,t)
surplus_fact(r,t)
surplus_share(r,t)
demand_share(r,t)
surplus_adj(r,t)
load_max_d(bse,sd,hd,r,t)
;

surplus_short(r,t) = sum(sd, days(sd) * sum(hd, sum(bse, cb_1_d(bse,sd,hd,r,t) * d0_d(bse,sd,hd,r,t) + 0.5 * cb_2_d(bse,sd,hd,r,t) * d0_d(bse,sd,hd,r,t) * d0_d(bse,sd,hd,r,t)))) * 1e-6 ;

surplus_long(r,t)                                                                               = 8760 * (                   cb_1dtwh(r,t)   * d0dtwh(r,t)   + 0.5 * cb_2dtwh(r,t) * d0dtwh(r,t)     * d0dtwh(r,t))    * 1e-6 ;
surplus_fact(r,t)$surplus_long(r,t)                                                             = round(surplus_short(r,t) / surplus_long(r,t),4) ;
surplus_share(r,t)$sum(rr, surplus_short(rr,t) + surplus_fact(rr,t) * surplus_long(rr,t))       = (surplus_short(r,t) + surplus_fact(r,t) * surplus_long(r,t)) / sum(rr, surplus_short(rr,t) + surplus_fact(rr,t) * surplus_long(rr,t)) ;
demand_share(r,t)$sum(rr, daref(rr,t))                                                          = daref(r,t) / sum(rr, daref(rr,t)) ;
surplus_adj(r,t)$surplus_share(r,t)                                                             = round(demand_share(r,t) / surplus_share(r,t),4) ;
load_max_d(bse,sd,hd,r,t)$cb_2_d(bse,sd,hd,r,t)                                                 = - round(cb_1_d(bse,sd,hd,r,t) / cb_2_d(bse,sd,hd,r,t) * 1e-3,4) ;

parameter
voll_d(sd,hd,r,t)
hhours_d(sd,hd,j) hourly weighting for storage technologies
;

voll_d(sd,hd,r,t)   = 3000 ;
hhours_d(sd,hd,"Storage_ST") = 1 ;
$if      set pumppeak    hhours_d(sd,hd,"PumpStorage") = 1 ;
 
set
sdone(sd)
sdtwo(sd)
sdnum(sd)
;

sdone(sd)$(ord(sd) eq 1) = YES ;
sdtwo(sd)$(ord(sd) ge 2) = YES ;
sdnum(sd)$(sd.val eq 37) = YES ;

* * * Availability factor matrix (too large to read in)
parameter
af_d(sd,hd,i,v,r,t)
af_int_d(sd,hd,i,v,r,t)
af_chp_d(sd,hd,i,v,r,t)
af_chp_int_d(sd,hd,i,v,r,t)
af_nochp_d(sd,hd,i,v,r,t)
af_nochp_int_d(sd,hd,i,v,r,t)          
afmin_d(sd,hd,i,v,r,t)
afmax_d(sd,hd,i,v,r,t)
gafmin_d(sd,hd,j,v,r,t)
gafmax_d(sd,hd,j,v,r,t)
gafminnv_d(sd,hd,j,r,t)
gafmaxnv_d(sd,hd,j,r,t)
;

af_d(sd,hd,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 1 ;
af_d(sd,hd,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 1 ;
af_chp_d(sd,hd,ivrt(chp(i),oldv(v),r,t)) = 1 ;
af_nochp_d(sd,hd,ivrt(nochp(i),oldv(v),r,t)) = 1 ;

afmin_d(sd,hd,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 0.01 ;
afmin_d(sd,hd,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 0.01 ;
afmax_d(sd,hd,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 1 ;
afmax_d(sd,hd,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 1 ;


* Set 2020-2023 availability to real world availability
af_d(sd,hd,ivrt(nuc(i),oldv(v),r,told(t))) = sum(sdm(sd,m), afm(m,i,r,t)) ;
$if not set chp af_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,told(t))) = sum(sdm(sd,m), afm(m,i,r,t)) ;
$if     set chp af_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,told(t))) = sum(sdm(sd,m), afm_nochp(m,i,r,t)) ;
$if     set chp af_d(sd,hd,ivrt(chp(i),oldv(v),r,told(t))) = sum(sdm(sd,m), afm(m,i,r,t)) ;
* Set 2024+ availability to 2023 value
af_d(sd,hd,ivrt(nuc(i),oldv(v),r,topt2024(t))) = sum(sdm(sd,m), afm(m,i,r,"2023")) ;
$if not set chp af_d(sd,hd,ivrt(bio(i),oldv(v),r,topt2024(t))) = sum(sdm(sd,m), afm(m,i,r,"2023")) ;
$if     set chp af_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = sum(sdm(sd,m), afm_nochp(m,i,r,"2023")) ;
$if     set chp af_d(sd,hd,ivrt(chp(i),oldv(v),r,topt2024(t))) = sum(sdm(sd,m), afm_chp(m,i,r,"2023")) ;
* Set min
$if     set minmax  afmin_d(sd,hd,ivrt(nuc(i),oldv(v),r,told(t)))           = min(sum(sdm(sd,m), afmax_m(m,i,r,t)), sum(sdm(sd,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmin_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,told(t)))     = min(sum(sdm(sd,m), afmax_m(m,i,r,t)), sum(sdm(sd,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmin_d(sd,hd,ivrt(nuc(i),oldv(v),r,topt2024(t)))       = afmin_d(sd,hd,i,v,r,"2023") ;
$if     set minmax  afmin_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmin_d(sd,hd,i,v,r,"2023") ;
* Set max
$if     set minmax  afmax_d(sd,hd,ivrt(nuc(i),oldv(v),r,told(t)))           = max(sum(sdm(sd,m), afmax_m(m,i,r,t)), sum(sdm(sd,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmax_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,told(t)))     = max(sum(sdm(sd,m), afmax_m(m,i,r,t)), sum(sdm(sd,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmax_d(sd,hd,ivrt(nuc(i),oldv(v),r,topt2024(t)))       = afmax_d(sd,hd,i,v,r,"2023") ;
$if     set minmax  afmax_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmax_d(sd,hd,i,v,r,"2023") ;
* Set nuclear and bio avai to zero when using minmax
$if     set minmax  af_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))       = 0 ;
$if     set minmax  af_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = 0 ;

* Set storage min max availabilities
gafmin_d(sd,hd,jvrt(jpump(j),v,r,t)) = sum(sdm(sd,m), gafmin_m(m,j,r,t)) ;
gafmin_d(sd,hd,jvrt(jres(j),v,r,t))  = sum(sdm(sd,m), gafmin_m(m,j,r,t)) ;
gafmax_d(sd,hd,jvrt(jpump(j),v,r,t)) = sum(sdm(sd,m), gafmax_m(m,j,r,t)) ;
gafmax_d(sd,hd,jvrt(jres(j),v,r,t))  = sum(sdm(sd,m), gafmax_m(m,j,r,t)) ;
gafminnv_d(sd,hd,jpump(j),r,t)       = sum(sdm(sd,m), gafmin_m(m,j,r,t)) ;
gafminnv_d(sd,hd,jres(j),r,t)        = sum(sdm(sd,m), gafmin_m(m,j,r,t)) ;
gafmaxnv_d(sd,hd,jpump(j),r,t)       = sum(sdm(sd,m), gafmax_m(m,j,r,t)) ;
gafmaxnv_d(sd,hd,jres(j),r,t)        = sum(sdm(sd,m), gafmax_m(m,j,r,t)) ;

* Set 2024+ availability of German nuclear to 0
af_d(sd,hd,ivrt(nuc(i),oldv(v),rge(r),topt2024(t))) = 0 ;
afmax_d(sd,hd,ivrt(nuc(i),oldv(v),rge(r),topt2024(t))) = 0 ;
afmin_d(sd,hd,ivrt(nuc(i),oldv(v),rge(r),topt2024(t))) = 0 ;
* Set 2024+ availability of French nuclear to 2021 value
af_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t)))    = sum(sdm(sd,m), afm(m,i,r,"2021")) ;
afmax_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sdm(sd,m), afmax_m(m,i,r,"2021")) ;
afmin_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sdm(sd,m), afmin_m(m,i,r,"2021")) ;
* Set 2022-2023 availability of French nuclear to 2021 value(as what-if-not-case)
$if      set frnucnormal     af_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),tcri(t)))    = sum(sdm(sd,m), afm(m,i,r,"2021")) ;
$if      set frnucnormal     afmax_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),tcri(t))) = sum(sdm(sd,m), afmax_m(m,i,r,"2021")) ;
$if      set frnucnormal     afmin_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),tcri(t))) = sum(sdm(sd,m), afmin_m(m,i,r,"2021")) ;
* Set 2024+ French nuclear availability as in 2023 as what-if-still-case
$if      set frnucperman     af_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t)))    = sum(sdm(sd,m), afm(m,i,r,"2023")) ;
$if      set frnucperman     afmax_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sdm(sd,m), afmax_m(m,i,r,"2023")) ;
$if      set frnucperman     afmin_d(sd,hd,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sdm(sd,m), afmin_m(m,i,r,"2023")) ;
* Set 2024+ hydro to 2020-2021 average
$if      set hydronormal     vrsc_exi_d(sd,hd,i,r,t)$(sameas(i,"Hydro") and t.val ge 2022 and t.val le 2023) = vrsc_exi_d(sd,hd,i,r,"2020")/2 + vrsc_exi_d(sd,hd,i,r,"2021")/2 ;
vrsc_exi_d(sd,hd,i,r,t)$(sameas(i,"Hydro") and t.val ge 2024) = vrsc_exi_d(sd,hd,i,r,"2020")/2 + vrsc_exi_d(sd,hd,i,r,"2021")/2 ;
* Set 2024+ hydro availability as in 2023 as what-if-still-case
$if      set hydroperman     vrsc_exi_d(sd,hd,i,r,t)$(sameas(i,"Hydro") and t.val ge 2024) = vrsc_exi_d(sd,hd,i,r,"2023") ;
* Set 2023+ German nuclear availability to 0 (business-as-usual)
$if      set gernucnormal   af_d(sd,hd,ivrt("Nuclear","1990","Germany",t))$(t.val ge 2023) = 0 ;
$if      set gernucnormal   afmax_d(sd,hd,ivrt("Nuclear","1990","Germany",t))$(t.val ge 2023) = 0 ;
$if      set gernucnormal   afmin_d(sd,hd,ivrt("Nuclear","1990","Germany",t))$(t.val ge 2023) = 0 ;
* Set 2023+ German nuclear availability to 2020-2021 average (extension)
$if      set extension      af_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m), afm(m,i,r,"2020")/2 + afm(m,i,r,"2021")/2) ;
$if      set extension      afmax_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m), afmax_m(m,i,r,"2020")/2 + afmax_m(m,i,r,"2021")/2) ;
$if      set extension      afmin_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m), afmin_m(m,i,r,"2020")/2 + afmin_m(m,i,r,"2021")/2) ;
* Set 2023+ German nuclear availability to real (stretching) until Aug 23 and to 2020-2021 average from Sep 23 onwards (stretchtension)
$if      set strext         af_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and t.val le 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m)$(m.val le 8),  afm(m,i,r,t)) +  eps ;
$if      set strext         af_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2024 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m)$(m.val ge 9), afm(m,i,r,"2020")/2 + afm(m,i,r,"2021")/2) ;
$if      set strext         afmax_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and t.val le 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m)$(m.val le 8),  afmax_m(m,i,r,t)) +  eps ;
$if      set strext         afmax_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2024 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m)$(m.val ge 9), afmax_m(m,i,r,"2020")/2 + afmax_m(m,i,r,"2021")/2) ;                     
$if      set strext         afmin_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and t.val le 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m)$(m.val le 8),  afmin_m(m,i,r,t)) +  eps ;
$if      set strext         afmin_d(sd,hd,ivrt(i,oldv(v),r,t))$(t.val ge 2024 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sdm(sd,m)$(m.val ge 9), afmin_m(m,i,r,"2020")/2 + afmin_m(m,i,r,"2021")/2) ; 
* 2023 afmin set to zero
afmin_d(sd,hd,ivrt(i,oldv(v),r,"2023"))$(sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = 0 ; 
* Get rid of all availability parameter
$if      set af1            af_d(sd,hd,ivrt(i,v,r,t)) = 0 ;

* * * Calibration of past years and old vintage generation
set
genset /nucl,sola,wind,hydr,pump,resv,ngas,biom,lign,oil,coal,ngas_chp,biom_chp,lign_chp,oil_chp,coal_chp,ngas_nochp,biom_nochp,lign_nochp,oil_nochp,coal_nochp,ngas_tog,biom_tog,
        lign_tog,oil_tog,coal_tog,sum,sum_dif,sum_norchp,sum_chp_nochp,sum_chp,sum_nochp,daref,daref_loss/
nor(genset) /nucl,sola,wind,hydr,pump,resv,ngas,biom,lign,oil,coal/
norchp(genset) /ngas,biom,lign,oil,coal/
genset_chp(genset) /ngas_chp,biom_chp,lign_chp,oil_chp,coal_chp/
genset_nochp(genset) /ngas_nochp,biom_nochp,lign_nochp,oil_nochp,coal_nochp/
res(genset) /nucl,sola,wind,hydr,pump,resv/
;

parameter
imp(r,t)
expo(r,t)
* Other
gen_nucl(r,t)
gen_sola(r,t)
gen_wind(r,t)
gen_hydr(r,t)
gen_pump(r,t)
gen_resv(r,t)
* CHP/NOCHP
gen_ngas(r,t)
gen_biom(r,t)
gen_lign(r,t)
gen_oil(r,t)
gen_coal(r,t)
* CHP
gen_chp_ngas(r,t)
gen_chp_biom(r,t)
gen_chp_lign(r,t)
gen_chp_oil(r,t)
gen_chp_coal(r,t)
* NOCHP
gen_nochp_ngas(r,t)
gen_nochp_biom(r,t)
gen_nochp_lign(r,t)
gen_nochp_oil(r,t)
gen_nochp_coal(r,t)
* Other
cap_nucl(r,t)
cap_sola(r,t)
cap_wind(r,t)
cap_hydr(r,t)
cap_pump(r,t)
cap_resv(r,t)
* CHP/NOCHP
cap_ngas(r,t)
cap_biom(r,t)
cap_lign(r,t)
cap_oil(r,t)
cap_coal(r,t)
* CHP
cap_chp_ngas(r,t)
cap_chp_biom(r,t)
cap_chp_lign(r,t)
cap_chp_oil(r,t)
cap_chp_coal(r,t)
* NOCHP
cap_nochp_ngas(r,t)
cap_nochp_biom(r,t)
cap_nochp_lign(r,t)
cap_nochp_oil(r,t)
cap_nochp_coal(r,t)
;

$gdxin database\setpar_%n%.gdx
$load gen_nucl
$load gen_ngas
$load gen_biom
$load gen_lign
$load gen_oil
$load gen_coal
$load gen_wind
$load gen_sola
$load gen_hydr
$load gen_pump
$load gen_resv
$load imp,expo
$load gen_chp_ngas,gen_chp_biom,gen_chp_lign,gen_chp_oil,gen_chp_coal
$load gen_nochp_ngas,gen_nochp_biom,gen_nochp_lign,gen_nochp_oil,gen_nochp_coal
$gdxin

Set
set_chp /TWh,GW,effrate,Twhheat,Gwheat,effrateheat,TWhth,effratetot,Coal,Oil,Lignite,Gasd,hd,Bioenergy/
;

Parameter
chpinfo2021(r,set_chp)
;

$gdxin database\data_chp\chp_shortrun_out.gdx
$load chpinfo2021
$gdxin


parameter
gen_new(genset,r,t)
gen_new_all(genset,t)
;

gen_coal("Czech",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Czech",t)) > 0) = chpinfo2021("Czech","Coal") * sum(v, capt("Coa_CHP",v,"Czech",t) * effrate("Coa_CHP",v,"Czech")) / sum(v, capt("Coa_CHP",v,"Czech",t)) ;
gen_coal("Romania",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Romania",t)) > 0) = chpinfo2021("Romania","Coal") * sum(v, capt("Coa_CHP",v,"Romania",t) * effrate("Coa_CHP",v,"Romania")) / sum(v, capt("Coa_CHP",v,"Romania",t)) ;
gen_coal("Hungary",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Hungary",t)) > 0) = chpinfo2021("Hungary","Coal") * sum(v, capt("Coa_CHP",v,"Hungary",t) * effrate("Coa_CHP",v,"Hungary")) / sum(v, capt("Coa_CHP",v,"Hungary",t)) ;
gen_coal("Bulgaria",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Bulgaria",t)) > 0) = chpinfo2021("Bulgaria","Coal") * sum(v, capt("Coa_CHP",v,"Bulgaria",t) * effrate("Coa_CHP",v,"Bulgaria")) / sum(v, capt("Coa_CHP",v,"Bulgaria",t)) ;

gen_chp_coal("Czech",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Czech",t)) > 0) = chpinfo2021("Czech","Coal") * sum(v, capt("Coa_CHP",v,"Czech",t) * effrate("Coa_CHP",v,"Czech")) / sum(v, capt("Coa_CHP",v,"Czech",t)) ;
gen_chp_coal("Romania",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Romania",t)) > 0) = chpinfo2021("Romania","Coal") * sum(v, capt("Coa_CHP",v,"Romania",t) * effrate("Coa_CHP",v,"Romania")) / sum(v, capt("Coa_CHP",v,"Romania",t)) ;
gen_chp_coal("Hungary",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Hungary",t)) > 0) = chpinfo2021("Hungary","Coal") * sum(v, capt("Coa_CHP",v,"Hungary",t) * effrate("Coa_CHP",v,"Hungary")) / sum(v, capt("Coa_CHP",v,"Hungary",t)) ;
gen_chp_coal("Bulgaria",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Bulgaria",t)) > 0) = chpinfo2021("Bulgaria","Coal") * sum(v, capt("Coa_CHP",v,"Bulgaria",t) * effrate("Coa_CHP",v,"Bulgaria")) / sum(v, capt("Coa_CHP",v,"Bulgaria",t)) ;

gen_new("Nucl",r,t) = gen_nucl(r,t) ;
gen_new("ngas",r,t) = gen_ngas(r,t) ;
gen_new("biom",r,t) = gen_biom(r,t) ;
gen_new("Lign",r,t) = gen_lign(r,t) ;
gen_new("Oil",r,t) = gen_oil(r,t) ;
gen_new("Coal",r,t) = gen_coal(r,t) ;
gen_new("Wind",r,t) = gen_wind(r,t) ;
gen_new("Sola",r,t) = gen_sola(r,t) ;
gen_new("Hydr",r,t) = gen_hydr(r,t) ;
gen_new("Pump",r,t) = gen_pump(r,t) ;
gen_new("Resv",r,t) = gen_resv(r,t) ;

gen_new("ngas_chp",r,t) = gen_chp_ngas(r,t) ;
gen_new("Biom_chp",r,t) = gen_chp_biom(r,t) ;
gen_new("Lign_chp",r,t) = gen_chp_lign(r,t) ;
gen_new("Oil_chp",r,t) = gen_chp_oil(r,t) ;
gen_new("Coal_chp",r,t) = gen_chp_coal(r,t) ;

gen_new("ngas_nochp",r,t) = gen_nochp_ngas(r,t) ;
gen_new("Biom_nochp",r,t) = gen_nochp_biom(r,t) ;
gen_new("Lign_nochp",r,t) = gen_nochp_lign(r,t) ;
gen_new("Oil_nochp",r,t) = gen_nochp_oil(r,t) ;
gen_new("Coal_nochp",r,t) = gen_nochp_coal(r,t) ;

gen_new("ngas_tog",r,t) = gen_new("ngas_nochp",r,t) + gen_new("ngas_chp",r,t) ;
gen_new("Biom_tog",r,t) = gen_new("biom_nochp",r,t) + gen_new("biom_chp",r,t) ;
gen_new("Lign_tog",r,t) = gen_new("lign_nochp",r,t) + gen_new("lign_chp",r,t) ;
gen_new("Oil_tog",r,t) = gen_new("oil_nochp",r,t) + gen_new("oil_chp",r,t) ;
gen_new("Coal_tog",r,t) = gen_new("coal_nochp",r,t) + gen_new("coal_chp",r,t) ;

gen_new("daref",r,t) = daref(r,t) ;
gen_new("daref_loss",r,t) = daref(r,t) * (1 + lossave(r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("ngas_nochp",r,t) = gen_new("ngas",r,t) - gen_new("ngas_chp",r,t) ;
gen_new("Biom_nochp",r,t) = gen_new("biom",r,t) - gen_new("biom_chp",r,t) ;
gen_new("Lign_nochp",r,t) = gen_new("lign",r,t) - gen_new("lign_chp",r,t) ;
gen_new("Oil_nochp",r,t) = gen_new("oil",r,t) - gen_new("oil_chp",r,t) ;
gen_new("Coal_nochp",r,t) = gen_new("Coal",r,t) - gen_new("Coal_chp",r,t) ;

gen_new("ngas_tog",r,t) = gen_new("ngas_nochp",r,t) + gen_new("ngas_chp",r,t) ;
gen_new("Biom_tog",r,t) = gen_new("biom_nochp",r,t) + gen_new("biom_chp",r,t) ;
gen_new("Lign_tog",r,t) = gen_new("lign_nochp",r,t) + gen_new("lign_chp",r,t) ;
gen_new("Oil_tog",r,t) = gen_new("oil_nochp",r,t) + gen_new("oil_chp",r,t) ;
gen_new("Coal_tog",r,t) = gen_new("coal_nochp",r,t) + gen_new("coal_chp",r,t) ;

gen_new("sum",r,t) = sum(nor(genset), gen_new(genset,r,t)) ;
gen_new("sum_dif",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) + sum(genset_nochp(genset), gen_new(genset,r,t)) + sum(res(genset), gen_new(genset,r,t)) ;
gen_new("sum_norchp",r,t) = sum(norchp(genset), gen_new(genset,r,t)) ;
gen_new("sum_chp_nochp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) + sum(genset_nochp(genset), gen_new(genset,r,t)) ;
gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;
gen_new("sum_nochp",r,t) = sum(genset_nochp(genset), gen_new(genset,r,t)) ;

gen_new_all(genset,t) = sum(r, gen_new(genset,r,t)) ;

gen_nucl(r,t) = gen_new("nucl",r,t) ;
gen_sola(r,t) = gen_new("sola",r,t) ;
gen_wind(r,t) = gen_new("wind",r,t) ;
gen_hydr(r,t) = gen_new("hydr",r,t) ;
gen_pump(r,t) = gen_new("pump",r,t) ;
gen_resv(r,t) = gen_new("resv",r,t) ;
* CHP/NOCHP
gen_ngas(r,t) = gen_new("ngas",r,t) ;
gen_biom(r,t) = gen_new("biom",r,t) ;
gen_lign(r,t) = gen_new("lign",r,t) ;
gen_oil(r,t) = gen_new("oil",r,t) ;
gen_coal(r,t) = gen_new("coal",r,t) ;
* CHP
gen_chp_ngas(r,t) = gen_new("ngas_chp",r,t) ;
gen_chp_biom(r,t) = gen_new("biom_chp",r,t) ;
gen_chp_lign(r,t) = gen_new("lign_chp",r,t) ;
gen_chp_oil(r,t) = gen_new("oil_chp",r,t) ;
gen_chp_coal(r,t) = gen_new("coal_chp",r,t) ;
* NOCHP
gen_nochp_ngas(r,t) = gen_new("ngas_nochp",r,t) ;
gen_nochp_biom(r,t) = gen_new("biom_nochp",r,t) ;
gen_nochp_lign(r,t) = gen_new("lign_nochp",r,t) ;
gen_nochp_oil(r,t) = gen_new("oil_nochp",r,t) ;
gen_nochp_coal(r,t) = gen_new("coal_nochp",r,t) ;

$if      set loadnormal     gen_nochp_ngas(r,"2023") = max(gen_new("ngas_nochp",r,"2022"),gen_new("ngas_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_biom(r,"2023") = max(gen_new("biom_nochp",r,"2022"),gen_new("biom_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_lign(r,"2023") = max(gen_new("lign_nochp",r,"2022"),gen_new("lign_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_oil(r,"2023") = max(gen_new("oil_nochp",r,"2022"),gen_new("oil_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_coal(r,"2023") = max(gen_new("coal_nochp",r,"2022"),gen_new("coal_nochp",r,"2023")) ;

Parameter
* Storage
gen_afmax_pump(r,t)
gen_afmax_resv(r,t)
gen_afmin_pump(r,t)
gen_afmin_resv(r,t) 
gen_comax_pump(r,t)
gen_comax_resv(r,t)
gen_comin_pump(r,t)
gen_comin_resv(r,t)
gafmaxnv_int_d(sd,hd,j,r,t)
gafminnv_int_d(sd,hd,j,r,t)
*gafmax_int_d(sd,hd,j,v,r,t)
*gafmin_int_d(sd,hd,j,v,r,t)
* Transmission 
gen_af_expo(r,t)
gen_co_expo(r,t)
gen_af_impo(r,t)
gen_co_impo(r,t)
taf_expo(r,t)
taf_impo(r,t)
taf_expo_int(r,t)
taf_impo_int(r,t)
;

 
gen_af_expo(r,t) = sum(rr, sum(tvrt(k,v,r,t), sum((sd,hd), days(sd) * tlifetime(k,v,r,t) * tcap(k,r,rr)))) * 1e-3 ;
gen_co_expo(r,t)$(gen_af_expo(r,t) > 0 and t.val le 2023) = expo(r,t) / gen_af_expo(r,t) ;
gen_af_impo(r,t) = sum(rr, sum(tvrt(k,v,r,t), sum((sd,hd), days(sd) * tlifetime(k,v,r,t) * tcap(k,rr,r)))) * 1e-3 ;
gen_co_impo(r,t)$(gen_af_impo(r,t) > 0 and t.val le 2023) = imp(r,t) / gen_af_impo(r,t) ;
   
taf_expo(r,t)$(gen_co_expo(r,t) > 1 and t.val le 2023) = gen_co_expo(r,t) ;
taf_impo(r,t)$(gen_co_impo(r,t) > 1 and t.val le 2023) = gen_co_impo(r,t) ;

taf_expo_int(r,t) = taf_expo(r,t) ;
taf_impo_int(r,t) = taf_impo(r,t) ;

taf_expo(r,t)$(t.val ge 2024) = taf_expo_int(r,"2024") ;
taf_impo(r,t)$(t.val ge 2024) = taf_impo_int(r,"2024") ;

$if     set taf1   taf_expo(r,t) = 1 ;
$if     set taf1   taf_impo(r,t) = 1 ;

* * Calculation of 2024 to 2050 generation
* Other technologies
gen_nucl(r,t)$(t.val ge 2024) = gen_nucl(r,"2023") ;
gen_nucl("Germany",t)$(t.val ge 2024) = 0 ;
gen_nucl(r,t)$(t.val ge 2024 and sameas(r,"France")) = 393.134 ;
gen_hydr(r,t)$(t.val ge 2024) = gen_hydr(r,"2020")/2 + gen_hydr(r,"2021")/2 ;
gen_resv(r,t)$(t.val ge 2024) = gen_resv(r,"2020")/2 + gen_resv(r,"2021")/2 ;
gen_pump(r,t)$(t.val ge 2024) = gen_pump(r,"2020")/2 + gen_pump(r,"2021")/2 ;
gen_wind(r,t)$(t.val ge 2024) = gen_wind(r,"2023") ;
gen_sola(r,t)$(t.val ge 2024) = gen_sola(r,"2023") ;
* CHP/NOCHP
gen_coal(r,t)$(t.val ge 2024) = gen_coal(r,"2023") ;
gen_lign(r,t)$(t.val ge 2024) = gen_lign(r,"2023") ;
gen_biom(r,t)$(t.val ge 2024) = gen_biom(r,"2023") ;
gen_ngas(r,t)$(t.val ge 2024) = gen_ngas(r,"2023") ;
gen_oil(r,t)$(t.val ge 2024) = gen_oil(r,"2023") ;
* CHP
gen_chp_coal(r,t)$(t.val ge 2024) = gen_chp_coal(r,"2023") ;
gen_chp_lign(r,t)$(t.val ge 2024) = gen_chp_lign(r,"2023") ;
gen_chp_biom(r,t)$(t.val ge 2024) = gen_chp_biom(r,"2023") ;
gen_chp_ngas(r,t)$(t.val ge 2024) = gen_chp_ngas(r,"2023") ;
gen_chp_oil(r,t)$(t.val ge 2024) = gen_chp_oil(r,"2023") ;
* NOCHP
gen_nochp_coal(r,t)$(t.val ge 2024) = gen_nochp_coal(r,"2023") ;
gen_nochp_lign(r,t)$(t.val ge 2024) = gen_nochp_lign(r,"2023") ;
gen_nochp_biom(r,t)$(t.val ge 2024) = gen_nochp_biom(r,"2023") ;
gen_nochp_ngas(r,t)$(t.val ge 2024) = gen_nochp_ngas(r,"2023") ;
gen_nochp_oil(r,t)$(t.val ge 2024) = gen_nochp_oil(r,"2023") ;

* Correct for hydro and nuclear crises years
$if      set frnucnormal gen_nucl(r,t)$(t.val ge 2022 and t.val le 2023 and sameas(r,"France")) = 393.134 ;
$if      set frnucperman gen_nucl(r,t)$(t.val ge 2024                   and sameas(r,"France")) = gen_nucl(r,"2023") ;
$if      set hydronormal gen_hydr(r,t)$(t.val ge 2022 and t.val le 2023                       ) = gen_hydr(r,"2020")/2 + gen_hydr(r,"2021")/2 ;
$if      set hydroperman gen_hydr(r,t)$(t.val ge 2024                                         ) = gen_hydr(r,"2023") ;
$if      set hydronormal gen_resv(r,t)$(t.val ge 2022 and t.val le 2023                       ) = gen_resv(r,"2020")/2 + gen_resv(r,"2021")/2 ;
$if      set hydroperman gen_resv(r,t)$(t.val ge 2024                                         ) = gen_resv(r,"2023") ;
$if      set hydronormal gen_pump(r,t)$(t.val ge 2022 and t.val le 2023                       ) = gen_pump(r,"2020")/2 + gen_pump(r,"2021")/2 ;
$if      set hydroperman gen_pump(r,t)$(t.val ge 2024                                         ) = gen_pump(r,"2023") ;

$if     set gaf1 gafmaxnv_d(sd,hd,j,r,t) = 1 ;
$if     set gaf1 gafminnv_d(sd,hd,j,r,t) = 0 ;
*$if     set gaf1 gafmax_d(sd,hd,j,v,r,t) = 1 ;
*$if     set gaf1 gafmin_d(sd,hd,j,v,r,t) = 0 ;

*gen_afmax_pump(r,t) = sum(jvrt(jpump(j),v,r,t), sum((sd,hd), days(sd)/4 * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * (1 + (gafmaxnv_d(sd,hd,j,r,t)-1)$gafmaxnv_d(sd,hd,j,r,t)))) * 1e-3 ;
gen_afmax_resv(r,t) = sum(jvrt(jres(j),v,r,t),   sum((sd,hd), days(sd)/4 * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * (1 + (gafmaxnv_d(sd,hd,j,r,t)-1)$gafmaxnv_d(sd,hd,j,r,t)))) * 1e-3 ;
*gen_afmin_pump(r,t) = sum(jvrt(jpump(j),v,r,t), sum((sd,hd), days(sd) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * gafminnv_d(sd,hd,j,r,t))) * 1e-3 ;
gen_afmin_resv(r,t) = sum(jvrt(jres(j),v,r,t),   sum((sd,hd), days(sd) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * gafminnv_d(sd,hd,j,r,t))) * 1e-3 ;

*gen_afmax_pump(r,t) = sum(jvrt(jpump(j),v,r,t), sum((sd,hd), days(sd)/4 * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * (1 + (gafmaxnv_d(sd,hd,j,r,t)-1)$gafmaxnv_d(sd,hd,j,r,t)))) * 1e-3 ;
gen_afmax_resv(r,t) = sum(jvrt(jres(j),v,r,t),   sum((sd,hd), days(sd)/4 * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * (1 + (gafmaxnv_d(sd,hd,j,r,t)-1)$gafmaxnv_d(sd,hd,j,r,t)))) * 1e-3 ;
*gen_afmin_pump(r,t) = sum(jvrt(jpump(j),v,r,t), sum((sd,hd), days(sd) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * gafminnv_d(sd,hd,j,r,t))) * 1e-3 ;
gen_afmin_resv(r,t) = sum(jvrt(jres(j),v,r,t),   sum((sd,hd), days(sd) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * gafminnv_d(sd,hd,j,r,t))) * 1e-3 ;

*gen_comax_pump(r,t)$(gen_afmax_pump(r,t) > 0 and t.val le 2023) = gen_pump(r,t) / gen_afmax_pump(r,t) ;
gen_comax_resv(r,t)$(gen_afmax_resv(r,t) > 0 and t.val le 2023) = gen_resv(r,t) / gen_afmax_resv(r,t) ;
*gen_comin_pump(r,t)$(gen_afmin_pump(r,t) > 0 and t.val le 2023) = gen_pump(r,t) / gen_afmin_pump(r,t) ;
gen_comin_resv(r,t)$(gen_afmin_resv(r,t) > 0 and t.val le 2023) = gen_resv(r,t) / gen_afmin_resv(r,t) ;

gafmaxnv_int_d(sd,hd,j,r,t) = gafmaxnv_d(sd,hd,j,r,t) ;
gafminnv_int_d(sd,hd,j,r,t) = gafminnv_d(sd,hd,j,r,t) ;

*gafmaxnv_d(sd,hd,j,r,t)$(sameas(j,"PumpStorage") and gen_comax_pump(r,t) > 1) = gafmaxnv_int_d(sd,hd,j,r,t) * gen_comax_pump(r,t) ;
gafmaxnv_d(sd,hd,j,r,t)$(sameas(j,"Reservoir") and gen_comax_resv(r,t) > 1) = gafmaxnv_int_d(sd,hd,j,r,t) * gen_comax_resv(r,t) ;

*gafminnv_d(sd,hd,j,r,t)$(sameas(j,"PumpStorage") and gen_comin_pump(r,t) < 1 and gen_comin_pump(r,t) > 0) = gafminnv_int_d(sd,hd,j,r,t) * gen_comin_pump(r,t) ;
gafminnv_d(sd,hd,j,r,t)$(sameas(j,"Reservoir") and gen_comin_resv(r,t) < 1 and gen_comin_resv(r,t) > 0) = gafminnv_int_d(sd,hd,j,r,t) * gen_comin_resv(r,t) ;
  
gafmaxnv_d(sd,hd,j,r,t)$(t.val ge 2024) = gafmaxnv_d(sd,hd,j,r,"2020")/2 + gafmaxnv_d(sd,hd,j,r,"2021")/2 ;
gafminnv_d(sd,hd,j,r,t)$(t.val ge 2024) = gafminnv_d(sd,hd,j,r,"2020")/2 + gafminnv_d(sd,hd,j,r,"2021")/2 ;

$if     set gaforg  gafmaxnv_d(sd,hd,j,r,t) = gafmaxnv_int_d(sd,hd,j,r,t) ;
$if     set gaforg  gafminnv_d(sd,hd,j,r,t) = gafminnv_int_d(sd,hd,j,r,t) ;
*$if     set gaforg  gafmax_d(sd,hd,j,v,r,t) = gafmax_int_d(sd,hd,j,v,r,t) ;
*$if     set gaforg  gafmin_d(sd,hd,j,v,r,t) = gafmin_int_d(sd,hd,j,v,r,t) ;


* Correct for German nuclear exit

$if      set gernucnormal    capt("Nuclear",oldv(v),"Germany","2023") = 0 ;
$if      set gernucnormal    gen_nucl(r,t)$(t.val ge 2023 and t.val le 2029 and sameas(r,"Germany")) = 0 ;
$if      set extension       gen_nucl(r,t)$(t.val ge 2023 and t.val le 2029 and sameas(r,"Germany")) = round(gen_nucl(r,"2020")/4 + gen_nucl(r,"2021")/4,4) ;
$if      set strext          gen_nucl(r,t)$(t.val ge 2023 and t.val le 2023 and sameas(r,"Germany")) = round(gen_nucl(r,"2023") + (gen_nucl(r,"2020")/4 + gen_nucl(r,"2021")/4)*3/12,4) ;
$if      set strext          gen_nucl(r,t)$(t.val ge 2024 and t.val le 2030 and sameas(r,"Germany")) = round(gen_nucl(r,"2020")/4 + gen_nucl(r,"2021")/4,4) ;

cap_nucl(r,t) = sum(ivrt(nuc(i),v,r,t), capt(i,v,r,t)) ;
cap_sola(r,t) = sum(ivrt(sol(i),v,r,t), capt(i,v,r,t)) ;
cap_wind(r,t) = sum(ivrt(wind(i),v,r,t), capt(i,v,r,t)) ;
cap_hydr(r,t) = sum(ivrt(hyd(i),v,r,t), capt(i,v,r,t)) ;
* CHP/NOCHP
cap_ngas(r,t) = sum(ivrt(gas(i),v,r,t), capt(i,v,r,t)) ;
cap_biom(r,t) = sum(ivrt(bio(i),v,r,t), capt(i,v,r,t)) ;
cap_lign(r,t) = sum(ivrt(lig(i),v,r,t), capt(i,v,r,t)) ;
cap_oil(r,t)  = sum(ivrt(oil(i),v,r,t), capt(i,v,r,t)) ;
cap_coal(r,t) = sum(ivrt(coa(i),v,r,t), capt(i,v,r,t)) ;
* CHP
cap_chp_ngas(r,t) = sum(ivrt_chp(gas(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_biom(r,t) = sum(ivrt_chp(bio(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_lign(r,t) = sum(ivrt_chp(lig(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_oil(r,t)  = sum(ivrt_chp(oil(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_coal(r,t) = sum(ivrt_chp(coa(i),v,r,t), capt(i,v,r,t)) ;
* NOCHP
cap_nochp_ngas(r,t) = sum(ivrt_nochp(gas(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_biom(r,t) = sum(ivrt_nochp(bio(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_lign(r,t) = sum(ivrt_nochp(lig(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_oil(r,t)  = sum(ivrt_nochp(oil(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_coal(r,t) = sum(ivrt_nochp(coa(i),v,r,t), capt(i,v,r,t)) ;

parameter
capt_add(i,v,r,t)
;

capt_add(ivrt(nuc(i),"2020",r,tcri(t)))$(cap_nucl(r,t) = 0 and gen_nucl(r,t) > 0) = gen_nucl(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(sol(i),"2020",r,tcri(t)))$(cap_sola(r,t) = 0 and gen_sola(r,t) > 0) = gen_sola(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(wind(i),"2020",r,tcri(t)))$(cap_wind(r,t) = 0 and gen_wind(r,t) > 0) = gen_wind(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(hyd(i),"2020",r,tcri(t)))$(cap_hydr(r,t) = 0 and gen_hydr(r,t) > 0) = gen_hydr(r,t) * 1e+3 / 8760 ;

capt_add(ivrt(gas(i),"2020",r,tcri(t)))$(cap_ngas(r,t) = 0 and gen_ngas(r,t) > 0) = gen_ngas(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(bio(i),"2020",r,tcri(t)))$(cap_biom(r,t) = 0 and gen_biom(r,t) > 0) = gen_biom(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(lig(i),"2020",r,tcri(t)))$(cap_lign(r,t) = 0 and gen_lign(r,t) > 0) = gen_lign(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(oil(i),"2020",r,tcri(t)))$(cap_oil(r,t) = 0 and gen_oil(r,t) > 0) = gen_oil(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(coa(i),"2020",r,tcri(t)))$(cap_coal(r,t) = 0 and gen_coal(r,t) > 0) = gen_coal(r,t) * 1e+3 / 8760 ;

capt_add(ivrt_chp(gas(i),"2020",r,tcri(t)))$(cap_ngas(r,t) = 0 and gen_chp_ngas(r,t) > 0) = gen_chp_ngas(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(bio(i),"2020",r,tcri(t)))$(cap_biom(r,t) = 0 and gen_chp_biom(r,t) > 0) = gen_chp_biom(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(lig(i),"2020",r,tcri(t)))$(cap_lign(r,t) = 0 and gen_chp_lign(r,t) > 0) = gen_chp_lign(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(oil(i),"2020",r,tcri(t)))$(cap_oil(r,t) = 0 and gen_chp_oil(r,t) > 0) = gen_chp_oil(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(coa(i),"2020",r,tcri(t)))$(cap_coal(r,t) = 0 and gen_chp_coal(r,t) > 0) = gen_chp_coal(r,t) * 1e+3 / 8760 ;

capt_add(ivrt_nochp(gas(i),"2020",r,tcri(t)))$(cap_ngas(r,t) = 0 and gen_nochp_ngas(r,t) > 0) = gen_nochp_ngas(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(bio(i),"2020",r,tcri(t)))$(cap_biom(r,t) = 0 and gen_nochp_biom(r,t) > 0) = gen_nochp_biom(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(lig(i),"2020",r,tcri(t)))$(cap_lign(r,t) = 0 and gen_nochp_lign(r,t) > 0) = gen_nochp_lign(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(oil(i),"2020",r,tcri(t)))$(cap_oil(r,t) = 0 and gen_nochp_oil(r,t) > 0) = gen_nochp_oil(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(coa(i),"2020",r,tcri(t)))$(cap_coal(r,t) = 0 and gen_nochp_coal(r,t) > 0) = gen_nochp_coal(r,t) * 1e+3 / 8760 ;

parameter
* Other
gen_af_nucl(r,t)
gen_af_sola(r,t)
gen_af_wind(r,t)
gen_af_hydr(r,t)
* Min/max issues
gen_afmin_nucl(r,t)
gen_afmax_nucl(r,t)
gen_afmin_biom(r,t)
gen_afmax_biom(r,t)
* CHP/NOCHP
gen_af_ngas(r,t)
gen_af_biom(r,t)
gen_af_lign(r,t)
gen_af_oil(r,t)
gen_af_coal(r,t)
* CHP
gen_af_chp_ngas(r,t)
gen_af_chp_biom(r,t)
gen_af_chp_lign(r,t)
gen_af_chp_oil(r,t)
gen_af_chp_coal(r,t)
* NOCHP
gen_af_nochp_ngas(r,t)
gen_af_nochp_biom(r,t)
gen_af_nochp_lign(r,t)
gen_af_nochp_oil(r,t)
gen_af_nochp_coal(r,t)
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)
gen_afmax_nochp_biom(r,t)
;

reliability(i,oldv(v),r) = 0 ;

* * Maximum available generation given availability, vrsc, and reliability as well as existing capacity (for oldv only)
* Other technologies
gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
* Min/max issues
gen_afmin_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
gen_afmin_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;

parameter
* Other
gen_co_nucl(r,t)
gen_co_sola(r,t)
gen_co_wind(r,t)
gen_co_hydr(r,t)
* Min/max issues
gen_comin_nucl(r,t)
gen_comax_nucl(r,t)
gen_comin_biom(r,t)
gen_comax_biom(r,t)
* CHP/NOCHP
gen_co_ngas(r,t)
gen_co_biom(r,t)
gen_co_lign(r,t)
gen_co_oil(r,t)
gen_co_coal(r,t)
* CHP
gen_co_chp_ngas(r,t)
gen_co_chp_biom(r,t)
gen_co_chp_lign(r,t)
gen_co_chp_oil(r,t)
gen_co_chp_coal(r,t)
* NOCHP
gen_co_nochp_ngas(r,t)
gen_co_nochp_biom(r,t)
gen_co_nochp_lign(r,t)
gen_co_nochp_oil(r,t)
gen_co_nochp_coal(r,t)
* NOCHP min/max issues
gen_comin_nochp_biom(r,t)
gen_comax_nochp_biom(r,t)
;

* * Correcting availability to match real generation (necessary due to segment simplification) for "kind of must-run technologies"
* All technologies
gen_co_nucl(r,topt2023(t))$(gen_af_nucl(r,t) > 0) = gen_nucl(r,t) / gen_af_nucl(r,t) ;
gen_co_hydr(r,topt2023(t))$(gen_af_hydr(r,t) > 0) = gen_hydr(r,t) / gen_af_hydr(r,t) ;
gen_co_wind(r,topt2023(t))$(gen_af_wind(r,t) > 0) = gen_wind(r,t) / gen_af_wind(r,t) ;
gen_co_sola(r,topt2023(t))$(gen_af_sola(r,t) > 0) = gen_sola(r,t) / gen_af_sola(r,t) ;
* Min/max issues
gen_comin_nucl(r,topt2023(t))$(gen_afmin_nucl(r,t) > 0) = gen_nucl(r,t) / gen_afmin_nucl(r,t) ;
gen_comax_nucl(r,topt2023(t))$(gen_afmax_nucl(r,t) > 0) = gen_nucl(r,t) / gen_afmax_nucl(r,t) ;
gen_comin_biom(r,topt2023(t))$(gen_afmin_biom(r,t) > 0) = gen_biom(r,t) / gen_afmin_biom(r,t) ;
gen_comax_biom(r,topt2023(t))$(gen_afmax_biom(r,t) > 0) = gen_biom(r,t) / gen_afmax_biom(r,t) ;
* CHP/NOCHP
gen_co_ngas(r,topt2023(t))$(gen_af_ngas(r,t) > 0) = gen_ngas(r,t) / gen_af_ngas(r,t) ;
gen_co_biom(r,topt2023(t))$(gen_af_biom(r,t) > 0) = gen_biom(r,t) / gen_af_biom(r,t) ;
gen_co_oil(r,topt2023(t))$(gen_af_oil(r,t) > 0  ) = gen_oil(r,t)  / gen_af_oil(r,t) ;
gen_co_coal(r,topt2023(t))$(gen_af_coal(r,t) > 0) = gen_coal(r,t) / gen_af_coal(r,t) ;
gen_co_lign(r,topt2023(t))$(gen_af_lign(r,t) > 0) = gen_lign(r,t) / gen_af_lign(r,t) ;
* CHP
gen_co_chp_ngas(r,topt2023(t))$(gen_af_chp_ngas(r,t) > 0) = gen_chp_ngas(r,t) / gen_af_chp_ngas(r,t) ;
gen_co_chp_coal(r,topt2023(t))$(gen_af_chp_coal(r,t) > 0) = gen_chp_coal(r,t) / gen_af_chp_coal(r,t) ;
gen_co_chp_lign(r,topt2023(t))$(gen_af_chp_lign(r,t) > 0) = gen_chp_lign(r,t) / gen_af_chp_lign(r,t) ; 
gen_co_chp_biom(r,topt2023(t))$(gen_af_chp_biom(r,t) > 0) = gen_chp_biom(r,t) / gen_af_chp_biom(r,t) ;
gen_co_chp_oil(r,topt2023(t))$(gen_af_chp_oil(r,t) > 0) = gen_chp_oil(r,t) / gen_af_chp_oil(r,t) ;
gen_co_chp_ngas(r,topt2023(t))$(gen_af_chp_ngas(r,t) = 0) = 0 ;
gen_co_chp_coal(r,topt2023(t))$(gen_af_chp_coal(r,t) = 0) = 0 ;
gen_co_chp_lign(r,topt2023(t))$(gen_af_chp_lign(r,t) = 0) = 0 ; 
gen_co_chp_biom(r,topt2023(t))$(gen_af_chp_biom(r,t) = 0) = 0 ;
gen_co_chp_oil(r,topt2023(t))$(gen_af_chp_oil(r,t) = 0) = 0 ;
* NOCHP
gen_co_nochp_ngas(r,topt2023(t))$(gen_af_nochp_ngas(r,t) > 0) = gen_nochp_ngas(r,t) / gen_af_nochp_ngas(r,t) ;
gen_co_nochp_coal(r,topt2023(t))$(gen_af_nochp_coal(r,t) > 0) = gen_nochp_coal(r,t) / gen_af_nochp_coal(r,t) ;
gen_co_nochp_lign(r,topt2023(t))$(gen_af_nochp_lign(r,t) > 0) = gen_nochp_lign(r,t) / gen_af_nochp_lign(r,t) ; 
gen_co_nochp_biom(r,topt2023(t))$(gen_af_nochp_biom(r,t) > 0) = gen_nochp_biom(r,t) / gen_af_nochp_biom(r,t) ;
gen_co_nochp_oil(r,topt2023(t))$(gen_af_nochp_oil(r,t) > 0) = gen_nochp_oil(r,t) / gen_af_nochp_oil(r,t) ;
* NOCHP min/max issues
gen_comin_nochp_biom(r,topt2023(t))$(gen_afmin_nochp_biom(r,t) > 0) = gen_nochp_biom(r,t) / gen_afmin_nochp_biom(r,t) ;
gen_comax_nochp_biom(r,topt2023(t))$(gen_afmax_nochp_biom(r,t) > 0) = gen_nochp_biom(r,t) / gen_afmax_nochp_biom(r,t) ;

parameter
af_int_d(sd,hd,i,v,r,t)
af_chp_int(sd,hd,i,v,r,t)
af_nochp_int(sd,hd,i,v,r,t)
afmin_int_d(sd,hd,i,v,r,t)
afmax_int_d(sd,hd,i,v,r,t)
afmin_nochp_d(sd,hd,i,v,r,t)
afmax_nochp_d(sd,hd,i,v,r,t)
afmin_nochp_int(sd,hd,i,v,r,t)
afmax_nochp_int(sd,hd,i,v,r,t)
;

* * Availability parameter correction 
* Interim availability
af_int_d(sd,hd,ivrt(i,oldv(v),r,t))               = af_d(sd,hd,i,v,r,t) ;
af_chp_int(sd,hd,ivrt_chp(i,oldv(v),r,t))       = af_chp_d(sd,hd,i,v,r,t) ;
af_nochp_int(sd,hd,ivrt(nochp(i),oldv(v),r,t))  = af_nochp_d(sd,hd,i,v,r,t) ;
afmin_int_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))       = afmin_d(sd,hd,i,v,r,t) ;
afmax_int_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))       = afmax_d(sd,hd,i,v,r,t) ;
afmin_int_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = afmin_d(sd,hd,i,v,r,t) ;
afmax_int_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = afmax_d(sd,hd,i,v,r,t) ;
* Other technologies
af_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))$(gen_co_nucl(r,t) > 1)  = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_nucl(r,t) ;
af_d(sd,hd,ivrt(wind(i),oldv(v),r,t))$(gen_co_wind(r,t) > 1) = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_wind(r,t) ;
af_d(sd,hd,ivrt(sol(i),oldv(v),r,t))$(gen_co_sola(r,t) > 1)  = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_sola(r,t) ;
af_d(sd,hd,ivrt(hyd(i),oldv(v),r,t))$(gen_co_hydr(r,t) > 1)  = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_hydr(r,t) ;
* Min/max issues
afmin_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))$(gen_comin_nucl(r,t) < 1 and gen_comin_nucl(r,t) > 0)                 = afmin_int_d(sd,hd,i,v,r,t) * gen_comin_nucl(r,t) ;
afmax_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))$(gen_comax_nucl(r,t) > 1)                                             = (1 + (afmax_int_d(sd,hd,i,v,r,t)-1)$afmax_int_d(sd,hd,i,v,r,t)) * gen_comax_nucl(r,t) ;
afmin_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comin_biom(r,t) < 1 and gen_comin_biom(r,t) > 0)           = afmin_int_d(sd,hd,i,v,r,t) * gen_comin_biom(r,t) ;
afmax_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comax_biom(r,t) > 1)                                       = (1 + (afmax_int_d(sd,hd,i,v,r,t)-1)$afmax_int_d(sd,hd,i,v,r,t)) * gen_comax_biom(r,t) ;
* CHP/NOCHP
af_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_co_biom(r,t) > 1)   = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_biom(r,t) ;
af_d(sd,hd,ivrt_nochp(coa(i),oldv(v),r,t))$(gen_co_coal(r,t) > 1)   = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_coal(r,t) ;
af_d(sd,hd,ivrt_nochp(lig(i),oldv(v),r,t))$(gen_co_lign(r,t) > 1)   = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_lign(r,t) ;
af_d(sd,hd,ivrt_nochp(gas(i),oldv(v),r,t))$(gen_co_ngas(r,t) > 1)   = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_ngas(r,t) ;
af_d(sd,hd,ivrt_nochp(oil(i),oldv(v),r,t))$(gen_co_oil(r,t) > 1)    = (1 + (af_int_d(sd,hd,i,v,r,t)-1)$af_int_d(sd,hd,i,v,r,t)) * gen_co_oil(r,t) ;
* CHP 
$if not  set chpmustrun     af_chp_d(sd,hd,ivrt_chp(gas(i),oldv(v),r,t))$(gen_co_chp_ngas(r,t) > 1) = (1 + (af_chp_int(sd,hd,i,v,r,t)-1)$af_chp_int(sd,hd,i,v,r,t)) * gen_co_chp_ngas(r,t) ;
$if not  set chpmustrun     af_chp_d(sd,hd,ivrt_chp(coa(i),oldv(v),r,t))$(gen_co_chp_coal(r,t) > 1) = (1 + (af_chp_int(sd,hd,i,v,r,t)-1)$af_chp_int(sd,hd,i,v,r,t)) * gen_co_chp_coal(r,t) ;
$if not  set chpmustrun     af_chp_d(sd,hd,ivrt_chp(lig(i),oldv(v),r,t))$(gen_co_chp_lign(r,t) > 1) = (1 + (af_chp_int(sd,hd,i,v,r,t)-1)$af_chp_int(sd,hd,i,v,r,t)) * gen_co_chp_lign(r,t) ;
$if not  set chpmustrun     af_chp_d(sd,hd,ivrt_chp(bio(i),oldv(v),r,t))$(gen_co_chp_biom(r,t) > 1) = (1 + (af_chp_int(sd,hd,i,v,r,t)-1)$af_chp_int(sd,hd,i,v,r,t)) *  gen_co_chp_biom(r,t) ;
$if not  set chpmustrun     af_chp_d(sd,hd,ivrt_chp(oil(i),oldv(v),r,t))$(gen_co_chp_oil(r,t) > 1) = (1 + (af_chp_int(sd,hd,i,v,r,t)-1)$af_chp_int(sd,hd,i,v,r,t)) * gen_co_chp_oil(r,t) ;
$if      set chpmustrun     af_chp_d(sd,hd,ivrt_chp(gas(i),oldv(v),r,t)) = af_chp_int(sd,hd,i,v,r,t) * gen_co_chp_ngas(r,t) ;
$if      set chpmustrun     af_chp_d(sd,hd,ivrt_chp(coa(i),oldv(v),r,t)) = af_chp_int(sd,hd,i,v,r,t) * gen_co_chp_coal(r,t) ;
$if      set chpmustrun     af_chp_d(sd,hd,ivrt_chp(lig(i),oldv(v),r,t)) = af_chp_int(sd,hd,i,v,r,t) * gen_co_chp_lign(r,t) ;
$if      set chpmustrun     af_chp_d(sd,hd,ivrt_chp(bio(i),oldv(v),r,t)) = af_chp_int(sd,hd,i,v,r,t) * gen_co_chp_biom(r,t) ;
$if      set chpmustrun     af_chp_d(sd,hd,ivrt_chp(oil(i),oldv(v),r,t)) = af_chp_int(sd,hd,i,v,r,t) * gen_co_chp_oil(r,t) ;
* NOCHP
af_nochp_d(sd,hd,ivrt_nochp(gas(i),oldv(v),r,t))$(gen_co_nochp_ngas(r,t) > 1) = (1 + (af_nochp_int(sd,hd,i,v,r,t)-1)$af_nochp_int(sd,hd,i,v,r,t)) * gen_co_nochp_ngas(r,t) ;
af_nochp_d(sd,hd,ivrt_nochp(coa(i),oldv(v),r,t))$(gen_co_nochp_coal(r,t) > 1) = (1 + (af_nochp_int(sd,hd,i,v,r,t)-1)$af_nochp_int(sd,hd,i,v,r,t)) * gen_co_nochp_coal(r,t) ;
af_nochp_d(sd,hd,ivrt_nochp(lig(i),oldv(v),r,t))$(gen_co_nochp_lign(r,t) > 1) = (1 + (af_nochp_int(sd,hd,i,v,r,t)-1)$af_nochp_int(sd,hd,i,v,r,t)) * gen_co_nochp_lign(r,t) ;
af_nochp_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_co_nochp_biom(r,t) > 1) = (1 + (af_nochp_int(sd,hd,i,v,r,t)-1)$af_nochp_int(sd,hd,i,v,r,t)) *  gen_co_nochp_biom(r,t) ;
af_nochp_d(sd,hd,ivrt_nochp(oil(i),oldv(v),r,t))$(gen_co_nochp_oil(r,t) > 1) = (1 + (af_nochp_int(sd,hd,i,v,r,t)-1)$af_nochp_int(sd,hd,i,v,r,t)) * gen_co_nochp_oil(r,t) ;
* NOCHP min/max issues
afmin_nochp_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comin_nochp_biom(r,t) < 1 and gen_comin_nochp_biom(r,t) > 0)  = afmin_int_d(sd,hd,i,v,r,t) * gen_comin_nochp_biom(r,t) ;
afmax_nochp_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comax_nochp_biom(r,t) > 1)                              = (1 + (afmax_int_d(sd,hd,i,v,r,t)-1)$afmax_int_d(sd,hd,i,v,r,t)) * gen_comax_nochp_biom(r,t) ;

* * 2024+ availability
* Interim availability
af_int_d(sd,hd,ivrt(i,oldv(v),r,t))               = af_d(sd,hd,i,v,r,t) ;
af_chp_int(sd,hd,ivrt_chp(i,oldv(v),r,t))       = af_chp_d(sd,hd,i,v,r,t) ;
af_nochp_int(sd,hd,ivrt(nochp(i),oldv(v),r,t))  = af_nochp_d(sd,hd,i,v,r,t) ;
afmin_int_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))       = afmin_d(sd,hd,i,v,r,t) ;
afmax_int_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))       = afmax_d(sd,hd,i,v,r,t) ;
afmin_int_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = afmin_d(sd,hd,i,v,r,t) ;
afmax_int_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = afmax_d(sd,hd,i,v,r,t) ;
afmin_nochp_int(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = afmin_nochp_d(sd,hd,i,v,r,t) ;
afmax_nochp_int(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t)) = afmax_nochp_d(sd,hd,i,v,r,t) ;
* Availability 2024+
af_d(sd,hd,ivrt(i,oldv(v),r,topt2024(t)))                     = af_int_d(sd,hd,i,v,r,"2023") ;
af_chp_d(sd,hd,ivrt_chp(i,oldv(v),r,topt2024(t)))             = af_chp_int(sd,hd,i,v,r,"2023") ;
af_nochp_d(sd,hd,ivrt(nochp(i),oldv(v),r,topt2024(t)))        = af_nochp_int(sd,hd,i,v,r,"2023") ;
afmin_d(sd,hd,ivrt(nuc(i),oldv(v),r,topt2024(t)))             = afmin_int_d(sd,hd,i,v,r,"2023") ;
afmax_d(sd,hd,ivrt(nuc(i),oldv(v),r,topt2024(t)))             = afmax_int_d(sd,hd,i,v,r,"2023") ;
afmin_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t)))       = afmin_int_d(sd,hd,i,v,r,"2023") ;
afmax_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t)))       = afmax_int_d(sd,hd,i,v,r,"2023") ;
afmin_nochp_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmin_nochp_int(sd,hd,i,v,r,"2023") ;
afmax_nochp_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmax_nochp_int(sd,hd,i,v,r,"2023") ;

* * * Final calculation of gen_af
* * Until 2023
* Other technologies
* * Maximum available generation given availability, vrsc, and reliability as well as existing capacity (for oldv only)
* Other technologies
loop(tcri(t),
gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
* Min/max issues
gen_afmin_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
gen_afmin_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_nochp_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_nochp_d(sd,hd,i,v,r,t)-1)$afmax_nochp_d(sd,hd,i,v,r,t)))) * 1e-3 ;
) ;

* * 2024+
loop(topt2024(t),
gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
* Min/max issues
gen_afmin_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
gen_afmin_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp_d(sd,hd,i,v,r,t)-1)$af_chp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp_d(sd,hd,i,v,r,t)-1)$af_nochp_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_nochp_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_nochp_d(sd,hd,i,v,r,t)-1)$afmax_nochp_d(sd,hd,i,v,r,t)))) * 1e-3 ;
) ;

parameter
* Other
gen_min_nucl(r,t)
gen_min_sola(r,t)
gen_min_wind(r,t)
gen_min_hydr(r,t)
* Min/max issues
gen_minmax_nucl(r,t)
gen_minmax_biom(r,t)
* CHP/NOCHP
gen_min_ngas(r,t)
gen_min_biom(r,t)
gen_min_lign(r,t)
gen_min_oil(r,t)
gen_min_coal(r,t)
* CHP
gen_min_chp_ngas(r,t)
gen_min_chp_biom(r,t)
gen_min_chp_lign(r,t)
gen_min_chp_oil(r,t)
gen_min_chp_coal(r,t)
* NOCHP
gen_min_nochp_ngas(r,t)
gen_min_nochp_biom(r,t)
gen_min_nochp_lign(r,t)
gen_min_nochp_oil(r,t)
gen_min_nochp_coal(r,t)
* NOCHP min/max issues
gen_minmax_nochp_biom(r,t)
;

loop(toptimize(t),
* Other technologies
gen_min_nucl(r,t)       = min(gen_af_nucl(r,t), gen_nucl(r,t)) ;
gen_min_hydr(r,t)       = min(gen_af_hydr(r,t), gen_hydr(r,t)) ;
gen_min_wind(r,t)       = min(gen_af_wind(r,t), gen_wind(r,t)) ;
gen_min_sola(r,t)       = min(gen_af_sola(r,t), gen_sola(r,t)) ;
* Min/max issues
gen_minmax_nucl(r,t)    = min(gen_afmax_nucl(r,t), gen_nucl(r,t)) ;
gen_minmax_biom(r,t)    = min(gen_afmax_biom(r,t), gen_biom(r,t)) ;
* CHP/NOCHP
gen_min_lign(r,t)       = min(gen_af_lign(r,t), gen_lign(r,t)) ;
gen_min_coal(r,t)       = min(gen_af_coal(r,t), gen_coal(r,t)) ;
gen_min_oil(r,t)        = min(gen_af_oil(r,t),  gen_oil(r,t)) ;
gen_min_biom(r,t)       = min(gen_af_biom(r,t), gen_biom(r,t)) ;
gen_min_ngas(r,t)       = min(gen_af_ngas(r,t), gen_ngas(r,t)) ;
* CHP
$if not  set chpmustrun     gen_min_chp_lign(r,t)   = min(gen_af_chp_lign(r,t), gen_chp_lign(r,t)) ;
$if not  set chpmustrun     gen_min_chp_coal(r,t)   = min(gen_af_chp_coal(r,t), gen_chp_coal(r,t)) ;
$if not  set chpmustrun     gen_min_chp_oil(r,t)    = min(gen_af_chp_oil(r,t), gen_chp_oil(r,t)) ;
$if not  set chpmustrun     gen_min_chp_biom(r,t)   = min(gen_af_chp_biom(r,t), gen_chp_biom(r,t)) ;
$if not  set chpmustrun     gen_min_chp_ngas(r,t)   = min(gen_af_chp_ngas(r,t), gen_chp_ngas(r,t)) ;
$if      set chpmustrun     gen_min_chp_lign(r,t)   = gen_af_chp_lign(r,t) ;
$if      set chpmustrun     gen_min_chp_coal(r,t)   = gen_af_chp_coal(r,t) ;
$if      set chpmustrun     gen_min_chp_oil(r,t)    = gen_af_chp_oil(r,t) ;
$if      set chpmustrun     gen_min_chp_biom(r,t)   = gen_af_chp_biom(r,t) ;
$if      set chpmustrun     gen_min_chp_ngas(r,t)   = gen_af_chp_ngas(r,t) ;
* NOCHP
gen_min_nochp_lign(r,t) = min(gen_af_nochp_lign(r,t), gen_nochp_lign(r,t)) ;
gen_min_nochp_coal(r,t) = min(gen_af_nochp_coal(r,t), gen_nochp_coal(r,t)) ;
gen_min_nochp_oil(r,t)  = min(gen_af_nochp_oil(r,t), gen_nochp_oil(r,t)) ;
gen_min_nochp_biom(r,t) = min(gen_af_nochp_biom(r,t), gen_nochp_biom(r,t)) ;
gen_min_nochp_ngas(r,t) = min(gen_af_nochp_ngas(r,t), gen_nochp_ngas(r,t)) ;
* NOCHP Min/max issues
gen_minmax_nochp_biom(r,t)    = min(gen_afmax_nochp_biom(r,t), gen_nochp_biom(r,t)) ;
) ;

parameter
gen_new2(genset,r,t)
gen_new2_all(genset,t)
;

$if not set minmax  gen_new2("Nucl",r,t) = gen_min_nucl(r,t) ;
$if     set minmax  gen_new2("Nucl",r,t) = gen_minmax_nucl(r,t) ;
gen_new2("ngas",r,t) = gen_min_ngas(r,t) ;
$if not set minmax  gen_new2("biom",r,t) = gen_min_biom(r,t) ;
$if     set minmax  gen_new2("biom",r,t) = gen_minmax_biom(r,t) ;
gen_new2("Lign",r,t) = gen_min_lign(r,t) ;
gen_new2("Oil",r,t) = gen_min_oil(r,t) ;
gen_new2("Coal",r,t) = gen_min_coal(r,t) ;
gen_new2("Wind",r,t) = gen_min_wind(r,t) ;
gen_new2("Sola",r,t) = gen_min_sola(r,t) ;
gen_new2("Hydr",r,t) = gen_hydr(r,t) ;
gen_new2("Pump",r,t) = gen_pump(r,t) ;
gen_new2("Resv",r,t) = gen_resv(r,t) ;

gen_new2("ngas_chp",r,t) = gen_min_chp_ngas(r,t) ;
gen_new2("Biom_chp",r,t) = gen_min_chp_biom(r,t) ;
gen_new2("Lign_chp",r,t) = gen_min_chp_lign(r,t) ;
gen_new2("Oil_chp",r,t) = gen_min_chp_oil(r,t) ;
gen_new2("Coal_chp",r,t) = gen_min_chp_coal(r,t) ;
gen_new2("ngas_nochp",r,t) = gen_min_nochp_ngas(r,t) ;
$if not set minmax  gen_new2("Biom_nochp",r,t) = gen_min_nochp_biom(r,t) ;
$if     set minmax  gen_new2("Biom_nochp",r,t) = gen_minmax_nochp_biom(r,t) ;
gen_new2("Lign_nochp",r,t) = gen_min_nochp_lign(r,t) ;
gen_new2("Oil_nochp",r,t) = gen_min_nochp_oil(r,t) ;
gen_new2("Coal_nochp",r,t) = gen_min_nochp_coal(r,t) ;

gen_new2("ngas_tog",r,t) = gen_new2("ngas_nochp",r,t) + gen_new2("ngas_chp",r,t) ;
gen_new2("Biom_tog",r,t) = gen_new2("biom_nochp",r,t) + gen_new2("biom_chp",r,t) ;
gen_new2("Lign_tog",r,t) = gen_new2("lign_nochp",r,t) + gen_new2("lign_chp",r,t) ;
gen_new2("Oil_tog",r,t) = gen_new2("oil_nochp",r,t) + gen_new2("oil_chp",r,t) ;
gen_new2("Coal_tog",r,t) = gen_new2("coal_nochp",r,t) + gen_new2("coal_chp",r,t) ;

gen_new2("daref",r,t) = daref(r,t) ;
gen_new2("daref_loss",r,t) = daref(r,t) * (1 + lossave(r,t)) ;

gen_new2("sum",r,t) = sum(nor(genset), gen_new2(genset,r,t)) ;
gen_new2("sum_dif",r,t) = sum(genset_chp(genset), gen_new2(genset,r,t)) + sum(genset_nochp(genset), gen_new2(genset,r,t)) + sum(res(genset), gen_new2(genset,r,t)) ;
gen_new2("sum_norchp",r,t) = sum(norchp(genset), gen_new2(genset,r,t)) ;
gen_new2("sum_chp_nochp",r,t) = sum(genset_chp(genset), gen_new2(genset,r,t)) + sum(genset_nochp(genset), gen_new2(genset,r,t)) ;
gen_new2("sum_chp",r,t) = sum(genset_chp(genset), gen_new2(genset,r,t)) ;
gen_new2("sum_nochp",r,t) = sum(genset_nochp(genset), gen_new2(genset,r,t)) ;

* * Recalculating ivrt in response of (not) modelling chp
capt(i,v,r,t)$(not toptimize(t)) = 0 ;
$if not  set chp            capt(nochp(i),v,r,toptimize(t)) = capt_int(i,v,r,t) + sum(mapchp(ii,i), capt_int(ii,v,r,t)) ;
$if not  set chp            capt(chp(i),v,r,t)   = 0 ;
$if      set gernucnormal   capt("Nuclear",oldv(v),"Germany","2023") = 0 ;
ivrt(i,v,r,t) = NO ;
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

ivrt_nochp(i,v,r,t) = NO ;
ivrt_chp(i,v,r,t) = NO ;
ivrt_nochp(ivrt(i,v,r,t))$(not chp(i)) = YES ;
ivrt_chp(ivrt(chp(i),v,r,t)) = YES ;

* * Setting final availability parameters
* Interim
af_int_d(sd,hd,ivrt(i,v,r,t)) = af_d(sd,hd,i,v,r,t) ;
afmax_int_d(sd,hd,ivrt(i,v,r,t)) = afmax_d(sd,hd,i,v,r,t) ;
afmin_int_d(sd,hd,ivrt(i,v,r,t)) = afmin_d(sd,hd,i,v,r,t) ;
af_d(sd,hd,i,v,r,t) = 0 ;
afmax_d(sd,hd,i,v,r,t) = 0 ;
afmin_d(sd,hd,i,v,r,t) = 0 ;
* Final parameters
af_d(sd,hd,ivrt(i,oldv(v),r,toptimize(t)))$(af_int_d(sd,hd,i,v,r,t) > 1)                            = round(af_int_d(sd,hd,i,v,r,t),8) ;
$if     set chp af_d(sd,hd,ivrt(nochp(i),oldv(v),r,toptimize(t)))$(af_nochp_d(sd,hd,i,v,r,t) > 1)   = round(af_nochp_d(sd,hd,i,v,r,t),8) ;
$if     set chp $if not  set chpmustrun     af_d(sd,hd,ivrt(chp(i),oldv(v),r,toptimize(t)))$(af_chp_d(sd,hd,i,v,r,t) > 1)       = round(af_chp_d(sd,hd,i,v,r,t),8) ;
$if     set chp $if      set chpmustrun     af_d(sd,hd,ivrt(chp(i),oldv(v),r,toptimize(t)))$(af_chp_d(sd,hd,i,v,r,t) >= 0)       = round(af_chp_d(sd,hd,i,v,r,t),8) ;
$if not set chp af_d(sd,hd,ivrt(chp(i),oldv(v),r,toptimize(t))) = 0 ;
afmax_d(sd,hd,ivrt(nuc(i),oldv(v),r,toptimize(t)))                                            = round(afmax_int_d(sd,hd,i,v,r,t),8) ;
afmin_d(sd,hd,ivrt(nuc(i),oldv(v),r,toptimize(t)))                                            = round(afmin_int_d(sd,hd,i,v,r,t),8) ;
$if     set chp afmax_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmax_nochp_d(sd,hd,i,v,r,t),8) ;
$if     set chp afmin_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmin_nochp_d(sd,hd,i,v,r,t),8) ;
$if not set chp afmax_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmax_int_d(sd,hd,i,v,r,t),8) ;
$if not set chp afmin_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmin_int_d(sd,hd,i,v,r,t),8) ;

parameter
* Other
gen_af2_nucl(r,t)
gen_af2_sola(r,t)
gen_af2_wind(r,t)
gen_af2_hydr(r,t)
* Min/max issues
gen_afmin2_nucl(r,t)
gen_afmax2_nucl(r,t)
gen_afmin2_biom(r,t)
gen_afmax2_biom(r,t)
* CHP/NOCHP
gen_af2_ngas(r,t)
gen_af2_biom(r,t)
gen_af2_lign(r,t)
gen_af2_oil(r,t)
gen_af2_coal(r,t)
* CHP
gen_af2_chp_ngas(r,t)
gen_af2_chp_biom(r,t)
gen_af2_chp_lign(r,t)
gen_af2_chp_oil(r,t)
gen_af2_chp_coal(r,t)
* NOCHP
gen_af2_nochp_ngas(r,t)
gen_af2_nochp_biom(r,t)
gen_af2_nochp_lign(r,t)
gen_af2_nochp_oil(r,t)
gen_af2_nochp_coal(r,t)
* NOCHP min/max issues
gen_afmin2_nochp_biom(r,t)
gen_afmax2_nochp_biom(r,t)
;

* Other technologies
loop(tcri(t),
gen_af2_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af2_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af2_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
* Min/max issues
gen_afmin2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
gen_afmin2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af2_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af2_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
) ;

* * 2024+
loop(topt2024(t),
gen_af2_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af2_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af2_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
* Min/max issues
gen_afmin2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
gen_afmin2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af2_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_d(sd,hd,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af2_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t))) * 1e-3 ;
gen_afmax2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum((sd,hd), days(sd) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)))) * 1e-3 ;
) ;

parameter
* Other
gen_min2_nucl(r,t)
gen_min2_sola(r,t)
gen_min2_wind(r,t)
gen_min2_hydr(r,t)
* Min/max issues
gen_minmax2_nucl(r,t)
gen_minmax2_biom(r,t)
* CHP/NOCHP
gen_min2_ngas(r,t)
gen_min2_biom(r,t)
gen_min2_lign(r,t)
gen_min2_oil(r,t)
gen_min2_coal(r,t)
* CHP
gen_min2_chp_ngas(r,t)
gen_min2_chp_biom(r,t)
gen_min2_chp_lign(r,t)
gen_min2_chp_oil(r,t)
gen_min2_chp_coal(r,t)
* NOCHP
gen_min2_nochp_ngas(r,t)
gen_min2_nochp_biom(r,t)
gen_min2_nochp_lign(r,t)
gen_min2_nochp_oil(r,t)
gen_min2_nochp_coal(r,t)
* NOCHP min/max issues
gen_minmax2_nochp_biom(r,t)
;

loop(toptimize(t),
* Other technologies
gen_min2_nucl(r,t)       = min(gen_af2_nucl(r,t), gen_nucl(r,t)) ;
gen_min2_hydr(r,t)       = min(gen_af2_hydr(r,t), gen_hydr(r,t)) ;
gen_min2_wind(r,t)       = min(gen_af2_wind(r,t), gen_wind(r,t)) ;
gen_min2_sola(r,t)       = min(gen_af2_sola(r,t), gen_sola(r,t)) ;
* Min/max issues
gen_minmax2_nucl(r,t)    = min(gen_afmax2_nucl(r,t), gen_nucl(r,t)) ;
gen_minmax2_biom(r,t)    = min(gen_afmax2_biom(r,t), gen_biom(r,t)) ;
* CHP/NOCHP
gen_min2_lign(r,t)       = min(gen_af2_lign(r,t), gen_lign(r,t)) ;
gen_min2_coal(r,t)       = min(gen_af2_coal(r,t), gen_coal(r,t)) ;
gen_min2_oil(r,t)        = min(gen_af2_oil(r,t),  gen_oil(r,t)) ;
gen_min2_biom(r,t)       = min(gen_af2_biom(r,t), gen_biom(r,t)) ;
gen_min2_ngas(r,t)       = min(gen_af2_ngas(r,t), gen_ngas(r,t)) ;
* CHP
$if not  set chpmustrun     gen_min2_chp_lign(r,t)   = min(gen_af2_chp_lign(r,t), gen_chp_lign(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_coal(r,t)   = min(gen_af2_chp_coal(r,t), gen_chp_coal(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_oil(r,t)    = min(gen_af2_chp_oil(r,t), gen_chp_oil(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_biom(r,t)   = min(gen_af2_chp_biom(r,t), gen_chp_biom(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_ngas(r,t)   = min(gen_af2_chp_ngas(r,t), gen_chp_ngas(r,t)) ;
$if      set chpmustrun     gen_min2_chp_lign(r,t)   = gen_af2_chp_lign(r,t) ;
$if      set chpmustrun     gen_min2_chp_coal(r,t)   = gen_af2_chp_coal(r,t) ;
$if      set chpmustrun     gen_min2_chp_oil(r,t)    = gen_af2_chp_oil(r,t) ;
$if      set chpmustrun     gen_min2_chp_biom(r,t)   = gen_af2_chp_biom(r,t) ;
$if      set chpmustrun     gen_min2_chp_ngas(r,t)   = gen_af2_chp_ngas(r,t) ;
* NOCHP
gen_min2_nochp_lign(r,t) = min(gen_af2_nochp_lign(r,t), gen_nochp_lign(r,t)) ;
gen_min2_nochp_coal(r,t) = min(gen_af2_nochp_coal(r,t), gen_nochp_coal(r,t)) ;
gen_min2_nochp_oil(r,t)  = min(gen_af2_nochp_oil(r,t), gen_nochp_oil(r,t)) ;
gen_min2_nochp_biom(r,t) = min(gen_af2_nochp_biom(r,t), gen_nochp_biom(r,t)) ;
gen_min2_nochp_ngas(r,t) = min(gen_af2_nochp_ngas(r,t), gen_nochp_ngas(r,t)) ;
* NOCHP Min/max issues
gen_minmax2_nochp_biom(r,t)    = min(gen_afmax_nochp_biom(r,t), gen_nochp_biom(r,t)) ;
) ;

parameter
gen_new3(genset,r,t)
gen_new3_all(genset,t)
;

$if not set minmax  gen_new3("Nucl",r,t) = gen_min2_nucl(r,t) ;
$if     set minmax  gen_new3("Nucl",r,t) = gen_minmax2_nucl(r,t) ;
gen_new3("ngas",r,t) = gen_min2_ngas(r,t) ;
$if not set minmax  gen_new3("biom",r,t) = gen_min2_biom(r,t) ;
$if     set minmax  gen_new3("biom",r,t) = gen_minmax2_biom(r,t) ;
gen_new3("Lign",r,t) = gen_min2_lign(r,t) ;
gen_new3("Oil",r,t) = gen_min2_oil(r,t) ;
gen_new3("Coal",r,t) = gen_min2_coal(r,t) ;
gen_new3("Wind",r,t) = gen_min2_wind(r,t) ;
gen_new3("Sola",r,t) = gen_min2_sola(r,t) ;
gen_new3("Hydr",r,t) = gen_hydr(r,t) ;
gen_new3("Pump",r,t) = gen_pump(r,t) ;
gen_new3("Resv",r,t) = gen_resv(r,t) ;

gen_new3("ngas_chp",r,t) = gen_min2_chp_ngas(r,t) ;
gen_new3("Biom_chp",r,t) = gen_min2_chp_biom(r,t) ;
gen_new3("Lign_chp",r,t) = gen_min2_chp_lign(r,t) ;
gen_new3("Oil_chp",r,t) = gen_min2_chp_oil(r,t) ;
gen_new3("Coal_chp",r,t) = gen_min2_chp_coal(r,t) ;
gen_new3("ngas_nochp",r,t) = gen_min2_nochp_ngas(r,t) ;
$if not set minmax  gen_new3("Biom_nochp",r,t) = gen_min2_nochp_biom(r,t) ;
$if     set minmax  gen_new3("Biom_nochp",r,t) = gen_minmax2_nochp_biom(r,t) ;
gen_new3("Lign_nochp",r,t) = gen_min2_nochp_lign(r,t) ;
gen_new3("Oil_nochp",r,t) = gen_min2_nochp_oil(r,t) ;
gen_new3("Coal_nochp",r,t) = gen_min2_nochp_coal(r,t) ;

gen_new3("ngas_tog",r,t) = gen_new3("ngas_nochp",r,t) + gen_new3("ngas_chp",r,t) ;
gen_new3("Biom_tog",r,t) = gen_new3("biom_nochp",r,t) + gen_new3("biom_chp",r,t) ;
gen_new3("Lign_tog",r,t) = gen_new3("lign_nochp",r,t) + gen_new3("lign_chp",r,t) ;
gen_new3("Oil_tog",r,t) = gen_new3("oil_nochp",r,t) + gen_new3("oil_chp",r,t) ;
gen_new3("Coal_tog",r,t) = gen_new3("coal_nochp",r,t) + gen_new3("coal_chp",r,t) ;

gen_new3("daref",r,t) = daref(r,t) ;
gen_new3("daref_loss",r,t) = daref(r,t) * (1 + lossave(r,t)) ;

gen_new3("sum",r,t) = sum(nor(genset), gen_new3(genset,r,t)) ;
gen_new3("sum_dif",r,t) = sum(genset_chp(genset), gen_new3(genset,r,t)) + sum(genset_nochp(genset), gen_new3(genset,r,t)) + sum(res(genset), gen_new3(genset,r,t)) ;
gen_new3("sum_norchp",r,t) = sum(norchp(genset), gen_new3(genset,r,t)) ;
gen_new3("sum_chp_nochp",r,t) = sum(genset_chp(genset), gen_new3(genset,r,t)) + sum(genset_nochp(genset), gen_new3(genset,r,t)) ;
gen_new3("sum_chp",r,t) = sum(genset_chp(genset), gen_new3(genset,r,t)) ;
gen_new3("sum_nochp",r,t) = sum(genset_nochp(genset), gen_new3(genset,r,t)) ;

gen_new3_all(genset,t) = sum(r, gen_new3(genset,r,t)) ;

Set
comset /new1,new2,new3/
;
Parameter
gen_comp(comset,genset,r,t)
gen_comp_all(comset,genset,t)
;

gen_comp("new1",genset,r,t) = gen_new(genset,r,t) ;
gen_comp("new2",genset,r,t) = gen_new2(genset,r,t) ;
gen_comp("new3",genset,r,t) = gen_new3(genset,r,t) ;
gen_comp_all(comset,genset,t) = sum(r, gen_comp(comset,genset,r,t)) ;