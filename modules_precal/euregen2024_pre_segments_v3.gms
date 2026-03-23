* * * Timeseries and calibration
set
s                                Segments
sm(s,m)                          Map between segments and months for assembling availability factors
hmaps(h,s)                       Map between hours and segments (which segment is for which real hour)
srep(s,h)                        Map from segments to representative (chosen) hours
peak(s,r,t)                      Peak segment
;

$gdxin database\setpar_%n%.gdx
$load s,srep,sm,hmaps
$gdxin

alias(s,ss) ;

parameter
hours(s)                                Number of hours per load segment
number                                  Number of segments
* Load
load(s,r,t)                             Base year load across segments including both retail and direct (GW) (corrected)
load_s(s,r,t)                           Base year load across segments including both retail and direct (GW) (uncorrected)
loadcorr(r,t)                           Correction of load to meet demand
loadmax(r,t) 
dref(r,t)                               Indexed reference growth path
daref(r,t)                              Reference annual demand by region over time (TWh)
* Sectoral load
load_sec(s,r,t,l)
load_sec_s(s,r,t,l)
loadcorr_sec(r,t,l)
dref_sec(r,t,l)
daref_sec(r,t,l)
* Variable renewables of old vintages
vrsc_exi(s,i,r,t)               Capacity factor for existing resources
vrsc_exi_s(s,i,r,t)             Capacity factor for existing resources
vrsccorr_exi(i,r,t)             Correction of wind to meet full-load hours
* Variable renewables of new vintages
vrsc(s,i,v,r)                           Capacity factor for variable resources (techboost yes no)
vrsc_s(s,i,v,r)                         Capacity factor for variable resources (uncorrected)
vrsccorr(i,v,r)                         Correction of wind to meet full-load hours
* Distribution grid losses
loss(s,r,t)
loss_s(s,r,t)
losscorr(r,t)
lossave(r,t)
* Full-load hours
irnwflh_h(i,v,r)                        Intermittent renewables full-load hours (hours)
irnwflh_s(i,v,r)                        Intermittent renewables full-load hours (segments)
irnwflh_exi_h(i,r,t)                        Intermittent renewables full-load hours (hours)
irnwflh_exi_s(i,r,t)                        Intermittent renewables full-load hours (segments)
* Upper invest limit
irnwlimUP_quantiles(i,r,quantiles)      Upper limit per quantile
* Inflows
inflowtot_h(j,v,r,t)
inflow_s(s,j,v,r,t)
inflow(s,j,v,r,t)
inflowtot_s(j,v,r,t)
inflowcorr(j,v,r,t)
inflowtot_nv_h(j,r,t)
inflow_nv_s(s,j,r,t)
inflow_nv(s,j,r,t)
inflowtot_nv_s(j,r,t)
inflowcorr_nv(j,r,t)
* Load correction
load_int(s,r,t)
daref_int(r,t)
daref_org(r,t)
;

$if not  set days $gdxin database\setpar_%n%.gdx
$load hours
$load load_s=load, loadcorr, dref, daref
$load load_sec_s=load_sec, loadcorr_sec, daref_sec, dref_sec 
$load vrsc_s=vrsc, vrsc_exi_s=vrsc_exi, vrsccorr, vrsccorr_exi, irnwflh_h, irnwflh_s, irnwflh_exi_h, irnwflh_exi_s, number, irnwlimUP_quantiles
$load loss_s=loss, losscorr, lossave
$load inflowtot_h, inflowtot_s, inflow_s=inflow, inflowcorr
$load inflowtot_nv_h, inflowtot_nv_s, inflow_nv_s=inflow_nv, inflowcorr_nv
$gdxin

Parameter
dareftest(r,t)
dareftest_sec(r,t,l)
irnwflhtest(i,v,r)                     
irnwflhtest_exi(i,r,t)
;

dareftest(r,t) = sum((s), hours(s) * load_s(s,r,t)) * 1e-3 ;
dareftest_sec(r,t,l) = sum((s), hours(s) * load_sec_s(s,r,t,l)) * 1e-3 ;
irnwflhtest(i,v,r) = sum((s), hours(s) * vrsc_s(s,i,v,r)) ;
irnwflhtest_exi(i,r,t) = sum((s), hours(s) * vrsc_exi_s(s,i,r,t)) ;

*loadcorr(r,t)$(dareftest(r,t) > 0) = daref(r,t) / dareftest(r,t) ;
*loadcorr_sec(r,t,l)$(dareftest_sec(r,t,l) > 0) = daref_sec(r,t,l) / dareftest_sec(r,t,l) ;
*vrsccorr(i,v,r)$(irnwflhtest(i,v,r) > 0) = irnwflh_h(i,v,r) / irnwflhtest(i,v,r) ;
*vrsccorr_exi(i,r,t)$(irnwflhtest_exi(i,r,t) > 0) = irnwflh_exi_h(i,r,t) / irnwflhtest_exi(i,r,t) ;


* Correct time series to match annual load and full-load hours of renewables
$if      set corr_peak  vrsc(s,i,v,r)                                               = round(min(vrsccorr(i,v,r) * vrsc_s(s,i,v,r),1), 4) ;
$if      set corr_peak  vrsc_exi(s,i,r,t)                                           = round(min(vrsccorr_exo(i,r,t) * vrsc_exi_s(s,i,r,t),1), 4) ;

$if      set corr_full  vrsc(s,i,v,r)$(vrsccorr(i,v,r) > 0)                         = round(vrsccorr(i,v,r) * vrsc_s(s,i,v,r), 4) ;
$if      set corr_full  vrsc_exi(s,i,r,t)$(vrsccorr_exi(i,r,t) > 0)                 = round(vrsccorr_exi(i,r,t) * vrsc_exi_s(s,i,r,t), 4) ;
$if      set corr_full  load(s,r,t)                                                 = round(loadcorr(r,t) * load_s(s,r,t), 4) ;
$if      set corr_full  load_sec(s,r,t,l)                                           = round(loadcorr_sec(r,t,l) * load_sec_s(s,r,t,l), 4) ;
$if      set corr_full  loss(s,r,t)                                                 = round(losscorr(r,t) * loss_s(s,r,t), 4) ;
$if      set corr_full  inflow(s,j,v,r,t)                                           = round(inflowcorr(j,v,r,t) * inflow_s(s,j,v,r,t), 4) ;
$if      set corr_full  inflow_nv(s,j,r,t)                                          = round(inflowcorr_nv(j,r,t) * inflow_nv_s(s,j,r,t), 4) ;

$if not  set corr_full $if not  set corr_peak    vrsc(s,i,v,r)                       = round(vrsc_s(s,i,v,r), 4) ;
$if not  set corr_full $if not  set corr_peak    vrsc_exi(s,i,r,t)                   = round(vrsc_exi_s(s,i,r,t), 4) ;
$if not  set corr_full $if not  set corr_peak    load(s,r,t)                         = round(load_s(s,r,t), 4) ;
$if not  set corr_full                          loss(s,r,t)                         = round(loss_s(s,r,t), 4) ;
$if not  set corr_full                          inflow(s,j,v,r,t)                   = round(inflow_s(s,j,v,r,t), 4) ;
$if not  set corr_full                          inflow_nv(s,j,r,t)                  = round(inflow_nv_s(s,j,r,t), 4) ;

* Correct PumpStorage inflow to avoid trouble
inflow(s,j,v,r,t)$(not sameas(j,"Reservoir")) = 0 ;
inflow_nv(s,j,r,t)$(not sameas(j,"Reservoir")) = 0 ;
inflowtot_s(j,v,r,t)$(not sameas(j,"Reservoir")) = 0 ;
inflowtot_nv_s(j,r,t)$(not sameas(j,"Reservoir")) = 0 ;

inflow(s,jres(j),v,r,topt2024(t)) = inflow(s,j,v,r,"2020")/2  + inflow(s,j,v,r,"2021")/2 ;
inflow_nv(s,jres(j),r,topt2024(t)) = inflow_nv(s,j,r,"2020")/2  + inflow_nv(s,j,r,"2021")/2 ;
$if     set hydronormal inflow_nv(s,jres(j),r,topt2023(t)) = inflow_nv(s,j,r,"2020")/2  + inflow_nv(s,j,r,"2021")/2 ;
$if     set hydronormal inflow(s,jres(j),v,r,topt2023(t)) = inflow(s,j,v,r,"2020")/2  + inflow(s,j,v,r,"2021")/2 ;

* Correct irish load profile
parameter
daref_ire(r,t)
;

daref_ire(r,t)$(sameas(r,"Ireland")) = daref(r,t) ;
daref_ire(r,topt2023(t))$(sameas(r,"Ireland")) = daref(r,"2021") * sum(rr$(not sameas(rr,"Ireland")), daref(rr,t)) / sum(rr$(not sameas(rr,"Ireland")), daref(rr,"2021")) ;
daref_ire(r,t)$(t.val ge 2024 and t.val < 2050 and sameas(r,"Ireland")) = daref_ire(r,"2023") + (daref_ire(r,"2050") - daref_ire(r,"2023")) * (t.val - 2023) / (2050 - 2023) ;

load(s,r,"2022")$(sameas(r,"Ireland")) = round(load(s,r,"2021") * daref_ire(r,"2022") / daref_ire(r,"2021"),4) ;
load(s,r,"2023")$(sameas(r,"Ireland")) = round(load(s,r,"2021") * daref_ire(r,"2023") / daref_ire(r,"2021"),4) ;
load(s,r,topt2024(t))$(sameas(r,"Ireland")) = round(load(s,r,"2021") * daref_ire(r,t) / daref_ire(r,"2021"),4) ;
daref("Ireland",t) = sum(s, hours(s) * load(s,"Ireland",t)) * 1e-3  ;

daref_org(r,t) = daref(r,t) ;
daref_int(r,t)$(t.val le 2021) = daref(r,t) ;
daref_int(r,t)$(t.val ge 2022 and t.val le 2050) = daref(r,"2021") + (daref(r,"2050") - daref(r,"2021")) * (t.val - 2021) / (2050 - 2021) ;

$if     set loadnormal  load_int(s,r,t) = load(s,r,t) ;
$if     set loadnormal  load(s,r,t) = load_int(s,r,t) * daref_int(r,t) / daref_org(r,t) ;
$if     set loadnormal  daref(r,t) = sum(s, hours(s) * load(s,r,t)) * 1e-3 ;

$if      set load2021   load(s,r,"2022") = round(load(s,r,"2021") * daref(r,"2022") / daref(r,"2021"),4) ;
$if      set load2021   load(s,r,"2023") = round(load(s,r,"2021") * daref(r,"2023") / daref(r,"2021"),4) ;
$if      set load202x   load(s,r,topt2024(t)) = round(load(s,r,"2021") * daref(r,t) / daref(r,"2021"),4) ;

Parameter
dareftest2(r,t)
dareftest2_sec(r,t,l)
irnwflhtest2(i,v,r)                     
irnwflhtest2_exi(i,r,t)
;

dareftest2(r,t) = sum((s), hours(s) * load(s,r,t)) * 1e-3 ;
dareftest2_sec(r,t,l) = sum((s), hours(s) * load_sec(s,r,t,l)) * 1e-3 ;
irnwflhtest2(i,v,r) = sum((s), hours(s) * vrsc(s,i,v,r)) ;
irnwflhtest2_exi(i,r,t) = sum((s), hours(s) * vrsc_exi(s,i,r,t)) ;

gen_new("daref",r,t) = daref(r,t) ;
gen_new("daref_loss",r,t) = daref(r,t) * (1 + lossave(r,t)) ; 

loadmax(r,t) = smax(s, load(s,r,t)) ;
peak(s,r,t) = YES$(load(s,r,t) eq loadmax(r,t)) ;

parameter
hhours(s,j) hourly weighting for storage technologies
;

hhours(s,j) = hours(s) ;
$if      set pumppeak    hhours(s,"PumpStorage") = 1 ;
 
set
sone(s)
stwo(s)
snum(s)
;

sone(s)$(sameas(s,"1")) = YES ;
stwo(s)$(s.val ge 2) = YES ;
snum(s)$(s.val eq number) = YES ;


* * * Availability factor matrix (too large to read in)
parameter
af(s,i,v,r,t)
af_int(s,i,v,r,t)
af_chp(s,i,v,r,t)
af_chp_int(s,i,v,r,t)
af_nochp(s,i,v,r,t)
af_nochp_int(s,i,v,r,t)          
afmin(s,i,v,r,t)
afmax(s,i,v,r,t)
gafmin(s,j,v,r,t)
gafmax(s,j,v,r,t)
gafminnv(s,j,r,t)
gafmaxnv(s,j,r,t)
;

* Set availability of old vintage nuclear, hydro, and bioenergy to zero 
af(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 1 ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 1 ;
af_chp(s,ivrt(chp(i),oldv(v),r,t)) = 1 ;
af_nochp(s,ivrt(nochp(i),oldv(v),r,t)) = 1 ;

afmin(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 0.01 ;
afmin(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 0.01 ;
afmax(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 1 ;
afmax(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 1 ;

* Set 2020-2023 availability to real world availability
af(s,ivrt(nuc(i),oldv(v),r,told(t))) = sum(sm(s,m), afm(m,i,r,t)) ;
$if not set chp af(s,ivrt_nochp(bio(i),oldv(v),r,told(t))) = sum(sm(s,m), afm(m,i,r,t)) ;
$if     set chp af(s,ivrt_nochp(bio(i),oldv(v),r,told(t))) = sum(sm(s,m), afm_nochp(m,i,r,t)) ;
$if     set chp af(s,ivrt(chp(i),oldv(v),r,told(t))) = sum(sm(s,m), afm_chp(m,i,r,t)) ;
* Set 2024+ availability to 2023 value
af(s,ivrt(nuc(i),oldv(v),r,topt2024(t))) = sum(sm(s,m), afm(m,i,r,"2023")) ;
$if not set chp af(s,ivrt(bio(i),oldv(v),r,topt2024(t))) = sum(sm(s,m), afm(m,i,r,"2023")) ;
$if     set chp af(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = sum(sm(s,m), afm_nochp(m,i,r,"2023")) ;
$if     set chp af(s,ivrt(chp(i),oldv(v),r,topt2024(t))) = sum(sm(s,m), afm_chp(m,i,r,"2023")) ;
* Set min
$if     set minmax  afmin(s,ivrt(nuc(i),oldv(v),r,told(t)))           = min(sum(sm(s,m), afmax_m(m,i,r,t)), sum(sm(s,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmin(s,ivrt_nochp(bio(i),oldv(v),r,told(t)))     = min(sum(sm(s,m), afmax_m(m,i,r,t)), sum(sm(s,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmin(s,ivrt(nuc(i),oldv(v),r,topt2024(t)))       = afmin(s,i,v,r,"2023") ;
$if     set minmax  afmin(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmin(s,i,v,r,"2023") ;
* Set max
$if     set minmax  afmax(s,ivrt(nuc(i),oldv(v),r,told(t)))           = max(sum(sm(s,m), afmax_m(m,i,r,t)), sum(sm(s,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmax(s,ivrt_nochp(bio(i),oldv(v),r,told(t)))     = max(sum(sm(s,m), afmax_m(m,i,r,t)), sum(sm(s,m), afmin_m(m,i,r,t))) ;
$if     set minmax  afmax(s,ivrt(nuc(i),oldv(v),r,topt2024(t)))       = afmax(s,i,v,r,"2023") ;
$if     set minmax  afmax(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmax(s,i,v,r,"2023") ;
* Set nuclear and bio avai to zero when using minmax
$if     set minmax  af(s,ivrt(nuc(i),oldv(v),r,t))       = 0 ;
$if     set minmax  af(s,ivrt_nochp(bio(i),oldv(v),r,t)) = 0 ;

* Set storage min max availabilities
gafmin(s,jvrt(jpump(j),v,r,t)) = sum(sm(s,m), gafmin_m(m,j,r,t)) ;
gafmin(s,jvrt(jres(j),v,r,t))  = sum(sm(s,m), gafmin_m(m,j,r,t)) ;
gafmax(s,jvrt(jpump(j),v,r,t)) = sum(sm(s,m), gafmax_m(m,j,r,t)) ;
gafmax(s,jvrt(jres(j),v,r,t))  = sum(sm(s,m), gafmax_m(m,j,r,t)) ;
gafminnv(s,jpump(j),r,t)       = sum(sm(s,m), gafmin_m(m,j,r,t)) ;
gafminnv(s,jres(j),r,t)        = sum(sm(s,m), gafmin_m(m,j,r,t)) ;
gafmaxnv(s,jpump(j),r,t)       = sum(sm(s,m), gafmax_m(m,j,r,t)) ;
gafmaxnv(s,jres(j),r,t)        = sum(sm(s,m), gafmax_m(m,j,r,t)) ;

* Set 2024+ availability of German nuclear to 0
af(s,ivrt(nuc(i),oldv(v),rge(r),topt2024(t))) = 0 ;
afmax(s,ivrt(nuc(i),oldv(v),rge(r),topt2024(t))) = 0 ;
afmin(s,ivrt(nuc(i),oldv(v),rge(r),topt2024(t))) = 0 ;
* Set 2024+ availability of French nuclear to 2021 value
af(s,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t)))    = sum(sm(s,m), afm(m,i,r,"2021")) ;
afmax(s,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sm(s,m), afmax_m(m,i,r,"2021")) ;
afmin(s,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sm(s,m), afmin_m(m,i,r,"2021")) ;
* Set 2022-2023 availability of French nuclear to 2021 value(as what-if-not-case)
$if      set frnucnormal     af(s,ivrt(nuc(i),oldv(v),rfr(r),tcri(t)))    = sum(sm(s,m), afm(m,i,r,"2021")) ;
$if      set frnucnormal     afmax(s,ivrt(nuc(i),oldv(v),rfr(r),tcri(t))) = sum(sm(s,m), afmax_m(m,i,r,"2021")) ;
$if      set frnucnormal     afmin(s,ivrt(nuc(i),oldv(v),rfr(r),tcri(t))) = sum(sm(s,m), afmin_m(m,i,r,"2021")) ;
* Set 2024+ French nuclear availability as in 2023 as what-if-still-case
$if      set frnucperman     af(s,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t)))    = sum(sm(s,m), afm(m,i,r,"2023")) ;
$if      set frnucperman     afmax(s,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sm(s,m), afmax_m(m,i,r,"2023")) ;
$if      set frnucperman     afmin(s,ivrt(nuc(i),oldv(v),rfr(r),topt2024(t))) = sum(sm(s,m), afmin_m(m,i,r,"2023")) ;
* Set 2024+ hydro to 2020-2021 average
$if      set hydronormal     vrsc_exi(s,i,r,t)$(sameas(i,"Hydro") and t.val ge 2022 and t.val le 2023) = vrsc_exi(s,i,r,"2020")/2 + vrsc_exi(s,i,r,"2021")/2 ;
vrsc_exi(s,i,r,t)$(sameas(i,"Hydro") and t.val ge 2024) = vrsc_exi(s,i,r,"2020")/2 + vrsc_exi(s,i,r,"2021")/2 ;
* Set 2024+ hydro availability as in 2023 as what-if-still-case
$if      set hydroperman     vrsc_exi(s,i,r,t)$(sameas(i,"Hydro") and t.val ge 2024) = vrsc_exi(s,i,r,"2023") ;
* Set 2023+ German nuclear availability to 0 (business-as-usual)
$if      set gernucnormal   af(s,ivrt("Nuclear","1990","Germany",t))$(t.val ge 2023) = 0 ;
$if      set gernucnormal   afmax(s,ivrt("Nuclear","1990","Germany",t))$(t.val ge 2023) = 0 ;
$if      set gernucnormal   afmin(s,ivrt("Nuclear","1990","Germany",t))$(t.val ge 2023) = 0 ;
* Set 2023+ German nuclear availability to 2020-2021 average (extension)
$if      set extension      af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m), afm(m,i,r,"2020")/2 + afm(m,i,r,"2021")/2) ;
$if      set extension      afmax(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m), afmax_m(m,i,r,"2020")/2 + afmax_m(m,i,r,"2021")/2) ;
$if      set extension      afmin(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m), afmin_m(m,i,r,"2020")/2 + afmin_m(m,i,r,"2021")/2) ;
* Set 2023+ German nuclear availability to real (stretching) until Aug 23 and to 2020-2021 average from Sep 23 onwards (stretchtension)
$if      set strext         af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and t.val le 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m)$(m.val le 8),  afm(m,i,r,t)) +  eps ;
$if      set strext         af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2024 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m)$(m.val ge 9), afm(m,i,r,"2020")/2 + afm(m,i,r,"2021")/2) ;
$if      set strext         afmax(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and t.val le 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m)$(m.val le 8),  afmax_m(m,i,r,t)) +  eps ;
$if      set strext         afmax(s,ivrt(i,oldv(v),r,t))$(t.val ge 2024 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m)$(m.val ge 9), afmax_m(m,i,r,"2020")/2 + afmax_m(m,i,r,"2021")/2) ;                     
$if      set strext         afmin(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023 and t.val le 2023 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m)$(m.val le 8),  afmin_m(m,i,r,t)) +  eps ;
$if      set strext         afmin(s,ivrt(i,oldv(v),r,t))$(t.val ge 2024 and sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = sum(sm(s,m)$(m.val ge 9), afmin_m(m,i,r,"2020")/2 + afmin_m(m,i,r,"2021")/2) ; 
* 2023 afmin set to zero
afmin(s,ivrt(i,oldv(v),r,"2023"))$(sameas(v,"1990") and sameas(i,"Nuclear") and sameas(r,"Germany")) = 0 ; 
* Get rid of all availability parameter
$if      set af1            af(s,ivrt(i,v,r,t)) = 0 ;

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
gafmaxnv_int(s,j,r,t)
gafminnv_int(s,j,r,t)
*gafmax_int(s,j,v,r,t)
*gafmin_int(s,j,v,r,t)
gen_af_expo(r,t)
gen_co_expo(r,t)
gen_af_impo(r,t)
gen_co_impo(r,t)
taf_expo(r,t)
taf_impo(r,t)
taf_expo_int(r,t)
taf_impo_int(r,t)
;

 
gen_af_expo(r,t) = sum(rr, sum(tvrt(k,v,r,t), sum(s, hours(s) * tlifetime(k,v,r,t) * tcap(k,r,rr)))) * 1e-3 ;
gen_co_expo(r,t)$(gen_af_expo(r,t) > 0 and t.val le 2023) = expo(r,t) / gen_af_expo(r,t) ;
gen_af_impo(r,t) = sum(rr, sum(tvrt(k,v,r,t), sum(s, hours(s) * tlifetime(k,v,r,t) * tcap(k,rr,r)))) * 1e-3 ;
gen_co_impo(r,t)$(gen_af_impo(r,t) > 0 and t.val le 2023) = imp(r,t) / gen_af_impo(r,t) ;
   
taf_expo(r,t)$(gen_co_expo(r,t) > 1 and t.val le 2023) = gen_co_expo(r,t) ;
taf_impo(r,t)$(gen_co_impo(r,t) > 1 and t.val le 2023) = gen_co_impo(r,t) ;

taf_expo_int(r,t) = taf_expo(r,t) ;
taf_impo_int(r,t) = taf_impo(r,t) ;

taf_expo(r,t)$(t.val ge 2024) = taf_expo_int(r,"2024") ;
taf_impo(r,t)$(t.val ge 2024) = taf_impo_int(r,"2024") ;

$if     set taf1   taf_expo(r,t) = 1 ;
$if     set taf1   taf_impo(r,t) = 1 ;

$if     set gaf1 gafmaxnv(s,j,r,t) = 1 ;
$if     set gaf1 gafminnv(s,j,r,t) = 0 ;
*$if     set gaf1 gafmax(s,j,v,r,t) = 1 ;
*$if     set gaf1 gafmin(s,j,v,r,t) = 0 ;

*gen_afmax_pump(r,t) = sum(jvrt(jpump(j),v,r,t), sum(s, hours(s) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * (1 + (gafmaxnv(s,j,r,t)-1)$gafmaxnv(s,j,r,t)))) * 1e-3 ;
gen_afmax_resv(r,t) = sum(jvrt(jres(j),v,r,t),   sum(s, hours(s) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * (1 + (gafmaxnv(s,j,r,t)-1)$gafmaxnv(s,j,r,t)))) * 1e-3 ;
*gen_afmin_pump(r,t) = sum(jvrt(jpump(j),v,r,t), sum(s, hours(s) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * gafminnv(s,j,r,t))) * 1e-3 ;
gen_afmin_resv(r,t) = sum(jvrt(jres(j),v,r,t),   sum(s, hours(s) * glifetime(j,v,r,t) * gcapt(j,v,r,t) * dchrgpen(j,v,r) * gafminnv(s,j,r,t))) * 1e-3 ;

*gen_comax_pump(r,t)$(gen_afmax_pump(r,t) > 0 and t.val le 2023) = gen_pump(r,t) / gen_afmax_pump(r,t) ;
gen_comax_resv(r,t)$(gen_afmax_resv(r,t) > 0 and t.val le 2023) = gen_resv(r,t) / gen_afmax_resv(r,t) ;
*gen_comin_pump(r,t)$(gen_afmin_pump(r,t) > 0 and t.val le 2023) = gen_pump(r,t) / gen_afmin_pump(r,t) ;
gen_comin_resv(r,t)$(gen_afmin_resv(r,t) > 0 and t.val le 2023) = gen_resv(r,t) / gen_afmin_resv(r,t) ;

gafmaxnv_int(s,j,r,t) = gafmaxnv(s,j,r,t) ;
gafminnv_int(s,j,r,t) = gafminnv(s,j,r,t) ;

*gafmaxnv(s,j,r,t)$(sameas(j,"PumpStorage") and gen_comax_pump(r,t) > 1) = gafmaxnv_int(s,j,r,t) * gen_comax_pump(r,t) ;
gafmaxnv(s,j,r,t)$(sameas(j,"Reservoir") and gen_comax_resv(r,t) > 1) = gafmaxnv_int(s,j,r,t) * gen_comax_resv(r,t) ;

*gafminnv(s,j,r,t)$(sameas(j,"PumpStorage") and gen_comin_pump(r,t) < 1 and gen_comin_pump(r,t) > 0) = gafminnv_int(s,j,r,t) * gen_comin_pump(r,t) ;
gafminnv(s,j,r,t)$(sameas(j,"Reservoir") and gen_comin_resv(r,t) < 1 and gen_comin_resv(r,t) > 0) = gafminnv_int(s,j,r,t) * gen_comin_resv(r,t) ;
  
gafmaxnv(s,j,r,t)$(t.val ge 2024) = gafmaxnv(s,j,r,"2020")/2 + gafmaxnv(s,j,r,"2021")/2 ;
gafminnv(s,j,r,t)$(t.val ge 2024) = gafminnv(s,j,r,"2020")/2 + gafminnv(s,j,r,"2021")/2 ;

$if     set gaforg  gafmaxnv(s,j,r,t) = gafmaxnv_int(s,j,r,t) ;
$if     set gaforg  gafminnv(s,j,r,t) = gafminnv_int(s,j,r,t) ;
*$if     set gaforg  gafmax(s,j,v,r,t) = gafmax_int(s,j,v,r,t) ;
*$if     set gaforg  gafmin(s,j,v,r,t) = gafmin_int(s,j,v,r,t) ;

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
gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
* Min/max issues
gen_afmin_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
gen_afmin_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;

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
af_int(s,i,v,r,t)
af_chp_int(s,i,v,r,t)
af_nochp_int(s,i,v,r,t)
afmin_int(s,i,v,r,t)
afmax_int(s,i,v,r,t)
afmin_nochp(s,i,v,r,t)
afmax_nochp(s,i,v,r,t)
afmin_nochp_int(s,i,v,r,t)
afmax_nochp_int(s,i,v,r,t)
;

* * Availability parameter correction 
* Interim availability
af_int(s,ivrt(i,oldv(v),r,t))               = af(s,i,v,r,t) ;
af_chp_int(s,ivrt_chp(i,oldv(v),r,t))       = af_chp(s,i,v,r,t) ;
af_nochp_int(s,ivrt(nochp(i),oldv(v),r,t))  = af_nochp(s,i,v,r,t) ;
afmin_int(s,ivrt(nuc(i),oldv(v),r,t))       = afmin(s,i,v,r,t) ;
afmax_int(s,ivrt(nuc(i),oldv(v),r,t))       = afmax(s,i,v,r,t) ;
afmin_int(s,ivrt_nochp(bio(i),oldv(v),r,t)) = afmin(s,i,v,r,t) ;
afmax_int(s,ivrt_nochp(bio(i),oldv(v),r,t)) = afmax(s,i,v,r,t) ;
* Other technologies
$if not  set nucmustrun     af(s,ivrt(nuc(i),oldv(v),r,t))$(gen_co_nucl(r,t) > 1)  = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_nucl(r,t) ;
$if      set nucmustrun     af(s,ivrt(nuc(i),oldv(v),r,t)) = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_nucl(r,t) ;
af(s,ivrt(wind(i),oldv(v),r,t))$(gen_co_wind(r,t) > 1) = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_wind(r,t) ;
af(s,ivrt(sol(i),oldv(v),r,t))$(gen_co_sola(r,t) > 1)  = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_sola(r,t) ;
af(s,ivrt(hyd(i),oldv(v),r,t))$(gen_co_hydr(r,t) > 1)  = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_hydr(r,t) ;
* Min/max issues
afmin(s,ivrt(nuc(i),oldv(v),r,t))$(gen_comin_nucl(r,t) < 1 and gen_comin_nucl(r,t) > 0)                 = afmin_int(s,i,v,r,t) * gen_comin_nucl(r,t) ;
afmax(s,ivrt(nuc(i),oldv(v),r,t))$(gen_comax_nucl(r,t) > 1)                                             = (1 + (afmax_int(s,i,v,r,t)-1)$afmax_int(s,i,v,r,t)) * gen_comax_nucl(r,t) ;
afmin(s,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comin_biom(r,t) < 1 and gen_comin_biom(r,t) > 0)           = afmin_int(s,i,v,r,t) * gen_comin_biom(r,t) ;
afmax(s,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comax_biom(r,t) > 1)                                       = (1 + (afmax_int(s,i,v,r,t)-1)$afmax_int(s,i,v,r,t)) * gen_comax_biom(r,t) ;
* CHP/NOCHP
af(s,ivrt_nochp(coa(i),oldv(v),r,t))$(gen_co_coal(r,t) > 1)   = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_coal(r,t) ;
af(s,ivrt_nochp(lig(i),oldv(v),r,t))$(gen_co_lign(r,t) > 1)   = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_lign(r,t) ;
af(s,ivrt_nochp(gas(i),oldv(v),r,t))$(gen_co_ngas(r,t) > 1)   = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_ngas(r,t) ;
af(s,ivrt_nochp(oil(i),oldv(v),r,t))$(gen_co_oil(r,t) > 1)    = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_oil(r,t) ;
$if not  set biomustrun     af(s,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_co_biom(r,t) > 1)   = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_biom(r,t) ;
$if      set biomustrun     af(s,ivrt_nochp(bio(i),oldv(v),r,t)) = (1 + (af_int(s,i,v,r,t)-1)$af_int(s,i,v,r,t)) * gen_co_biom(r,t) ;
* CHP 
$if not  set chpmustrun     af_chp(s,ivrt_chp(gas(i),oldv(v),r,t))$(gen_co_chp_ngas(r,t) > 1) = (1 + (af_chp_int(s,i,v,r,t)-1)$af_chp_int(s,i,v,r,t)) * gen_co_chp_ngas(r,t) ;
$if not  set chpmustrun     af_chp(s,ivrt_chp(coa(i),oldv(v),r,t))$(gen_co_chp_coal(r,t) > 1) = (1 + (af_chp_int(s,i,v,r,t)-1)$af_chp_int(s,i,v,r,t)) * gen_co_chp_coal(r,t) ;
$if not  set chpmustrun     af_chp(s,ivrt_chp(lig(i),oldv(v),r,t))$(gen_co_chp_lign(r,t) > 1) = (1 + (af_chp_int(s,i,v,r,t)-1)$af_chp_int(s,i,v,r,t)) * gen_co_chp_lign(r,t) ;
$if not  set chpmustrun     af_chp(s,ivrt_chp(bio(i),oldv(v),r,t))$(gen_co_chp_biom(r,t) > 1) = (1 + (af_chp_int(s,i,v,r,t)-1)$af_chp_int(s,i,v,r,t)) * gen_co_chp_biom(r,t) ;
$if not  set chpmustrun     af_chp(s,ivrt_chp(oil(i),oldv(v),r,t))$(gen_co_chp_oil(r,t) > 1) = (1 + (af_chp_int(s,i,v,r,t)-1)$af_chp_int(s,i,v,r,t)) * gen_co_chp_oil(r,t) ;
$if      set chpmustrun     af_chp(s,ivrt_chp(gas(i),oldv(v),r,t)) = af_chp_int(s,i,v,r,t) * gen_co_chp_ngas(r,t) ;
$if      set chpmustrun     af_chp(s,ivrt_chp(coa(i),oldv(v),r,t)) = af_chp_int(s,i,v,r,t) * gen_co_chp_coal(r,t) ;
$if      set chpmustrun     af_chp(s,ivrt_chp(lig(i),oldv(v),r,t)) = af_chp_int(s,i,v,r,t) * gen_co_chp_lign(r,t) ;
$if      set chpmustrun     af_chp(s,ivrt_chp(bio(i),oldv(v),r,t)) = af_chp_int(s,i,v,r,t) * gen_co_chp_biom(r,t) ;
$if      set chpmustrun     af_chp(s,ivrt_chp(oil(i),oldv(v),r,t)) = af_chp_int(s,i,v,r,t) * gen_co_chp_oil(r,t) ;
* NOCHP
af_nochp(s,ivrt_nochp(gas(i),oldv(v),r,t))$(gen_co_nochp_ngas(r,t) > 1) = (1 + (af_nochp_int(s,i,v,r,t)-1)$af_nochp_int(s,i,v,r,t)) * gen_co_nochp_ngas(r,t) ;
af_nochp(s,ivrt_nochp(coa(i),oldv(v),r,t))$(gen_co_nochp_coal(r,t) > 1) = (1 + (af_nochp_int(s,i,v,r,t)-1)$af_nochp_int(s,i,v,r,t)) * gen_co_nochp_coal(r,t) ;
af_nochp(s,ivrt_nochp(lig(i),oldv(v),r,t))$(gen_co_nochp_lign(r,t) > 1) = (1 + (af_nochp_int(s,i,v,r,t)-1)$af_nochp_int(s,i,v,r,t)) * gen_co_nochp_lign(r,t) ;
$if not  set biomustrun     af_nochp(s,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_co_nochp_biom(r,t) > 1) = (1 + (af_nochp_int(s,i,v,r,t)-1)$af_nochp_int(s,i,v,r,t)) *  gen_co_nochp_biom(r,t) ;
$if      set biomustrun     af_nochp(s,ivrt_nochp(bio(i),oldv(v),r,t)) = (1 + (af_nochp_int(s,i,v,r,t)-1)$af_nochp_int(s,i,v,r,t)) *  gen_co_nochp_biom(r,t) ;
af_nochp(s,ivrt_nochp(oil(i),oldv(v),r,t))$(gen_co_nochp_oil(r,t) > 1) = (1 + (af_nochp_int(s,i,v,r,t)-1)$af_nochp_int(s,i,v,r,t)) * gen_co_nochp_oil(r,t) ;
* NOCHP min/max issues
afmin_nochp(s,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comin_nochp_biom(r,t) < 1 and gen_comin_nochp_biom(r,t) > 0)  = afmin_int(s,i,v,r,t) * gen_comin_nochp_biom(r,t) ;
afmax_nochp(s,ivrt_nochp(bio(i),oldv(v),r,t))$(gen_comax_nochp_biom(r,t) > 1)                              = (1 + (afmax_int(s,i,v,r,t)-1)$afmax_int(s,i,v,r,t)) * gen_comax_nochp_biom(r,t) ;

* * 2024+ availability
* Interim availability
af_int(s,ivrt(i,oldv(v),r,t))               = af(s,i,v,r,t) ;
af_chp_int(s,ivrt_chp(i,oldv(v),r,t))       = af_chp(s,i,v,r,t) ;
af_nochp_int(s,ivrt(nochp(i),oldv(v),r,t))  = af_nochp(s,i,v,r,t) ;
afmin_int(s,ivrt(nuc(i),oldv(v),r,t))       = afmin(s,i,v,r,t) ;
afmax_int(s,ivrt(nuc(i),oldv(v),r,t))       = afmax(s,i,v,r,t) ;
afmin_int(s,ivrt_nochp(bio(i),oldv(v),r,t)) = afmin(s,i,v,r,t) ;
afmax_int(s,ivrt_nochp(bio(i),oldv(v),r,t)) = afmax(s,i,v,r,t) ;
afmin_nochp_int(s,ivrt_nochp(bio(i),oldv(v),r,t)) = afmin_nochp(s,i,v,r,t) ;
afmax_nochp_int(s,ivrt_nochp(bio(i),oldv(v),r,t)) = afmax_nochp(s,i,v,r,t) ;
* Availability 2024+
af(s,ivrt(i,oldv(v),r,topt2024(t)))                     = af_int(s,i,v,r,"2023") ;
$if not set frnucnormal af(s,ivrt(nuc(i),oldv(v),"France",topt2024(t)))         = af_int(s,i,v,"France","2023") * 393.134 / 354.105 ;
af_chp(s,ivrt_chp(i,oldv(v),r,topt2024(t)))             = af_chp_int(s,i,v,r,"2023") ;
af_nochp(s,ivrt(nochp(i),oldv(v),r,topt2024(t)))        = af_nochp_int(s,i,v,r,"2023") ;
afmin(s,ivrt(nuc(i),oldv(v),r,topt2024(t)))             = afmin_int(s,i,v,r,"2023") ;
afmax(s,ivrt(nuc(i),oldv(v),r,topt2024(t)))             = afmax_int(s,i,v,r,"2023") ;
$if not set frnucnormal afmax(s,ivrt(nuc(i),oldv(v),"France",topt2024(t)))         = afmax_int(s,i,v,"France","2023") * 393.134 / 354.105 ;
afmin(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t)))       = afmin_int(s,i,v,r,"2023") ;
afmax(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t)))       = afmax_int(s,i,v,r,"2023") ;
afmin_nochp(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmin_nochp_int(s,i,v,r,"2023") ;
afmax_nochp(s,ivrt_nochp(bio(i),oldv(v),r,topt2024(t))) = afmax_nochp_int(s,i,v,r,"2023") ;

* * * Final calculation of gen_af
* * Until 2023
* Other technologies
* * Maximum available generation given availability, vrsc, and reliability as well as existing capacity (for oldv only)
* Other technologies
loop(tcri(t),
$if not  set nucmustrun     gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set nucmustrun     gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
* Min/max issues
gen_afmin_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
gen_afmin_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_nochp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_nochp(s,i,v,r,t))) * 1e-3 ;
gen_afmax_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_nochp(s,i,v,r,t)-1)$afmax_nochp(s,i,v,r,t)))) * 1e-3 ;
) ;

* * 2024+
loop(topt2024(t),
$if not  set nucmustrun     gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set nucmustrun     gen_af_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
* Min/max issues
gen_afmin_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
gen_afmin_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_nochp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_nochp(s,i,v,r,t))) * 1e-3 ;
gen_afmax_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_nochp(s,i,v,r,t)-1)$afmax_nochp(s,i,v,r,t)))) * 1e-3 ;
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
$if not  set nucmustrun     gen_min_nucl(r,t)       = min(gen_af_nucl(r,t), gen_nucl(r,t)) ;
$if      set nucmustrun     gen_min_nucl(r,t)       = gen_af_nucl(r,t) ;
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
$if not  set biomustrun     gen_min_biom(r,t)       = min(gen_af_biom(r,t), gen_biom(r,t)) ;
$if      set biomustrun     gen_min_biom(r,t)       = gen_af_biom(r,t) ;
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
$if not  set biomustrun     gen_min_nochp_biom(r,t) = min(gen_af_nochp_biom(r,t), gen_nochp_biom(r,t)) ;
$if      set biomustrun     gen_min_nochp_biom(r,t) = gen_af_nochp_biom(r,t) ;
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
af_int(s,ivrt(i,v,r,t)) = af(s,i,v,r,t) ;
afmax_int(s,ivrt(i,v,r,t)) = afmax(s,i,v,r,t) ;
afmin_int(s,ivrt(i,v,r,t)) = afmin(s,i,v,r,t) ;
af(s,i,v,r,t) = 0 ;
afmax(s,i,v,r,t) = 0 ;
afmin(s,i,v,r,t) = 0 ;
* Final parameters
af(s,ivrt(i,oldv(v),r,toptimize(t)))$(af_int(s,i,v,r,t) > 1)                            = round(af_int(s,i,v,r,t),8) ;
$if     set biomustrun     af(s,ivrt(bio(i),oldv(v),r,toptimize(t)))$(af_int(s,i,v,r,t) >= 0)  = round(af_int(s,i,v,r,t),8) ;
$if     set nucmustrun     af(s,ivrt(nuc(i),oldv(v),r,toptimize(t)))$(af_int(s,i,v,r,t) >= 0)  = round(af_int(s,i,v,r,t),8) ;
$if     set chp af(s,ivrt(nochp(i),oldv(v),r,toptimize(t)))$(af_nochp(s,i,v,r,t) > 1)   = round(af_nochp(s,i,v,r,t),8) ;
$if     set chp $if not  set chpmustrun     af(s,ivrt(chp(i),oldv(v),r,toptimize(t)))$(af_chp(s,i,v,r,t) > 1)       = round(af_chp(s,i,v,r,t),8) ;
$if     set chp $if      set chpmustrun     af(s,ivrt(chp(i),oldv(v),r,toptimize(t)))$(af_chp(s,i,v,r,t) >= 0)      = round(af_chp(s,i,v,r,t),8) ;
$if not set chp af(s,ivrt(chp(i),oldv(v),r,toptimize(t))) = 0 ;
afmax(s,ivrt(nuc(i),oldv(v),r,toptimize(t)))                                            = round(afmax_int(s,i,v,r,t),8) ;
afmin(s,ivrt(nuc(i),oldv(v),r,toptimize(t)))                                            = round(afmin_int(s,i,v,r,t),8) ;
$if     set chp afmax(s,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmax_nochp(s,i,v,r,t),8) ;
$if     set chp afmin(s,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmin_nochp(s,i,v,r,t),8) ;
$if not set chp afmax(s,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmax_int(s,i,v,r,t),8) ;
$if not set chp afmin(s,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))                      = round(afmin_int(s,i,v,r,t),8) ;

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
$if not  set nucmustrun     gen_af2_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set nucmustrun     gen_af2_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af2_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
gen_af2_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,t)) * 1e-3 ;
* Min/max issues
gen_afmin2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
gen_afmin2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af2_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af2_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af2_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af2_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af2_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af2_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * af_nochp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_nochp(s,i,v,r,t))) * 1e-3 ;
gen_afmax2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_nochp(s,i,v,r,t)-1)$afmax_nochp(s,i,v,r,t)))) * 1e-3 ;
) ;

* * 2024+
loop(topt2024(t),
$if not  set nucmustrun     gen_af2_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set nucmustrun     gen_af2_nucl(r,t)        = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_hydr(r,t)        = sum(ivrt(hyd(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af2_wind(r,t)        = sum(ivrt(wind(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
gen_af2_sola(r,t)        = sum(ivrt(sol(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * lifetime(i,v,r,t) * capt(i,v,r,"2023")) * 1e-3 ;
* Min/max issues
gen_afmin2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax2_nucl(r,t)     = sum(ivrt(nuc(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
gen_afmin2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t))) * 1e-3 ;
gen_afmax2_biom(r,t)     = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)))) * 1e-3 ;
* CHP/NOCHP
gen_af2_ngas(r,t)        = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_coal(r,t)        = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_lign(r,t)        = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af2_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af2_biom(r,t)        = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_oil(r,t)         = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* CHP
$if not  set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_chp(s,i,v,r,t)-1)$af_chp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_ngas(r,t)    = sum(ivrt_chp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_coal(r,t)    = sum(ivrt_chp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_lign(r,t)    = sum(ivrt_chp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_biom(r,t)    = sum(ivrt_chp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set chpmustrun     gen_af2_chp_oil(r,t)     = sum(ivrt_chp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_chp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP
gen_af2_nochp_ngas(r,t)  = sum(ivrt_nochp(gas(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_coal(r,t)  = sum(ivrt_nochp(coa(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_lign(r,t)  = sum(ivrt_nochp(lig(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if not  set biomustrun     gen_af2_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
$if      set biomustrun     gen_af2_nochp_biom(r,t)  = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * af_nochp(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
gen_af2_nochp_oil(r,t)   = sum(ivrt_nochp(oil(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (af_nochp(s,i,v,r,t)-1)$af_nochp(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)))) * 1e-3 ;
* NOCHP min/max issues
gen_afmin2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_nochp(s,i,v,r,t))) * 1e-3 ;
gen_afmax2_nochp_biom(r,t)   = sum(ivrt_nochp(bio(i),oldv(v),r,toptimize(t)), sum(s, hours(s) * lifetime(i,v,r,t) * capt(i,v,r,"2023") * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_nochp(s,i,v,r,t)-1)$afmax_nochp(s,i,v,r,t)))) * 1e-3 ;
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
$if not  set nucmustrun     gen_min2_nucl(r,t)       = min(gen_af_nucl(r,t), gen_nucl(r,t)) ;
$if      set nucmustrun     gen_min2_nucl(r,t)       = gen_af_nucl(r,t) ;
gen_min2_hydr(r,t)       = min(gen_af_hydr(r,t), gen_hydr(r,t)) ;
gen_min2_wind(r,t)       = min(gen_af_wind(r,t), gen_wind(r,t)) ;
gen_min2_sola(r,t)       = min(gen_af_sola(r,t), gen_sola(r,t)) ;
* Min/max issues
gen_minmax2_nucl(r,t)    = min(gen_afmax_nucl(r,t), gen_nucl(r,t)) ;
gen_minmax2_biom(r,t)    = min(gen_afmax_biom(r,t), gen_biom(r,t)) ;
* CHP/NOCHP
gen_min2_lign(r,t)       = min(gen_af_lign(r,t), gen_lign(r,t)) ;
gen_min2_coal(r,t)       = min(gen_af_coal(r,t), gen_coal(r,t)) ;
gen_min2_oil(r,t)        = min(gen_af_oil(r,t),  gen_oil(r,t)) ;
$if not  set biomustrun     gen_min2_biom(r,t)       = min(gen_af_biom(r,t), gen_biom(r,t)) ;
$if      set biomustrun     gen_min2_biom(r,t)       = gen_af_biom(r,t) ;
gen_min2_ngas(r,t)       = min(gen_af_ngas(r,t), gen_ngas(r,t)) ;
* CHP
$if not  set chpmustrun     gen_min2_chp_lign(r,t)   = min(gen_af_chp_lign(r,t), gen_chp_lign(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_coal(r,t)   = min(gen_af_chp_coal(r,t), gen_chp_coal(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_oil(r,t)    = min(gen_af_chp_oil(r,t), gen_chp_oil(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_biom(r,t)   = min(gen_af_chp_biom(r,t), gen_chp_biom(r,t)) ;
$if not  set chpmustrun     gen_min2_chp_ngas(r,t)   = min(gen_af_chp_ngas(r,t), gen_chp_ngas(r,t)) ;
$if      set chpmustrun     gen_min2_chp_lign(r,t)   = gen_af_chp_lign(r,t) ;
$if      set chpmustrun     gen_min2_chp_coal(r,t)   = gen_af_chp_coal(r,t) ;
$if      set chpmustrun     gen_min2_chp_oil(r,t)    = gen_af_chp_oil(r,t) ;
$if      set chpmustrun     gen_min2_chp_biom(r,t)   = gen_af_chp_biom(r,t) ;
$if      set chpmustrun     gen_min2_chp_ngas(r,t)   = gen_af_chp_ngas(r,t) ;
* NOCHP
gen_min2_nochp_lign(r,t) = min(gen_af_nochp_lign(r,t), gen_nochp_lign(r,t)) ;
gen_min2_nochp_coal(r,t) = min(gen_af_nochp_coal(r,t), gen_nochp_coal(r,t)) ;
gen_min2_nochp_oil(r,t)  = min(gen_af_nochp_oil(r,t), gen_nochp_oil(r,t)) ;
$if not  set biomustrun     gen_min2_nochp_biom(r,t) = min(gen_af_nochp_biom(r,t), gen_nochp_biom(r,t)) ;
$if      set biomustrun     gen_min2_nochp_biom(r,t) = gen_af_nochp_biom(r,t) ;
gen_min2_nochp_ngas(r,t) = min(gen_af_nochp_ngas(r,t), gen_nochp_ngas(r,t)) ;
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