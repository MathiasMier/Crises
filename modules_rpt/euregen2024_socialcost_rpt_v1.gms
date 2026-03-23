* * * Social cost reporting
Parameters
ap_ivrt(ap,tyrpt,v,r,t)                      Air pollution by ivrt   (Mt)
ap_irt(ap,tyrpt,r,t)                         Air pollution by irt    (Mt)
ap_rt(ap,r,t)                            Air pollution by rt     (Mt)
ap_it(ap,tyrpt,t)                            Air pollution by it     (Mt)
ap_t(ap,t)                               Air pollution by t      (Mt)
ap_i_acc(ap,tyrpt)                           Air pollution acc by i  (Mt)
ap_r_acc(ap,r)                           Air pollution acc by r  (Mt)
ap_acc(ap)                               Air pollution acc       (Mt)

scap_impactap_ivrt(impactap,tyrpt,v,r,t)     Social cost of air pollution by ivrt (billion EUR)
scap_impactap_irt(impactap,tyrpt,r,t)        Social cost of air pollution by irt  (billion EUR)
scap_impactap_rt(impactap,r,t)           Social cost of air pollution by rt   (billion EUR)
scap_impactap_it(impactap,tyrpt,t)           Social cost of air pollution by it   (billion EUR)
scap_impactap_t(impactap,t)              Social cost of air pollution by t    (billion EUR)
scap_impactap_i_acc(impactap,tyrpt)          Social cost of air pollution acc by i(billion EUR)
scap_impactap_r_acc(impactap,r)          Social cost of air pollution acc by r(billion EUR)
scap_impactap_acc(impactap)              Social cost of air pollution acc     (billion EUR)

scap_ap_ivrt(ap,tyrpt,v,r,t)                 Social cost of air pollution by ivrt (billion EUR)
scap_ap_irt(ap,tyrpt,r,t)                    Social cost of air pollution by irt  (billion EUR)
scap_ap_rt(ap,r,t)                       Social cost of air pollution by rt   (billion EUR)
scap_ap_it(ap,tyrpt,t)                       Social cost of air pollution by it   (billion EUR)
scap_ap_t(ap,t)                          Social cost of air pollution by t    (billion EUR)
scap_ap_i_acc(ap,tyrpt)                      Social cost of air pollution acc by i(billion EUR)
scap_ap_r_acc(ap,r)                      Social cost of air pollution acc by r(billion EUR)
scap_ap_acc(ap)                          Social cost of air pollution acc     (billion EUR)

scap_ivrt(tyrpt,v,r,t)                       Social cost of air pollution by ivrt (billion EUR)
scap_irt(tyrpt,r,t)                          Social cost of air pollution by irt  (billion EUR)
scap_rt(r,t)                             Social cost of air pollution by rt   (billion EUR)
scap_it(tyrpt,t)                             Social cost of air pollution by it   (billion EUR)
scap_t(t)                                Social cost of air pollution by t    (billion EUR)
scap_i_acc(tyrpt)                            Social cost of air pollution acc by i(billion EUR)
scap_r_acc(r)                            Social cost of air pollution acc by r(billion EUR)
scap_acc                                 Social cost of air pollution acc     (billion EUR)
*scap_effect(ap,r,t)                      Effective social cost of air pollution (in model in EUR per t)

co2_ivrt(tyrpt,v,r,t)
co2_irt(tyrpt,r,t)
co2_rt(r,t)
co2_it(tyrpt,t)
co2_t(t)
co2_i_acc(tyrpt)
co2_r_acc(r)
co2_acc

scc_ivrt(tyrpt,v,r,t)
scc_irt(tyrpt,r,t)
scc_rt(r,t)
scc_it(tyrpt,t)
scc_t(t)
scc_i_acc(tyrpt)
scc_r_acc(r)
scc_acc
*scc_effect(t)                         Effective social cost of air pollution (in model in EUR per t)

Airpollution_rt_rpt(t,r,*)
Airpollution_it_rpt(t,tyrpt,*)
Airpollution_t_rpt(t,*)
Airpollution_r_acc_rpt(r,*)
Airpollution_i_acc_rpt(tyrpt,*)
Airpollution_acc_rpt(*)

Socialcost_rt_rpt(t,r,*)
Socialcost_it_rpt(t,tyrpt,*)
Socialcost_t_rpt(t,*)
Socialcost_r_acc_rpt(r,*)
Socialcost_i_acc_rpt(tyrpt,*)
Socialcost_acc_rpt(*)

Socialcost_impact_rt_rpt(t,r,*)
Socialcost_impact_it_rpt(t,tyrpt,*)
Socialcost_impact_t_rpt(t,*)
Socialcost_impact_r_acc_rpt(r,*)
Socialcost_impact_i_acc_rpt(tyrpt,*)
Socialcost_impact_acc_rpt(*)


SCCtot_rt_rpt(t,r)
SCCtot_it_rpt(t,tyrpt)
SCCspe_rt_rpt(t,r)
SCCspe_it_rpt(t,tyrpt)

SCAPtot_rt_rpt(t,r)
SCAPtot_it_rpt(t,tyrpt)
SCAPspe_rt_rpt(t,r)
SCAPspe_it_rpt(t,tyrpt)

Socialtotcost_rt_rpt(t,r)
Socialtotcost_it_rpt(t,tyrpt)
Socialspecost_rt_rpt(t,r)
Socialspecost_it_rpt(t,tyrpt)

SCCspe_t_rpt(t)
SCAPspe_t_rpt(t)
Socialspecost_t_rpt(t)

disc_scc_r_acc(r)
disc_scap_r_acc(r) 

disc_scc_acc
disc_scap_acc 
;

* emitap in t/MWhel and XTWH in TWH = 10^6 t = Mt
ap_ivrt(ap,tyrpt,v,r,t)  = sum(xtyperpt(tyrpt,type), sum(idef(i,type), emitap(i,ap,v,r) * XTWHL(i,v,r,t))) ;
$if set ignoreairpollution_rpt  ap_ivrt(ap,tyrpt,v,r,t) 0 =

ap_irt(ap,tyrpt,r,t)     = sum(v$(v.val le t.val),  ap_ivrt(ap,tyrpt,v,r,t)) ;
ap_rt(ap,r,t)            = sum(tyrpt,  ap_irt(ap,tyrpt,r,t)) ;
ap_it(ap,tyrpt,t)        = sum(r,  ap_irt(ap,tyrpt,r,t)) ;
ap_t(ap,t)               = sum(r,  ap_rt(ap,r,t)) ;
ap_i_acc(ap,tyrpt)       = sum(t$(t.val > 2022),  nyrs(t) * ap_it(ap,tyrpt,t)) ;
ap_r_acc(ap,r)           = sum(t$(t.val > 2022),  nyrs(t) * ap_rt(ap,r,t)) ;
ap_acc(ap)               = sum(t$(t.val > 2022),  nyrs(t) * ap_t(ap,t)) ;

* scap in EUR per t = 10^6 EUR
scap_impactap_ivrt(impactap,tyrpt,v,r,t) = sum(ap,            sum(xtyperpt(tyrpt,type), sum(idef(i,type), emitap(i,ap,v,r) * XTWHL(i,v,r,t) * scap(ap,impactap,r,t)))) * 1e-3 ;
$if set ignoreairpollution_rpt  scap_impactap_ivrt(impactap,tyrpt,v,r,t) = 0 ;

scap_ap_ivrt(ap,tyrpt,v,r,t)             = sum(impactap,      sum(xtyperpt(tyrpt,type), sum(idef(i,type), emitap(i,ap,v,r) * XTWHL(i,v,r,t) * scap(ap,impactap,r,t)))) * 1e-3 ;
scap_ivrt(tyrpt,v,r,t)                   = sum((ap,impactap), sum(xtyperpt(tyrpt,type), sum(idef(i,type), emitap(i,ap,v,r) * XTWHL(i,v,r,t) * scap(ap,impactap,r,t)))) * 1e-3 ;

scap_impactap_irt(impactap,tyrpt,r,t)    = sum(v$(v.val le t.val),       scap_impactap_ivrt(impactap,tyrpt,v,r,t)) ;
scap_ap_irt(ap,tyrpt,r,t)                = sum(v$(v.val le t.val),       scap_ap_ivrt(ap,tyrpt,v,r,t)) ;
scap_irt(tyrpt,r,t)                      = sum(v$(v.val le t.val),       scap_ivrt(tyrpt,v,r,t)) ;

scap_impactap_rt(impactap,r,t)           = sum(tyrpt,                    scap_impactap_irt(impactap,tyrpt,r,t)) ;
scap_ap_rt(ap,r,t)                       = sum(tyrpt,                    scap_ap_irt(ap,tyrpt,r,t) ) ;
scap_rt(r,t)                             = sum(tyrpt,                    scap_irt(tyrpt,r,t)) ;

scap_impactap_it(impactap,tyrpt,t)       = sum(r,                        scap_impactap_irt(impactap,tyrpt,r,t)) ;
scap_ap_it(ap,tyrpt,t)                   = sum(r,                        scap_ap_irt(ap,tyrpt,r,t)) ;
scap_it(tyrpt,t)                         = sum(r,                        scap_irt(tyrpt,r,t)) ;

scap_impactap_t(impactap,t)              = sum(tyrpt,                    scap_impactap_it(impactap,tyrpt,t)) ;
scap_ap_t(ap,t)                          = sum(tyrpt,                    scap_ap_it(ap,tyrpt,t)) ;
scap_t(t)                                = sum(tyrpt,                    scap_it(tyrpt,t)) ;

scap_impactap_i_acc(impactap,tyrpt)      = sum(t$(t.val > 2022), nyrs(t) *              scap_impactap_it(impactap,tyrpt,t)) ;
scap_ap_i_acc(ap,tyrpt)                  = sum(t$(t.val > 2022), nyrs(t) *              scap_ap_it(ap,tyrpt,t)) ;
scap_i_acc(tyrpt)                        = sum(t$(t.val > 2022), nyrs(t) *              scap_it(tyrpt,t)) ;

scap_impactap_r_acc(impactap,r)          = sum(t$(t.val > 2022), nyrs(t) *              scap_impactap_rt(impactap,r,t)) ;
scap_ap_r_acc(ap,r)                      = sum(t$(t.val > 2022), nyrs(t) *              scap_ap_rt(ap,r,t)) ;
scap_r_acc(r)                            = sum(t$(t.val > 2022), nyrs(t) *              scap_rt(r,t)) ;

scap_impactap_acc(impactap)              = sum(t$(t.val > 2022), nyrs(t) *              scap_impactap_t(impactap,t)) ;
scap_ap_acc(ap)                          = sum(t$(t.val > 2022), nyrs(t) *              scap_ap_t(ap,t)) ;
scap_acc                                 = sum(t$(t.val > 2022), nyrs(t) *              scap_t(t)) ;

co2_ivrt(tyrpt,v,r,t)                    = sum(xtyperpt(tyrpt,type), sum(idef(i,type), emit(i,v,r) * XTWHL(i,v,r,t))) ;
co2_irt(tyrpt,r,t)                       = sum(v$(v.val le t.val),       co2_ivrt(tyrpt,v,r,t)) ;
co2_rt(r,t)                              = sum(tyrpt,                    co2_irt(tyrpt,r,t)) ;
co2_it(tyrpt,t)                          = sum(r,                        co2_irt(tyrpt,r,t)) ;
co2_t(t)                                 = sum(r,                        co2_rt(r,t)) ;
co2_i_acc(tyrpt)                         = sum(t$(t.val > 2022),  nyrs(t) *             co2_it(tyrpt,t)) ;
co2_r_acc(r)                             = sum(t$(t.val > 2022),  nyrs(t) *             co2_rt(r,t)) ;
co2_acc                                  = sum(t$(t.val > 2022),  nyrs(t) *             co2_t(t)) ;

scc_ivrt(tyrpt,v,r,t)                    =                               co2_ivrt(tyrpt,v,r,t) * scc_int(t) * 1e-3 ;
scc_irt(tyrpt,r,t)                       = sum(v$(v.val le t.val),       scc_ivrt(tyrpt,v,r,t)) ;
scc_rt(r,t)                              = sum(tyrpt,                    scc_irt(tyrpt,r,t)) ;
scc_it(tyrpt,t)                          = sum(r,                        scc_irt(tyrpt,r,t)) ;
scc_t(t)                                 = sum(r,                        scc_rt(r,t)) ;
scc_i_acc(tyrpt)                         = sum(t$(t.val > 2022),  nyrs(t) *             scc_it(tyrpt,t)) ;
scc_r_acc(r)                             = sum(t$(t.val > 2022),  nyrs(t) *             scc_rt(r,t)) ;
scc_acc                                  = sum(t$(t.val > 2022),  nyrs(t) *             scc_t(t)) ;

disc_scc_r_acc(r)                        = sum(t$(t.val > 2022), dfact(t) *             scc_rt(r,t)) ;
disc_scap_r_acc(r)                       = sum(t$(t.val > 2022), dfact(t) *             scap_rt(r,t)) ;

disc_scc_acc                             = sum(t$(t.val > 2022), dfact(t) *             scc_t(t)) ;
disc_scap_acc                            = sum(t$(t.val > 2022), dfact(t) *             scap_t(t)) ;

parameter
Demand_rpt(t)
;

Demand_rpt(t) = sum(r, daref(r,t)) ;

parameter
price_avg(t)
price_scc(t)
price_scap(t)
;

**
price_avg(t)  = 0 ;
price_scc(t)  = scc_t(t) * 1e+3 / (sum((s,r), load(s,r,t) * hours(s)) * 1e-3) ;
price_scap(t)  = scap_t(t) * 1e+3 / (sum((s,r), load(s,r,t) * hours(s)) * 1e-3) ;

parameter
PriceSCC_rpt(t,*)
PriceSCC_total_rpt(*)
;

PriceSCC_rpt(t,"Price-Avg") = price_avg(t) ;
PriceSCC_rpt(t,"Price-SCC") = price_scc(t) ;
PriceSCC_rpt(t,"Price-SCAP") = price_scap(t) ;


PriceSCC_total_rpt("CO2-ACC") = co2_acc ;
PriceSCC_total_rpt("AP-ACC") = sum(ap, ap_acc(ap)) ;
PriceSCC_total_rpt("SCC-ACC") = scc_acc ;
PriceSCC_total_rpt("SCAP-ACC") = scap_acc ;
PriceSCC_total_rpt("DISCSCC-ACC") = disc_scc_acc ;
PriceSCC_total_rpt("DISCSCAP-ACC") = disc_scap_acc ;

* Air pollution
Airpollution_rt_rpt(t,r,"CO2") = co2_rt(r,t) + eps ;
Airpollution_rt_rpt(t,r,"NH3") = ap_rt("NH3",r,t) + eps ;
Airpollution_rt_rpt(t,r,"NOX") = ap_rt("NOX",r,t) + eps ;
Airpollution_rt_rpt(t,r,"SO2") = ap_rt("SO2",r,t) + eps ;
Airpollution_rt_rpt(t,r,"PPM2.5") = ap_rt("PPM25",r,t) + eps ;
Airpollution_rt_rpt(t,r,"PPM10") = ap_rt("PPM10",r,t) + eps ;
Airpollution_rt_rpt(t,r,"NMVOC") = ap_rt("NMVOC",r,t) + eps ;

Airpollution_it_rpt(t,tyrpt,"CO2") = co2_it(tyrpt,t) + eps ;
Airpollution_it_rpt(t,tyrpt,"NH3") = ap_it("NH3",tyrpt,t) + eps ;
Airpollution_it_rpt(t,tyrpt,"NOX") = ap_it("NOX",tyrpt,t) + eps ;
Airpollution_it_rpt(t,tyrpt,"SO2") = ap_it("SO2",tyrpt,t) + eps ;
Airpollution_it_rpt(t,tyrpt,"PPM2.5") = ap_it("PPM25",tyrpt,t) + eps ;
Airpollution_it_rpt(t,tyrpt,"PPM10") = ap_it("PPM10",tyrpt,t) + eps ;
Airpollution_it_rpt(t,tyrpt,"NMVOC") = ap_it("NMVOC",tyrpt,t) + eps ;

Airpollution_t_rpt(t,"CO2") = co2_t(t) + eps ;
Airpollution_t_rpt(t,"NH3") = ap_t("NH3",t) + eps ;
Airpollution_t_rpt(t,"NOX") = ap_t("NOX",t) + eps ;
Airpollution_t_rpt(t,"SO2") = ap_t("SO2",t) + eps ;
Airpollution_t_rpt(t,"PPM2.5") = ap_t("PPM25",t) + eps ;
Airpollution_t_rpt(t,"PPM10") = ap_t("PPM10",t) + eps ;
Airpollution_t_rpt(t,"NMVOC") = ap_t("NMVOC",t) + eps ;

Airpollution_r_acc_rpt(r,"CO2") = co2_r_acc(r) + eps ;
Airpollution_r_acc_rpt(r,"NH3") = ap_r_acc("NH3",r) + eps ;
Airpollution_r_acc_rpt(r,"NOX") = ap_r_acc("NOX",r) + eps ;
Airpollution_r_acc_rpt(r,"SO2") = ap_r_acc("SO2",r) + eps ;
Airpollution_r_acc_rpt(r,"PPM2.5") = ap_r_acc("PPM25",r) + eps ;
Airpollution_r_acc_rpt(r,"PPM10") = ap_r_acc("PPM10",r) + eps ;
Airpollution_r_acc_rpt(r,"NMVOC") = ap_r_acc("NMVOC",r) + eps ;

Airpollution_i_acc_rpt(tyrpt,"CO2") = co2_i_acc(tyrpt) + eps ;
Airpollution_i_acc_rpt(tyrpt,"NH3") = ap_i_acc("NH3",tyrpt) + eps ;
Airpollution_i_acc_rpt(tyrpt,"NOX") = ap_i_acc("NOX",tyrpt) + eps ;
Airpollution_i_acc_rpt(tyrpt,"SO2") = ap_i_acc("SO2",tyrpt) + eps ;
Airpollution_i_acc_rpt(tyrpt,"PPM2.5") = ap_i_acc("PPM25",tyrpt) + eps ;
Airpollution_i_acc_rpt(tyrpt,"PPM10") = ap_i_acc("PPM10",tyrpt) + eps ;
Airpollution_i_acc_rpt(tyrpt,"NMVOC") = ap_i_acc("NMVOC",tyrpt) + eps ;

Airpollution_acc_rpt("CO2") = co2_acc + eps ;
Airpollution_acc_rpt("NH3") = ap_acc("NH3") + eps ;
Airpollution_acc_rpt("NOX") = ap_acc("NOX") + eps ;
Airpollution_acc_rpt("SO2") = ap_acc("SO2") + eps ;
Airpollution_acc_rpt("PPM2.5") = ap_acc("PPM25") + eps ;
Airpollution_acc_rpt("PPM10") = ap_acc("PPM10") + eps ;
Airpollution_acc_rpt("NMVOC") = ap_acc("NMVOC") + eps ;

* Social cost by air pollutant
Socialcost_rt_rpt(t,r,"CO2") = scc_rt(r,t) + eps ;
Socialcost_rt_rpt(t,r,"NH3") = scap_ap_rt("NH3",r,t) + eps ;
Socialcost_rt_rpt(t,r,"NOX") = scap_ap_rt("NOX",r,t) + eps ;
Socialcost_rt_rpt(t,r,"SO2") = scap_ap_rt("SO2",r,t) + eps ;
Socialcost_rt_rpt(t,r,"PPM2.5") = scap_ap_rt("PPM25",r,t) + eps ;
Socialcost_rt_rpt(t,r,"PPM10") = scap_ap_rt("PPM10",r,t) + eps ;
Socialcost_rt_rpt(t,r,"NMVOC") = scap_ap_rt("NMVOC",r,t) ;

Socialcost_it_rpt(t,tyrpt,"CO2") = scc_it(tyrpt,t) + eps ;
Socialcost_it_rpt(t,tyrpt,"NH3") = scap_ap_it("NH3",tyrpt,t) + eps ;
Socialcost_it_rpt(t,tyrpt,"NOX") = scap_ap_it("NOX",tyrpt,t) + eps ;
Socialcost_it_rpt(t,tyrpt,"SO2") = scap_ap_it("SO2",tyrpt,t) + eps ;
Socialcost_it_rpt(t,tyrpt,"PPM2.5") = scap_ap_it("PPM25",tyrpt,t) + eps ;
Socialcost_it_rpt(t,tyrpt,"PPM10") = scap_ap_it("PPM10",tyrpt,t) + eps ;
Socialcost_it_rpt(t,tyrpt,"NMVOC") = scap_ap_it("NMVOC",tyrpt,t) + eps ;

Socialcost_t_rpt(t,"CO2") = scc_t(t) + eps ;
Socialcost_t_rpt(t,"NH3") = scap_ap_t("NH3",t) + eps ;
Socialcost_t_rpt(t,"NOX") = scap_ap_t("NOX",t) + eps ;
Socialcost_t_rpt(t,"SO2") = scap_ap_t("SO2",t) + eps ;
Socialcost_t_rpt(t,"PPM2.5") = scap_ap_t("PPM25",t) + eps ;
Socialcost_t_rpt(t,"PPM10") = scap_ap_t("PPM10",t) + eps ;
Socialcost_t_rpt(t,"NMVOC") = scap_ap_t("NMVOC",t) + eps ;

Socialcost_r_acc_rpt(r,"CO2") = scc_r_acc(r) + eps ;
Socialcost_r_acc_rpt(r,"NH3") = scap_ap_r_acc("NH3",r) + eps ;
Socialcost_r_acc_rpt(r,"NOX") = scap_ap_r_acc("NOX",r) + eps ;
Socialcost_r_acc_rpt(r,"SO2") = scap_ap_r_acc("SO2",r) + eps ;
Socialcost_r_acc_rpt(r,"PPM2.5") = scap_ap_r_acc("PPM25",r) + eps ;
Socialcost_r_acc_rpt(r,"PPM10") = scap_ap_r_acc("PPM10",r) + eps ;
Socialcost_r_acc_rpt(r,"NMVOC") = scap_ap_r_acc("NMVOC",r) + eps ;

Socialcost_i_acc_rpt(tyrpt,"CO2") = scc_i_acc(tyrpt) + eps ;
Socialcost_i_acc_rpt(tyrpt,"NH3") = scap_ap_i_acc("NH3",tyrpt) + eps ;
Socialcost_i_acc_rpt(tyrpt,"NOX") = scap_ap_i_acc("NOX",tyrpt) + eps ;
Socialcost_i_acc_rpt(tyrpt,"SO2") = scap_ap_i_acc("SO2",tyrpt) + eps ;
Socialcost_i_acc_rpt(tyrpt,"PPM2.5") = scap_ap_i_acc("PPM25",tyrpt) + eps ;
Socialcost_i_acc_rpt(tyrpt,"PPM10") = scap_ap_i_acc("PPM10",tyrpt) + eps ;
Socialcost_i_acc_rpt(tyrpt,"NMVOC") = scap_ap_i_acc("NMVOC",tyrpt) + eps ;

Socialcost_acc_rpt("CO2") = scc_acc + eps ;
Socialcost_acc_rpt("NH3") = scap_ap_acc("NH3") + eps ;
Socialcost_acc_rpt("NOX") = scap_ap_acc("NOX") + eps ;
Socialcost_acc_rpt("SO2") = scap_ap_acc("SO2") + eps ;
Socialcost_acc_rpt("PPM2.5") = scap_ap_acc("PPM25") + eps ;
Socialcost_acc_rpt("PPM10") = scap_ap_acc("PPM10") + eps ;
Socialcost_acc_rpt("NMVOC") = scap_ap_acc("NMVOC") + eps ;

* Social cost by impact
Socialcost_impact_rt_rpt(t,r,"Health") = scap_impactap_rt("Health",r,t) + eps ;
Socialcost_impact_rt_rpt(t,r,"Crop") = scap_impactap_rt("Crop",r,t) + eps ;
Socialcost_impact_rt_rpt(t,r,"Biodiv") = scap_impactap_rt("Biodiv",r,t) + eps ;
Socialcost_impact_rt_rpt(t,r,"Materials") = scap_impactap_rt("Materials",r,t) + eps ;
Socialcost_impact_rt_rpt(t,r,"row") = scap_impactap_rt("row",r,t) + eps ;

Socialcost_impact_it_rpt(t,tyrpt,"Health") = scap_impactap_it("Health",tyrpt,t) + eps ;
Socialcost_impact_it_rpt(t,tyrpt,"Crop") = scap_impactap_it("Crop",tyrpt,t) + eps ;
Socialcost_impact_it_rpt(t,tyrpt,"Biodiv") = scap_impactap_it("Biodiv",tyrpt,t) + eps ;
Socialcost_impact_it_rpt(t,tyrpt,"Materials") = scap_impactap_it("Materials",tyrpt,t) + eps ;
Socialcost_impact_it_rpt(t,tyrpt,"row") = scap_impactap_it("row",tyrpt,t) + eps ;

Socialcost_impact_t_rpt(t,"Health") = scap_impactap_t("Health",t) + eps ;
Socialcost_impact_t_rpt(t,"Crop") = scap_impactap_t("Crop",t) + eps ;
Socialcost_impact_t_rpt(t,"Biodiv") = scap_impactap_t("Biodiv",t) + eps ;
Socialcost_impact_t_rpt(t,"Materials") = scap_impactap_t("Materials",t) + eps ;
Socialcost_impact_t_rpt(t,"row") = scap_impactap_t("row",t) + eps ;

Socialcost_impact_r_acc_rpt(r,"Health") = scap_impactap_r_acc("Health",r) + eps ;
Socialcost_impact_r_acc_rpt(r,"Crop") = scap_impactap_r_acc("Crop",r) + eps ;
Socialcost_impact_r_acc_rpt(r,"Biodiv") = scap_impactap_r_acc("Biodiv",r) + eps ;
Socialcost_impact_r_acc_rpt(r,"Materials") = scap_impactap_r_acc("Materials",r) + eps ;
Socialcost_impact_r_acc_rpt(r,"row") = scap_impactap_r_acc("row",r) + eps ;

Socialcost_impact_i_acc_rpt(tyrpt,"Health") = scap_impactap_i_acc("Health",tyrpt) + eps ;
Socialcost_impact_i_acc_rpt(tyrpt,"Crop") = scap_impactap_i_acc("Crop",tyrpt) + eps ;
Socialcost_impact_i_acc_rpt(tyrpt,"Biodiv") = scap_impactap_i_acc("Biodiv",tyrpt) + eps ;
Socialcost_impact_i_acc_rpt(tyrpt,"Materials") = scap_impactap_i_acc("Materials",tyrpt) + eps ;
Socialcost_impact_i_acc_rpt(tyrpt,"row") = scap_impactap_i_acc("row",tyrpt) + eps ;

Socialcost_impact_acc_rpt("Health") = scap_impactap_acc("Health") + eps ;
Socialcost_impact_acc_rpt("Crop") = scap_impactap_acc("Crop") + eps ;
Socialcost_impact_acc_rpt("Biodiv") = scap_impactap_acc("Biodiv") + eps ;
Socialcost_impact_acc_rpt("Materials") = scap_impactap_acc("Materials") + eps ;
Socialcost_impact_acc_rpt("row") = scap_impactap_acc("row") + eps ;

SCCtot_rt_rpt(t,r)                                               = scc_rt(r,t)           + eps ;
SCCtot_it_rpt(t,tyrpt)                                           = scc_it(tyrpt,t)       + eps ;
*  Billion (10^9) EUR by TWh (10^12) 10^9 EUR / 10^6 MWH = 10^3 EUR / MWh
SCCspe_rt_rpt(t,r)$(sum(tyrpt, gen_xtype(tyrpt,r,t)) > 0)        = scc_rt(r,t)           / sum(tyrpt, gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
SCCspe_it_rpt(t,tyrpt)$(sum(r, gen_xtype(tyrpt,r,t)) > 0)        = scc_it(tyrpt,t)       / sum(r, gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
SCCspe_t_rpt(t)$(sum((r,tyrpt), gen_xtype(tyrpt,r,t)) > 0)       = scc_t(t)              / sum((r,tyrpt), gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
SCCspe_rt_rpt(t,r)$(sum(tyrpt, gen_xtype(tyrpt,r,t)) = 0)        = eps ;
SCCspe_it_rpt(t,tyrpt)$(sum(r, gen_xtype(tyrpt,r,t)) = 0)        = eps ;
SCCspe_t_rpt(t)$(sum((r,tyrpt), gen_xtype(tyrpt,r,t)) = 0)       = eps ;

SCAPtot_rt_rpt(t,r)                                              = scap_rt(r,t)          + eps ;
SCAPtot_it_rpt(t,tyrpt)                                          = scap_it(tyrpt,t)      + eps ;
SCAPspe_rt_rpt(t,r)$(sum(tyrpt, gen_xtype(tyrpt,r,t)) > 0)       = scap_rt(r,t)          / sum(tyrpt, gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
SCAPspe_it_rpt(t,tyrpt)$(sum(r, gen_xtype(tyrpt,r,t)) > 0)       = scap_it(tyrpt,t)      / sum(r, gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
SCAPspe_t_rpt(t)$(sum((r,tyrpt), gen_xtype(tyrpt,r,t)) > 0)      = scap_t(t)             / sum((r,tyrpt), gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
SCAPspe_rt_rpt(t,r)$(sum(tyrpt, gen_xtype(tyrpt,r,t)) = 0)       = eps ;
SCAPspe_it_rpt(t,tyrpt)$(sum(r, gen_xtype(tyrpt,r,t)) = 0)       = eps ;
SCAPspe_t_rpt(t)$(sum((r,tyrpt), gen_xtype(tyrpt,r,t)) = 0)      = eps ;

Socialtotcost_rt_rpt(t,r)                                        = (scap_rt(r,t) + scc_rt(r,t))          + eps ;
Socialtotcost_it_rpt(t,tyrpt)                                    = (scap_it(tyrpt,t) + scc_it(tyrpt,t))  + eps ;
Socialspecost_rt_rpt(t,r)$(sum(tyrpt, gen_xtype(tyrpt,r,t)) > 0) = (scap_rt(r,t) + scc_rt(r,t))          / sum(tyrpt, gen_xtype(tyrpt,r,t)) * 1e+3 + eps ;
Socialspecost_it_rpt(t,tyrpt)$(sum(r, gen_xtype(tyrpt,r,t)) > 0) = (scap_it(tyrpt,t) + scc_it(tyrpt,t))  / sum(r, gen_xtype(tyrpt,r,t))  * 1e+3 + eps ;
Socialspecost_t_rpt(t)$(sum((r,tyrpt),gen_xtype(tyrpt,r,t)) > 0) = (scc_t(t) + scap_t(t))                / sum((r,tyrpt), gen_xtype(tyrpt,r,t))  * 1e+3 + eps ;
Socialspecost_rt_rpt(t,r)$(sum(tyrpt, gen_xtype(tyrpt,r,t)) = 0) = eps ;
Socialspecost_it_rpt(t,tyrpt)$(sum(r, gen_xtype(tyrpt,r,t)) = 0) = eps ;
Socialspecost_t_rpt(t)$(sum((r,tyrpt),gen_xtype(tyrpt,r,t)) = 0) = eps ;

parameter
scapequalr(ap,impactap,t)
scapequalr_emit_impactap(impactap,i,v,t)
scapequalr_emit_ap(ap,i,v,t)
scapequalr_emit(i,v,t)
;

scapequalr(ap,impactap,t)                    = sum(r, daref(r,t) * scap_i(ap,impactap,r,t))                  / sum(r, daref(r,t)) ;
scapequalr_emit_impactap(impactap,i,v,t)     = sum(r, daref(r,t) * scap_emit_impactap_i(impactap,i,v,r,t))   / sum(r, daref(r,t)) ;
scapequalr_emit_ap(ap,i,v,t)                 = sum(r, daref(r,t) * scap_emit_ap_i(ap,i,v,r,t))               / sum(r, daref(r,t)) ;
scapequalr_emit(i,v,t)                       = sum(r, daref(r,t) * scap_emit_i(i,v,r,t))                     / sum(r, daref(r,t)) ;