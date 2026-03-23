* * * Social cost modules
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