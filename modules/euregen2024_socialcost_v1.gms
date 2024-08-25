* * * Social cost module
set
ap                               Air pollutant
ap                               Air pollutant
impactap                         Impact of air pollutant
emfap_sc                         Scenario emission factor ;

$gdxin precal\precal_%n%.gdx
$load ap, impactap, emfap_sc
$gdxin

parameter
gdpgrowth(r,t)                                           GDP growth index
gdpdistri(r,t)                                           GDP distributional index
emfap(i,ap,v)                                            Emission factor air pollutant (t per MWh thermal)
emitap(i,ap,v,r)                                         Emission factor air pollutant (t per MWh electric)
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
scc_int(t)
scc_emit(i,v,r,t)                                        Social cost of carbon (2015er EUR per MWh electric)
drate_scap                                               Annual discount rate
dfact_scap(t)                                            Discount factor for time period t (reflects number of years) for both
drate_scc                                                Annual discount rate
dfact_scc(t)                                             Discount factor for time period t (reflects number of years) for both
;

$gdxin precal\precal_%n%.gdx
$load gdpgrowth
$load gdpdistri
$load emfap
$load emitap
$load scap_i
$load scap_emit_impactap_i
$load scap_emit_ap_i
$load scap_emit_i
$load scapr
$load scapr_emit_impactap
$load scapr_emit_ap
$load scapr_emit
$load scap
$load scap_emit_impactap
$load scap_emit_ap
$load scap_emit
$load scc, scc_int
$load scc_emit
$load drate_scap  
$load dfact_scap
$load drate_scc
$load dfact_scc
$gdxindxin