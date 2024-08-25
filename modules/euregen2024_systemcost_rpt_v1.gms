* * * System cost reporting
set
set_systemcost     /"System investment cost (billion)", "System fixed cost (billion)", "System variable cost (billion)", "System lost load cost (billion)", "System cost (billion)", "External carbon cost (billion)", "External air pollution cost (billion)",
                    "Carbon tax revenue (billion)", "Air pollution tax revenue (billion)", "Export benefits (billion)", "Import cost (billion)",
                    "CO2 (Mt)", "AP (Mt)", "Generation wind (TWh)", "Generation solar (TWh)", "Generation nuclear (TWh)", "Generation ccs (TWh)", "Generation bio (TWh)", "Generation (TWh)", "Demand (TWh)", "Industrial demand (TWh)", "Exports (TWh)", "Imports (TWh)",
                    "Industrial demand share", "Renewable share", "Intermittent renewable share", "Low carbon share",
                    "System cost after trade (billion)", "System cost after taxes (billion)", "System cost after damages (billion)", "System cost after taxes and damages (billion)", "System cost after trade and taxes (billion)",
                    "System cost after trade and taxes and damages (billion)",
                    "System cost (EUR per MWh)", "System cost after trade (EUR per MWh)", "System cost after taxes (EUR per MWh)", "System cost after damages (EUR per MWh)", "System cost after taxes and damages (EUR per MWh)", "System cost after trade and taxes (EUR per MWh)",
                    "System cost after trade and taxes and damages (EUR per MWh)",
                    "Electricity revenue (billion)", "Carbon price (billion)", "Electricity price (EUR per MWh)", "Carbon price (EUR per MWh)", "Carbon price (EUR per ton)",                  
                    "Capacity subsidies solar (billion)", "Capacity subsidies wind onshore (billion)", "Capacity subsidies wind offshore (billion)", "Capacity subsidies biomass (billion)", "Capacity subsidies water (billion)",
                    "Capacity subsidies no water (billion)", "Capacity subsidies rnw (billion)", "Capacity subsidies nuclear (billion)", "Capacity subsidies (billion)",
                    "Capacity subsidies solar (EUR per kW added)", "Capacity subsidies wind onshore (EUR per kW added)", "Capacity subsidies wind offshore (EUR per kW added)", "Capacity subsidies biomass (EUR per kW added)", "Capacity subsidies water (EUR per kW added)",
                    "Capacity subsidies no water (EUR per kW added)", "Capacity subsidies nuclear (EUR per kW added)",
                    "Capacity subsidies solar (EUR per kW)", "Capacity subsidies wind onshore (EUR per kW)", "Capacity subsidies wind offshore (EUR per kW)", "Capacity subsidies biomass (EUR per kW)", "Capacity subsidies water (EUR per kW)",
                    "Capacity subsidies no water (EUR per kW)", "Capacity subsidies nuclear (EUR per kW)",
                    "Capacity subsidies solar (share)", "Capacity subsidies wind onshore (share)", "Capacity subsidies wind offshore (share)", "Capacity subsidies biomass (share)", "Capacity subsidies water (share)",
                    "Capacity subsidies no water (share)", "Capacity subsidies nuclear (share)",
                    "Generation subsidies rnw (billion)", "Generation subsidies nuclear (billion)", "Generation subsidies (billion)",
                    "Generation subsidies rnw (EUR per MWh rnw)", "Generation subsidies nuclear (EUR per MWh nuc)", "Generation subsidies (EUR per MWh rnw + nuc)",
                    "Generation subsidies rnw (EUR per MWh)", "Generation subsidies nuclear (EUR per MWh)",
                    "Generation subsidies (EUR per MWh)", "Capacity subsidies (EUR per MWh)", "Subsidies (EUR per MWh)",
                    "Subsidies rnw (EUR per MWh rnw)", "Subsidies nuclear (EUR per MWh nuc)", "Subsidies (EUR per MWh rnw + nuc)",
                    "Subsidies rnw (billion)", "Subsidies nuclear (billion)", "Subsidies (billion)",  "Subsidies rnw (EUR per MWh)", "Subsidies nuclear (EUR per MWh)" 
                   /
value              /nodisc,npv/
;


parameter
SystemCost_rpt(t,r,set_systemcost)       Total cost reporting vector 
TotalSystemCost_rpt(t,set_systemcost)    Total cost reporting vector 
AggreSystemCost_rpt(r,set_systemcost,value)    Total cost reporting vector 
TotalAggreSystemCost_rpt(set_systemcost,value) Total cost reporting vector 
;

* * System cost
* Emissions
SystemCost_rpt(t,r,"CO2 (Mt)")                                      = co2emit(r,t) + eps ;
SystemCost_rpt(t,r,"AP (Mt)")                                       = 0 + eps ;
*sum(ap, ap_rt(ap,r,t)) + eps ;
* Generation and demand
SystemCost_rpt(t,r,"Generation wind (TWh)")                         = sum(wind(i), gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Generation solar (TWh)")                        = sum(sol(i),  gen(i,r,t)) + eps ; 
SystemCost_rpt(t,r,"Generation nuclear (TWh)")                      = sum(nuc(i),  gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Generation ccs (TWh)")                          = sum(ccs(i),  gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Generation bio (TWh)")                          = sum(bio(i),  gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Generation (TWh)")                              = sum(i,       gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Demand (TWh)")                                  = daref(r,t) + eps ;
SystemCost_rpt(t,r,"Industrial demand (TWh)")                       = daref_sec(r,t,"ind") + eps ;
SystemCost_rpt(t,r,"Industrial demand share")                       = daref_sec(r,t,"ind") / daref(r,t) + eps ;
SystemCost_rpt(t,r,"Exports (TWh)")                                 = 1e-3 * sum((s,k,rr)$(tmap(k,r,rr) and trnspen(k,r,rr) > 0), hours(s) * E.L(s,k,r,rr,t) / trnspen(k,r,rr)) + eps ;
SystemCost_rpt(t,r,"Imports (TWh)")                                 = 1e-3 * sum((s,k,rr)$(tmap(k,rr,r)), hours(s) * E.L(s,k,rr,r,t)) + eps ;

SystemCost_rpt(t,r,"Renewable share")$(sum(i, gen(i,r,t)) > 0)                               = sum(rnw(i),  gen(i,r,t))    / sum(i, gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Intermittent renewable share")$(sum(i, gen(i,r,t)) > 0)                  = sum(irnw(i), gen(i,r,t))    / sum(i, gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Low carbon share")$(sum(i, gen(i,r,t)) > 0)                              = sum(lowcarb(i), gen(i,r,t)) / sum(i, gen(i,r,t)) + eps ;

* Absolut cost
SystemCost_rpt(t,r,"System investment cost (billion)")              = inves_zeta_nodisc(r,t) / nyrs(t) + eps ;
SystemCost_rpt(t,r,"System fixed cost (billion)")                   = fixed(r,t) + eps ;
SystemCost_rpt(t,r,"System variable cost (billion)")                = varia(r,t) + eps ;
SystemCost_rpt(t,r,"System lost load cost (billion)")               = lostl(r,t) + eps ;
SystemCost_rpt(t,r,"System cost (billion)")                         = inves_normal_nodisc(r,t) / nyrs(t) + fixed(r,t) + varia(r,t) + lostl(r,t) + eps ;
SystemCost_rpt(t,r,"External carbon cost (billion)")                = co2emit(r,t) * scc_t(t) * 1e-3 + eps ;
SystemCost_rpt(t,r,"External air pollution cost (billion)")         = 0 + eps ;
*scap_rt(r,t) + eps ;
SystemCost_rpt(t,r,"Carbon tax revenue (billion)")                  = carbt(r,t) + eps ;
SystemCost_rpt(t,r,"Air pollution tax revenue (billion)")           = 0 + eps ;
*airpt(r,t) + eps ;
SystemCost_rpt(t,r,"Export benefits (billion)")                     = 1e-6 * sum((s,k,rr)$(tmap(k,r,rr) and trnspen(k,r,rr) > 0), hours(s) * E.L(s,k,r,rr,t) / trnspen(k,r,rr) * price(s,rr,t)) + eps ;
SystemCost_rpt(t,r,"Import cost (billion)")                         = 1e-6 * sum((s,k,rr)$(tmap(k,rr,r)), hours(s) * E.L(s,k,rr,r,t)                   * price(s,rr,t)) + eps ;
SystemCost_rpt(t,r,"System cost after trade (billion)")                         = SystemCost_rpt(t,r,"System cost (billion)") + SystemCost_rpt(t,r,"Import cost (billion)") - SystemCost_rpt(t,r,"Export benefits (billion)") ;
SystemCost_rpt(t,r,"System cost after taxes (billion)")                         = SystemCost_rpt(t,r,"System cost (billion)") - SystemCost_rpt(t,r,"Carbon tax revenue (billion)") - SystemCost_rpt(t,r,"Air pollution tax revenue (billion)") ;
SystemCost_rpt(t,r,"System cost after damages (billion)")                       = SystemCost_rpt(t,r,"System cost (billion)") + SystemCost_rpt(t,r,"External carbon cost (billion)") + SystemCost_rpt(t,r,"External air pollution cost (billion)") ;
SystemCost_rpt(t,r,"System cost after taxes and damages (billion)")             = SystemCost_rpt(t,r,"System cost after taxes (billion)") + SystemCost_rpt(t,r,"External carbon cost (billion)") + SystemCost_rpt(t,r,"External air pollution cost (billion)") ;
SystemCost_rpt(t,r,"System cost after trade and taxes (billion)")               = SystemCost_rpt(t,r,"System cost after trade (billion)") - SystemCost_rpt(t,r,"Carbon tax revenue (billion)") - SystemCost_rpt(t,r,"Air pollution tax revenue (billion)") ;
SystemCost_rpt(t,r,"System cost after trade and taxes and damages (billion)")   = SystemCost_rpt(t,r,"System cost after trade and taxes (billion)") + SystemCost_rpt(t,r,"External carbon cost (billion)") + SystemCost_rpt(t,r,"External air pollution cost (billion)") ;
SystemCost_rpt(t,r,"Electricity revenue (billion)")                             = Electricity1_rpt(t,r,"price-avg") * daref(r,t) * 1e-3 + eps ;
SystemCost_rpt(t,r,"Carbon price (billion)")                                    = co2emit(r,t) * co2prreg(r,t) * 1e-3 + eps ;
* Relative cost
SystemCost_rpt(t,r,"System cost (EUR per MWh)")                                         = 1e+3 * SystemCost_rpt(t,r,"System cost (billion)")                                   / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"System cost after trade (EUR per MWh)")                             = 1e+3 * SystemCost_rpt(t,r,"System cost after trade (billion)")                       / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"System cost after taxes (EUR per MWh)")                             = 1e+3 * SystemCost_rpt(t,r,"System cost after taxes (billion)")                       / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"System cost after damages (EUR per MWh)")                           = 1e+3 * SystemCost_rpt(t,r,"System cost after damages (billion)")                     / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"System cost after taxes and damages (EUR per MWh)")                 = 1e+3 * SystemCost_rpt(t,r,"System cost after taxes and damages (billion)")           / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"System cost after trade and taxes (EUR per MWh)")                   = 1e+3 * SystemCost_rpt(t,r,"System cost after trade and taxes (billion)")             / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"System cost after trade and taxes and damages (EUR per MWh)")       = 1e+3 * SystemCost_rpt(t,r,"System cost after trade and taxes and damages (billion)") / SystemCost_rpt(t,r,"Demand (TWh)") ;
SystemCost_rpt(t,r,"Electricity price (EUR per MWh)")                                   = Electricity1_rpt(t,r,"price-avg") ;
SystemCost_rpt(t,r,"Carbon price (EUR per MWh)")                                        = 1e+3 * SystemCost_rpt(t,r,"Carbon price (billion)")                                  / SystemCost_rpt(t,r,"Demand (TWh)") ;                       
SystemCost_rpt(t,r,"Carbon price (EUR per ton)")                                        = co2prreg(r,t) ;
* Capacity subsidies/year
SystemCost_rpt(t,r,"Capacity subsidies solar (billion)")            = capmarket.M("solarpv",r,t)   * 1e-3 *        sum(sol(i), IX.L(i,r,t)) / dfact(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind onshore (billion)")     = capmarket.M("windon",r,t)    * 1e-3 *        sum(windon(i), IX.L(i,r,t)) / dfact(t) + eps ; 
SystemCost_rpt(t,r,"Capacity subsidies wind offshore (billion)")    = capmarket.M("windon",r,t)    * 1e-3 *        sum(windoff(i), IX.L(i,r,t)) / dfact(t) + eps ; 
SystemCost_rpt(t,r,"Capacity subsidies biomass (billion)")          = capmarket.M("biomass",r,t)   * 1e-3 *        sum(bio(i), IX.L(i,r,t)) / dfact(t) + eps ; 
SystemCost_rpt(t,r,"Capacity subsidies water (billion)")            = capmarket.M("water",r,t)     * 1e-3 *        sum(i$(sameas(i,"Hydro")), IX.L(i,r,t)) / dfact(t) + eps ; 
SystemCost_rpt(t,r,"Capacity subsidies no water (billion)")         = capmarket.M("nowater",r,t)   * 1e-3 *        sum(irnw(i)$(not sameas(i,"Hydro")), IX.L(i,r,t)) / dfact(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies nuclear (billion)")          = investlimLO.M("Nuclear",r,t) * 1e-3 *        sum(nuc(i), IX.L(i,r,t)) / dfact(t) + eps ;
* Capacity subsidies true value
SystemCost_rpt(t,r,"Capacity subsidies solar (EUR per kW added)")            = capmarket.M("solarpv",r,t)   / dfact(t) * nyrs(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind onshore (EUR per kW added)")     = capmarket.M("windon",r,t)    / dfact(t) * nyrs(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind offshore (EUR per kW added)")    = capmarket.M("windoff",r,t)   / dfact(t) * nyrs(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies biomass (EUR per kW added)")          = capmarket.M("biomass",r,t)   / dfact(t) * nyrs(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies water (EUR per kW added)")            = capmarket.M("water",r,t)     / dfact(t) * nyrs(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies no water (EUR per kW added)")         = capmarket.M("nowater",r,t)   / dfact(t) * nyrs(t) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies nuclear (EUR per kW added)")          = investlimLO.M("Nuclear",r,t) / dfact(t) * nyrs(t) + eps ;
* Capacity subsidies/added kW
SystemCost_rpt(t,r,"Capacity subsidies solar (EUR per kW)")$(sum(sol(i),     IX.L(i,r,t)) > 0)                          = SystemCost_rpt(t,r,"Capacity subsidies solar (billion)")          * nyrs(t) / sum(sol(i),                         IX.L(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind onshore (EUR per kW)")$(sum(windon(i),  IX.L(i,r,t)) > 0)                   = SystemCost_rpt(t,r,"Capacity subsidies wind onshore (billion)")   * nyrs(t) / sum(windon(i),                      IX.L(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind offshore (EUR per kW)")$(sum(windoff(i), IX.L(i,r,t)) > 0)                  = SystemCost_rpt(t,r,"Capacity subsidies wind offshore (billion)")  * nyrs(t) / sum(windoff(i),                     IX.L(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies biomass (EUR per kW)")$(sum(bio(i), IX.L(i,r,t)) > 0)                            = SystemCost_rpt(t,r,"Capacity subsidies biomass (billion)")        * nyrs(t) / sum(bio(i),                         IX.L(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies water (EUR per kW)")$(sum(i$(sameas(i,"hydro")), IX.L(i,r,t)) > 0)               = SystemCost_rpt(t,r,"Capacity subsidies water (billion)")          * nyrs(t) / sum(i$(sameas(i,"hydro")),          IX.L(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies no water (EUR per kW)")$(sum(rnw(i)$(not sameas(i,"hydro")), IX.L(i,r,t)) > 0)   = SystemCost_rpt(t,r,"Capacity subsidies no water (billion)")       * nyrs(t) / sum(rnw(i)$(not sameas(i,"hydro")), IX.L(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies nuclear (EUR per kW)")$(sum(nuc(i),     IX.L(i,r,t)) > 0)                        = SystemCost_rpt(t,r,"Capacity subsidies nuclear (billion)")        * nyrs(t) / sum(nuc(i),                         IX.L(i,r,t)) + eps ;
* Subsidies share via specific investment cost
SystemCost_rpt(t,r,"Capacity subsidies solar (share)")$(sum(tv(t,v), capcost("OpenPV_q90",v,r)) > 0)            = capmarket.M("solarpv",r,t)   / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("OpenPV_q90",v,r)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind onshore (share)")$(sum(tv(t,v), capcost("WindOn_q90",v,r)) > 0)     = capmarket.M("windon",r,t)    / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("WindOn_q90",v,r)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies wind offshore (share)")$(sum(tv(t,v), capcost("WindOff_q90",v,r)) > 0)   = capmarket.M("windon",r,t)    / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("WindOff_q90",v,r)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies biomass (share)")$(sum(tv(t,v), capcost("bioenergy",v,r)) > 0)           = capmarket.M("Biomass",r,t)   / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("Bioenergy",v,r)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies water (share)")$(sum(tv(t,v), capcost("hydro",v,r)) > 0)                 = capmarket.M("Water",r,t)     / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("Hydro",v,r))+ eps ;
SystemCost_rpt(t,r,"Capacity subsidies no water (share)")$(sum(tv(t,v), capcost("WindOn_q90",v,r)) > 0)         = capmarket.M("NoWater",r,t)   / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("WindOn_q90",v,r)) + eps ;
SystemCost_rpt(t,r,"Capacity subsidies nuclear (share)")$(sum(tv(t,v), capcost("Nuclear",v,r)) > 0)             = investlimLO.M("Nuclear",r,t) / dfact(t) * nyrs(t) / sum(tv(t,v), capcost("Nuclear",v,r)) + eps ;
* Meta cost
SystemCost_rpt(t,r,"Capacity subsidies rnw (billion)")              = SystemCost_rpt(t,r,"Capacity subsidies solar (billion)") + SystemCost_rpt(t,r,"Capacity subsidies wind onshore (billion)")
                                                                    + SystemCost_rpt(t,r,"Capacity subsidies wind offshore (billion)") + SystemCost_rpt(t,r,"Capacity subsidies biomass (billion)")
                                                                    + SystemCost_rpt(t,r,"Capacity subsidies water (billion)") + SystemCost_rpt(t,r,"Capacity subsidies no water (billion)") + eps ;
SystemCost_rpt(t,r,"Capacity subsidies (billion)")                  = SystemCost_rpt(t,r,"Capacity subsidies rnw (billion)") + SystemCost_rpt(t,r,"Capacity subsidies nuclear (billion)") + eps ;    
* Subsdies/year
SystemCost_rpt(t,r,"Generation subsidies rnw (billion)")            =
$if set resmarket   (resmarket_con.M(r,t) + resmarket_gen.M(r,t)) * 1e-3 * sum(rnw(i), gen(i,r,t)) / dfact(t)
+ eps ;
SystemCost_rpt(t,r,"Generation subsidies nuclear (billion)")$(sameas(r,"France"))        =
$if set frnuctgt    frnuctarget.M(t)  * 1e-3 * sum(nuc(i), gen(i,r,t)) / dfact(t)
+ eps ;
SystemCost_rpt(t,r,"Generation subsidies (billion)")                = SystemCost_rpt(t,r,"Generation subsidies rnw (billion)") + SystemCost_rpt(t,r,"Generation subsidies nuclear (billion)") + eps ;
* Subsidies/generated irnw unit
SystemCost_rpt(t,r,"Generation subsidies rnw (EUR per MWh)")        =
$if set resmarket   (resmarket_con.M(r,t) + resmarket_gen.M(r,t))  / dfact(t)
+ eps ;
SystemCost_rpt(t,r,"Generation subsidies nuclear (EUR per MWh)")$(sameas(r,"France"))    =
$if set frnuctgt    frnuctarget.M(t)  / dfact(t)
+ eps ;
* Total subsidies/year
SystemCost_rpt(t,r,"Subsidies rnw (billion)")                       = SystemCost_rpt(t,r,"Capacity subsidies rnw (billion)") + SystemCost_rpt(t,r,"Generation subsidies rnw (billion)") + eps ;                                                                   
SystemCost_rpt(t,r,"Subsidies nuclear (billion)")                   = SystemCost_rpt(t,r,"Capacity subsidies nuclear (billion)") + SystemCost_rpt(t,r,"Generation subsidies nuclear (billion)") + eps ;
SystemCost_rpt(t,r,"Subsidies (billion)")                           = SystemCost_rpt(t,r,"Subsidies rnw (billion)") + SystemCost_rpt(t,r,"Subsidies nuclear (billion)") + eps ;
* Subsidies/MWh rnw/nuc generation
SystemCost_rpt(t,r,"Subsidies rnw (EUR per MWh rnw)")$(sum(rnw(i),  gen(i,r,t)) > 0)                                    = 1e+3 * SystemCost_rpt(t,r,"Subsidies rnw (billion)")         / sum(rnw(i),  gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Subsidies nuclear (EUR per MWh nuc)")$(sum(nuc(i),  gen(i,r,t)) > 0)                                = 1e+3 * SystemCost_rpt(t,r,"Subsidies nuclear (billion)")     / sum(nuc(i),  gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Subsidies (EUR per MWh rnw + nuc)")$(sum(nuc(i),  gen(i,r,t)) > 0 and sum(rnw(i),  gen(i,r,t)) > 0) = 1e+3 * SystemCost_rpt(t,r,"Subsidies (billion)") / (sum(nuc(i),  gen(i,r,t)) + sum(rnw(i),  gen(i,r,t))) + eps ;
SystemCost_rpt(t,r,"Subsidies (EUR per MWh rnw + nuc)")$(sum(nuc(i),  gen(i,r,t)) > 0 and sum(rnw(i),  gen(i,r,t)) = 0) = 1e+3 * SystemCost_rpt(t,r,"Subsidies (billion)") / sum(nuc(i),  gen(i,r,t)) + eps ;
SystemCost_rpt(t,r,"Subsidies (EUR per MWh rnw + nuc)")$(sum(nuc(i),  gen(i,r,t)) = 0 and sum(rnw(i),  gen(i,r,t)) > 0) = 1e+3 * SystemCost_rpt(t,r,"Subsidies (billion)") / sum(rnw(i),  gen(i,r,t)) + eps ;
* Subsidies/MWh electricity generated
SystemCost_rpt(t,r,"Capacity subsidies (EUR per MWh)")              = 1e+3 * SystemCost_rpt(t,r,"Capacity subsidies (billion)")    / SystemCost_rpt(t,r,"Demand (TWh)") + eps ;                                                                 
SystemCost_rpt(t,r,"Generation subsidies (EUR per MWh)")            = 1e+3 * SystemCost_rpt(t,r,"Generation subsidies (billion)")  / SystemCost_rpt(t,r,"Demand (TWh)") + eps ;
SystemCost_rpt(t,r,"Subsidies rnw (EUR per MWh)")                   = 1e+3 * SystemCost_rpt(t,r,"Subsidies rnw (billion)")         / SystemCost_rpt(t,r,"Demand (TWh)") + eps ;
SystemCost_rpt(t,r,"Subsidies nuclear (EUR per MWh)")               = 1e+3 * SystemCost_rpt(t,r,"Subsidies nuclear (billion)")     / SystemCost_rpt(t,r,"Demand (TWh)") + eps ;
SystemCost_rpt(t,r,"Subsidies (EUR per MWh)")                       = 1e+3 * SystemCost_rpt(t,r,"Subsidies (billion)")             / SystemCost_rpt(t,r,"Demand (TWh)") + eps ;

parameter
SystemCost_GER_rpt(t,set_systemcost)
SystemCost_FR_rpt(t,set_systemcost)
;

SystemCost_GER_rpt(t,set_systemcost) = sum(r$(sameas(r,"Germany")), SystemCost_rpt(t,r,set_systemcost)) + eps ;
SystemCost_FR_rpt(t,set_systemcost) = sum(r$(sameas(r,"France")), SystemCost_rpt(t,r,set_systemcost)) + eps ;

* * Total system cost
TotalSystemCost_rpt(t,set_systemcost)             = sum(r, SystemCost_rpt(t,r,set_systemcost)) + eps ;
* Relative cost
TotalSystemCost_rpt(t,"Industrial demand share")$(sum(r, daref(r,t))  > 0)                                                              = sum(r, daref_sec(r,t,"ind")) / sum(r, daref(r,t)) + eps ;
TotalSystemCost_rpt(t,"Renewable share")$(sum(r, sum(i,      gen(i,r,t))) > 0)                                                          = sum(r, sum(rnw(i),  gen(i,r,t))) / sum(r, sum(i,      gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Intermittent renewable share")$(sum(r, sum(i,      gen(i,r,t))) > 0)                                             = sum(r, sum(irnw(i), gen(i,r,t))) / sum(r, sum(i,      gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Low carbon share")$(sum(r, sum(i,      gen(i,r,t))) > 0)                                                         = sum(r, sum(lowcarb(i), gen(i,r,t))) / sum(r, sum(i,      gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"System cost (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                                          = 1e+3 * TotalSystemCost_rpt(t,"System cost (billion)")                                   / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"System cost after trade (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                              = 1e+3 * TotalSystemCost_rpt(t,"System cost after trade (billion)")                       / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"System cost after taxes (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                              = 1e+3 * TotalSystemCost_rpt(t,"System cost after taxes (billion)")                       / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"System cost after damages (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                            = 1e+3 * TotalSystemCost_rpt(t,"System cost after damages (billion)")                     / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"System cost after taxes and damages (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                  = 1e+3 * TotalSystemCost_rpt(t,"System cost after taxes and damages (billion)")           / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"System cost after trade and taxes (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                    = 1e+3 * TotalSystemCost_rpt(t,"System cost after trade and taxes (billion)")             / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"System cost after trade and taxes and damages (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)        = 1e+3 * TotalSystemCost_rpt(t,"System cost after trade and taxes and damages (billion)") / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"Electricity price (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                                    = 1e+3 * TotalSystemCost_rpt(t,"Electricity revenue (billion)")                           / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;
TotalSystemCost_rpt(t,"Carbon price (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0)                                         = 1e+3 * TotalSystemCost_rpt(t,"Carbon price (billion)")                                  / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ;    
TotalSystemCost_rpt(t,"Carbon price (EUR per ton)")                                                                                     = co2prwei(t) ;                             

TotalSystemCost_rpt(t,"Subsidies rnw (EUR per MWh rnw)")$(sum(r, sum(rnw(i),  gen(i,r,t))) > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies rnw (billion)") / sum(r, sum(rnw(i),  gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Subsidies nuclear (EUR per MWh nuc)")$(sum(r, sum(nuc(i),  gen(i,r,t))) > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies nuclear (billion)") / sum(r, sum(nuc(i),  gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Subsidies (EUR per MWh rnw + nuc)")$(sum(r, sum(nuc(i),  gen(i,r,t))) > 0 and sum(r, sum(rnw(i),  gen(i,r,t))) > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies (billion)") / sum(r, sum(nuc(i),  gen(i,r,t)) + sum(rnw(i),  gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Subsidies (EUR per MWh rnw + nuc)")$(sum(r, sum(nuc(i),  gen(i,r,t))) > 0 and sum(r, sum(rnw(i),  gen(i,r,t))) = 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies (billion)") / sum(r, sum(nuc(i),  gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Subsidies (EUR per MWh rnw + nuc)")$(sum(r, sum(nuc(i),  gen(i,r,t))) = 0 and sum(r, sum(rnw(i),  gen(i,r,t))) > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies (billion)") / sum(r, sum(rnw(i),  gen(i,r,t))) + eps ;
TotalSystemCost_rpt(t,"Subsidies rnw (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies rnw (billion)") / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ; 
TotalSystemCost_rpt(t,"Subsidies nuclear (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies nuclear (billion)") / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ; 
TotalSystemCost_rpt(t,"Subsidies (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0) = 1e+3 * TotalSystemCost_rpt(t,"Subsidies (billion)") / TotalSystemCost_rpt(t,"Demand (TWh)") + eps ; 

* * Aggregate cost
AggreSystemCost_rpt(r,set_systemcost,"nodisc")      = sum(t$(t.val > 2022), nyrs(t)  * SystemCost_rpt(t,r,set_systemcost)) ;
AggreSystemCost_rpt(r,set_systemcost,"npv")         = sum(t$(t.val > 2022), dfact(t) * SystemCost_rpt(t,r,set_systemcost)) ;
* Relative cost
AggreSystemCost_rpt(r,"Industrial demand share",value)                                           = sum(t$(t.val > 2022), nyrs(t)  * daref_sec(r,t,"ind"))        / sum(t$(t.val > 2022), nyrs(t)  * daref(r,t)) + eps ;
AggreSystemCost_rpt(r,"Renewable share",value)                                                   = sum(t$(t.val > 2022), nyrs(t)  * sum(rnw(i),  gen(i,r,t)))    / sum(t$(t.val > 2022), nyrs(t)  * sum(i,      gen(i,r,t))) + eps ;
AggreSystemCost_rpt(r,"Intermittent renewable share",value)                                      = sum(t$(t.val > 2022), nyrs(t)  * sum(irnw(i), gen(i,r,t)))    / sum(t$(t.val > 2022), nyrs(t)  * sum(i,      gen(i,r,t))) + eps ;
AggreSystemCost_rpt(r,"Low carbon share",value)                                                  = sum(t$(t.val > 2022), nyrs(t)  * sum(lowcarb(i), gen(i,r,t))) / sum(t$(t.val > 2022), nyrs(t)  * sum(i,      gen(i,r,t))) + eps ;
AggreSystemCost_rpt(r,"System cost (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                                         = 1e+3 * AggreSystemCost_rpt(r,"System cost (billion)",value)                                   / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"System cost after trade (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                             = 1e+3 * AggreSystemCost_rpt(r,"System cost after trade (billion)",value)                       / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"System cost after taxes (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                             = 1e+3 * AggreSystemCost_rpt(r,"System cost after taxes (billion)",value)                       / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"System cost after damages (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                           = 1e+3 * AggreSystemCost_rpt(r,"System cost after damages (billion)",value)                     / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"System cost after taxes and damages (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                 = 1e+3 * AggreSystemCost_rpt(r,"System cost after taxes and damages (billion)",value)           / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"System cost after trade and taxes (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                   = 1e+3 * AggreSystemCost_rpt(r,"System cost after trade and taxes (billion)",value)             / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"System cost after trade and taxes and damages (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)       = 1e+3 * AggreSystemCost_rpt(r,"System cost after trade and taxes and damages (billion)",value) / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"Electricity price (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                                   = 1e+3 * AggreSystemCost_rpt(r,"Electricity revenue (billion)",value)                           / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;
AggreSystemCost_rpt(r,"Carbon price (EUR per MWh)",value)$(AggreSystemCost_rpt(r,"Demand (TWh)",value) > 0)                                        = 1e+3 * AggreSystemCost_rpt(r,"Carbon price (billion)",value)                                  / AggreSystemCost_rpt(r,"Demand (TWh)",value) ;                       
AggreSystemCost_rpt(r,"Carbon price (EUR per ton)",value)$(AggreSystemCost_rpt(r,"CO2 (Mt)",value) ne 0)                                           = 1e+3 * AggreSystemCost_rpt(r,"Carbon price (billion)",value)                                  / abs(AggreSystemCost_rpt(r,"CO2 (Mt)",value)) ;

* * Total aggregate cost
TotalAggreSystemCost_rpt(set_systemcost,"nodisc")      = sum(t$(t.val > 2022), nyrs(t)  * TotalSystemCost_rpt(t,set_systemcost)) ;
TotalAggreSystemCost_rpt(set_systemcost,"npv")         = sum(t$(t.val > 2022), dfact(t) * TotalSystemCost_rpt(t,set_systemcost)) ;
* Relative cost
TotalAggreSystemCost_rpt("Industrial demand share",value)                                           = sum(t$(t.val > 2022), nyrs(t)  * sum(r, daref_sec(r,t,"ind")))        / sum(t$(t.val > 2022), nyrs(t)  * sum(r, daref(r,t))) + eps ;
TotalAggreSystemCost_rpt("Renewable share",value)                                                   = sum(t$(t.val > 2022), nyrs(t)  * sum(r, sum(rnw(i),  gen(i,r,t))))    / sum(t$(t.val > 2022), nyrs(t)  * sum(r, sum(i,      gen(i,r,t)))) + eps ;
TotalAggreSystemCost_rpt("Intermittent renewable share",value)                                      = sum(t$(t.val > 2022), nyrs(t)  * sum(r, sum(irnw(i), gen(i,r,t))))    / sum(t$(t.val > 2022), nyrs(t)  * sum(r, sum(i,      gen(i,r,t)))) + eps ;
TotalAggreSystemCost_rpt("Low carbon share",value)                                                  = sum(t$(t.val > 2022), nyrs(t)  * sum(r, sum(lowcarb(i), gen(i,r,t)))) / sum(t$(t.val > 2022), nyrs(t)  * sum(r, sum(i,      gen(i,r,t)))) + eps ;
TotalAggreSystemCost_rpt("System cost (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                                         = 1e+3 * TotalAggreSystemCost_rpt("System cost (billion)",value)                                   / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("System cost after trade (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                             = 1e+3 * TotalAggreSystemCost_rpt("System cost after trade (billion)",value)                       / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("System cost after taxes (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                             = 1e+3 * TotalAggreSystemCost_rpt("System cost after taxes (billion)",value)                       / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("System cost after damages (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                           = 1e+3 * TotalAggreSystemCost_rpt("System cost after damages (billion)",value)                     / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("System cost after taxes and damages (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                 = 1e+3 * TotalAggreSystemCost_rpt("System cost after taxes and damages (billion)",value)           / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("System cost after trade and taxes (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                   = 1e+3 * TotalAggreSystemCost_rpt("System cost after trade and taxes (billion)",value)             / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("System cost after trade and taxes and damages (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)       = 1e+3 * TotalAggreSystemCost_rpt("System cost after trade and taxes and damages (billion)",value) / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("Electricity price (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                                   = 1e+3 * TotalAggreSystemCost_rpt("Electricity revenue (billion)",value)                           / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;
TotalAggreSystemCost_rpt("Carbon price (EUR per MWh)",value)$(TotalAggreSystemCost_rpt("Demand (TWh)",value) > 0)                                        = 1e+3 * TotalAggreSystemCost_rpt("Carbon price (billion)",value)                                  / TotalAggreSystemCost_rpt("Demand (TWh)",value) ;                       
TotalAggreSystemCost_rpt("Carbon price (EUR per ton)",value)$(TotalAggreSystemCost_rpt("CO2 (Mt)",value) ne 0)                                           = 1e+3 * TotalAggreSystemCost_rpt("Carbon price (billion)",value)                                  / abs(TotalAggreSystemCost_rpt("CO2 (Mt)",value)) ;

parameters
Interval_rpt(r,*)
TotalInterval_rpt(*)
;

Interval_rpt(r,"Wind (TWh)")                                                    = AggreSystemCost_rpt(r,"Generation wind (TWh)","nodisc") ;
Interval_rpt(r,"Solar (TWh)")                                                   = AggreSystemCost_rpt(r,"Generation solar (TWh)","nodisc") ;
Interval_rpt(r,"Nuclear (TWh)")                                                 = AggreSystemCost_rpt(r,"Generation nuclear (TWh)","nodisc") ;
Interval_rpt(r,"CCS (TWh)")                                                     = AggreSystemCost_rpt(r,"Generation ccs (TWh)","nodisc") ;
Interval_rpt(r,"CO2 (Mt)")                                                      = AggreSystemCost_rpt(r,"CO2 (Mt)","nodisc") ;
Interval_rpt(r,"Demand (TWh)")                                                  = AggreSystemCost_rpt(r,"Demand (TWh)","nodisc") ;
Interval_rpt(r,"Industrial demand (TWh)")                                       = AggreSystemCost_rpt(r,"Industrial demand (TWh)","nodisc") ;
Interval_rpt(r,"System cost (EUR per MWh)")                                     = AggreSystemCost_rpt(r,"System cost (EUR per MWh)","nodisc") ;
Interval_rpt(r,"System cost after trade (EUR per MWh)")                         = AggreSystemCost_rpt(r,"System cost after trade (EUR per MWh)","nodisc") ;
Interval_rpt(r,"System cost after trade and taxes (EUR per MWh)")               = AggreSystemCost_rpt(r,"System cost after trade and taxes (EUR per MWh)","nodisc") ;
Interval_rpt(r,"System cost after trade and taxes and damages (EUR per MWh)")   = AggreSystemCost_rpt(r,"System cost after trade and taxes and damages (EUR per MWh)","nodisc") ;
Interval_rpt(r,"Electricity price (EUR per MWh)")                               = AggreSystemCost_rpt(r,"Electricity price (EUR per MWh)","nodisc") ;
Interval_rpt(r,"Carbon price (EUR per MWh)")                                    = AggreSystemCost_rpt(r,"Carbon price (EUR per MWh)","nodisc") ;
Interval_rpt(r,"Carbon price (EUR per ton)")                                    = AggreSystemCost_rpt(r,"Carbon price (EUR per ton)","nodisc") ;

TotalInterval_rpt("Wind (TWh)")                                                    = TotalAggreSystemCost_rpt("Generation wind (TWh)","nodisc") ;
TotalInterval_rpt("Solar (TWh)")                                                   = TotalAggreSystemCost_rpt("Generation solar (TWh)","nodisc") ;
TotalInterval_rpt("Nuclear (TWh)")                                                 = TotalAggreSystemCost_rpt("Generation nuclear (TWh)","nodisc") ;
TotalInterval_rpt("CCS (TWh)")                                                     = TotalAggreSystemCost_rpt("Generation ccs (TWh)","nodisc") ;
TotalInterval_rpt("CO2 (Mt)")                                                      = TotalAggreSystemCost_rpt("CO2 (Mt)","nodisc") ;
TotalInterval_rpt("Demand (TWh)")                                                  = TotalAggreSystemCost_rpt("Demand (TWh)","nodisc") ;
TotalInterval_rpt("Industrial demand (TWh)")                                       = TotalAggreSystemCost_rpt("Industrial demand (TWh)","nodisc") ;
TotalInterval_rpt("System cost (EUR per MWh)")                                     = TotalAggreSystemCost_rpt("System cost (EUR per MWh)","nodisc") ;
TotalInterval_rpt("System cost after trade (EUR per MWh)")                         = TotalAggreSystemCost_rpt("System cost after trade (EUR per MWh)","nodisc") ;
TotalInterval_rpt("System cost after trade and taxes (EUR per MWh)")               = TotalAggreSystemCost_rpt("System cost after trade and taxes (EUR per MWh)","nodisc") ;
TotalInterval_rpt("System cost after trade and taxes and damages (EUR per MWh)")   = TotalAggreSystemCost_rpt("System cost after trade and taxes and damages (EUR per MWh)","nodisc") ;
TotalInterval_rpt("Electricity price (EUR per MWh)")                               = TotalAggreSystemCost_rpt("Electricity price (EUR per MWh)","nodisc") ;
TotalInterval_rpt("Carbon price (EUR per MWh)")                                    = TotalAggreSystemCost_rpt("Carbon price (EUR per MWh)","nodisc") ;
TotalInterval_rpt("Carbon price (EUR per ton)")                                    = TotalAggreSystemCost_rpt("Carbon price (EUR per ton)","nodisc") ;

parameter
ihk_rpt(t,*)
ihk_GER_rpt(t,*)

;

ihk_rpt(t,"Price (EUR per MWh)") = Electricity1_total_rpt(t,"price-avg") ;
ihk_rpt(t,"CO2 price (EUR per ton)") = co2prwei(t) ;
ihk_rpt(t,"CO2 price (EUR per MWh)") = TotalSystemCost_rpt(t,"Carbon price (EUR per MWh)") ;

ihk_rpt(t,"Wind market value (EUR per MWh)") = Electricity2_total_rpt(t,"price-wind");
ihk_rpt(t,"Solar market value (EUR per MWh)") = Electricity2_total_rpt(t,"price-sol") ;
ihk_rpt(t,"Biomass market value (EUR per MWh)") = Electricity2_total_rpt(t,"price-bio") ;
ihk_rpt(t,"Nuclear market value (EUR per MWh)") = Electricity2_total_rpt(t,"price-nuc") ;
ihk_rpt(t,"Gas market value (EUR per MWh)") = Electricity2_total_rpt(t,"price-gas") ;
ihk_rpt(t,"CCS market value (EUR per MWh)") = Electricity2_total_rpt(t,"price-ccs") ;

ihk_rpt(t,"Renewable subsidies (EUR per MWh rnw)") = TotalSystemCost_rpt(t,"Subsidies rnw (EUR per MWh rnw)") ;
ihk_rpt(t,"Nuclear subsidies (EUR per MWh nuc)") = TotalSystemCost_rpt(t,"Subsidies nuclear (EUR per MWh nuc)") ;
ihk_rpt(t,"Total subsidies (EUR per MWh rnw + nuc)") = TotalSystemCost_rpt(t,"Subsidies (EUR per MWh rnw + nuc)") ;
ihk_rpt(t,"Renewable subsidies (EUR per MWh)") = TotalSystemCost_rpt(t,"Subsidies rnw (EUR per MWh)") ;
ihk_rpt(t,"Nuclear subsidies (EUR per MWh)") = TotalSystemCost_rpt(t,"Subsidies nuclear (EUR per MWh)") ;
ihk_rpt(t,"Total subsidies (EUR per MWh)") = TotalSystemCost_rpt(t,"Subsidies (EUR per MWh)") ;

ihk_rpt(t,"Social cost of carbon (EUR per ton)") = scc_t(t) + eps ;
ihk_rpt(t,"Social cost of carbon (billion)") = sum(r, co2emit(r,t)) * scc_t(t) * 1e-3 + eps ;
ihk_rpt(t,"Social cost of carbon (EUR per MWh)")$(TotalSystemCost_rpt(t,"Demand (TWh)") > 0) = 1e+3 * ihk_rpt(t,"Social cost of carbon (billion)") / TotalSystemCost_rpt(t,"Demand (TWh)") ;      

ihk_rpt(t,"Price incl. subsidies (EUR per MWh)") = ihk_rpt(t,"Price (EUR per MWh)") + ihk_rpt(t,"Total subsidies (EUR per MWh)") ;
ihk_rpt(t,"Price less CO2 plus SCC (EUR per MWh)") = ihk_rpt(t,"Price (EUR per MWh)") - ihk_rpt(t,"CO2 price (EUR per MWh)") + ihk_rpt(t,"Social cost of carbon (EUR per MWh)") ;
ihk_rpt(t,"Price less CO2 plus SCC incl. subsidies (EUR per MWh)") = ihk_rpt(t,"Price (EUR per MWh)") - ihk_rpt(t,"CO2 price (EUR per MWh)") + ihk_rpt(t,"Social cost of carbon (EUR per MWh)") + ihk_rpt(t,"Total subsidies (EUR per MWh)") ;

ihk_GER_rpt(t,"Price (EUR per MWh)") = Electricity1_GER_rpt(t,"price-avg") ;
ihk_GER_rpt(t,"CO2 price (EUR per ton)") = co2prreg("Germany",t) ;
ihk_GER_rpt(t,"CO2 price (EUR per MWh)") = SystemCost_GER_rpt(t,"Carbon price (EUR per MWh)") ;

ihk_GER_rpt(t,"Wind market value (EUR per MWh)") = Electricity2_GER_rpt(t,"price-wind");
ihk_GER_rpt(t,"Solar market value (EUR per MWh)") = Electricity2_GER_rpt(t,"price-sol") ;
ihk_GER_rpt(t,"Biomass market value (EUR per MWh)") = Electricity2_GER_rpt(t,"price-bio") ;
ihk_GER_rpt(t,"Nuclear market value (EUR per MWh)") = Electricity2_GER_rpt(t,"price-nuc") ;
ihk_GER_rpt(t,"Gas market value (EUR per MWh)") = Electricity2_GER_rpt(t,"price-gas") ;
ihk_GER_rpt(t,"CCS market value (EUR per MWh)") = Electricity2_GER_rpt(t,"price-ccs") ;

ihk_GER_rpt(t,"Renewable subsidies (EUR per MWh rnw)") = SystemCost_GER_rpt(t,"Subsidies rnw (EUR per MWh rnw)") ;
ihk_GER_rpt(t,"Nuclear subsidies (EUR per MWh nuc)") = SystemCost_GER_rpt(t,"Subsidies nuclear (EUR per MWh nuc)") ;
ihk_GER_rpt(t,"Total subsidies (EUR per MWh rnw + nuc)") = SystemCost_GER_rpt(t,"Subsidies (EUR per MWh rnw + nuc)") ;
ihk_GER_rpt(t,"Renewable subsidies (EUR per MWh)") = SystemCost_GER_rpt(t,"Subsidies rnw (EUR per MWh)") ;
ihk_GER_rpt(t,"Nuclear subsidies (EUR per MWh)") = SystemCost_GER_rpt(t,"Subsidies nuclear (EUR per MWh)") ;
ihk_GER_rpt(t,"Total subsidies (EUR per MWh)") = SystemCost_GER_rpt(t,"Subsidies (EUR per MWh)") ;

ihk_GER_rpt(t,"Social cost of carbon (EUR per ton)") = ihk_rpt(t,"Social cost of carbon (EUR per ton)") ;
ihk_GER_rpt(t,"Social cost of carbon (billion)") = sum(r$(sameas(r,"Germany")), co2emit(r,t)) * ihk_GER_rpt(t,"Social cost of carbon (EUR per ton)") * 1e-3 + eps ;
ihk_GER_rpt(t,"Social cost of carbon (EUR per MWh)") = 1e+3 * ihk_GER_rpt(t,"Social cost of carbon (billion)") / SystemCost_GER_rpt(t,"Demand (TWh)") ;


ihk_GER_rpt(t,"Social cost of carbon (EUR per ton)") = scc_t(t) + eps ;
ihk_GER_rpt(t,"Social cost of carbon (billion)") = co2emit("Germany",t) * scc_t(t) * 1e-3 + eps ;
ihk_GER_rpt(t,"Social cost of carbon (EUR per MWh)")$(SystemCost_GER_rpt(t,"Demand (TWh)") > 0) = 1e+3 * ihk_GER_rpt(t,"Social cost of carbon (billion)") / SystemCost_GER_rpt(t,"Demand (TWh)") ;   

ihk_GER_rpt(t,"Price incl. subsidies (EUR per MWh)") = ihk_GER_rpt(t,"Price (EUR per MWh)") + ihk_GER_rpt(t,"Total subsidies (EUR per MWh)") ;
ihk_GER_rpt(t,"Price less CO2 plus SCC (EUR per MWh)") = ihk_GER_rpt(t,"Price (EUR per MWh)") - ihk_GER_rpt(t,"CO2 price (EUR per MWh)") + ihk_GER_rpt(t,"Social cost of carbon (EUR per MWh)") ;
ihk_GER_rpt(t,"Price less CO2 plus SCC incl. subsidies (EUR per MWh)") = ihk_GER_rpt(t,"Price (EUR per MWh)") - ihk_GER_rpt(t,"CO2 price (EUR per MWh)") + ihk_GER_rpt(t,"Social cost of carbon (EUR per MWh)") + ihk_GER_rpt(t,"Total subsidies (EUR per MWh)") ;

pricetot_rrpt("sys-sub",t) = ihk_rpt(t,"Total subsidies (EUR per MWh)") + eps ;
pricetot_rrpt("GER-sub",t) = ihk_GER_rpt(t,"Total subsidies (EUR per MWh)") + eps ;
