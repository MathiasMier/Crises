* * * Define reporting type for simplification
set
tyrpt                    Reporting technology types
                         /Bioenergy, Coal, Gas-CCGT, Gas-OCGT, Gas-ST, Geothermal, Hydro, Lignite, Nuclear,
                         OilOther, Solar, WindOff, WindOn, Bio-CCS, Coal-CCS, Gas-CCS, Lignite-CCS/
xtyperpt(tyrpt,type)     Map between technology types
                         /Bioenergy.biow, Coal.hdcl, Gas-CCGT.ngcc, Gas-OCGT.nggt, Gas-ST.ngst, Geothermal.geot,
                         Hydro.Hydro, Lignite.lign, Nuclear.nuc, OilOther.ptsg, Solar.slpv,
                         WindOff.wind-os, WindOn.wind, Bio-CCS.becs, Coal-CCS.hdcs, Gas-CCS.ngcs, Lignite-CCS.lics/

rrpt                     Reporting regions
                         /North, South, Central, East/

xrrpt(rrpt,r)            Map rrpt r
$if      set spatial28                        /North.(Britain, Ireland, Norway, Sweden, Finland, Denmark),
$if      set spatial28                         South.(Portugal, Spain, France, Italy),
$if      set spatial28                         Central.(Belgium, Luxembourg, Netherlands, Austria, Switzerland, Germany)
$if      set spatial28                         East.(Czech,Poland,Slovakia,Estonia,Lithuania,Latvia,Croatia,Hungary,Slovenia,Bulgaria,Greece,Romania)/

$if      set spatialveryhigh                       /North.(Britain, Ireland, Norway, Sweden, Finland, Denmark),
$if      set spatialveryhigh                        South.(Portugal, Spain, France, Italy),
$if      set spatialveryhigh                        Central.(Belux, Netherlands, Austria, Switzerland, Germany)
$if      set spatialveryhigh                        East.(Czeslovak,Poland,EE-NE,EE-SW,Bulgaria,Greece,Romania)/

$if      set spatialhigh                           /North.(Britain, Norway, Sweden, Finland, Denmark),
$if      set spatialhigh                            South.(Iberia, France, Italy),
$if      set spatialhigh                            Central.(Belux, Netherlands, Alpine, Germany)
$if      set spatialhigh                            East.(Czeslovak,Poland,EE-NE,EE-SW,EE-SE)/

$if      set spatialmid                            /North.(Britain, Scanda),
$if      set spatialmid                             South.(Iberia, France, Italy),
$if      set spatialmid                             Central.(Benelux, Alpine, Germany)
$if      set spatialmid                             East.(EE-NW,EE-NE,EE-SW,EE-SE)/

$if      set spatiallow                            /North.(Britain, Scanda),
$if      set spatiallow                             South.(Iberia, France, Italy),
$if      set spatiallow                             Central.(Central)
$if      set spatiallow                             East.(East)/

$if      set spatialverylow                        /North.(North),
$if      set spatialverylow                         South.(South),
$if      set spatialverylow                         Central.(Central)
$if      set spatialverylow                         East.(East)/

xrrpt_(rrpt,r)           Alias map rrpt r
;

alias(r,rr) ;
alias(rrpt,rrpt_) ;
xrrpt_(rrpt,r)$xrrpt(rrpt,r) = YES ;

$if      set days       $include    modules_rpt\euregen2024_days_rpt_v1
$if not  set days       $include    modules_rpt\euregen2024_segments_rpt_v1

* * * Emissions reporting
parameters
co2emit(r,t)                     CO2 emissions (MtCO2)
co2emit_fuel(r,t,fuel)           CO2 emissions by fuel (MtCO2)
co2preu(t)                       CO2 emissions price (EUR per tCO2) in EU
co2pruk(t)                       CO2 emissions price (EUR per tCO2) in UK
co2prreg(r,t)                    Regional CO2 emissions price (EUR per tCO2)
co2prwei(t)                      Emission-weighted CO2 price (EUR per tCO2)
co2capt(r,t)                     CO2 emissions captued (MtCO2)
Emissions_rpt(t,r,*)             Emissions reporting vector (MtC2)
Emissions_ByFuel_rpt(t,r,*)      Emissions by fuel reporting vector (MtCO2)
Emissions_total_ByFuel_rpt(t,*)  Total emissions by fuel reporting vector (MtCO2)
Emissions_GER_ByFuel_rpt(t,*)    Total emissions by fuel reporting vector (MtCO2)
Emissions_total_rpt(t,*)         Total emissions reporting vector (MtC2)
Emissions_GER_rpt(t,*)           Total emissions reporting vector (MtC2)
Emissions_FR_rpt(t,*)           Total emissions reporting vector (MtC2)
gasuse(r,t)
gasuseeu(t)

;

gasuse(r,t)             = sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t) / effrate(i,v,r)) ;
gasuseeu(t)             = sum(r, gasuse(r,t)) ;
co2emit(r,t)            = sum(ivrt(i,v,r,t), emit(i,v,r) * XTWHL(i,v,r,t)) ;
co2emit_fuel(r,t,fuel)  = sum(xfueli(fuel,i), sum(ivrt(i,v,r,t), emit(i,v,r) * XTWHL(i,v,r,t))) ;
co2capt(r,t)            = sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWHL(i,v,r,t)) ;

co2preu(t)         = 0
*$if      set scc                            + scc(t)
$if not  set static $if      set co2price   + co2p(t)
$if not  set static $if      set co2mark    + abs(euets.M(t)) / dfact(t)
$if not  set static $if      set co2iter    + abs(it_euets.M(t)) / dfact(t)
$if not  set static $if      set co2mips    + abs(eqs_euets.M(t)) / dfact(t)
$if      set static $if      set co2price   + co2price_model_ave("Germany",t)
$if      set static $if not  set co2price   + euets_static.M(t)



                ;
                
co2pruk(t) = 0
*$if      set scc                            + scc(t)
$if not  set static $if      set co2price   + co2p(t)
$if not  set static $if not  set co2price   + abs(ukets.M(t)) / dfact(t)
$if      set static $if      set co2price   + co2price_model_ave("Britain",t)
$if      set static $if not  set co2price   + abs(ukets_static.M(t))  / dfact(t)
                ;
   
co2prreg(r,t)$(not sameas(r,"Britain")) = co2preu(t) ;
co2prreg("Britain",t) = co2pruk(t) ;

$if      set euetsbreak  co2prreg(r,t)$(not sameas(r,"Britain") and t.val ge 2031) = abs(co2market_r.M(r,t)) / dfact(t) ;
$if      set euetsbreak  co2prreg(r,t)$(not sameas(r,"Britain") and t.val le 2030) = co2preu(t) ;

co2prwei(t)$(sum(r, co2emit(r,t)) ne 0) = sum(r, co2emit(r,t) * co2prreg(r,t)) / sum(r, co2emit(r,t)) ;


* CO2 emissions is the dummy for the python algorithm but could be filled by assumptions about remaining EU ETS emissions (for example)
Emissions_rpt(t,r,"CO2-emissions")                      = eps ;
Emissions_rpt(t,r,"CO2-emissions-elec")                 = co2emit(r,t) + eps ;
Emissions_rpt(t,r,"CO2-price")                          = co2prreg(r,t) + eps ;
Emissions_rpt(t,r,"CO2-captured")                       = co2capt(r,t) + eps ;

Emissions_total_rpt(t,"Total CO2-emissions-elec")       = sum(r, co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"Total CO2-captured")             = sum(r, co2capt(r,t)) + eps ;
Emissions_total_rpt(t,"CO2 price")                      = co2prwei(t) + eps ;

Emissions_total_rpt(t,"Total CO2-emissions-elec-eu")    = sum(r$(not sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"Total CO2-captured-eu")          = sum(r$(not sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_total_rpt(t,"CO2 price-eu")                   = co2preu(t) + eps ;

Emissions_total_rpt(t,"Total CO2-emissions-elec-uk")    = sum(r$(sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"Total CO2-captured-uk")          = sum(r$(sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_total_rpt(t,"CO2 price-uk")                   = co2pruk(t) + eps ;

Emissions_total_rpt(t,"CO2elec-EUETS") = sum(reu(r), co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"CO2ind-EUETS") = eps ;
Emissions_total_rpt(t,"CO2-EUETS") = sum(reu(r), co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"CO2elec-UKETS") = sum(rbr(r), co2emit(r,t)) + eps ;

Emissions_GER_rpt(t,"Total CO2-emissions-elec")         = sum(r$(sameas(r,"Germany")), co2emit(r,t)) + eps ;
Emissions_GER_rpt(t,"Total CO2-captured")               = sum(r$(sameas(r,"Germany")), co2capt(r,t)) + eps ;
Emissions_GER_rpt(t,"CO2 price")                        = co2prreg("Germany",t) + eps ;

Emissions_GER_rpt(t,"Total CO2-emissions-elec-eu")    = sum(r$(not sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_GER_rpt(t,"Total CO2-captured-eu")          = sum(r$(not sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_GER_rpt(t,"CO2 price-eu")                   = co2preu(t) + eps ;

Emissions_GER_rpt(t,"Total CO2-emissions-elec-uk")    = sum(r$(sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_GER_rpt(t,"Total CO2-captured-uk")          = sum(r$(sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_GER_rpt(t,"CO2 price-uk")                   = co2pruk(t) + eps ;

Emissions_FR_rpt(t,"Total CO2-emissions-elec")          = sum(r$(sameas(r,"France")), co2emit(r,t)) + eps ;
Emissions_FR_rpt(t,"Total CO2-captured")                = sum(r$(sameas(r,"France")), co2capt(r,t)) + eps ;
Emissions_FR_rpt(t,"CO2 price")                         = co2prreg("France",t) + eps ;

Emissions_FR_rpt(t,"Total CO2-emissions-elec-eu")    = sum(r$(not sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_FR_rpt(t,"Total CO2-captured-eu")          = sum(r$(not sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_FR_rpt(t,"CO2 price-eu")                   = co2preu(t) + eps ;

Emissions_FR_rpt(t,"Total CO2-emissions-elec-uk")    = sum(r$(sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_FR_rpt(t,"Total CO2-captured-uk")          = sum(r$(sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_FR_rpt(t,"CO2 price-uk")                   = co2pruk(t) + eps ;

Emissions_ByFuel_rpt(t,r,"CO2-emissions-bioenergy")     = co2emit_fuel(r,t,"Bioenergy") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-coal")          = co2emit_fuel(r,t,"Coal") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-gas")           = co2emit_fuel(r,t,"Gas") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-lignite")       = co2emit_fuel(r,t,"Lignite") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-oil/other")     = co2emit_fuel(r,t,"Oil") + eps ;

Emissions_total_ByFuel_rpt(t,"CO2-emissions-bioenergy") = sum(r, co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-coal")      = sum(r, co2emit_fuel(r,t,"Coal")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-gas")       = sum(r, co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-lignite")   = sum(r, co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-oil/other") = sum(r, co2emit_fuel(r,t,"Oil")) + eps ;

Emissions_total_ByFuel_rpt(t,"CO2-emissions-bioenergy-eu") = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-coal-eu")      = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Coal")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-gas-eu")       = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-lignite-eu")   = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-oil/other-eu") = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Oil")) + eps ;

Emissions_total_ByFuel_rpt(t,"CO2-emissions-bioenergy-uk") = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-coal-uk")      = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Coal")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-gas-uk")       = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-lignite-uk")   = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-oil/other-uk") = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Oil")) + eps ;

Emissions_GER_ByFuel_rpt(t,"CO2-emissions-bioenergy") =  sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-coal") =       sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Coal")) + eps;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-gas") =        sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-lignite") =    sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-oil/other") =  sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Oil")) + eps ;

$include    modules_rpt\euregen2024_electricity_rpt_v1

* * * Electricity generation reporting
Parameters
gen(i,r,t)                                       Generation by technology (TWh)
gen_type(type,r,t)                               Generation by type (TWh)
gen_xtype(tyrpt,r,t)                             Generation by xtype (TWh)
ElectricityGeneration_rpt(t,r,i)                 Electricity generation reporting vector (TWh)
ElectricityGeneration_total_rpt(t,i)             Electricity generation reporting vector (TWh)
ElectricityGeneration_type_rpt(t,r,type)         Electricity generation reporting vector (TWh)
ElectricityGeneration_total_type_rpt(t,type)     Electricity generation reporting vector (TWh)
ElectricityGeneration_xtype_rpt(t,r,tyrpt)       Electricity generation reporting vector (TWh)
ElectricityGeneration_total_xtype_rpt(t,tyrpt)   Electricity generation reporting vector (TWh)
ElectricityGeneration_GER_xtype_rpt(t,tyrpt)     Electricity generation reporting vector (TWh)
ElectricityGeneration_FR_xtype_rpt(t,tyrpt)     Electricity generation reporting vector (TWh)
;

gen(i,r,t)               = sum(ivrt(i,v,r,t), XTWHL(i,v,r,t)) ;
gen_type(type,r,t)       = sum(idef(i,type), gen(i,r,t));
gen_xtype(tyrpt,r,t)     = sum(xtyperpt(tyrpt,type), gen_type(type,r,t)) ;

ElectricityGeneration_rpt(t,r,i)                 = gen(i,r,t) ;
ElectricityGeneration_total_rpt(t,i)             = sum(r, gen(i,r,t)) ;
ElectricityGeneration_type_rpt(t,r,type)         = gen_type(type,r,t) ;
ElectricityGeneration_total_type_rpt(t,type)     = sum(r, gen_type(type,r,t)) ;
ElectricityGeneration_xtype_rpt(t,r,tyrpt)       = gen_xtype(tyrpt,r,t) ;
ElectricityGeneration_total_xtype_rpt(t,tyrpt)   = sum(r, gen_xtype(tyrpt,r,t)) + eps ;
ElectricityGeneration_GER_xtype_rpt(t,tyrpt)     = sum(r$sameas(r,"Germany"), gen_xtype(tyrpt,r,t)) + eps ;
ElectricityGeneration_FR_xtype_rpt(t,tyrpt)      = sum(r$sameas(r,"France"), gen_xtype(tyrpt,r,t)) + eps ;

* * * Intalled capacities reporting
Parameters
installed(i,r,t)                                 Installed capacity by region (GW)
installed_type(type,r,t)                         Installed capacity by region and type (GW)
InstalledCapacities_rpt(t,r,i)                   Installed capacities reporting vector (GW)
InstalledCapacities_total_rpt(t,i)               Installed capacities reporting vector (GW)
InstalledCapacities_type_rpt(t,r,type)           Installed capacities reporting vector (GW)
InstalledCapacities_total_type_rpt(t,type)       Installed capacities reporting vector (GW)
InstalledCapacities_xtype_rpt(t,r,tyrpt)         Installed capacities reporting vector (GW)
InstalledCapacities_total_xtype_rpt(t,tyrpt)     Installed capacities reporting vector (GW)
InstalledCapacities_GER_xtype_rpt(t,tyrpt)       Installed capacities reporting vector (GW)
InstalledCapacities_FR_xtype_rpt(t,tyrpt)        Installed capacities reporting vector (GW)
;

installed(i,r,t)         = sum(ivrt(i,v,r,t), XC.L(i,v,r,t)) ;
installed_type(type,r,t) = sum(idef(i,type), installed(i,r,t)) ;

InstalledCapacities_rpt(t,r,i)                   = installed(i,r,t) ;
InstalledCapacities_total_rpt(t,i)                = sum(r, installed(i,r,t)) ;
InstalledCapacities_type_rpt(t,r,type)           = installed_type(type,r,t) ;
InstalledCapacities_total_type_rpt(t,type)       = sum(r, installed_type(type,r,t)) ;
InstalledCapacities_xtype_rpt(t,r,tyrpt)         = sum(xtyperpt(tyrpt,type), installed_type(type,r,t)) ;
InstalledCapacities_total_xtype_rpt(t,tyrpt)     = sum(r, sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;
InstalledCapacities_GER_xtype_rpt(t,tyrpt)       = sum(r$sameas(r,"Germany"), sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;
InstalledCapacities_FR_xtype_rpt(t,tyrpt)        = sum(r$sameas(r,"France"), sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;

* * * Added capacities reporting
Parameters
added(i,r,t)                                     Added capacity by region (GW)
added_type(type,r,t)                             Added capacity by region and type (GW)
AddedCapacities_rpt(t,r,i)                       Added capacities reporting vector (GW)
AddedCapacities_total_rpt(t,i)                   Added capacities reporting vector (GW)
AddedCapacities_type_rpt(t,r,type)               Added capacities reporting vector (GW)
AddedCapacities_total_type_rpt(t,type)           Added capacities reporting vector (GW)
AddedCapacities_xtype_rpt(t,r,tyrpt)             Added capacities reporting vector (GW)
AddedCapacities_total_xtype_rpt(t,tyrpt)         Added capacities reporting vector (GW)
AddedCapacities_GER_xtype_rpt(t,tyrpt)           Added capacities reporting vector (GW)
AddedCapacities_FR_xtype_rpt(t,tyrpt)            Added capacities reporting vector (GW)
;

added(i,r,t)         = IX.L(i,r,t) ;
added_type(type,r,t) = sum(idef(i,type), added(i,r,t)) ;

AddedCapacities_rpt(t,r,i)                      = added(i,r,t) ;
AddedCapacities_total_rpt(t,i)                  = sum(r, added(i,r,t)) ;
AddedCapacities_type_rpt(t,r,type)              = added_type(type,r,t) ;
AddedCapacities_total_type_rpt(t,type)          = sum(r, added_type(type,r,t)) ;
AddedCapacities_xtype_rpt(t,r,tyrpt)            = sum(xtyperpt(tyrpt,type), added_type(type,r,t)) ;
AddedCapacities_total_xtype_rpt(t,tyrpt)        = sum(r, sum(xtyperpt(tyrpt,type), added_type(type,r,t))) + eps ;
AddedCapacities_GER_xtype_rpt(t,tyrpt)          = sum(r$sameas(r,"Germany"), sum(xtyperpt(tyrpt,type), added_type(type,r,t))) + eps ;
AddedCapacities_FR_xtype_rpt(t,tyrpt)           = sum(r$sameas(r,"France"), sum(xtyperpt(tyrpt,type), added_type(type,r,t))) + eps ;

parameters
AccAddedCapacities_xtype_rpt(t,r,tyrpt)
AccAddedCapacities_total_xtype_rpt(t,tyrpt)
AccAddedCapacities_GER_xtype_rpt(t,tyrpt)
AccAddedCapacities_FR_xtype_rpt(t,tyrpt)
;

AccAddedCapacities_xtype_rpt(t,r,tyrpt)         = sum(tt$(tt.val le t.val), AddedCapacities_xtype_rpt(tt,r,tyrpt)) + eps ;
AccAddedCapacities_total_xtype_rpt(t,tyrpt)     = sum(tt$(tt.val le t.val), AddedCapacities_total_xtype_rpt(tt,tyrpt)) + eps ;
AccAddedCapacities_GER_xtype_rpt(t,tyrpt)       = sum(tt$(tt.val le t.val), AddedCapacities_GER_xtype_rpt(tt,tyrpt)) + eps ;
AccAddedCapacities_FR_xtype_rpt(t,tyrpt)        = sum(tt$(tt.val le t.val), AddedCapacities_FR_xtype_rpt(tt,tyrpt)) + eps ;

$ontext
* * * Retired capacities reporting
parameters
retired(i,r,t)                                   Retired capacity by region (GW)
retired_type(type,r,t)                           Retired capacity by region and type (GW)
RetiredCapacities_rpt(t,r,i)                       Retired capacities reporting vector (GW)
RetiredCapacities_total_rpt(t,i)                 Retired capacities reporting vector (GW)
RetiredCapacities_type_rpt(t,r,type)               Retired capacities reporting vector (GW)
RetiredCapacities_total_type_rpt(t,type)         Retired capacities reporting vector (GW)
RetiredCapacities_xtype_rpt(t,r,tyrpt)             Retired capacities reporting vector (GW)
RetiredCapacities_total_xtype_rpt(t,tyrpt)       Retired capacities reporting vector (GW)
;

$if not  set static     retired(i,r,"2020")            = sum(ivrt(i,v,r,"2020"), XC.L(i,v,r,"2020") - capt(i,v,r,"2023")) ;
retired(i,r,t)$(t.val > 2022)  = sum(ivrt(i,v,r,t)$(t.val > v.val), XC.L(i,v,r,t-1) - XC.L(i,v,r,t)) ;
retired_type(type,r,t)         = sum(idef(i,type), retired(i,r,t)) ;

RetiredCapacities_rpt(t,r,i)               = retired(i,r,t) ;
RetiredCapacities_total_rpt(t,i)           = sum(r, retired(i,r,t)) ;
RetiredCapacities_type_rpt(t,r,type)       = retired_type(type,r,t) ;
RetiredCapacities_total_type_rpt(t,type)   = sum(r, retired_type(type,r,t)) ;
RetiredCapacities_xtype_rpt(t,r,tyrpt)     = sum(xtyperpt(tyrpt,type), retired_type(type,r,t)) ;
RetiredCapacities_total_xtype_rpt(t,tyrpt) = sum(r, sum(xtyperpt(tyrpt,type), retired_type(type,r,t))) + eps ;

* * * RetiredEarly capacities reporting
parameters
retiredearly(i,r,t)                                      RetiredEarly capacity by region (GW)
retiredearly_type(type,r,t)                              RetiredEarly capacity by region and type (GW)
RetiredEarlyCapacities_rpt(t,r,i)                        RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_total_rpt(t,i)                  RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_type_rpt(t,r,type)                RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_total_type_rpt(t,type)          RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_xtype_rpt(t,r,tyrpt)              RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_total_xtype_rpt(t,tyrpt)        RetiredEarly capacities reporting vector (GW)
;

$if not  set static     retiredearly(i,r,"2020")            = sum(ivrt(i,v,r,"2020"), XC.L(i,v,r,"2020") - capt(i,v,r,"2023")) ;
retiredearly(i,r,t)$(t.val > 2022)  = sum(ivrt(i,v,r,t)$(t.val > v.val), (XC.L(i,v,r,t-1) - XC.L(i,v,r,t)) * lifetime(i,v,r,t)) ;
retiredearly_type(type,r,t)         = sum(idef(i,type), retiredearly(i,r,t)) ;

RetiredEarlyCapacities_rpt(t,r,i)                        = retiredearly(i,r,t) ;
RetiredEarlyCapacities_total_rpt(t,i)                  = sum(r, retiredearly(i,r,t)) ;
RetiredEarlyCapacities_type_rpt(t,r,type)                = retiredearly_type(type,r,t) ;
RetiredEarlyCapacities_total_type_rpt(t,type)          = sum(r, retiredearly_type(type,r,t)) ;
RetiredEarlyCapacities_xtype_rpt(t,r,tyrpt)              = sum(xtyperpt(tyrpt,type), retiredearly_type(type,r,t)) ;
RetiredEarlyCapacities_total_xtype_rpt(t,tyrpt)        = sum(r, sum(xtyperpt(tyrpt,type), retiredearly_type(type,r,t))) + eps ;


* * * Cost caluclations
Parameters
inves_zeta(r,t)
inves_zeta_nodisc(r,t)
inves_normal(r,t)           Annual investment cost (EUR billions)
inves_annui(r,t)            Annual investment cost (EUR billions)
inves_ccost(r,t)            Annual investment cost (EUR billions)
inves_normal_nodisc(r,t)    Annual investment cost (EUR billions)
inves_annui_nodisc(r,t)     Annual investment cost (EUR billions)
inves_ccost_nodisc(r,t)     Annual investment cost (EUR billions)
fixed(r,t)                  Annual fixed cost (EUR billions)
varia(r,t)                  Annual dispatch cost(EUR billions)
lostl(r,t)                  Lost load 
carbt(r,t)                  Carbon tax (or EU ETS) revenue
airpt(r,t)                  Air pollution tax revenue
;

parameter
zeta_invir_rpt(inv,i,v,r)
zeta_invjr_rpt(inv,j,v,r)
zeta_invkr_rpt(inv,k,v,r)
zeta_invir_nodisc_rpt(inv,i,v,r)
zeta_invjr_nodisc_rpt(inv,j,v,r)
zeta_invkr_nodisc_rpt(inv,k,v,r)
;

$gdxin precal\precal_%n%.gdx
$load zeta_invir_rpt
$load zeta_invjr_rpt
$load zeta_invkr_rpt
$load zeta_invir_nodisc_rpt
$load zeta_invjr_nodisc_rpt
$load zeta_invkr_nodisc_rpt
$gdxin 


inves_zeta(r,t) =      1e-3 * sum(new(i),              sum(inv,  share(inv,i,r) * IX.L(i,r,t)    * sum(tv(t,v)$ivrt(i,v,r,t),  (capcost(i,v,r) + deccost(i,v,r)) * zeta_invir_rpt(inv,i,v,r)))) 
$if      set storage + 1e-3 * sum(newj(j),             sum(inv, gshare(inv,j,r) * IGL(j,r,t)    * sum(tv(t,v)$jvrt(j,v,r,t),  gcapcost(j,v,r)                   * zeta_invjr_rpt(inv,j,v,r)))) 
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t),  tcapcost(k,r,rr)                  * zeta_invkr_rpt(inv,k,v,r))))
;

inves_zeta_nodisc(r,t) =   1e-3 * sum(new(i),              sum(inv,  share(inv,i,r) * IX.L(i,r,t)    * sum(tv(t,v)$ivrt(i,v,r,t),  (capcost(i,v,r) + deccost(i,v,r)) * zeta_invir_nodisc_rpt(inv,i,v,r)))) 
$if      set storage     + 1e-3 * sum(newj(j),             sum(inv, gshare(inv,j,r) * IGL(j,r,t)    * sum(tv(t,v)$jvrt(j,v,r,t),  gcapcost(j,v,r)                   * zeta_invjr_nodisc_rpt(inv,j,v,r)))) 
$if      set trans       + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t),  tcapcost(k,r,rr)                  * zeta_invkr_nodisc_rpt(inv,k,v,r))))
;

inves_normal(r,t) =    1e-3 * sum(new,                                        IX.L(new,r,t)             * sum(tv(t,v)$ivrt(new,v,r,t),         capcost(new,v,r)           *  modeldepr(new,v,r,t) ))        * dfact(t) / nyrs(t)
$if      set storage + 1e-3 * sum(newj,                                       IGL(newj,r,t)            * sum(tv(t,v)$jvrt(newj,v,r,t),       gcapcost(newj,v,r)          * gmodeldepr(newj,v,r,t) ))       * dfact(t) / nyrs(t)
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t), tcapcost(k,r,rr) * tmodeldepr(k,v,r,t) )) * dfact(t) / nyrs(t)
;

inves_normal_nodisc(r,t) =    1e-3 * sum(new,                                        IX.L(new,r,t)             * sum(tv(t,v)$ivrt(new,v,r,t),         capcost(new,v,r)           *  modeldepr_nodisc(new,v,r,t) ))
$if      set storage        + 1e-3 * sum(newj,                                       IGL(newj,r,t)            * sum(tv(t,v)$jvrt(newj,v,r,t),       gcapcost(newj,v,r)          * gmodeldepr_nodisc(newj,v,r,t) ))
$if      set trans          + 1e-3 * sum((rr,k)$tmap(k,r,rr), IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t), tcapcost(k,r,rr) * tmodeldepr_nodisc(k,v,r,t) ))
;

inves_annui(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        *  annuity_ir(new,v,r)          * dfact(t) ))
$if      set storage + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * annuity_jr(newj,v,r)       * dfact(t) ))
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * annuity_kr(k,v,r) * dfact(t) ))
;

inves_annui_nodisc(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        *  annuity_ir(new,v,r)          * nyrs(t) ))
$if      set storage        + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * annuity_jr(newj,v,r)       * nyrs(t) ))
$if      set trans          + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * annuity_kr(k,v,r) * nyrs(t) ))
;

inves_ccost(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        * drate * dfact(t) ))
$if      set storage + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * drate * dfact(t) ))
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * drate * dfact(t) ))
;

inves_ccost_nodisc(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        * drate * nyrs(t) ))
$if      set storage        + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * drate * nyrs(t) ))
$if      set trans          + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * drate * nyrs(t) ))
;

fixed(r,t) =           1e-3 * sum(ivrt(i,v,r,t), XC.L(i,v,r,t) *  fomcost(i,v,r))
$if      set storage + 1e-3 * sum(jvrt(j,v,r,t), GCL(j,v,r,t) * gfomcost(j,v,r))
$if      set trans   + 1e-3 * sum((rr,k)$tcap(k,r,rr), (tcap(k,r,rr) + TC.L(k,r,rr,t)) * tfomcost(k,r,rr) )
;

varia(r,t) =           1e-3 * sum(ivrt(i,v,r,t), XTWHL(i,v,r,t) * discost(i,v,r,t))
$if not  set days   $if      set storage + 1e-6 * sum(jvrt(j,v,r,t), sum(s, (GL(s,j,v,r,t) + GDL(s,j,v,r,t)) * hours(s)) * gvomcost(j,v,r))
$if not  set days   $if      set trans   + 1e-6 * sum((rr,k)$tcap(k,r,rr), sum(s, E.L(s,k,r,rr,t) * hours(s)) * tvomcost(k,r,rr))

$if      set days   $if      set storage + 1e-6 * sum(jvrt(j,v,r,t), sum((sd,hd), (GL_D(sd,hd,j,v,r,t) + GDL_D(sd,hd,j,v,r,t)) * days(sd)) * gvomcost(j,v,r))
$if      set days   $if      set trans   + 1e-6 * sum((rr,k)$tcap(k,r,rr), sum((sd,hd), E_D.L(sd,hd,k,r,rr,t) * days(sd)) * tvomcost(k,r,rr))
;

$if not  set days   lostl(r,t) = 1e-6 * voll(r,t) * sum(s, BS.L(s,r,t) * hours(s)) ;
$if      set days   lostl(r,t) = 1e-6 * voll(r,t) * sum((sd,hd), BS_D.L(sd,hd,r,t) * days(sd)) ;

carbt(r,t) =
$if not  set days   $if      set scc         1e-6 * dfact_scc(t)  / dfact(t) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(s, XL(s,i,v,r,t) * hours(s))) ;
$if not  set days   $if not  set scc         1e-6 * sum(ivrt(i,v,r,t), co2prreg(r,t) * emit(i,v,r) * sum(s, XL(s,i,v,r,t) * hours(s))) ;
$if not  set days   $if      set socialcost airpt(r,t) = 1e-6 * dfact_scap(t) / dfact(t) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(s, XL(s,i,v,r,t) * hours(s))) ;

$if      set days   $if      set scc         1e-6 * dfact_scc(t)  / dfact(t) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum((sd,hd), XL_D(sd,hd,i,v,r,t) * days(sd))) ;
$if      set days   $if not  set scc         1e-6 * sum(ivrt(i,v,r,t), co2prreg(r,t) * emit(i,v,r) * sum((sd,hd), XL_D(sd,hd,i,v,r,t) * days(sd))) ;
$if      set days   $if      set socialcost airpt(r,t) = 1e-6 * dfact_scap(t) / dfact(t) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum((sd,hd), XL_D(sd,hd,i,v,r,t) * days(sd))) 
$offText

$ontext
parameters
EmissionsCost_rpt(t,*)
EmissionsCost_Accumulated_rpt(t,*)
;

EmissionsCost_rpt(t,"CO2 emissions (annual)")            = sum(r, co2emit(r,t)) ;
EmissionsCost_rpt(t,"CO2 emissions captured (annual)")   = sum(r, co2capt(r,t)) + eps ;
EmissionsCost_rpt(t,"CO2 price")                         = co2prwei(t) ;
$if not  set static     EmissionsCost_rpt(t,"CO2 emissions abated (annual)")     = sum(r, co2emit(r,"2020") - co2emit(r,t)) ;
$if      set static     EmissionsCost_rpt(t,"CO2 emissions abated (annual)")     = 0 ;
EmissionsCost_rpt(t,"Cost (annual)")                     = sum(r, inves_annui_nodisc(r,t) / nyrs(t) + fixed(r,t) + varia(r,t)) ;
EmissionsCost_rpt(t,"Investment cost (annual)")          = sum(r, inves_annui_nodisc(r,t) / nyrs(t)) ;
EmissionsCost_rpt(t,"Fixed cost (annual)")               = sum(r, fixed(r,t)) ;
EmissionsCost_rpt(t,"Variable cost (annual)")            = sum(r, varia(r,t)) ;

EmissionsCost_Accumulated_rpt(t,"CO2 emissions")            = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, co2emit(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"CO2 emissions captured")   = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, co2capt(r,tt))) + eps ;
EmissionsCost_Accumulated_rpt(t,"CO2 price")                = co2prwei(t) ;
$if not  set static     EmissionsCost_Accumulated_rpt(t,"CO2 emissions abated")     = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, co2emit(r,"2020") - co2emit(r,tt))) ;
$if      set static     EmissionsCost_Accumulated_rpt(t,"CO2 emissions abated")     = 0 ;
EmissionsCost_Accumulated_rpt(t,"Cost")                     = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, inves_annui_nodisc(r,tt) / nyrs(tt) + fixed(r,tt) + varia(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"Investment cost")          = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, inves_annui_nodisc(r,tt) / nyrs(tt))) ;
EmissionsCost_Accumulated_rpt(t,"Fixed cost")               = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, fixed(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"Variable cost")            = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, varia(r,tt))) ;
$offtext

$ontext
* * * Curtailment reporting (of intermittent renewable energies)
parameters
curtailment(i,r,t)                       Curtailment by technology (TWh)
curtailment_type(type,r,t)               Curtailment by type (TWh)
Curtailment_rpt(t,r,i)                   Curtailment reporting vector (TWh)
Curtailment_total_rpt(t,i)               Curtailment reporting vector (TWh)
Curtailment_type_rpt(t,r,type)           Curtailment reporting vector (TWh)
Curtailment_total_type_rpt(t,type)       Curtailment reporting vector (TWh)
Curtailment_xtype_rpt(t,r,tyrpt)         Curtailment reporting vector (TWh)
Curtailment_total_xtype_rpt(t,tyrpt)     Curtailment reporting vector (TWh)
;

$if not  set days   curtailment(i,r,t)$sum((s,v), vrsc(s,i,v,r))   = 1e-3 * sum((s,ivrt(i,v,r,t)), XC.L(i,v,r,t) * hours(s) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r))) - gen(i,r,t) ;
$if      set days   curtailment(i,r,t)$sum((sd,hd,v), vrsc_d(sd,hd,i,v,r))   = 1e-3 * sum((sd,hd,ivrt(i,v,r,t)), XC.L(i,v,r,t) * days(sd) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (vrsc_d(sd,hd,i,v,r)-1)$vrsc_d(sd,hd,i,v,r))) - gen(i,r,t) ;

curtailment_type(type,r,t)               = sum(idef(i,type), curtailment(i,r,t)) ;
Curtailment_rpt(t,r,i)                   = curtailment(i,r,t) ;
Curtailment_type_rpt(t,r,type)           = curtailment_type(type,r,t) ;
Curtailment_xtype_rpt(t,r,tyrpt)         = sum(xtyperpt(tyrpt,type), curtailment_type(type,r,t)) ;
Curtailment_total_rpt(t,i)               = sum(r, curtailment(i,r,t)) ;
Curtailment_total_type_rpt(t,type)       = sum(r, curtailment_type(type,r,t)) ;
Curtailment_total_xtype_rpt(t,tyrpt)     = sum(r, sum(xtyperpt(tyrpt,type), curtailment_type(type,r,t))) + eps ;

* * * Full-load hours generation
Parameters
flhgen(i,r,t)                               Full-load hours by technology (h)
flh_type(type,r,t)                       Full load hours by type (h)
FlhGeneration_rpt(t,r,i)                 Full-load hours generation reporting vector (h)
FlhGeneration_type_rpt(t,r,type)         Full-load hours generation reporting vector (h)
FlhGeneration_xtype_rpt(t,r,tyrpt)       Full-load hours generation reporting vector (h)
FlhGeneration_total_rpt(t,i)             Full-load hours generation reporting vector (h)
FlhGeneration_total_type_rpt(t,type)     Full-load hours generation reporting vector (h)
FlhGeneration_total_xtype_rpt(t,tyrpt)   Full-load hours generation reporting vector (h)
;

flhgen(i,r,t)$(installed(i,r,t) > 0.1)                                                      = 1e+3 * gen(i,r,t)             / installed(i,r,t) ;
flh_type(type,r,t)$(installed_type(type,r,t) > 0.1)                                      = 1e+3 * gen_type(type,r,t)     / installed_type(type,r,t) ;

FlhGeneration_rpt(t,r,i)                 = flhgen(i,r,t) ;
FlhGeneration_type_rpt(t,r,type)         = flh_type(type,r,t) ;
FlhGeneration_xtype_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), installed_type(type,r,t)) > 0.1)                   = 1e+3 * sum(xtyperpt(tyrpt,type), gen_type(type,r,t)) / sum(xtyperpt(tyrpt,type), installed_type(type,r,t)) ;
FlhGeneration_total_rpt(t,i)$(sum(r, installed(i,r,t)) > 0.1)                                                    = 1e+3 * sum(r, gen(i,r,t)) / sum(r, installed(i,r,t)) ;
FlhGeneration_total_type_rpt(t,type)$(sum(r, installed_type(type,r,t)) > 0.1)                                    = 1e+3 * sum(r, gen_type(type,r,t)) / sum(r, installed_type(type,r,t)) ;
FlhGeneration_total_xtype_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) > 0.1)       = 1e+3 * sum(r, sum(xtyperpt(tyrpt,type), gen_type(type,r,t))) / sum(r, sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;
$offtext

* * * Transfers reporting
parameters
* Exports
export_krr(k,r,r,t)
export_rr(r,r,t)
export_kr(k,r,t)
export_r(r,t)
* Imports
import_krr(k,r,r,t)
import_rr(r,r,t)
import_kr(k,r,t)
import_r(r,t)
* Net trade capacities
ntc_krr(k,r,r,t)
ntc_rr(r,r,t)
ntc_kr(k,r,t)
ntc_r(r,t)
* Reporting vectors
NTCTransfers_rpt(t,r,*)               
NTCTransfers_total_rpt(t,*)           
NTCTransfers_GER_rpt(t,*)             
NTCTransfers_FR_rpt(t,*)      
;

$if not  set days   export_krr(k,r,rr,t) = sum(s, hours(s) * E.L(s,k,r,rr,t) * 1e-3) ;
$if      set days   export_krr(k,r,rr,t) = sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t) * 1e-3) ;
export_rr(r,rr,t) = sum(k, export_krr(k,r,rr,t)) ;
export_kr(k,r,t) = sum(rr, export_krr(k,r,rr,t)) ;
export_r(r,t) = sum((k,rr), export_krr(k,r,rr,t)) ;

$if not  set days   import_krr(k,r,rr,t) = sum(s, hours(s) * E.L(s,k,rr,r,t) * 1e-3) ;
$if      set days   import_krr(k,r,rr,t) = sum((sd,hd), days(sd) * E_D.L(sd,hd,k,rr,r,t) * 1e-3) ;
import_rr(r,rr,t) = sum(k, import_krr(k,r,rr,t)) ;
import_kr(k,r,t) = sum(rr, import_krr(k,r,rr,t)) ;
import_r(r,t) = sum((k,rr), import_krr(k,r,rr,t)) ;

ntc_krr(k,r,rr,t) = TC.L(k,r,rr,t) ;
ntc_rr(r,rr,t) = sum(k, ntc_krr(k,r,rr,t)) ;
ntc_kr(k,r,t) = sum(rr, ntc_krr(k,r,rr,t)) ;
ntc_r(r,t) = sum((k,rr), ntc_krr(k,r,rr,t)) ;

NTCTransfers_rpt(t,r,"NTC (GW)")            = ntc_r(r,t) + eps ;
NTCTransfers_rpt(t,r,"Transfer (TWh)")      = export_r(r,t) + eps ;
NTCTransfers_rpt(t,r,"Imports (TWh)")       = import_r(r,t) + eps ;

NTCTransfers_total_rpt(t,"NTC (GW)")        = sum(r, ntc_r(r,t)) + eps ;
NTCTransfers_total_rpt(t,"Transfer (TWh)")  = sum(r, export_r(r,t)) + eps ;
NTCTransfers_total_rpt(t,"Imports (TWh)")   = sum(r, import_r(r,t)) + eps ;

NTCTransfers_GER_rpt(t,"NTC (GW)")        = sum(r$(sameas(r,"Germany")), NTCTransfers_rpt(t,r,"NTC (GW)")) ;
NTCTransfers_GER_rpt(t,"Transfer (TWh)")  = sum(r$(sameas(r,"Germany")), NTCTransfers_rpt(t,r,"Transfer (TWh)")) ;
NTCTransfers_GER_rpt(t,"Imports (TWh)")   = sum(r$(sameas(r,"Germany")), NTCTransfers_rpt(t,r,"Imports (TWh)")) ;

NTCTransfers_FR_rpt(t,"NTC (GW)")        = sum(r$(sameas(r,"France")), NTCTransfers_rpt(t,r,"NTC (GW)")) ;
NTCTransfers_FR_rpt(t,"Transfer (TWh)")  = sum(r$(sameas(r,"France")), NTCTransfers_rpt(t,r,"Transfer (TWh)")) ;
NTCTransfers_FR_rpt(t,"Imports (TWh)")   = sum(r$(sameas(r,"France")), NTCTransfers_rpt(t,r,"Imports (TWh)")) ;

$ontext
* * * Transfer rents reporting
parameter
rent_krr(k,r,r,t) Rent by technology (billion EUR)
rent_kr(k,r,t) Rent by technology in one region (billion EUR)
rent_rr(r,r,t) Rent (billion EUR)
rent_r(r,t) Rent in one region (billion EUR)
Rent_rpt(t,r,r) Rent reporting vector (billion EUR)
Rent_r_rpt(r,t) Rent reporting vector (billion EUR)
Rent_total_rpt(t) Rent reporting vector (billion EUR)
;

$if not  set days   rent_krr(k,r,rr,t) = sum(s, E.L(s,k,r,rr,t) * hours(s) * (price(s,rr,t) - price(s,r,t))) * 1e-6 ;
$if      set days   rent_krr(k,r,rr,t) = sum((sd,hd), E_D.L(sd,hd,k,r,rr,t) * days(sd) * (price_d(sd,hd,rr,t) - price_d(sd,hd,r,t))) * 1e-6 ;

rent_kr(k,r,t) = sum(rr, rent_krr(k,r,rr,t)) ;
rent_rr(r,rr,t) = sum(k, rent_krr(k,r,rr,t)) ;
rent_r(r,t) = sum((rr,k), rent_krr(k,r,rr,t)) ;

Rent_rpt(t,r,rr)          = rent_rr(r,rr,t) ;
Rent_r_rpt(r,t)           = rent_r(r,t) ;
Rent_total_rpt(t)         = sum(r, rent_r(r,t)) + eps ;
$offtext

* * * Storage reporting
Set
storagerpt /Pump-GenCap,Res-GenCap,Hyd-GenCap,Bat-GenCap,
            Pump-ResCap,Res-ResCap,Hyd-ResCap,Bat-ResCap,
            Pump-FromMarket,Res-FromMarket,Hyd-FromMarket,Bat-FromMarket,
            Pump-ToMarket,Res-ToMarket,Hyd-ToMarket,Bat-ToMarket,
            GenCap,ResCap,FromMarket,ToMarket,Hyd-ChaCap,Hyd-Use,Hyd-Import,Hyd-Price,Hyd-Impprice/
;

Parameter
ginstalledd(r,j,t)                       Installed discharge capacity (GW)
ginstalledc(r,j,t)                       Installed charge capacity (GW)
ginstalledr(r,j,t)                       Installed reservoir capacity (TWh)
gcharge(r,j,t) Stored energy (from market) (TWh)
greserv(r,j,t) Stored energy (into reservoir) (TWh)
gdcharge(r,j,t) Discharged energy (from reservoir) (TWh)
gmarket(r,j,t) Provided energy (to market) (TWh)
gloss(r,j,t) Loss energy (from market to market) (TWh)
geffr(r,j,t) Round-trip efficiency (loss to market) (%)
* Reporting vector
Storage_rpt(t,r,storagerpt)
Storage_total_rpt(t,storagerpt)
Storage_GER_rpt(t,storagerpt)
Storage_FR_rpt(t,storagerpt) 
;

* Variable calculations
$if not  set hydrogensimple ginstalledd(r,j,t)               = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set hydrogensimple ginstalledd(r,j,t)$(not ghyd(j)) = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set storagebalnv   ginstalledd(r,nvj(j),t)          = GCNV.L(j,r,t) ;
$if      set hydrogensimple ginstalledd(r,ghyd(j),t)         = sum(jvrt(j,v,r,t), GCD.L(j,v,r,t)) ;

$if not  set hydrogensimple ginstalledc(r,j,t)               = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set hydrogensimple ginstalledc(r,j,t)$(not ghyd(j)) = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set storagebalnv   ginstalledc(r,nvj(j),t)          = GCNV.L(j,r,t) ;
$if      set hydrogensimple ginstalledc(r,ghyd(j),t)         = sum(jvrt(j,v,r,t), GCC.L(j,v,r,t)) ;

$if not  set hydrogensimple ginstalledr(r,j,t)               = sum(jvrt(j,v,r,t), GCL(j,v,r,t) * ghours(j,v,r)) * 1e-3;
$if      set hydrogensimple ginstalledr(r,j,t)$(not ghyd(j)) = sum(jvrt(j,v,r,t), GCL(j,v,r,t) * ghours(j,v,r)) * 1e-3;
$if      set storagebalnv   ginstalledr(r,nvj(j),t)          = GCNV.L(j,r,t) * ghoursnv(j,r,t) * 1e-3 ;
$if      set hydrogensimple ginstalledr(r,ghyd(j),t)         = sum(jvrt(j,v,r,t), GCR.L(j,v,r,t)) * 1e-3;


$if not  set days   gcharge(r,j,t)           = sum((s,jvrt(j,v,r,t)), GL(s,j,v,r,t) * hours(s) ) * 1e-3 ;
$if not  set days   $if      set storagebalnv  gcharge(r,nvj(j),t)      = sum((s), GNV.L(s,j,r,t) * hours(s) ) * 1e-3 ;
$if not  set days   greserv(r,j,t)           = sum((s,jvrt(j,v,r,t)), GL(s,j,v,r,t) * chrgpen(j,v,r) * hours(s) ) * 1e-3 ;
$if not  set days   $if      set storagebalnv   greserv(r,nvj(j),t)      = sum((s), GNV.L(s,j,r,t) * chrgpennv(j,r) * hours(s) ) * 1e-3 ;
$if not  set days   gdcharge(r,j,t)          = sum((s,jvrt(j,v,r,t)), GDL(s,j,v,r,t) * hours(s) ) * 1e-3 ;
$if not  set days   $if      set storagebalnv   gdcharge(r,nvj(j),t)     = sum((s), GDNV.L(s,j,r,t) * hours(s) ) * 1e-3 ;
$if not  set days   gmarket(r,j,t)           = sum((s,jvrt(j,v,r,t)), GDL(s,j,v,r,t) * dchrgpen(j,v,r) * hours(s) ) * 1e-3 ;
$if not  set days   $if      set storagebalnv   gmarket(r,nvj(j),t)      = sum((s), GDNV.L(s,j,r,t) * dchrgpennv(j,r) * hours(s) ) * 1e-3 ;

$if      set days   gcharge(r,j,t)           = sum((sd,hd,jvrt(j,v,r,t)), GL_D(sd,hd,j,v,r,t) * days(sd) ) * 1e-3 ;
$if      set days   $if      set storagebalnv   gcharge(r,nvj(j),t)      = sum((sd,hd), GNV_D.L(sd,hd,j,r,t) * days(sd) ) * 1e-3 ;
$if      set days   greserv(r,j,t)           = sum((sd,hd,jvrt(j,v,r,t)), GL_D(sd,hd,j,v,r,t) * chrgpen(j,v,r) * days(sd) ) * 1e-3 ;
$if      set days   $if      set storagebalnv   greserv(r,nvj(j),t)      = sum((sd,hd), GNV_D.L(sd,hd,j,r,t) * chrgpennv(j,r) * days(sd) ) * 1e-3 ;
$if      set days   gdcharge(r,j,t)          = sum((sd,hd,jvrt(j,v,r,t)), GDL_D(sd,hd,j,v,r,t) * days(sd) ) * 1e-3 ;
$if      set days   $if      set storagebalnv   gdcharge(r,nvj(j),t)     = sum((sd,hd), GDNV_D.L(sd,hd,j,r,t) * days(sd) ) * 1e-3 ;
$if      set days   gmarket(r,j,t)           = sum((sd,hd,jvrt(j,v,r,t)), GDL_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r) * days(sd) ) * 1e-3 ;
$if      set days   $if      set storagebalnv   gmarket(r,nvj(j),t)      = sum((sd,hd), GDNV_D.L(sd,hd,j,r,t) * dchrgpennv(j,r) * days(sd) ) * 1e-3 ;

gloss(r,j,t) = gcharge(r,j,t) - gmarket(r,j,t) ;
geffr(r,j,t)$(gmarket(r,j,t) > 0) = gloss(r,j,t) / gmarket(r,j,t) ;

Storage_rpt(t,r,"Pump-GenCap")          = ginstalledd(r,"Pumpstorage",t) + eps ;
Storage_rpt(t,r,"Res-GenCap")           = ginstalledd(r,"Reservoir",t) + eps ;
Storage_rpt(t,r,"Hyd-GenCap")           = ginstalledd(r,"Storage_LT",t) + eps ;
Storage_rpt(t,r,"Bat-GenCap")           = ginstalledd(r,"Storage_ST",t) + eps ;

Storage_rpt(t,r,"Pump-ResCap")          = ginstalledr(r,"Pumpstorage",t) + eps ;
Storage_rpt(t,r,"Res-ResCap")           = ginstalledr(r,"Reservoir",t) + eps ;
Storage_rpt(t,r,"Hyd-ResCap")           = ginstalledr(r,"Storage_LT",t) + eps ;
Storage_rpt(t,r,"Bat-ResCap")           = ginstalledr(r,"Storage_ST",t) + eps ;

Storage_rpt(t,r,"Pump-FromMarket")      = gcharge(r,"Pumpstorage",t) + eps ;
Storage_rpt(t,r,"Res-FromMarket")       = gcharge(r,"Reservoir",t) + eps ;
Storage_rpt(t,r,"Hyd-FromMarket")       = gcharge(r,"Storage_LT",t) + eps ;
Storage_rpt(t,r,"Bat-FromMarket")       = gcharge(r,"Storage_ST",t) + eps ;

Storage_rpt(t,r,"Pump-ToMarket")      = gmarket(r,"Pumpstorage",t) + eps ;
Storage_rpt(t,r,"Res-ToMarket")       = gmarket(r,"Reservoir",t) + eps ;
Storage_rpt(t,r,"Hyd-ToMarket")       = gmarket(r,"Storage_LT",t) + eps ;
Storage_rpt(t,r,"Bat-ToMarket")       = gmarket(r,"Storage_ST",t) + eps ;

Storage_rpt(t,r,"GenCap")               = sum(j, ginstalledd(r,j,t)) + eps ;
Storage_rpt(t,r,"ResCap")               = sum(j, ginstalledr(r,j,t)) + eps ;
Storage_rpt(t,r,"FromMarket")           = sum(j, gcharge(r,j,t)) + eps ;
Storage_rpt(t,r,"ToMarket")             = sum(j, gmarket(r,j,t)) + eps ;

Storage_rpt(t,r,"Hyd-ChaCap")           = ginstalledc(r,"Storage_LT",t) + eps ;
$if      set hydrogensimple     Storage_rpt(t,r,"Hyd-Use")              = sum(ghyd(j), HRES.L(j,r,t)) + eps ;
$if      set hydrogensimple     Storage_rpt(t,r,"Hyd-Price")            = sum(ghyd(j), demand_hydrogen.M(r,t)) / dfact(t) + eps ;
$if      set hydrogenimport     Storage_rpt(t,r,"Hyd-Import")           = sum(ghyd(j), HIMP.L(j,r,t)) + eps ;
$if      set hydrogenimport     Storage_rpt(t,r,"Hyd-Impprice")         = himport(r,t) + eps ;

Storage_total_rpt(t,storagerpt)         = sum(r, Storage_rpt(t,r,storagerpt)) + eps ;

Storage_total_rpt(t,"Hyd-Price")$(sum(r, hydtwh(r,t)) > 0)  = sum(r, Storage_rpt(t,r,"Hyd-Price") * hydtwh(r,t)) / sum(r, hydtwh(r,t)) + eps ;
Storage_total_rpt(t,"Hyd-Price")$(sum(r, hydtwh(r,t)) = 0)  = eps ;
Storage_total_rpt(t,"Hyd-Impprice")$(sum(r, hydtwh(r,t)) > 0)  = sum(r, himport(r,t) * hydtwh(r,t)) / sum(r, hydtwh(r,t)) + eps ;
Storage_total_rpt(t,"Hyd-Impprice")$(sum(r, hydtwh(r,t)) = 0)  = eps ;

Storage_GER_rpt(t,storagerpt)           = sum(r$(sameas(r,"Germany")), Storage_rpt(t,r,storagerpt)) + eps ;
Storage_FR_rpt(t,storagerpt)            = sum(r$(sameas(r,"France")), Storage_rpt(t,r,storagerpt)) + eps ;
                         
$if      set ihk            $include    modules_rpt\euregen2024_ihk_rpt_v1
$if      set socialcost     $include    modules_rpt\euregen2024_socialcost_rpt_v1
$if      set socialcost     $include    modules_rpt\euregen2024_systemcost_rpt_v1
$if      set noweffect      $include    modules_rpt\euregen2024_noweffect_rpt_v1
$if      set crises         $include    modules_rpt\euregen2024_crises_rpt_v2

$if not  set static tv("2020","2020") = YES ;

$if      set costcal        $include    modules_rpt\euregen2024_cost_rpt_v1

parameter
Trade_xtype(t,r,rr,tyrpt)
ElectricityGenerationTrade_xtype_rpt(t,r,tyrpt)
;

alias(tyrpt,tyrptt) ;

Trade_xtype(t,r,rr,tyrpt)$(sum(tyrptt, gen_xtype(tyrptt,rr,t)) + gmarket(rr,"reservoir",t) + gmarket(rr,"pumpstorage",t) > 0) = import_rr(r,rr,t) * gen_xtype(tyrpt,rr,t) / (sum(tyrptt, gen_xtype(tyrptt,rr,t)) + gmarket(rr,"reservoir",t) + gmarket(rr,"pumpstorage",t)) ;
Trade_xtype(t,r,rr,tyrpt)$(sum(tyrptt, gen_xtype(tyrptt,rr,t)) + gmarket(rr,"reservoir",t) + gmarket(rr,"pumpstorage",t) > 0 and sameas(r,"Hydro")) = import_rr(r,rr,t) * (gen_xtype(tyrpt,rr,t) + gmarket(rr,"reservoir",t) + gmarket(rr,"pumpstorage",t)) / (sum(tyrptt, gen_xtype(tyrptt,rr,t)) + gmarket(rr,"reservoir",t) + gmarket(rr,"pumpstorage",t)) ;

ElectricityGenerationTrade_xtype_rpt(t,r,tyrpt) = gen_xtype(tyrpt,r,t) + sum(rr, Trade_xtype(t,r,rr,tyrpt)) + eps ;
ElectricityGenerationTrade_xtype_rpt(t,r,tyrpt)$(sameas(tyrpt,"Hydro")) = gen_xtype(tyrpt,r,t) + gmarket(r,"reservoir",t) + gmarket(r,"pumpstorage",t) + sum(rr, Trade_xtype(t,r,rr,tyrpt)) + eps ;

parameter
lostload(r,t) TWh
lostload_total(t) TWh
;

$if not  set days                       lostload(r,t) = sum(s, hours(s) * BS.L(s,r,t)) * 1e-3
$if not  set days   $if set elastic     + sum(bse, sum(s, hours(s) * BSELAS.L(bse,s,r,t))) * 1e-3

$if      set days                       lostload(r,t) = sum((sd,hd), days(sd) * BS_D.L(sd,hd,r,t)) * 1e-3
$if      set days   $if set elastic     + sum(bse, sum((sd,hd), days(sd) * BSELAS_D.L(bse,sd,hd,r,t))) * 1e-3
                + eps ;  

lostload_total(t) = sum(r, lostload(r,t)) + eps ;

$if      set learning                               $include  modules_rpt\euregen2024_lea_rpt_v1

$if      set biomarket                              $include    modules_rpt\euregen2024_biomass_rpt_v1
$if      set biomarket_r                            $include    modules_rpt\euregen2024_biomass_rpt_v1

$if not  set static                                 $include    modules_rpt\euregen2024_euets_rpt_v1
$if not  set static     $if      set co2iter        $include    modules_rpt\euregen2024_co2iter_rpt_v1

* * * Set exogenous investment schock variables when running Ukrain Russian war scenarios
parameters
ixfx(i,r,t)
itfx(k,r,r,t)
igfx(j,r,t)
igfx(j,r,t)
;

ixfx(i,r,topt2030(t))     = IX.L(i,r,t) ;
itfx(k,r,rr,topt2030(t))  = IT.L(k,r,rr,t) ;
igfx(j,r,topt2030(t))     = IGL(j,r,t) ;

$if      set writelimits    execute_unload 'limits\%sn%_%l%_%s%.gdx', ixfx, itfx, igfx ;

parameter
capt_static(i,v,r,t)
gcapt_static(j,v,r,t)
gccapt_static(j,v,r,t)
gdcapt_static(j,v,r,t)
grcapt_static(j,v,r,t)
gcaptnv_static(j,r,t)
tcapt_static(k,r,r,t)
co2eleeu_static(t)
co2eleuk_static(t)
;

capt_static(i,v,r,t) = XC.L(i,v,r,t) ;
gcapt_static(j,v,r,t) = GC.L(j,v,r,t) ;
gccapt_static(j,v,r,t) = GCC.L(j,v,r,t) ;
gdcapt_static(j,v,r,t) = GCD.L(j,v,r,t) ;
grcapt_static(j,v,r,t) = GCR.L(j,v,r,t) ;
gcaptnv_static(j,r,t) = GCNV.L(j,r,t) ;
tcapt_static(k,r,rr,t) = TC.L(k,r,rr,t) ;
co2eleeu_static(t) = ECEU.L(t) ;
co2eleuk_static(t) = ECUK.L(t) ;

$if      set staticcap       execute_unload 'static\%sn%_%l%_%s%.gdx', capt_static,gcapt_static,gccapt_static,gdcapt_static,grcapt_static,gcaptnv_static,tcapt_static,co2eleeu_static,co2eleuk_static,co2prreg;
$if      set priceref        execute_unload 'elastic\%s%_priceref.gdx' price, Electricity1_rpt ;
$include    modules_rpt\euregen2024_report_rpt_v1
$if      set excel      $include    modules_rpt\euregen2024_excel_rpt_v1