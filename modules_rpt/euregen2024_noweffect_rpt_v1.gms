parameter
noweffect_rpt(t,*,*)
realeffect_rpt(t,*,*)
effect_rpt(t,*)
price_rpt(t,r)
;

price_rpt(t,r) = Electricity1_rpt(t,r,"price-avg") + eps ;

noweffect_rpt(t2022(t),"EUR","Gasuse") = gasuseeu(t) + eps ;
noweffect_rpt(t2022(t),"EUR","Price") = Electricity1_total_rpt(t,"price-avg") + eps ;
noweffect_rpt(t2022(t),"EUR","Coal") = ElectricityGeneration_total_xtype_rpt(t,"Coal") + eps ;
noweffect_rpt(t2022(t),"EUR","Lignite") = ElectricityGeneration_total_xtype_rpt(t,"Lignite") + eps ;
noweffect_rpt(t2022(t),"EUR","Gas") = ElectricityGeneration_total_xtype_rpt(t,"Gas-CCGT") + ElectricityGeneration_total_xtype_rpt(t,"Gas-OCGT") + ElectricityGeneration_total_xtype_rpt(t,"Gas-ST")+ eps ;
noweffect_rpt(t2022(t),"EUR","Nuclear") = ElectricityGeneration_total_xtype_rpt(t,"Nuclear") + eps ;
noweffect_rpt(t2022(t),"EUR","Hydro") = ElectricityGeneration_total_xtype_rpt(t,"Hydro") + Storage_total_rpt(t,"Res-ToMarket") + eps ;
noweffect_rpt(t2022(t),"EUR","Wind") = ElectricityGeneration_total_xtype_rpt(t,"WindOn") + ElectricityGeneration_total_xtype_rpt(t,"WindOff") + eps ;
noweffect_rpt(t2022(t),"EUR","Solar") = ElectricityGeneration_total_xtype_rpt(t,"Solar") + eps ;
noweffect_rpt(t2022(t),"EUR","Imports") = sum(r, import_r(r,t)) + eps ;
noweffect_rpt(t2022(t),"EUR","Exports") = sum(r, export_r(r,t)) + eps ;
noweffect_rpt(t2022(t),"EUR","CO2 price") = Emissions_total_rpt(t,"CO2 price")  + eps ;
noweffect_rpt(t2022(t),"EUR","CO2") = Emissions_total_rpt(t,"Total CO2-emissions-elec")  + eps ;
noweffect_rpt(t2022(t),"EUR","Biomass") = ElectricityGeneration_total_xtype_rpt(t,"Bioenergy") + eps ;
noweffect_rpt(t2022(t),"EUR","Oil") = ElectricityGeneration_total_xtype_rpt(t,"OilOther") + eps ;

noweffect_rpt(t2022(t),"GER","Gasuse") = gasuse("Germany",t) + eps ;
noweffect_rpt(t2022(t),"GER","Price") = Electricity1_rpt(t,"Germany","price-avg") + eps ;
noweffect_rpt(t2022(t),"GER","Coal") = ElectricityGeneration_xtype_rpt(t,"Germany","Coal") + eps ;
noweffect_rpt(t2022(t),"GER","Lignite") = ElectricityGeneration_xtype_rpt(t,"Germany","Lignite") + eps ;
noweffect_rpt(t2022(t),"GER","Gas") = ElectricityGeneration_xtype_rpt(t,"Germany","Gas-CCGT") + ElectricityGeneration_xtype_rpt(t,"Germany","Gas-OCGT") + ElectricityGeneration_xtype_rpt(t,"Germany","Gas-ST")+ eps ;
noweffect_rpt(t2022(t),"GER","Nuclear") = ElectricityGeneration_xtype_rpt(t,"Germany","Nuclear") + eps ;
noweffect_rpt(t2022(t),"GER","Hydro") = ElectricityGeneration_xtype_rpt(t,"Germany","Hydro") + Storage_GER_rpt(t,"Res-ToMarket") + eps ;
noweffect_rpt(t2022(t),"GER","Wind") = ElectricityGeneration_xtype_rpt(t,"Germany","WindOn") + ElectricityGeneration_xtype_rpt(t,"Germany","WindOff") + eps ;
noweffect_rpt(t2022(t),"GER","Solar") = ElectricityGeneration_xtype_rpt(t,"Germany","Solar") + eps ;
noweffect_rpt(t2022(t),"GER","Imports") = import_r("Germany",t) + eps ;
noweffect_rpt(t2022(t),"GER","Exports") = export_r("Germany",t) + eps ;
noweffect_rpt(t2022(t),"GER","CO2 price") = Emissions_total_rpt(t,"CO2 price") + eps ;
noweffect_rpt(t2022(t),"GER","CO2") = Emissions_rpt(t,"Germany","CO2-emissions-elec")  + eps ;
noweffect_rpt(t2022(t),"GER","Biomass") = ElectricityGeneration_xtype_rpt(t,"Germany","Bioenergy") + eps ;
noweffect_rpt(t2022(t),"GER","Oil") = ElectricityGeneration_xtype_rpt(t,"Germany","OilOther") + eps ;

noweffect_rpt(t2022(t),"FR","Gasuse") = gasuse("France",t) + eps ;
noweffect_rpt(t2022(t),"FR","Price") = Electricity1_rpt(t,"France","price-avg") + eps ;
noweffect_rpt(t2022(t),"FR","Coal") = ElectricityGeneration_xtype_rpt(t,"France","Coal") + eps ;
noweffect_rpt(t2022(t),"FR","Lignite") = ElectricityGeneration_xtype_rpt(t,"France","Lignite") + eps ;
noweffect_rpt(t2022(t),"FR","Gas") = ElectricityGeneration_xtype_rpt(t,"France","Gas-CCGT") + ElectricityGeneration_xtype_rpt(t,"France","Gas-OCGT") + ElectricityGeneration_xtype_rpt(t,"France","Gas-ST")+ eps ;
noweffect_rpt(t2022(t),"FR","Nuclear") = ElectricityGeneration_xtype_rpt(t,"France","Nuclear") + eps ;
noweffect_rpt(t2022(t),"FR","Hydro") = ElectricityGeneration_xtype_rpt(t,"France","Hydro") + Storage_FR_rpt(t,"Res-ToMarket") + eps ;
noweffect_rpt(t2022(t),"FR","Wind") = ElectricityGeneration_xtype_rpt(t,"France","WindOn") + ElectricityGeneration_xtype_rpt(t,"France","WindOff") + eps ;
noweffect_rpt(t2022(t),"FR","Solar") = ElectricityGeneration_xtype_rpt(t,"France","Solar") + eps ;
noweffect_rpt(t2022(t),"FR","Imports") = import_r("France",t) + eps ;
noweffect_rpt(t2022(t),"FR","Exports") = export_r("France",t) + eps ;
noweffect_rpt(t2022(t),"FR","CO2 price") = Emissions_total_rpt(t,"CO2 price")  + eps ;
noweffect_rpt(t2022(t),"FR","CO2") = Emissions_rpt(t,"France","CO2-emissions-elec")  + eps ;
noweffect_rpt(t2022(t),"FR","Biomass") = ElectricityGeneration_xtype_rpt(t,"France","Bioenergy") + eps ;
noweffect_rpt(t2022(t),"FR","Oil") = ElectricityGeneration_xtype_rpt(t,"France" ,"OilOther") + eps ;

realeffect_rpt(t2022(t),"EUR","Gasuse") = eps ;
realeffect_rpt(t2022(t),"EUR","Price") = eps ;
realeffect_rpt(t2022(t),"EUR","Coal") = sum(r, gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Lignite") = sum(r, gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Gas") = sum(r, gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Nuclear") = sum(r, gen_min2_nucl(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Hydro") = sum(r, gen_min2_hydr(r,t) + gen_resv(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Wind") = sum(r, gen_min2_wind(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Solar") = sum(r, gen_min2_sola(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Imports") = sum(r, imp(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","Exports") = sum(r, expo(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","CO2 price") = 81.48 + eps ;
$if not  set co2price    $if not  set static  realeffect_rpt(t2022(t),"EUR","CO2") = co2ele_org(t) + co2eleuk_in(t) + eps ;
realeffect_rpt(t2022(t),"EUR","Biomass") = sum(r, gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) + eps ;
realeffect_rpt(t2022(t),"EUR","oil") = sum(r, gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) + eps ;

realeffect_rpt(t2022(t),"GER","Gasuse") = eps ;
realeffect_rpt(t2022(t),"GER","Price") = 235.31 + eps ;
realeffect_rpt(t2022(t),"GER","Coal") = sum(rge(r), gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Lignite") = sum(rge(r), gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Gas") = sum(rge(r), gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Nuclear") = sum(rge(r), gen_min2_nucl(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Hydro") = sum(rge(r), gen_min2_hydr(r,t) + gen_resv(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Wind") = sum(rge(r), gen_min2_wind(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Solar") = sum(rge(r), gen_min2_sola(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Imports") = sum(rge(r), imp(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","Exports") = sum(rge(r), expo(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","CO2 price") = eps ;
realeffect_rpt(t2022(t),"GER","CO2") = eps ;
realeffect_rpt(t2022(t),"GER","Biomass") = sum(rge(r), gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) + eps ;
realeffect_rpt(t2022(t),"GER","oil") = sum(rge(r), gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) + eps ;

realeffect_rpt(t2022(t),"FR","Gasuse") = eps ;
realeffect_rpt(t2022(t),"FR","Price") = 365.52 + eps ;
realeffect_rpt(t2022(t),"FR","Coal") = sum(rfr(r), gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Lignite") = sum(rfr(r), gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Gas") = sum(rfr(r), gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Nuclear") = sum(rfr(r), gen_min2_nucl(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Hydro") = sum(rfr(r), gen_min2_hydr(r,t) + gen_resv(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Wind") = sum(rfr(r), gen_min2_wind(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Solar") = sum(rfr(r), gen_min2_sola(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Imports") = sum(rfr(r), imp(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","Exports") = sum(rfr(r), expo(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","CO2 price") = eps ;
realeffect_rpt(t2022(t),"FR","CO2") = eps ;
realeffect_rpt(t2022(t),"FR","Biomass") = sum(rfr(r), gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) + eps ;
realeffect_rpt(t2022(t),"FR","oil") = sum(rfr(r), gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) + eps ;

set
t23(t)
;

t23(t)$(sameas(t,"2023")) = yes ;

noweffect_rpt(t23(t),"EUR","Gasuse") = gasuseeu(t) + eps ;
noweffect_rpt(t23(t),"EUR","Price") = Electricity1_total_rpt(t,"price-avg") + eps ;
noweffect_rpt(t23(t),"EUR","Coal") = ElectricityGeneration_total_xtype_rpt(t,"Coal") + eps ;
noweffect_rpt(t23(t),"EUR","Lignite") = ElectricityGeneration_total_xtype_rpt(t,"Lignite") + eps ;
noweffect_rpt(t23(t),"EUR","Gas") = ElectricityGeneration_total_xtype_rpt(t,"Gas-CCGT") + ElectricityGeneration_total_xtype_rpt(t,"Gas-OCGT") + ElectricityGeneration_total_xtype_rpt(t,"Gas-ST")+ eps ;
noweffect_rpt(t23(t),"EUR","Nuclear") = ElectricityGeneration_total_xtype_rpt(t,"Nuclear") + eps ;
noweffect_rpt(t23(t),"EUR","Hydro") = ElectricityGeneration_total_xtype_rpt(t,"Hydro") + Storage_total_rpt(t,"Res-ToMarket") + eps ;
noweffect_rpt(t23(t),"EUR","Wind") = ElectricityGeneration_total_xtype_rpt(t,"WindOn") + ElectricityGeneration_total_xtype_rpt(t,"WindOff") + eps ;
noweffect_rpt(t23(t),"EUR","Solar") = ElectricityGeneration_total_xtype_rpt(t,"Solar") + eps ;
noweffect_rpt(t23(t),"EUR","Imports") = sum(r, import_r(r,t)) + eps ;
noweffect_rpt(t23(t),"EUR","Exports") = sum(r, export_r(r,t)) + eps ;
noweffect_rpt(t23(t),"EUR","CO2 price") = Emissions_total_rpt(t,"CO2 price")  + eps ;
noweffect_rpt(t23(t),"EUR","CO2") = Emissions_total_rpt(t,"Total CO2-emissions-elec")  + eps ;
noweffect_rpt(t23(t),"EUR","Biomass") = ElectricityGeneration_total_xtype_rpt(t,"Bioenergy") + eps ;
noweffect_rpt(t23(t),"EUR","Oil") = ElectricityGeneration_total_xtype_rpt(t,"OilOther") + eps ;

noweffect_rpt(t23(t),"GER","Gasuse") = gasuse("Germany",t) + eps ;
noweffect_rpt(t23(t),"GER","Price") = Electricity1_rpt(t,"Germany","price-avg") + eps ;
noweffect_rpt(t23(t),"GER","Coal") = ElectricityGeneration_xtype_rpt(t,"Germany","Coal") + eps ;
noweffect_rpt(t23(t),"GER","Lignite") = ElectricityGeneration_xtype_rpt(t,"Germany","Lignite") + eps ;
noweffect_rpt(t23(t),"GER","Gas") = ElectricityGeneration_xtype_rpt(t,"Germany","Gas-CCGT") + ElectricityGeneration_xtype_rpt(t,"Germany","Gas-OCGT") + ElectricityGeneration_xtype_rpt(t,"Germany","Gas-ST")+ eps ;
noweffect_rpt(t23(t),"GER","Nuclear") = ElectricityGeneration_xtype_rpt(t,"Germany","Nuclear") + eps ;
noweffect_rpt(t23(t),"GER","Hydro") = ElectricityGeneration_xtype_rpt(t,"Germany","Hydro") + Storage_GER_rpt(t,"Res-ToMarket") + eps ;
noweffect_rpt(t23(t),"GER","Wind") = ElectricityGeneration_xtype_rpt(t,"Germany","WindOn") + ElectricityGeneration_xtype_rpt(t,"Germany","WindOff") + eps ;
noweffect_rpt(t23(t),"GER","Solar") = ElectricityGeneration_xtype_rpt(t,"Germany","Solar") + eps ;
noweffect_rpt(t23(t),"GER","Imports") = import_r("Germany",t) + eps ;
noweffect_rpt(t23(t),"GER","Exports") = export_r("Germany",t) + eps ;
noweffect_rpt(t23(t),"GER","CO2 price") = Emissions_total_rpt(t,"CO2 price") + eps ;
noweffect_rpt(t23(t),"GER","CO2") = Emissions_rpt(t,"Germany","CO2-emissions-elec")  + eps ;
noweffect_rpt(t23(t),"GER","Biomass") = ElectricityGeneration_xtype_rpt(t,"Germany","Bioenergy") + eps ;
noweffect_rpt(t23(t),"GER","Oil") = ElectricityGeneration_xtype_rpt(t,"Germany","OilOther") + eps ;

noweffect_rpt(t23(t),"FR","Gasuse") = gasuse("France",t) + eps ;
noweffect_rpt(t23(t),"FR","Price") = Electricity1_rpt(t,"France","price-avg") + eps ;
noweffect_rpt(t23(t),"FR","Coal") = ElectricityGeneration_xtype_rpt(t,"France","Coal") + eps ;
noweffect_rpt(t23(t),"FR","Lignite") = ElectricityGeneration_xtype_rpt(t,"France","Lignite") + eps ;
noweffect_rpt(t23(t),"FR","Gas") = ElectricityGeneration_xtype_rpt(t,"France","Gas-CCGT") + ElectricityGeneration_xtype_rpt(t,"France","Gas-OCGT") + ElectricityGeneration_xtype_rpt(t,"France","Gas-ST")+ eps ;
noweffect_rpt(t23(t),"FR","Nuclear") = ElectricityGeneration_xtype_rpt(t,"France","Nuclear") + eps ;
noweffect_rpt(t23(t),"FR","Hydro") = ElectricityGeneration_xtype_rpt(t,"France","Hydro") + Storage_FR_rpt(t,"Res-ToMarket") + eps ;
noweffect_rpt(t23(t),"FR","Wind") = ElectricityGeneration_xtype_rpt(t,"France","WindOn") + ElectricityGeneration_xtype_rpt(t,"France","WindOff") + eps ;
noweffect_rpt(t23(t),"FR","Solar") = ElectricityGeneration_xtype_rpt(t,"France","Solar") + eps ;
noweffect_rpt(t23(t),"FR","Imports") = import_r("France",t) + eps ;
noweffect_rpt(t23(t),"FR","Exports") = export_r("France",t) + eps ;
noweffect_rpt(t23(t),"FR","CO2 price") = Emissions_total_rpt(t,"CO2 price")  + eps ;
noweffect_rpt(t23(t),"FR","CO2") = Emissions_rpt(t,"France","CO2-emissions-elec")  + eps ;
noweffect_rpt(t23(t),"FR","Biomass") = ElectricityGeneration_xtype_rpt(t,"France","Bioenergy") + eps ;
noweffect_rpt(t23(t),"FR","Oil") = ElectricityGeneration_xtype_rpt(t,"France" ,"OilOther") + eps ;

realeffect_rpt(t23(t),"EUR","Gasuse") = eps ;
realeffect_rpt(t23(t),"EUR","Price") = eps ;
realeffect_rpt(t23(t),"EUR","Coal") = sum(r, gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Lignite") = sum(r, gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Gas") = sum(r, gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Nuclear") = sum(r, gen_min2_nucl(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Hydro") = sum(r, gen_min2_hydr(r,t) + gen_resv(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Wind") = sum(r, gen_min2_wind(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Solar") = sum(r, gen_min2_sola(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Imports") = sum(r, imp(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Exports") = sum(r, expo(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","CO2 price") = 89.38 + eps ;
$if not  set co2price    $if not  set static  realeffect_rpt(t23(t),"EUR","CO2") = co2ele_org(t) + co2eleuk_in(t) + eps ;
realeffect_rpt(t23(t),"EUR","Biomass") = sum(r, gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","oil") = sum(r, gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) + eps ;

realeffect_rpt(t23(t),"GER","Gasuse") = eps ;
realeffect_rpt(t23(t),"GER","Price") = 235.31 + eps ;
realeffect_rpt(t23(t),"GER","Coal") = sum(rge(r), gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Lignite") = sum(rge(r), gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Gas") = sum(rge(r), gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Nuclear") = sum(rge(r), gen_min2_nucl(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Hydro") = sum(rge(r), gen_min2_hydr(r,t) + gen_resv(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Wind") = sum(rge(r), gen_min2_wind(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Solar") = sum(rge(r), gen_min2_sola(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Imports") = sum(rge(r), imp(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Exports") = sum(rge(r), expo(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","CO2 price") = eps ;
realeffect_rpt(t23(t),"GER","CO2") = eps ;
realeffect_rpt(t23(t),"GER","Biomass") = sum(rge(r), gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","oil") = sum(rge(r), gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) + eps ;

realeffect_rpt(t23(t),"FR","Gasuse") = eps ;
realeffect_rpt(t23(t),"FR","Price") = 365.52 + eps ;
realeffect_rpt(t23(t),"FR","Coal") = sum(rfr(r), gen_min2_chp_coal(r,t) + gen_min2_nochp_coal(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Lignite") = sum(rfr(r), gen_min2_chp_lign(r,t) + gen_min2_nochp_lign(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Gas") = sum(rfr(r), gen_min2_chp_ngas(r,t) + gen_min2_nochp_ngas(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Nuclear") = sum(rfr(r), gen_min2_nucl(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Hydro") = sum(rfr(r), gen_min2_hydr(r,t) + gen_resv(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Wind") = sum(rfr(r), gen_min2_wind(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Solar") = sum(rfr(r), gen_min2_sola(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Imports") = sum(rfr(r), imp(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Exports") = sum(rfr(r), expo(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","CO2 price") = eps ;
realeffect_rpt(t23(t),"FR","CO2") = eps ;
realeffect_rpt(t23(t),"FR","Biomass") = sum(rfr(r), gen_min2_chp_biom(r,t) + gen_min2_nochp_biom(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","oil") = sum(rfr(r), gen_min2_chp_oil(r,t) + gen_min2_nochp_oil(r,t)) + eps ;

noweffect_rpt(t2022(t),"EUR","Pump") = Storage_total_rpt(t,"Pump-ToMarket") + eps ;
noweffect_rpt(t23(t),"EUR","Pump") = Storage_total_rpt(t,"Pump-ToMarket") + eps ;
realeffect_rpt(t2022(t),"EUR","Pump") = sum(r, gen_pump(r,t)) + eps ;
realeffect_rpt(t23(t),"EUR","Pump") = sum(r, gen_pump(r,t)) + eps ;

noweffect_rpt(t2022(t),"GER","Pump") = Storage_GER_rpt(t,"Pump-ToMarket") + eps ;
noweffect_rpt(t23(t),"GER","Pump") = Storage_GER_rpt(t,"Pump-ToMarket") + eps ;
realeffect_rpt(t2022(t),"GER","Pump") = sum(rge(r), gen_pump(r,t)) + eps ;
realeffect_rpt(t23(t),"GER","Pump") = sum(rge(r), gen_pump(r,t)) + eps ;

noweffect_rpt(t2022(t),"FR","Pump") = Storage_FR_rpt(t,"Pump-ToMarket") + eps ;
noweffect_rpt(t23(t),"FR","Pump") = Storage_FR_rpt(t,"Pump-ToMarket") + eps ;
realeffect_rpt(t2022(t),"FR","Pump") = sum(rfr(r), gen_pump(r,t)) + eps ;
realeffect_rpt(t23(t),"FR","Pump") = sum(rfr(r), gen_pump(r,t)) + eps ;


effect_rpt(t,"Price") = Electricity1_total_rpt(t,"price-avg") + eps ;
effect_rpt(t,"GER-Price") = Electricity1_rpt(t,"Germany","price-avg") + eps ;
effect_rpt(t,"FR-Price") = Electricity1_rpt(t,"France","price-avg") + eps ;
effect_rpt(t,"CO2") = Emissions_total_rpt(t,"Total CO2-emissions-elec") + eps ;
effect_rpt(t,"GER-CO2") = Emissions_rpt(t,"Germany","CO2-emissions-elec")  + eps ;
effect_rpt(t,"FR-CO2") = Emissions_rpt(t,"France","CO2-emissions-elec")  + eps ;
effect_rpt(t,"CO2 price") = Emissions_total_rpt(t,"CO2 price") + eps ;
effect_rpt(t,"GER-CO2 price") = Emissions_rpt(t,"Germany","CO2-price")  + eps ;
effect_rpt(t,"FR-CO2 price") = Emissions_rpt(t,"France","CO2-price")  + eps ;
effect_rpt(t,"Gasuse") = gasuseeu(t) + eps ;
effect_rpt(t,"GER-Gasuse") = gasuse("Germany",t) + eps ;
effect_rpt(t,"FR-Gasuse") = gasuse("France",t) + eps ;