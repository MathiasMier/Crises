Parameter
scc_v(v)
scc_t(t)
;

scc_v("2020") =  50 ;
scc_v("2050") = 250 ;
scc_v(v) $(v.val ge 2021 and v.val le 2049) = scc_v("2020") + (scc_v("2050") - scc_v("2020")) * (v.val - 2020) / (2050 - 2020) ;
scc_t(t) = sum(tv(t,v), scc_v(v)) ;

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