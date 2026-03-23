parameters
discost_rpt(t,r,tyrpt)
discost_total_rpt(t,tyrpt)
discostco2_rpt(t,r,tyrpt)
discostco2_total_rpt(t,tyrpt)
fomcost_rpt(t,r,tyrpt)
fomcost_total_rpt(t,tyrpt)
capcost_rpt(t,r,tyrpt)
capcost_total_rpt(t,tyrpt)
effrate_rpt(t,r,tyrpt)
effrate_total_rpt(t,tyrpt)
emit_rpt(t,r,tyrpt)
emit_total_rpt(t,tyrpt)
;

discost_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) > 0)                 = sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), discost(i,v,r,t) * XTWHL(i,v,r,t))))
                                                                                                                                    / sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) + eps ;
discost_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) > 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), discost(i,v,r,t) * XTWHL(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) + eps ;
                                                                                                                    
discostco2_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) > 0)              = sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), discostco2(i,v,r,t) * XTWHL(i,v,r,t))))
                                                                                                                                    / sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) + eps ;
discostco2_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) > 0)  = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), discostco2(i,v,r,t) * XTWHL(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) + eps ;
                                                                                                                     
fomcost_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) > 0)                 = sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), fomcost(i,v,r) * XTWHL(i,v,r,t))))
                                                                                                                                    / sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) + eps ;
fomcost_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) > 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), fomcost(i,v,r) * XTWHL(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) + eps ;                                                                                                                        

capcost_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) > 0)                 = sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), capcost(i,v,r) * XTWHL(i,v,r,t))))
                                                                                                                                    / sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) + eps ;
capcost_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) > 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), capcost(i,v,r) * XTWHL(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) + eps ;                                                                                                                        

effrate_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) > 0)                 = sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), effrate(i,v,r) * XTWHL(i,v,r,t))))
                                                                                                                                    / sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) + eps ;
effrate_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) > 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), effrate(i,v,r) * XTWHL(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) + eps ;  

emit_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) > 0)                    = sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), emit(i,v,r) * XTWHL(i,v,r,t))))
                                                                                                                                    / sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t)))) + eps ;
emit_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) > 0)        = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), emit(i,v,r) * XTWHL(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) + eps ;  

$ontext
discost_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) = 0 and sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(discost(i,v,r,t) > 0))))) > 0)
                                                                                                                                   = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), discost(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(discost(i,v,r,t) > 0))))) + eps ;
                                                                                                                                    
discostco2_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) = 0)  = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), discostco2(i,v,r,t)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(discostco2(i,v,r,t) > 0))))) + eps ;
                                                                                                                                    
fomcost_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) = 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), fomcost(i,v,r)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(fomcost(i,v,r) > 0))))) + eps ;
                                                                                                                                    
capcost_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) = 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), capcost(i,v,r)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(capcost(i,v,r) > 0))))) + eps ;
                                                                                                                                    
effrate_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) = 0)     = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), effrate(i,v,r)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(effrate(i,v,r) > 0))))) + eps ;
                                                                                                                                    
emit_total_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), XTWHL(i,v,r,t))))) = 0)        = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), emit(i,v,r)))))
                                                                                                                                    / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val le t.val), 1$(emit(i,v,r) > 0))))) + eps ;  
$offText

parameters
discost_tv_rpt(t,tyrpt)
discostco2_tv_rpt(t,tyrpt)
co2cost_tv_rpt(t,tyrpt)
fomcost_tv_rpt(t,tyrpt)
capcost_tv_rpt(t,tyrpt)
effrate_tv_rpt(t,tyrpt)
emit_tv_rpt(t,tyrpt)
;

discost_tv_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$discost(i,v,r,t))))) > 0) = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), discost(i,v,r,t))))) / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$discost(i,v,r,t))))) + eps ;
discostco2_tv_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$discostco2(i,v,r,t))))) > 0) = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), discostco2(i,v,r,t))))) / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$discostco2(i,v,r,t))))) + eps ;
co2cost_tv_rpt(t,tyrpt) = discostco2_tv_rpt(t,tyrpt) - discost_tv_rpt(t,tyrpt) + eps ;
capcost_tv_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$capcost(i,v,r))))) > 0) = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), capcost(i,v,r))))) / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$capcost(i,v,r))))) + eps ;
fomcost_tv_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$fomcost(i,v,r))))) > 0) = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), fomcost(i,v,r))))) / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$fomcost(i,v,r))))) + eps ;
effrate_tv_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$effrate(i,v,r))))) > 0) = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), effrate(i,v,r))))) / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$effrate(i,v,r))))) + eps ;
emit_tv_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$emit(i,v,r))))) > 0) = sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), emit(i,v,r))))) / sum(r, sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(v$(v.val eq t.val), 1$emit(i,v,r))))) + eps ;

parameter
capcost_rrpt(i,v)
fomcost_rrpt(i,v)
vomcost_rrpt(i,v)
effrate_rrpt(i,v)
emit_rrpt(i,v)
irnwflh_windon_rrpt(r,t)
irnwflh_windoff_rrpt(r,t)
irnwflh_openpv_rrpt(r,t)
irnwflh_roofpv_rrpt(r,t)
irnwflh_windon90_rrpt(r,t)
irnwflh_windoff90_rrpt(r,t)
irnwflh_openpv90_rrpt(r,t)
irnwflh_roofpv90_rrpt(r,t)
irnwlim_rrpt(r,superirnw) 
pfuel_rrpt(fuel,t)
pco2_rrpt(r,t)
price_rrpt(r,t)
pricetot_rrpt(*,t)
gcapcost_rrpt(j,v)
gfomcost_rrpt(j,v)
gvomcost_rrpt(j,v)
geffrate_rrpt(j,v)
tcapcost_rrpt(k,t)
tfomcost_rrpt(k,t)
tvomcost_rrpt(k,t)
teffrate_rrpt(k,t)
irnwflh_rrpt(superirnw,t)
irnwflh90_rrpt(superirnw,t)
daref_ind(r,t)
daref_res(r,t)
daref_com(r,t)
daref_tra(r,t)
daref_oth(r,t)
daref_hyd(r,t)
;

capcost_rrpt(i,v)$(sum(r, 1$capcost(i,v,r)) > 0) = sum(r, capcost(i,v,r)) / sum(r, 1$capcost(i,v,r)) + eps ;
fomcost_rrpt(i,v) = sum(r, fomcost(i,v,r)) / sum(r, 1$fomcost(i,v,r)) + eps ;
vomcost_rrpt(i,v) = sum(r, vomcost(i,v,r)) / sum(r, 1$vomcost(i,v,r)) + eps ;
effrate_rrpt(i,v) = sum(r, effrate(i,v,r)) / sum(r, 1$effrate(i,v,r)) + eps ;
emit_rrpt(i,v)    = sum(r, emit(i,v,r)) / sum(r, 1$emit(i,v,r)) + eps ;

irnwflh_windon_rrpt(r,t)$(t.val < 2024)  = sum(windon(i), irnwflh_exi_h(i,r,t)) / sum(windon(i), 1$irnwflh_exi_h(i,r,t)) + eps ;
irnwflh_windoff_rrpt(r,t)$(t.val < 2024) = sum(windoff(i), irnwflh_exi_h(i,r,t)) / sum(windoff(i), 1$irnwflh_exi_h(i,r,t)) + eps ;
irnwflh_openpv_rrpt(r,t)$(t.val < 2024)  = sum(open(i), irnwflh_exi_h(i,r,t)) / sum(open(i), 1$irnwflh_exi_h(i,r,t)) + eps ;
irnwflh_roofpv_rrpt(r,t)$(t.val < 2024)  = sum(roof(i), irnwflh_exi_h(i,r,t)) / sum(roof(i), 1$irnwflh_exi_h(i,r,t)) + eps ;

irnwflh_windon_rrpt(r,t)$(t.val ge 2024)  = sum(tv(t,v), sum(windon(i), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(windon(i), 1$irnwflh_h(i,v,r))) + eps ;
irnwflh_windoff_rrpt(r,t)$(t.val ge 2024) = sum(tv(t,v), sum(windoff(i), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(windoff(i), 1$irnwflh_h(i,v,r))) + eps ;
irnwflh_openpv_rrpt(r,t)$(t.val ge 2024)  = sum(tv(t,v), sum(open(i), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(open(i), 1$irnwflh_h(i,v,r))) + eps ;
irnwflh_roofpv_rrpt(r,t)$(t.val ge 2024)  = sum(tv(t,v), sum(roof(i), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(roof(i), 1$irnwflh_h(i,v,r))) + eps ;

irnwflh_windon90_rrpt(r,t)$(t.val < 2024)  = sum(windon(i)$(sameas(i,"WindOn_exi")), irnwflh_exi_h(i,r,t)) / sum(windon(i)$(sameas(i,"WindOn_exi")), 1$irnwflh_exi_h(i,r,t)) + eps ;
irnwflh_windoff90_rrpt(r,t)$(t.val < 2024) = sum(windoff(i)$(sameas(i,"WindOff_exi")), irnwflh_exi_h(i,r,t)) / sum(windoff(i)$(sameas(i,"WindOff_exi")), 1$irnwflh_exi_h(i,r,t)) + eps ;
irnwflh_openpv90_rrpt(r,t)$(t.val < 2024)  = sum(open(i)$(sameas(i,"OpenPV_exi")), irnwflh_exi_h(i,r,t)) / sum(open(i)$(sameas(i,"OpenPV_exi")), 1$irnwflh_exi_h(i,r,t)) + eps ;
irnwflh_roofpv90_rrpt(r,t)$(t.val < 2024)  = sum(roof(i)$(sameas(i,"RoofPV_exi")), irnwflh_exi_h(i,r,t)) / sum(roof(i)$(sameas(i,"RoofPV_exi")), 1$irnwflh_exi_h(i,r,t)) + eps ;

irnwflh_windon90_rrpt(r,t)$(t.val ge 2024)  = sum(tv(t,v), sum(windon(i)$(sameas(i,"WindOn_q90")), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(windon(i)$(sameas(i,"WindOn_q90")), 1$irnwflh_h(i,v,r))) + eps ;
irnwflh_windoff90_rrpt(r,t)$(t.val ge 2024) = sum(tv(t,v), sum(windoff(i)$(sameas(i,"WindOff_q90")), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(windoff(i)$(sameas(i,"WindOff_q90")), 1$irnwflh_h(i,v,r))) + eps ;
irnwflh_openpv90_rrpt(r,t)$(t.val ge 2024)  = sum(tv(t,v), sum(open(i)$(sameas(i,"OpenPV_q90")), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(open(i)$(sameas(i,"OpenPV_q90")), 1$irnwflh_h(i,v,r))) + eps ;
irnwflh_roofpv90_rrpt(r,t)$(t.val ge 2024)  = sum(tv(t,v), sum(roof(i)$(sameas(i,"RoofPV_q90")), irnwflh_h(i,v,r))) / sum(tv(t,v), sum(roof(i)$(sameas(i,"RoofPV_q90")), 1$irnwflh_h(i,v,r))) + eps ;

irnwflh_rrpt(superirnw,t)$(t.val < 2024 and sum(r, sum(superirnw_map(exi(i),superirnw), sum(v$(v.val le t.val), capt(i,v,r,t)))) > 0) = sum(r, sum(superirnw_map(exi(i),superirnw), irnwflh_exi_h(i,r,t) * sum(v$(v.val le t.val), capt(i,v,r,t))))
            / sum(r, sum(superirnw_map(exi(i),superirnw), sum(v$(v.val le t.val), capt(i,v,r,t)))) + eps ;

irnwflh_rrpt(superirnw,t)$(t.val ge 2024 and sum(r, sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles))) > 0) = sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles) * irnwflh_h(i,v,r))))
            / sum(r, sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles))) + eps ;

irnwflh90_rrpt(superirnw,t)$(t.val < 2024 and sum(r, sum(superirnw_map(exi(i),superirnw), sum(v$(v.val le t.val), capt(i,v,r,t)))) > 0) = sum(r, sum(superirnw_map(exi(i),superirnw), irnwflh_exi_h(i,r,t) * sum(v$(v.val le t.val), capt(i,v,r,t))))
            / sum(r, sum(superirnw_map(exi(i),superirnw), sum(v$(v.val le t.val), capt(i,v,r,t)))) + eps ;
irnwflh90_rrpt(superirnw,t)$(t.val ge 2024 and sameas(superirnw,"windon") and sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"WindOn_q90")), irnwlimUP_quantiles(i,r,quantiles)))) > 0) = sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"WindOn_q90")), irnwlimUP_quantiles(i,r,quantiles) * irnwflh_h(i,v,r))))
            / sum(r, sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"WindOn_q90")), irnwlimUP_quantiles(i,r,quantiles))) + eps ;
irnwflh90_rrpt(superirnw,t)$(t.val ge 2024 and sameas(superirnw,"windoff") and sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"windoff_q90")), irnwlimUP_quantiles(i,r,quantiles)))) > 0) = sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"windoff_q90")), irnwlimUP_quantiles(i,r,quantiles) * irnwflh_h(i,v,r))))
            / sum(r, sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"windoff_q90")), irnwlimUP_quantiles(i,r,quantiles))) + eps ;
irnwflh90_rrpt(superirnw,t)$(t.val ge 2024 and sameas(superirnw,"openpv") and sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"openpv_q90")), irnwlimUP_quantiles(i,r,quantiles)))) > 0) = sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"openpv_q90")), irnwlimUP_quantiles(i,r,quantiles) * irnwflh_h(i,v,r))))
            / sum(r, sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"openpv_q90")), irnwlimUP_quantiles(i,r,quantiles))) + eps ;
irnwflh90_rrpt(superirnw,t)$(t.val ge 2024 and sameas(superirnw,"roofpv") and sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"roofpv_q90")), irnwlimUP_quantiles(i,r,quantiles)))) > 0) = sum(r, sum(tv(t,v), sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"roofpv_q90")), irnwlimUP_quantiles(i,r,quantiles) * irnwflh_h(i,v,r))))
            / sum(r, sum(superirnw_mapq(i,quantiles,superirnw)$(sameas(i,"roofpv_q90")), irnwlimUP_quantiles(i,r,quantiles))) + eps ;
            
irnwlim_rrpt(r,superirnw) = sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles)) + eps ;
pfuel_rrpt(fuel,t) = sum(r, pfuel(fuel,r,t)) / sum(r, 1$pfuel(fuel,r,t)) + eps ;
pco2_rrpt(r,t) = co2prreg(r,t) + eps ;
price_rrpt(r,t) = Electricity1_rpt(t,r,"price-avg") + eps ;
pricetot_rrpt("sys-price",t) = Electricity1_total_rpt(t,"price-avg") + eps ;
pricetot_rrpt("sys-CO2",t) = co2prwei(t) + eps ;
pricetot_rrpt("GER-price",t) = Electricity1_GER_rpt(t,"price-avg") + eps ;
pricetot_rrpt("GER-CO2",t) = co2prreg("Germany",t) + eps ;

gcapcost_rrpt(j,v) = sum(r, gcapcost(j,v,r)) / sum(r, 1$gcapcost(j,v,r)) + eps ;
gfomcost_rrpt(j,v) = sum(r, gfomcost(j,v,r)) / sum(r, 1$gfomcost(j,v,r)) + eps ;
gvomcost_rrpt(j,v) = sum(r, gvomcost(j,v,r)) / sum(r, 1$gvomcost(j,v,r)) + eps ;
geffrate_rrpt(j,v) = sum(r, chrgpen(j,v,r) * dchrgpen(j,v,r)) / sum(r, 1$dchrgpen(j,v,r)) + eps ;
geffrate_rrpt(jres(j),v) = sum(r, dchrgpen(j,v,r)) / sum(r, 1$dchrgpen(j,v,r)) + eps ;

tcapcost_rrpt(k,t)$(sum(r, sum(rr, TC.L(k,r,rr,t))) > 0) = sum(r, sum(rr, tcapcost(k,r,rr) * TC.L(k,r,rr,t))) / sum(r, sum(rr, TC.L(k,r,rr,t))) + eps ;
tfomcost_rrpt(k,t)$(sum(r, sum(rr, TC.L(k,r,rr,t))) > 0) = sum(r, sum(rr, tfomcost(k,r,rr) * TC.L(k,r,rr,t))) / sum(r, sum(rr, TC.L(k,r,rr,t))) + eps ;
tvomcost_rrpt(k,t)$(sum(r, sum(rr, TC.L(k,r,rr,t))) > 0) = sum(r, sum(rr, tvomcost(k,r,rr) * TC.L(k,r,rr,t))) / sum(r, sum(rr, TC.L(k,r,rr,t))) + eps ;
$if not set days    teffrate_rrpt(k,t)$(sum(r, sum(rr, sum(s, hours(s) * E.L(s,k,r,rr,t)))) > 0) = sum(r, sum(rr, sum(s, hours(s) * E.L(s,k,r,rr,t)))) / sum(r, sum(rr, sum(s, hours(s) / trnspen(k,r,rr) * E.L(s,k,r,rr,t)))) + eps ;
$if     set days    teffrate_rrpt(k,t)$(sum(r, sum(rr, sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t)))) > 0) = sum(r, sum(rr, sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t)))) / sum(r, sum(rr, sum((sd,hd), days(sd) / trnspen(k,r,rr) * E_D.L(sd,hd,k,r,rr,t)))) + eps ;

daref_ind(r,t) = daref_sec(r,t,"ind") + eps ;
daref_res(r,t) = daref_sec(r,t,"res") + eps ;
daref_com(r,t) = daref_sec(r,t,"com") + eps ;
daref_tra(r,t) = daref_sec(r,t,"tra") + eps ;
daref_oth(r,t) = daref_sec(r,t,"oth") + eps ;
$if not set hydrogensimple  daref_hyd(r,t) = daref_sec(r,t,"hyd") + eps ;
$if     set hydrogensimple  $if not set days    daref_hyd(r,t) = hydtwh(r,t) + eps ;
$if     set hydrogensimple  $if     set days    daref_hyd(r,t) = hydtwh(r,t) + eps ;

parameter
gcapcost_rpt(t,r,j)
gcapcost_total_rpt(t,j)
gfomcost_rpt(t,r,j)
gfomcost_total_rpt(t,j)
geffrate_rpt(t,r,j)
geffrate_total_rpt(t,j)
;

gcapcost_rpt(t,r,j) = sum(tv(t,v), gcapcost(j,v,r)) + eps ;
gcapcost_total_rpt(t,j)$sum(r, sum(tv(t,v), 1$gcapcost(j,v,r))) = sum(r, sum(tv(t,v), gcapcost(j,v,r))) / sum(r, sum(tv(t,v), 1$gcapcost(j,v,r)))    + eps ;

gfomcost_rpt(t,r,j) = sum(tv(t,v), gfomcost(j,v,r)) + eps ;
gfomcost_total_rpt(t,j)$sum(r, sum(tv(t,v), 1$gfomcost(j,v,r))) = sum(r, sum(tv(t,v), gfomcost(j,v,r))) / sum(r, sum(tv(t,v), 1$gfomcost(j,v,r))) + eps ;

geffrate_rpt(t,r,j)$(gcharge(r,j,t) > 0) = gmarket(r,j,t) /  gcharge(r,j,t) + eps ;
geffrate_total_rpt(t,j)$(sum(r, gcharge(r,j,t)) > 0) = sum(r, gmarket(r,j,t)) /  sum(r, gcharge(r,j,t)) + eps ;

parameter
tcapcost_rpt(t,r,k)
tcapcost_total_rpt(t,k)
tfomcost_rpt(t,r,k)
tfomcost_total_rpt(t,k)
teffrate_rpt(t,r,k)
teffrate_total_rpt(t,k)
;

tcapcost_rpt(t,r,k)$(sum(rr, TC.L(k,r,rr,t)) > 0) = sum(rr, tcapcost(k,r,rr) * TC.L(k,r,rr,t)) / sum(rr, TC.L(k,r,rr,t)) + eps ;
tcapcost_total_rpt(t,k)$(sum(r, sum(rr, TC.L(k,r,rr,t))) > 0) = sum(r, sum(rr, tcapcost(k,r,rr) * TC.L(k,r,rr,t))) / sum(r, sum(rr, TC.L(k,r,rr,t))) + eps ;

tfomcost_rpt(t,r,k)$(sum(rr, TC.L(k,r,rr,t)) > 0) = sum(rr, tfomcost(k,r,rr) * TC.L(k,r,rr,t)) / sum(rr, TC.L(k,r,rr,t)) + eps ;
tfomcost_total_rpt(t,k)$(sum(r, sum(rr, TC.L(k,r,rr,t))) > 0) = sum(r, sum(rr, tfomcost(k,r,rr) * TC.L(k,r,rr,t))) / sum(r, sum(rr, TC.L(k,r,rr,t))) + eps ;

$if not set days    teffrate_rpt(t,r,k)$(sum(rr, sum(s, hours(s) * E.L(s,k,r,rr,t))) > 0) = sum(rr, sum(s, hours(s) * E.L(s,k,r,rr,t))) / sum(rr, sum(s, hours(s) / trnspen(k,r,rr) * E.L(s,k,r,rr,t))) + eps ;
$if not set days    teffrate_total_rpt(t,k)$(sum(r, sum(rr, sum(s, hours(s) * E.L(s,k,r,rr,t)))) > 0) = sum(r, sum(rr, sum(s, hours(s) * E.L(s,k,r,rr,t)))) / sum(r, sum(rr, sum(s, hours(s) / trnspen(k,r,rr) * E.L(s,k,r,rr,t)))) + eps ;

$if     set days    teffrate_rpt(t,r,k)$(sum(rr, sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t))) > 0) = sum(rr, sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t))) / sum(rr, sum((sd,hd), days(sd) / trnspen(k,r,rr) * E_D.L(sd,hd,k,r,rr,t))) + eps ;
$if     set days    teffrate_total_rpt(t,k)$(sum(r, sum(rr, sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t)))) > 0) = sum(r, sum(rr, sum((sd,hd), days(sd) * E_D.L(sd,hd,k,r,rr,t)))) / sum(r, sum(rr, sum((sd,hd), days(sd) / trnspen(k,r,rr) * E_D.L(sd,hd,k,r,rr,t)))) + eps ;
