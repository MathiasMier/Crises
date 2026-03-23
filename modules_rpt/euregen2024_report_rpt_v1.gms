execute_unload'report\%exl%_rpt.gdx',                              Emissions_total_rpt,
$if      set details                                             Emissions_rpt, Emissions_GER_rpt, Emissions_FR_rpt,
                                                                 Electricity1_total_rpt,
$if      set details                                             Electricity1_rpt,
$if      set details                                             Electricity1_GER_rpt,
$if      set details                                             Electricity1_FR_rpt,
*                                                                 Electricity2_total_rpt,
*$if      set details                                             Electricity2_rpt,
*$if      set details                                             Electricity2_GER_rpt,
*$if      set details                                             Electricity2_FR_rpt,
*                                                                 Electricity3_total_rpt,
*$if      set details                                             Electricity3_rpt,
*$if      set details                                             Electricity3_GER_rpt,
*$if      set details                                             Electricity3_FR_rpt,
*
$if      set details                                             discostco2,
*$if      set details                                            Electricity1_UK_rpt,
*$if      set details                                            Electricity2_UK_rpt,
*$if      set details                                            Electricity3_UK_rpt,
*$if      set details                                            Electricity1_DK_rpt,
*$if      set details                                            Electricity2_DK_rpt,
*$if      set details                                            Electricity3_DK_rpt,
*$if      set details                                            Electricity1_NO_rpt,
*$if      set details                                            Electricity2_NO_rpt,
*$if      set details                                            Electricity3_NO_rpt,
*$if      set details                                            Electricity1_PL_rpt,
*$if      set details                                            Electricity2_PL_rpt,
*$if      set details                                            Electricity3_PL_rpt,
*$if      set details                                            Electricity1_ES_rpt,
*$if      set details                                            Electricity2_ES_rpt,
*$if      set details                                            Electricity3_ES_rpt,

                                                                 ElectricityGeneration_total_xtype_rpt,
$if      set details                                             ElectricityGeneration_xtype_rpt,
$if      set details                                             ElectricityGeneration_GER_xtype_rpt,
$if      set details                                             ElectricityGeneration_FR_xtype_rpt,
                                                                 InstalledCapacities_total_xtype_rpt,
$if      set details                                             InstalledCapacities_xtype_rpt,
$if      set details                                             InstalledCapacities_GER_xtype_rpt,
$if      set details                                             InstalledCapacities_FR_xtype_rpt,
                                                                 AddedCapacities_total_xtype_rpt,
$if      set details                                             AddedCapacities_xtype_rpt, AddedCapacities_GER_xtype_rpt,AddedCapacities_FR_xtype_rpt,
                                                                 AccAddedCapacities_total_xtype_rpt,
$if      set details                                             AccAddedCapacities_xtype_rpt, AccAddedCapacities_GER_xtype_rpt,AccAddedCapacities_FR_xtype_rpt,

                                                                Storage_total_rpt,
$if      set details                                             Storage_rpt, Storage_GER_rpt, Storage_FR_rpt,
                                                                 NTCTransfers_total_rpt
$if      set details                                             ,NTCTransfers_rpt, NTCTransfers_GER_rpt,NTCTransfers_FR_rpt                                         
$if      set details                                             ,gasuseeu, lostload, lostload_total

$if      set systemcost                                          ,SystemCost_rpt, SystemCost_GER_rpt, SystemCost_FR_rpt, TotalSystemCost_rpt, AggreSystemCost_rpt, TotalAggreSystemCost_rpt, Interval_rpt, TotalInterval_rpt
$if      set systemcost                                          ,ihk_rpt, ihk_GER_rpt

$if      set biomarket                                           ,Biomass_total_rpt, Biomass_rpt, Biomass_GER_rpt, Biomass_FR_rpt
$if      set biomarket_r                                         ,Biomass_total_rpt, Biomass_rpt, Biomass_GER_rpt, Biomass_FR_rpt

$if      set costcal                                             ,discost_rpt, discost_tv_rpt, discostco2_rpt, discostco2_tv_rpt
$if      set costcal                                             ,capcost_rrpt,fomcost_rrpt,vomcost_rrpt,effrate_rrpt,emit_rrpt,irnwlim_rrpt,irnwflh_windon_rrpt,irnwflh_windoff_rrpt,irnwflh_openpv_rrpt,irnwflh_roofpv_rrpt
$if      set costcal                                             ,pfuel_rrpt,pco2_rrpt,price_rrpt,pricetot_rrpt,irnwflh_windon90_rrpt,irnwflh_windoff90_rrpt,irnwflh_openpv90_rrpt,irnwflh_roofpv90_rrpt,irnwflh_rrpt,irnwflh90_rrpt
$if      set costcal                                             ,gcapcost_rrpt,gfomcost_rrpt,gvomcost_rrpt,geffrate_rrpt,tcapcost_rrpt,tfomcost_rrpt,tvomcost_rrpt,teffrate_rrpt,daref,daref_ind,daref_res,daref_com,daref_tra,daref_oth,daref_hyd

$if      set noweffect                                           ,noweffect_rpt, realeffect_rpt, effect_rpt,price_rpt

$if      set learningcal                                         ,capirnw_q,capirnw,capirnw_max,limirnw_q,limirnw,limirnw_max,shairnw_q,shairnw,shairnw_max
$if      set flhcal                                              ,flhreport, flhpoteni, investflh_irt, investflh_it, investflh_rt, investflh_ir, investflh_i, investflh_r, investflh_t, investflh, investflheur_it, investflheur_i, investflheur_t, investflheur
$if      set flhcal                                              ,stock, accspendings, flheur_ls_rpt, flheur_cost_rpt, flheur_stock_rpt, flheur_check_rpt

$if not  set static                                              ,euets_rpt
$if      set crises                                              ,gen_crises,cap_crises,afcap_crises,sharecap_crises,shareafcap_crises,sharegen_crises,market_crises,discostco2_tyrpt,discostco2_nochp_tyrpt,discostco2_chp_tyrpt,discostco2_gas,discostco2_gasnochp,discostco2_gaschp
$if      set crises                                              ,freq,density,ElectricityGenerationTrade_xtype_rpt
*,gen_pump,gen_resv,gen_hydr,gen_nucl
                                                                 ;
                                                            