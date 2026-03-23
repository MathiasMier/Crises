execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Emissions_total_rpt rng=Emissions!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Emissions_GER_rpt rng=Emissions_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Emissions_FR_rpt rng=Emissions_FR!a1'

execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_total_rpt rng=Electricity1!a1'
*execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_total_rpt rng=Electricity2!a1'
*execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_total_rpt rng=Electricity3!a1'

execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_GER_rpt rng=Electricity1_GER!a1'
*execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_GER_rpt rng=Electricity2_GER!a1'
*execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_GER_rpt rng=Electricity3_GER!a1'

execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_FR_rpt rng=Electricity1_FR!a1'
*execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_FR_rpt rng=Electricity2_FR!a1'
*execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_FR_rpt rng=Electricity3_FR!a1'

execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=ElectricityGeneration_total_xtype_rpt rng=Generation!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=ElectricityGeneration_GER_xtype_rpt rng=Generation_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=ElectricityGeneration_FR_xtype_rpt rng=Generation_FR!a1'
execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=AddedCapacities_total_xtype_rpt rng=Added!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=AddedCapacities_GER_xtype_rpt rng=Added_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=AddedCapacities_FR_xtype_rpt rng=Added_FR!a1'
execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=AccAddedCapacities_total_xtype_rpt rng=AccAdded!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=AccAddedCapacities_GER_xtype_rpt rng=AccAdded_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=AccAddedCapacities_FR_xtype_rpt rng=AccAdded_FR!a1'
execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=InstalledCapacities_total_xtype_rpt rng=Installed!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=InstalledCapacities_GER_xtype_rpt rng=Installed_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=InstalledCapacities_FR_xtype_rpt rng=Installed_FR!a1'
execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Storage_total_rpt rng=Storage!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Storage_GER_rpt rng=Storage_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Storage_FR_rpt rng=Storage_FR!a1'
execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=NTCTransfers_total_rpt rng=NTCTransfers!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=NTCTransfers_GER_rpt rng=NTCTransfers_GER!a1'
$if      set details    execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=NTCTransfers_FR_rpt rng=NTCTransfers_FR!a1'

$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=stock rng=stock!a1 par=accspendings rng=accspendings!a1 par=flheur_ls_rpt rng=flheur_ls_rpt!a1 par=flheur_cost_rpt rng=flheur_cost_rpt!a1 par=flheur_check_rpt rng=flheur_check_rpt!a1 par=flheur_stock_rpt rng=flheur_stock_rpt!a1'                                    
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_GER_rpt rng=Electricity1_GER!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_GER_rpt rng=Electricity2_GER!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_GER_rpt rng=Electricity3_GER!a1'

*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_FR_rpt rng=Electricity1_FR!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_FR_rpt rng=Electricity2_FR!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_FR_rpt rng=Electricity3_FR!a1'

*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_UK_rpt rng=Electricity1_UK!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_UK_rpt rng=Electricity2_UK!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_UK_rpt rng=Electricity3_UK!a1'

*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_DK_rpt rng=Electricity1_DK!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_DK_rpt rng=Electricity2_DK!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_DK_rpt rng=Electricity3_DK!a1'

*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_NO_rpt rng=Electricity1_NO!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_NO_rpt rng=Electricity2_NO!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_NO_rpt rng=Electricity3_NO!a1'

*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_PL_rpt rng=Electricity1_PL!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_PL_rpt rng=Electricity2_PL!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_PL_rpt rng=Electricity3_PL!a1'

*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity1_ES_rpt rng=Electricity1_ES!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity2_ES_rpt rng=Electricity2_ES!a1'
*$if      set flhcal                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=Electricity3_ES_rpt rng=Electricity3_ES!a1'

$if      set noweffect              execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=noweffect_rpt rng=noweffect!a1 par=realeffect_rpt rng=realeffect!a1 par=effect_rpt rng=effect!a1 par=price_rpt rng=price!a1'

$if      set systemcost             execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=ihk_rpt rng=ihk!a1'
$if      set systemcost             execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=ihk_GER_rpt rng=ihk_GER!a1'

$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=capcost_rrpt rng=capcost!a1 par=irnwlim_rrpt         rng=irnwlim!a1          par=pfuel_rrpt rng=pfuel!a1 par=daref_ind rng=daref_ind!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=fomcost_rrpt rng=fomcost!a1 par=irnwflh_windon_rrpt  rng=irnwflh_windon!a1   par=pco2_rrpt rng=pco2!a1 par=daref_res rng=daref_res!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=vomcost_rrpt rng=vomcost!a1 par=irnwflh_windoff_rrpt rng=irnwflh_windoff!a1  par=daref rng=daref!a1 par=daref_com rng=daref_com!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=effrate_rrpt rng=effrate!a1 par=irnwflh_openpv_rrpt  rng=irnwflh_openpv!a1   par=price_rrpt rng=price!a1 par=daref_tra rng=daref_tra!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=emit_rrpt    rng=emit!a1    par=irnwflh_roofpv_rrpt  rng=irnwflh_roofpv!a1   par=pricetot_rrpt rng=pricetot!a1 par=daref_oth rng=daref_oth!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=irnwflh_windon90_rrpt  rng=irnwflh_windon90!a1   par=irnwflh_rrpt  rng=irnwflh!a1 par=daref_hyd rng=daref_hyd!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=irnwflh_windoff90_rrpt rng=irnwflh_windoff90!a1  par=irnwflh90_rrpt  rng=irnwflh90!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=irnwflh_openpv90_rrpt  rng=irnwflh_openpv90!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=irnwflh_roofpv90_rrpt  rng=irnwflh_roofpv90!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=gcapcost_rrpt rng=gcapcost!a1 par=gfomcost_rrpt      rng=gfomcost!a1         par=gvomcost_rrpt      rng=gvomcost!a1         par=geffrate_rrpt rng=geffrate!a1'
$if      set costcal                execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=tcapcost_rrpt rng=tcapcost!a1 par=tfomcost_rrpt      rng=tfomcost!a1         par=tvomcost_rrpt      rng=tvomcost!a1         par=teffrate_rrpt rng=teffrate!a1'

$if      set systemcost             execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=TotalSystemCost_rpt rng=SystemCost!a1'
$if      set systemcost             execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=SystemCost_GER_rpt rng=SystemCost_GER!a1'
$if      set systemcost             execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=SystemCost_FR_rpt rng=SystemCost_FR!a1'
$if      set systemcost             execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=TotalAggreSystemCost_rpt rng=AgreeCost_total!a1 par=TotalInterval_rpt rng=TotalInterval!a1'

$if not  set static                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=euets_rpt rng=euets!a1'

$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=sharecap_crises rng=sharecap!a1'
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=shareafcap_crises rng=shareafcap!a1'
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=sharegen_crises rng=sharegen!a1'
$ontext
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=gen_crises
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=cap_crises
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=afcap_crises

$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=market_crises
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=discostco2_tyrpt
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=discostco2_nochp_tyrpt
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=discostco2_chp_tyrpt
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=discostco2_gas
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=discostco2_gasnochp
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=discostco2_gaschp
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=marginal
$if      set crises                 execute 'gdxxrw.exe report\%exl%_rpt.gdx o=excel\%exl%.xlsx par=marginalirnw

$offtext