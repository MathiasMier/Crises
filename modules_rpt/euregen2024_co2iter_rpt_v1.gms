* * * Set exogonous electriicty co2 emissions into the iterative file
parameter
co2elec_out(t) 
co2ind_out(t)
co2elecuk_out(t)
;

co2elec_out(t) = co2ele_in(t) + eps ;
co2elec_out(t)$(ECEU.L(t) > 0) = ECEU.L(t) + eps ;
co2elec_out(t)$(ECEU.L(t) = 0) = ECEU.L(t) + eps ;
co2elec_out(t)$(ECEU.L(t) < 0) = ECEU.L(t) + eps ;
co2ind_out(t) = co2ind_in(t) + eps ;
co2ind_out(t)$(ECEU.L(t) > 0) = ECEU.L(t) * co2elecind(t) + co2indorgfix_in(t)  + eps ;
co2ind_out(t)$(ECEU.L(t) < 0) = ECEU.L(t) * co2elecind(t) + co2indorgfix_in(t)  + eps ;

co2elecuk_out(t) = co2eleuk_in(t) + eps ;
co2elecuk_out(t)$(ECUK.L(t) > 0) = ECUK.L(t) + eps ;
co2elecuk_out(t)$(ECUK.L(t) < 0) = ECUK.L(t) + eps ; 

execute_unload   'euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx',             co2elec_out, co2ind_out ;
execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\%s%.xlsx                                       par=co2elec_out rng=co2elec_out!a1'

$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_loa.xlsx          par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_frn_loa.xlsx      par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_hyd_loa.xlsx      par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_frn_hyd_loa.xlsx  par=co2elec_out rng=co2elec_out!a1'

$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_loa.xlsx              par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_frn_loa.xlsx          par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_hyd_loa.xlsx          par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_frn_hyd_loa.xlsx      par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_str_frn_hyd_loa.xlsx  par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_str_frn_hyd.xlsx      par=co2elec_out rng=co2elec_out!a1'

$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_loa.xlsx              par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_frn_loa.xlsx          par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_hyd_loa.xlsx          par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_frn_hyd_loa.xlsx      par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_str_frn_hyd_loa.xlsx  par=co2elec_out rng=co2elec_out!a1'
$if      set baupiter   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_str_frn_hyd.xlsx      par=co2elec_out rng=co2elec_out!a1'

$if      set precal6d   execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_indfix_shortrun_6d_crises\%s%.xlsx     par=co2elec_out rng=co2elec_out!a1'
