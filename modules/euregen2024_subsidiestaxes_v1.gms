* * * Subsidies and taxes (negative subsidies are taxes)
parameter
irnwsub(r,t)                     New irnw production subsidy (EUR per MWh)
rnwsub(r,t)                      New rnw production subsidy (EUR per MWh)
solsub(r,t)                      New solar PV production subsidy in (EUR per MWh)
windsub(r,t)                     New wind production subsidy (EUR per MWh)
nucsub(r,t)                      New nuclear production subsidy (EUR per MWh)
lowcarbsub(r,t)                  New rnw nuc and CCS production subsidy (EUR per MWh)
irnwsub_cap(r,t)                 New irnw capacity subsidy (EUR per kW)
rnwsub_cap(r,t)                  New rnw capacity subsidy (EUR per kW)
solsub_cap(r,t)                  New solar PV capacity subsidy in (EUR per kW)
windsub_cap(r,t)                 New wind capacity subsidy (EUR per kW)
nucsub_cap(r,t)                  New nuclear capacity subsidy (EUR per kW)
lowcarbsub_cap(r,t)              New rnw nuc and CCS capacity subsidy (EUR per kW)
;

$gdxin precal\precal_%n%.gdx
$load irnwsub
$load rnwsub
$load solsub
$load windsub
$load nucsub
$load lowcarbsub
$load irnwsub_cap
$load rnwsub_cap
$load solsub_cap
$load windsub_cap
$load nucsub_cap
$load lowcarbsub_cap
$gdxinin