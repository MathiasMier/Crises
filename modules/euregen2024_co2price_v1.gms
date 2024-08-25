parameter
co2price_mod(t,*)
co2price_new(r,t)
;

$if      set from6d     $gdxin report\6d_%s%_rpt.gdx
$if      set from31s    $gdxin report\31s_%s%_rpt.gdx
$if      set from119s   $gdxin report\119s_%s%_rpt.gdx
$load co2price_mod=Emissions_total_rpt
$gdxin

co2price_new(r,t) = co2price_mod(t,"CO2 price-eu") ;           
co2price_new("Britain",t) = co2price_mod(t,"CO2 price-uk")  ;
    
* * * Dispatch cost
discost(ivrt(i,v,r,t)) =
*        Variable O&M costs
                           vomcost(i,v,r) 
*        Fuel costs (including region-specific price delta) including regional adder relative                    
                         + round(sum(xfueli(fuel,i), pfuel(fuel,r,t) / effrate(i,v,r)), 8)
* Determine true average load of each vintages and calibrate for ramping cost here
* Determine true average load of each vintages and calibrate for ramping cost here
$if      set ramcost     * (1 + effloss(i,v,r) / 0.5 )
*        Regional adder absolute
*                         + (pfadd(fuel,r,t)$xfueli(fuel,i))$effrate(i,v,r)
*        CO2 price (includes benefits from negative emissions)
$if      set co2price    + emit(i,v,r) * co2price_new(r,t)
*        CCS costs (from capturing)
$if      set ccs         + round(co2captured(i,v,r) * ccscost(r,t), 8)
;

