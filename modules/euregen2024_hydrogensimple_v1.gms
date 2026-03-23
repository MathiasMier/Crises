parameter
himport(r,t)                     Hydrogen import price (EUR per MWh)
hydtwh(r,t)
hydtwh_int(r,t)
;

$gdxin precal\precal_%n%.gdx
$load himport,hydtwh_int=hydtwh
$gdxin

hydtwh(r,t) = 1.0 * hydtwh_int(r,t) ;
$if      set hydrogenten    hydtwh(r,t) = 0.1 * hydtwh_int(r,t) ;
$if      set hydrogenhalf   hydtwh(r,t) = 0.5 * hydtwh_int(r,t) ;
$if      set hydrogendouble hydtwh(r,t) = 2.0 * hydtwh_int(r,t) ;
$if      set hydrogentriple hydtwh(r,t) = 3.0 * hydtwh_int(r,t) ;
hydtwh_int(r,t) = hydtwh(r,t) ;
$if not  set hydrogenimport hydtwh(r,t) = round(1/3 * hydtwh(r,t),4) ;

positive variable
HIMP(j,r,t)             Imported hydrogen (TWh)
HRES(j,r,t)             Used hydrogen outside (TWh)
;

equation
demand_hydrogen(r,t)
demand_hydrogenimport(r,t)
;

demand_hydrogen(r,toptimize(t))..
        HRES("Storage_LT",r,t) =e= hydtwh(r,t) ;

demand_hydrogenimport(r,toptimize(t))..
        HIMP("Storage_LT",r,t) =l= round(2/3 * hydtwh(r,t),4) ;