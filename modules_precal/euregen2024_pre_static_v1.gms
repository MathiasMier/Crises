* Renewable targets
set
d /1*365/
comm /Gas,Coal,Oil,Uranium,CO2/
maphd(h,d)
;

*$onecho >temp\gdxxrw.rsp
*set=maphd      rng=maphd!a2                rdim=2 cdim=0 values=nodata

*$offecho
*$call 'gdxxrw i=database\data\static_data.xlsx o=database\data_static\set_static.gdx trace=3 log=database\temp\set_static.log @temp\gdxxrw.rsp';

$gdxin database\data_static\set_static
$load maphd
$gdxin

parameter
static2022(d,comm)
static2023(d,comm)
co2p_static(s,t)
co2p_static_ave(t)
pfuel_static(s,fuel,r,t)
;

*$onecho >temp\gdxxrw.rsp
*par=static2022           rng=2022!a1                rdim=1 cdim=1
*par=static2023           rng=2023!a1                rdim=1 cdim=1
*$offecho

*$call 'gdxxrw i=database\data\static_data.xlsx o=database\data_static\par_static.gdx trace=3 log=database\temp\par_static.log @temp\gdxxrw.rsp';

$gdxin database\data_static\par_static
$load static2022,static2023
$gdxin

co2p_static(s,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"CO2"))) ;
*co2p_static(s,"2023") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"CO2"))) ;

pfuel_static(s,"Gas",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Gas"))) ;
*pfuel_static(s,"Gas",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Gas"))) ;

pfuel_static(s,"Coal",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Coal"))) ;
*pfuel_static(s,"Coal",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Coal"))) ;

pfuel_static(s,"Oil",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Oil"))) ;
*pfuel_static(s,"Oil",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Oil"))) ;

pfuel_static(s,"Uranium",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Uranium"))) ;
*pfuel_static(s,"Uranium",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Uranium"))) ;

co2p_static_ave("2022") = 81.48 ;
*co2p_static_ave("2023") = 89.38 ;

