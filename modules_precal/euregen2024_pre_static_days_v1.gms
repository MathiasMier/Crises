* Renewable targets
set
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
co2p_static_d(sd,t)
co2p_static_ave(t)
pfuel_static_d(sd,fuel,r,t)
;

*$onecho >temp\gdxxrw.rsp
*par=static2022           rng=2022!a1                rdim=1 cdim=1
*par=static2023           rng=2023!a1                rdim=1 cdim=1
*$offecho

*$call 'gdxxrw i=database\data\static_data.xlsx o=database\data_static\par_static.gdx trace=3 log=database\temp\par_static.log @temp\gdxxrw.rsp';

$gdxin database\data_static\par_static
$load static2022,static2023
$gdxin

co2p_static_d(sd,"2022") = sum(srepdays(sd,d), static2022(d,"CO2")) ;
*co2p_static_d(sd,"2023") = sum(srep_d(sd,h), sum(maphd(h,d), static2022(d,"CO2"))) ;

pfuel_static_d(sd,"Gas",r,"2022") = sum(srepdays(sd,d), static2022(d,"Gas")) ;
*pfuel_static_d(sd,"Gas",r,"2023") = sum(srep_d(sd,h), sum(maphd(h,d), static2023(d,"Gas"))) ;

pfuel_static_d(sd,"Coal",r,"2022") = sum(srepdays(sd,d), static2022(d,"Coal")) ;
*pfuel_static_d(sd,"Coal",r,"2023") = sum(srep_d(sd,h), sum(maphd(h,d), static2023(d,"Coal"))) ;

pfuel_static_d(sd,"Oil",r,"2022") = sum(srepdays(sd,d), static2022(d,"Oil")) ;
*pfuel_static_d(sd,"Oil",r,"2023") = sum(srep_d(sd,h), sum(maphd(h,d), static2023(d,"Oil"))) ;

pfuel_static_d(sd,"Uranium",r,"2022") = sum(srepdays(sd,d), static2022(d,"Uranium")) ;
*pfuel_static_d(sd,"Uranium",r,"2023") = sum(srep_d(sd,h), sum(maphd(h,d), static2023(d,"Uranium"))) ;

co2p_static_ave("2022") = 81.48 ;
*co2p_static_ave("2023") = 89.38 ;

