set
invset /windon,windoff,solarpv/
tinvset(t) /2025,2026,2027,2028,2029/
;

parameter
invlimit(invset,r,t)
;

invlimit("windon",r,"2024") = round(max(sum(windon(i),capt(i,"2023",r,"2023")),sum(windon(i),capt(i,"2022",r,"2022"))) + 3 * daref(r,"2022") / daref("Germany","2022"),4) ;
invlimit("windoff",r,"2024") = round(max(sum(windoff(i),capt(i,"2023",r,"2023")),sum(windoff(i),capt(i,"2022",r,"2022"))) + 1 * daref(r,"2022") / daref("Germany","2022"),4) ;
invlimit("solarpv",r,"2024") = round(max(sum(sol(i),capt(i,"2023",r,"2023")),sum(sol(i),capt(i,"2022",r,"2022"))) + 5 * daref(r,"2022") / daref("Germany","2022"),4) ;
invlimit(invset,r,"2030") = invlimit(invset,r,"2024") * 2 ;
invlimit(invset,r,tinvset(t)) = round(invlimit(invset,r,"2024") + (invlimit(invset,r,"2030") - invlimit(invset,r,"2024")) * (t.val - 2024) / (2030 - 2024),4) ;

Equations
investlimit_windon(r,t)
investlimit_windoff(r,t)
investlimit_sol(r,t)
;

investlimit_windon(r,topt2024_2030(t))..
    sum(windon(i), IX(i,r,t)) =l= invlimit("windon",r,t) ;

investlimit_windoff(r,topt2024_2030(t))..
    sum(windoff(i), IX(i,r,t)) =l= invlimit("windoff",r,t) ;
    
investlimit_sol(r,topt2024_2030(t))..
    sum(sol(i), IX(i,r,t)) =l= invlimit("solarpv",r,t) ;