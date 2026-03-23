* * * Allocate not vintage-specific variable into vintage-specific variables
Parameter
XL(s,i,v,r,t)
XTWHL(i,v,r,t)
*shareirnw(i,v,r,t)
shareirnw_s(s,i,v,r,t)
;

alias(irnw,irnww) ;
alias(ivrt,ivrtt) ;
alias(jvrt,jvrtt) ;
alias(newv,newvv) ;
alias(oldv,oldvv) ;

XL(s,ivrt(i,v,r,t)) = X.L(s,i,v,r,t) ;
XTWHL(ivrt(i,v,r,t)) = XTWH.L(i,v,r,t) ;

*$if set mergeirnw   shareirnw(ivrt(irnw(i),newv(v),r,t))$(sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) > 0) = sum(s, hours(s) * vrsc(s,i,v,r) * XC.L(i,v,r,t))
*$if set mergeirnw       / sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) ; 
*$if set mergeirnw   shareirnw(ivrt(irnw(i),oldv(v),r,t))$(sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) > 0) = sum(s, hours(s) * vrsc_exi(s,i,r,t) * XC.L(i,v,r,t))
*$if set mergeirnw       / sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) ; 
$if set mergeirnw   shareirnw_s(s,ivrt(irnw(i),newv(v),r,t))$((sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) > 0) = vrsc(s,i,v,r) * XC.L(i,v,r,t)
$if set mergeirnw       / (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) ; 
$if set mergeirnw   shareirnw_s(s,ivrt(irnw(i),oldv(v),r,t))$((sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) > 0) = vrsc_exi(s,i,r,t) * XC.L(i,v,r,t)
$if set mergeirnw       / (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) ; 

$if set mergeirnw   XL(s,ivrt(irnw(i),v,r,t)) = XIRNW.L(s,r,t) * shareirnw_s(s,i,v,r,t) ;
$if set mergeirnw   XTWHL(ivrt(irnw(i),v,r,t)) = sum(s, hours(s) * XL(s,i,v,r,t)) * 1e-3 ;
*$if set mergeirnw   XTWHL(ivrt(irnw(i),v,r,t)) = XTWHIRNW.L(r,t) * shareirnw(i,v,r,t) ;
*$if set mergeirnw   XL(s,ivrt(irnw(i),v,r,t)) = XIRNW.L(s,r,t) * shareirnw(i,v,r,t) ;

parameter
IGL(j,r,t)                              Interim capacity for "no storage" run
GCL(j,v,r,t)                            Interim capacity for "no storage" run
GBL(s,j,v,r,t)                          Interim capacity for "no storage" run
GL(s,j,v,r,t)                           Interim capacity for "no storage" run
GDL(s,j,v,r,t)                          Interim capacity for "no storage" run
;

* Interim calculations
IGL(j,r,t)      = eps ;
GCL(j,v,r,t)    = eps ;
GBL(s,j,v,r,t)  = eps ;
GL(s,j,v,r,t)   = eps ;
GDL(s,j,v,r,t)  = eps ;

$if not  set storage  IG.L(j,r,t) = eps ;
$if not  set storage  GC.L(j,v,r,t) = eps ;
$if not  set storage  GB.L(s,j,v,r,t) = eps ;
$if not  set storage  G.L(s,j,v,r,t) = eps ;
$if not  set storage  GD.L(s,j,v,r,t) = eps ;

$if not  set storage  GCNV.L(j,r,t) = eps ;
$if not  set storage  GBNV.L(s,j,r,t) = eps ;
$if not  set storage  GNV.L(s,j,r,t) = eps ;
$if not  set storage  GDNV.L(s,j,r,t) = eps ;

$if      set storage                               IGL(j,r,t)                   = IG.L(j,r,t)   ;
$if      set storage $if not  set storagebalnv     GCL(jvrt(j,v,r,t))           = GC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     GCL(jvrt(nonvj(j),v,r,t))    = GC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     $if    set hydrogensimple    GCL(jvrt(ghyd(j),v,r,t))    = GCC.L(j,v,r,t) ;

$if not  set static  $if      set storage $if      set storagebalnv     GCL(jvrt(nvj(j),v,r,t))$(sum(vv, gcapt(j,vv,r,"2023")) > 0)      = GCNV.L(j,r,t) * gcapt(j,v,r,"2023") / sum(vv, gcapt(j,vv,r,"2023")) ;
$if      set static  $if      set storage $if      set storagebalnv     GCL(jvrt(nvj(j),v,r,t))$(sum(vv, gcapt_static(j,vv,r,t)) > 0)    = GCNV.L(j,r,t) * gcapt_static(j,v,r,t) / sum(vv, gcapt_static(j,vv,r,t)) ;

$if      set storage $if not  set storagebalnv     GBL(s,jvrt(j,v,r,t))  = GB.L(s,j,v,r,t) ;
$if      set storage $if not  set storagebalnv     GL(s,jvrt(j,v,r,t))   = G.L(s,j,v,r,t)  ;
$if      set storage $if not  set storagebalnv     GDL(s,jvrt(j,v,r,t))  = GD.L(s,j,v,r,t) ;
$if      set storage $if      set storagebalnv     GL(s,jvrt(nonvj(j),v,r,t))  = G.L(s,j,v,r,t)  ;
$if      set storage $if      set storagebalnv     GDL(s,jvrt(nonvj(j),v,r,t)) = GD.L(s,j,v,r,t) ;
$if      set storage $if      set storagebalnv     GBL(s,jvrt(nonvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * ghours(j,v,r) * GCL(j,v,r,t) / sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     $if    set hydrogensimple    GBL(s,jvrt(ghyd(j),v,r,t))$(sum(vv, GCR.L(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * GCL(j,v,r,t) / sum(vv, GCR.L(j,vv,r,t)) ;


$if not  set static  $if      set storage $if      set storagebalnv     GBL(s,jvrt(nvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * ghours(j,v,r) * gcapt(j,v,r,"2023") / sum(vv, ghours(j,vv,r) * gcapt(j,vv,r,"2023")) ;
$if      set static  $if      set storage $if      set storagebalnv     GBL(s,jvrt(nvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * ghours(j,v,r) * gcapt_static(j,v,r,t) / sum(vv, ghours(j,vv,r) * gcapt_static(j,vv,r,t)) ;

$if      set storage $if      set storagebalnv     GL(s,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GNV.L(s,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     GDL(s,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GDNV.L(s,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;

parameter
load_res(s,r,t)
;

$if set hydrogensimple load_res(s,r,t) = load(s,r,t) - load_sec(s,r,t,"hyd") ;
$if set hydrogensimple daref(r,t) = sum(s, hours(s) * load_res(s,r,t)) * 1e-3 ;