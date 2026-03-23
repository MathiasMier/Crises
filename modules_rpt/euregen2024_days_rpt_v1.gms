* * * Allocate not vintage-specific variable into vintage-specific variables
Parameter
XL_d(sd,hd,i,v,r,t)
XTWHL(i,v,r,t)
*shareirnw(i,v,r,t)
shareirnw_s_d(sd,hd,i,v,r,t)
;

alias(irnw,irnww) ;
alias(ivrt,ivrtt) ;
alias(jvrt,jvrtt) ;
alias(newv,newvv) ;
alias(oldv,oldvv) ;

XL_d(sd,hd,ivrt(i,v,r,t)) = X_d.L(sd,hd,i,v,r,t) ;
XTWHL(ivrt(i,v,r,t)) = XTWH.L(i,v,r,t) ;

*$if set mergeirnw   shareirnw(ivrt(irnw(i),newv(v),r,t))$(sum((sd,hd), days(sd) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t)))) > 0) = sum((sd,hd), days(sd) * vrsc_d(sd,hd,i,v,r) * XC.L(i,v,r,t))
*$if set mergeirnw       / sum((sd,hd), days(sd) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t)))) ; 
*$if set mergeirnw   shareirnw(ivrt(irnw(i),oldv(v),r,t))$(sum((sd,hd), days(sd) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t)))) > 0) = sum((sd,hd), days(sd) * vrsc_exi_d(sd,hd,i,r,t) * XC.L(i,v,r,t))
*$if set mergeirnw       / sum((sd,hd), days(sd) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t)))) ; 
$if set mergeirnw   shareirnw_s_d(sd,hd,ivrt(irnw(i),newv(v),r,t))$((sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t))) > 0) = vrsc_d(sd,hd,i,v,r) * XC.L(i,v,r,t)
$if set mergeirnw       / (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t))) ; 
$if set mergeirnw   shareirnw_s_d(sd,hd,ivrt(irnw(i),oldv(v),r,t))$((sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t))) > 0) = vrsc_exi_d(sd,hd,i,r,t) * XC.L(i,v,r,t)
$if set mergeirnw       / (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc_d(sd,hd,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi_d(sd,hd,ii,r,t) * XC.L(ii,vv,r,t))) ; 

$if set mergeirnw   XL_d(sd,hd,ivrt(irnw(i),v,r,t)) = XIRNW_d.L(sd,hd,r,t) * shareirnw_s_d(sd,hd,i,v,r,t) ;
$if set mergeirnw   XTWHL(ivrt(irnw(i),v,r,t)) = sum((sd,hd), days(sd) * XL_d(sd,hd,i,v,r,t)) * 1e-3 ;
*$if set mergeirnw   XTWHL(ivrt(irnw(i),v,r,t)) = XTWHIRNW.L(r,t) * shareirnw(i,v,r,t) ;
*$if set mergeirnw   XL_d(sd,hd,ivrt(irnw(i),v,r,t)) = XIRNW_d.L(sd,hd,r,t) * shareirnw(i,v,r,t) ;

parameter
IGL(j,r,t)                              Interim capacity for "no storage" run
GCL(j,v,r,t)                            Interim capacity for "no storage" run
GBL_d(sd,hd,j,v,r,t)                          Interim capacity for "no storage" run
GL_d(sd,hd,j,v,r,t)                           Interim capacity for "no storage" run
GDL_d(sd,hd,j,v,r,t)                          Interim capacity for "no storage" run
;

* Interim calculations
IGL(j,r,t)      = eps ;
GCL(j,v,r,t)    = eps ;
GBL_d(sd,hd,j,v,r,t)  = eps ;
GL_d(sd,hd,j,v,r,t)   = eps ;
GDL_d(sd,hd,j,v,r,t)  = eps ;

$if not  set storage  IG.L(j,r,t) = eps ;
$if not  set storage  GC.L(j,v,r,t) = eps ;
$if not  set storage  GB_d.L(sd,hd,j,v,r,t) = eps ;
$if not  set storage  G_d.L(sd,hd,j,v,r,t) = eps ;
$if not  set storage  GD_d.L(sd,hd,j,v,r,t) = eps ;

$if not  set storage  GCNV.L(j,r,t) = eps ;
$if not  set storage  GBNV_d.L(sd,hd,j,r,t) = eps ;
$if not  set storage  GNV_d.L(sd,hd,j,r,t) = eps ;
$if not  set storage  GDNV_d.L(sd,hd,j,r,t) = eps ;

$if      set storage                               IGL(j,r,t)                   = IG.L(j,r,t)   ;
$if      set storage $if not  set storagebalnv     GCL(jvrt(j,v,r,t))           = GC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     GCL(jvrt(nonvj(j),v,r,t))    = GC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     GCL(jvrt(nvj(j),v,r,t))$(sum(vv, gcapt(j,vv,r,"2023")) > 0)      = GCNV.L(j,r,t) * gcapt(j,v,r,"2023") / sum(vv, gcapt(j,vv,r,"2023")) ;
$if      set storage $if      set storagebalnv     $if    set hydrogensimple    GCL(jvrt(ghyd(j),v,r,t))    = GCC.L(j,v,r,t) ;
$if      set storage $if not  set storagebalnv     GBL_D(sd,hd,jvrt(j,v,r,t))  = GB_D.L(sd,hd,j,v,r,t) ;
$if      set storage $if not  set storagebalnv     GL_D(sd,hd,jvrt(j,v,r,t))   = G_D.L(sd,hd,j,v,r,t)  ;
$if      set storage $if not  set storagebalnv     GDL_D(sd,hd,jvrt(j,v,r,t))  = GD_D.L(sd,hd,j,v,r,t) ;
$if      set storage $if      set storagebalnv     GL_D(sd,hd,jvrt(nonvj(j),v,r,t))  = G_D.L(sd,hd,j,v,r,t)  ;
$if      set storage $if      set storagebalnv     GDL_D(sd,hd,jvrt(nonvj(j),v,r,t)) = GD_D.L(sd,hd,j,v,r,t) ;
$if      set storage $if      set storagebalnv     GBL_D(sd,hd,jvrt(nonvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV_DD.L(sd,j,r,t) * ghours(j,v,r) * GCL(j,v,r,t) / sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     $if    set hydrogensimple    GBL_D(sd,hd,jvrt(ghyd(j),v,r,t))$(sum(vv, GCR.L(j,vv,r,t)) > 0) = GBNV_DD.L(sd,j,r,t) * GCL(j,v,r,t) / sum(vv, GCR.L(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     GBL_D(sd,hd,jvrt(nvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV_DD.L(sd,j,r,t) * ghours(j,v,r) * gcapt(j,v,r,"2023") / sum(vv, ghours(j,vv,r) * gcapt(j,vv,r,"2023")) ;
$if      set storage $if      set storagebalnv     GL_D(sd,hd,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GNV_D.L(sd,hd,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     GDL_D(sd,hd,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GDNV_D.L(sd,hd,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     GDL_d(sd,hd,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GDNV_d.L(sd,hd,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;

parameter
load_res_d(sd,hd,r,t)
;

$if      set hydrogensimple load_res_d(sd,hd,r,t) = load_d(sd,hd,r,t) - load_sec_d(sd,hd,r,t,"hyd") ;
$if      set hydrogensimple daref(r,t) = sum((sd,hd), days(sd) * load_res_d(sd,hd,r,t)) * 1e-3 ;