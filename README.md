(A, introduction) First, start with downloading databases and model framework. Unzip model framework and place unzipped databases into the subfolder databases.

The framework consists of
1) gms.files (code)
2) gdx.files (data, output, report)
3) bat.files (to run codes)
4) xlsx.files (input and output data)
5) opt.files (license option files; the displayed version uses gurobi, which is hardcoded in "euregen2024_mod_v28.gms"; it can be changed easily at the respective position at line 1993)

The framework has
1) Database routine (multiple files, full access is restricted because some data inputs cannot get freely circulated)
2) Precal (precalculation) routine (euregen2024_pre_v7.gms)
3) Dynamic model (euregen2024_mod_v28.gms)
4) Static model (euregen2024_mod_v28.gms)
5) Reporting routine (euregen2024_rpt_v9.gms)

(B, precalculation) Next, continue with the precalculation routines by running all relevant precal.files
1) precal_baup_6d.bat
2) precal_reco_6d.bat
3) precal_high_6d.bat

(C, benchmark run without any crises) Next, start the iteration routine with the no crises reference scenario by running 
1) dynamic_iter_bauploa_6d.bat
The routine is already converged according to the EU ETS model but you can rerun it or just run it ones (with reporting).

(D, iteration routine and sensitivies for long-run scenario) After convergence, start the other scenarios by running
1) dynamic_iter_baup_6d.bat
2) dynamic_iter_reco_6d.bat
3) dynamic_iter_high_6d.bat
Note gain that the routines are already converged to the EU ETS model.
Furthermore, sensitivities runs come from
4) dynamic_mark_sensi_high_6d.bat

(E, static model for hourly prices in 2022, main results) The dynamic model outputs can now be used as inputs for the static runs. Start those by running
1) static_price_baup_2022.bat
2) static_price_reco_2022.bat
3) static_price_high_2022.bat
Furthermore, sensitivities runs come from
4) static_price_sensi_high_2022.bat

(F, static model for houry prices in 2023, supplementary material) Note that there are several more bat.files to redo the analysis for 2023
1) static_price_baup_2023.bat
2) static_price_reco_2023.bat
3) static_price_high_2023.bat
 
Diagrams and tables: There are several excel files with diagrams and tables

Main text
- Figure 1 comes from sheet "dia" in Diagrams_Fuelprices_RevisionCrises.xlsx
- Table 1 comes from sheet "pfuel" in Tables_RevisionCrises.xlsx
- Table 2 comes from sheet "Scenarios" in Diagrams_RevisionCrises.xlsx
- Figure 2 was created with power point but the source is lost
- Figure 3 comes from sheet "Dia" in Diagrams_RevisionCrises.xlsx
- Figure 4 comes from sheet "Countryshares" in Diagrams_RevisionCrises.xlsx
- Table 4 comes from sheet "TAB_nowreal_2022" in Diagrams_RevisionCrises.xlsx

Appendix
- Nomenclature comes from Nomenclature.xlsx
- Table A.2 comes from sheet "nuclearpipe" in Tables_Capacities_RevisionCrises.xlsx
- Table C.4 comes from sheet "euets" in Tables_RevisionCrises.xlsx
- Figure D.1 comes from sheet "Dia" in Diagrams_RevisionCrises.xlsx
- Table D.5 (line 25) and D.6 (upper part line 1, lower part line 53) come from sheet "CO2" (line 25) in Diagrams_RevisionCrises.xlsx
- Table D.7 comes from sheet "CO2" (line 79) in Diagrams_RevisionCrises.xlsx
- Table E.8 comes from sheet "DTAB_2022" (line 2) in Diagrams_RevisionCrises.xlsx
- Figure F.2 comes from sheet "Importmix_fuel" in Diagrams_RevisionCrises.xlsx
- Table G.9 comes from sheet "nowreal_app" in Diagrams_RevisionCrises.xlsx

Supplementary material
- Tables I.1 to I.5 come from sheet "cost" in Tables_RevisionCrises.xlsx
- Tables II.6 to II.8 come from sheet "daref" in Tables_RevisionCrises.xlsx
- Supplementary material III. comes from sheet "capt" (until line 356) in Tables_Capacities_RevisionCrises.xlsx
- Supplementary material IV. comes from sheet "capt" (after line 357) in Tables_Capacities_RevisionCrises.xlsx
- Supplementary material V. comes from sheet "gcapt" in Tables_Capacities_RevisionCrises.xlsx
- Tables VI.13, VI.14, and VI.15 in Supplementary material VI. come from sheet "gen" in Tables_Capacities_RevisionCrises.xlsx
- Table VII.16 in Supplementary material VII. comes from sheet "irnwlim" in Tables_RevisionCrises.xlsx
- Tables VII.17 to VII.21 in Supplementary material VII. come from sheet "flh" in Tables_RevisionCrises.xlsx
- Tables VII.22 to VII.25 in Supplementary material VII. come from sheet "flh90" in Tables_RevisionCrises.xlsx
- Table VIII.26 in Supplementary material VIII. comes from sheet "DTAB_2023" (after line 66) in Diagrams_RevisionCrises.xlsx
- Table VIII.27 in Supplementary material VIII. comes from sheet "DTAB_2023" (before line 66) in Diagrams_RevisionCrises.xlsx
- Figure IX.1 in Supplementary material IX. comes from sheet "Technologymix_hydro" in Diagrams_RevisionCrises.xlsx
- Figure IX.2 in Supplementary material IX. comes from sheet "Importmix_hydro" in Diagrams_RevisionCrises.xlsx
- Figure IX.3 in Supplementary material IX. comes from sheet "Technologymix_nuc" in Diagrams_RevisionCrises.xlsx
- Figure IX.4 in Supplementary material IX. comes from sheet "Importmix_nuc" in Diagrams_RevisionCrises.xlsx
- Figure X.5 in Supplementary material X. comes from sheet "bau_scatter_gen" in Diagrams_RevisionCrises.xlsx
- Figure X.6 in Supplementary material X. comes from sheet "high_frn_hyd_scatter_gen" in Diagrams_RevisionCrises.xlsx
- Figure X.7 in Supplementary material X. comes from sheet "high_frn_hyd_scatter_gen_fullpr" in Diagrams_RevisionCrises.xlsx
