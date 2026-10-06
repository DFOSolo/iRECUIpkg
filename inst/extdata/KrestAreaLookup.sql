
select ia.area_id, ia.text, gf.display_name as EstimationArea, cla.lrg_area_nme, csa.sml_Area_nme,ccp.project_shrt_nme as program, cla.lrg_area_id, 
csa.std_ref_type_Cde, cp.project_grp_lng ,
case when cp.project_grp_lng = 'BC Interior' then 1
when cp.project_grp_lng = 'Lower Fraser' then  2
when cp.project_grp_lng = 'South Coast' then  0
when cp.project_grp_lng = 'North Coast' then 3
when cp.project_grp_lng = 'REGIONAL' then 4 else 0 end

as Administrative_Area
from otolith_V1.creel_Crest_sml_Area csa
left join otolith_V1.creel_Crest_lrg_area cla on csa.lrg_Area_id_id = cla.lrg_Area_id
left join otolith_V1.creel_crest_program ccp on cla.program_id_id = ccp.program_id
left join otolith_V1.creel_projects cp on ccp.project_id_id = cp.projects_id
left join otolith_V1.refdata_geographic_features gf on csa.std_ref_type_Cde = gf.abbreviation
left join otolith_V1.creel_irec_area ia on cla.lrg_Area_id = ia.lrg_Area_id_id
where csa.sml_Area_nme = cla.lrg_Area_nme
and csa.std_ref_type_Cde is not null
and ia.area_id is not null

/*
Area    Start_date  End_date
19      2012-07-01  2014-03-31
19(GS)  2014-04-01
19(JDF) 2014-04-01

20      2012-07-01  2020-03-31
20(E)   2020-04-01
20(W)   2020-04-01

23      2012-07-01  2014-03-31
23(Alb) 2014-04-01
23(Ba)  2014-04-01

*/
