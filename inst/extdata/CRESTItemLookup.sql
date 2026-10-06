with d as (SELECT ii.TEXT, ii.column_name, 
CASE WHEN LENGTH(ii.column_name) > 30 
    THEN upper(SUBSTR(ii.column_name, -30)) 
    ELSE upper(ii.column_name) END AS db_item,
    id.text as disposition, im.alttext as modifier, ig.text as grouping, 
    case when coalesce(ias.species_txt, chc.species_txt) is null then ii.text || ' * (No Hart Code)' else  coalesce(ias.species_txt, chc.species_txt) end as common_name, 
    rt.display_name/*, case when chc.hart_Cd = '618' then '597' else chc.hart_Cd end as species*/, rt.pac_code as species, irr.text as LengthClass
                    FROM otolith_V1.creel_irec_item ii 
                    LEFT JOIN otolith_V1.creel_irec_retainability irr on irr.retainability_id = ii.retainability_id_id
                    left join otolith_v1.creel_irec_disposition id on ii.disposition_id_id = id.disposition_id
                    left join otolith_V1.creel_irec_modifier im on ii.modifier_id_id = im.modifier_id
                    left join otolith_v1.creel_irec_grouping ig on ii.grouping_id_id = ig.grouping_id
                    left join otolith_v1.creel_hart_Cd chc on ii.hart_cd_id_id = chc.hart_Cd
                    LEFT JOIN otolith_v1.REFDATA_RESOURCE_TYPES rt ON chc.rsty_id_id = rt.rsty_id
                    left join otolith_v1.creel_irec_altspp ias on ii.hart_Cd_id_id = ias.hart_cd
                   )
                   select * from d 