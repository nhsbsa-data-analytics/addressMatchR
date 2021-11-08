--------------------------------------------------------------------------------
-- PART ONE: PROCESS AND STACK MULTIPLE SLA PER UPRN ---------------------------


--drop table int615_ab_plus_base;
--create table int615_ab_plus_base compress for query high as

with

ab_postcodes as (
select
    /*+ materialize */
    distinct
    trim(replace(postcode_locator, ' ', ''))  as  ab_postcode
from
    dall_ref.addressbase_plus
where
    1=1
    and class = 'RI01'
    and release_date = '15-MAR-21'
)
--select count(*) from ab_postcodes;
,


-- NOTE: SOME FIELD NAME DIFFERENT TO PUBLISHED SQL CODE TO GENERATE SLA
-- DEPENDENT_LOCALITY == DEP_LOCALITY
-- DEPENDENT_THOROUGHFARE == DEP_THOROUGHFARE
-- DOUBLE_DEPENDENT_LOCALITY == DOU_DEP_LOCALITY

sla as (
SELECT
    /*+ materialize */
    uprn,
    class,
    addressbase_postal,
    trim(replace(postcode_locator, ' ', ''))  as  ab_postcode,
    -- DPA SLA
    (
        CASE WHEN department_name           IS NOT NULL THEN department_name|| ', '             ELSE '' END
     || CASE WHEN rm_organisation_name      IS NOT NULL THEN rm_organisation_name || ', '       ELSE '' END
     || CASE WHEN sub_building_name         IS NOT NULL THEN sub_building_name || ', '          ELSE '' END
     || CASE WHEN building_name             IS NOT NULL THEN building_name || ', '              ELSE '' END
     || CASE WHEN building_number           IS NOT NULL THEN building_number || ' '             ELSE '' END
     || CASE WHEN po_box_number             IS NOT NULL THEN 'PO BOX ' || po_box_number || ', ' ELSE '' END
     || CASE WHEN dep_thoroughfare          IS NOT NULL THEN dep_thoroughfare || ', '           ELSE '' END
     || CASE WHEN thoroughfare              IS NOT NULL THEN thoroughfare || ', '               ELSE '' END
     || CASE WHEN dou_dep_locality          IS NOT NULL THEN dou_dep_locality || ', '           ELSE '' END
     || CASE WHEN dep_locality              IS NOT NULL THEN dep_locality || ', '               ELSE '' END
     || CASE WHEN post_town                 IS NOT NULL THEN post_town || ', '                  ELSE '' END
     || postcode
    ) AS dpa_single_line_address,
    -- GEO_SLA
    (
        CASE WHEN la_organisation    IS NOT NULL                                                         THEN la_organisation || ', '          ELSE '' END
        -- sao
     || CASE WHEN sao_text           IS NOT NULL                                                         THEN sao_text        || ', '          ELSE '' END
        -- no sao start suffix
     || CASE WHEN sao_start_number   IS NOT NULL AND sao_start_suffix IS NULL AND sao_end_number IS NULL THEN TO_CHAR(sao_start_number) || ', '
             WHEN sao_start_number   IS NULL                                                             THEN ''                               ELSE TO_CHAR(sao_start_number) ||'' END
        -- no sao end number
     || CASE WHEN sao_start_suffix   IS NOT NULL AND sao_end_number IS NULL                              THEN sao_start_suffix || ', '
             WHEN sao_start_suffix   IS NOT NULL AND sao_end_number IS NOT NULL                          THEN sao_start_suffix                 ELSE '' END
        -- sao start number & sao end number -> add '-' between them 
     || CASE WHEN sao_end_suffix     IS NOT NULL AND sao_end_number IS NOT NULL                          THEN '-'
             WHEN sao_start_number   IS NOT NULL AND sao_end_number IS NOT NULL                          THEN '-'                              ELSE '' END
        -- sao end number and sao end suffix
     || CASE WHEN sao_end_number     IS NOT NULL AND sao_end_suffix IS NULL                              THEN TO_CHAR(sao_end_number) || ', '
             WHEN sao_end_number     IS NULL                                                             THEN ''                               ELSE TO_CHAR(sao_end_number) END
        -- sao end suffix
     || CASE WHEN sao_end_suffix     IS NOT NULL                                                         THEN sao_end_suffix || ', '           ELSE '' END
        -- pao
     || CASE WHEN pao_text           IS NOT NULL                                                         THEN pao_text || ', '                 ELSE '' END
        -- no pao start suffix
     || CASE WHEN pao_start_number   IS NOT NULL AND pao_start_suffix IS NULL AND pao_end_number IS NULL THEN TO_CHAR(pao_start_number) || ' '
             WHEN pao_start_number   IS NULL                                                             THEN ''                               ELSE TO_CHAR(pao_start_number) || '' END
        -- no pao end number
     || CASE WHEN pao_start_suffix   IS NOT NULL AND pao_end_number IS NULL                              THEN pao_start_suffix || ', '
             WHEN pao_start_suffix   IS NOT NULL AND pao_end_number IS NOT NULL                          THEN pao_start_suffix                 ELSE '' END
        -- pao start number & pao end number -> add '-' between them 
     || CASE WHEN pao_end_suffix     IS NOT NULL AND pao_end_number IS NOT NULL                          THEN '-'
             WHEN pao_start_number   IS NOT NULL AND pao_end_number IS NOT NULL                          THEN '-'                              ELSE '' END
        -- pao end number and pao end suffix
     || CASE WHEN pao_end_number     IS NOT NULL AND pao_end_suffix IS NULL                              THEN TO_CHAR(pao_end_number) || ', '
             WHEN pao_end_number     IS NULL                                                             THEN ''                               ELSE TO_CHAR(pao_end_number) END
        -- pao end suffix
     || CASE WHEN pao_end_suffix     IS NOT NULL                                                         THEN pao_end_suffix || ', '           ELSE '' END
        -- street information
     || CASE WHEN street_description IS NOT NULL                                                         THEN street_description || ', '       ELSE '' END
        -- localilty
     || CASE WHEN locality           IS NOT NULL                                                         THEN locality || ', '                 ELSE '' END
        -- town
     || CASE WHEN town_name          IS NOT NULL                                                         THEN town_name || ', '                ELSE '' END
        -- postcode
     || CASE WHEN postcode_locator   IS NOT NULL                                                         THEN postcode_locator                 ELSE '' END
    ) AS geo_single_line_address 
from
    dall_ref.addressbase_plus
where
    1=1
    and release_date = '15-MAR-21'
    and trim(replace(postcode_locator, ' ', '')) in (select ab_postcode from ab_postcodes)
    and class not like 'L%'
    and class not like 'Z%'
    and country = 'E'
)
--select * from sla order by uprn;
--select count(*) from sla;
,

sla_edit as (
select
    /*+ materialize */
    distinct
    uprn,
    class,
    addressbase_postal,
    ab_postcode,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(geo_single_line_address, '[,][^,]+$'),              --split postcode (!!!)
                                                                                                                    '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                                    '&', ' AND '),                      --replace the ampersand character with the string "and"
                                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                                    ) as geo_single_line_address,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(dpa_single_line_address, '[,][^,]+$'),              --split postcode (!!!)
                                                                                                                    '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                                    '&', ' AND '),                      --replace the ampersand character with the string "and"
                                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                                    ) as dpa_single_line_address
from
    sla
--where uprn <= 1000000
)
--select * from sla_edit;
,

dpa_tokens as (
select
    /*+ materialize */
    uprn,
    dpa_single_line_address,
    trim(regexp_substr(dpa_single_line_address || ' ' || ab_postcode,'[^ ]+', 1, lines.column_value))  as  dpa_address_token,
    row_number() over (partition by UPRN order by LINES.COLUMN_VALUE)            as  dpa_row_num
from
    sla_edit,
    table(cast(multiset(select level from dual connect by instr(dpa_single_line_address || ' ' || ab_postcode, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
)
--select * from dpa_tokens;
,  
    
geo_tokens as (
select
    /*+ materialize */
    uprn,
    geo_single_line_address,
    trim(regexp_substr(geo_single_line_address || ' ' || ab_postcode, '[^ ]+', 1, lines.column_value))  as  geo_address_token,
    row_number() over (partition by UPRN order by LINES.COLUMN_VALUE)            as  geo_row_num
from
    sla_edit,
    table(cast(multiset(select level from dual connect by instr(geo_single_line_address || ' ' || ab_postcode, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
)
--select * from geo_tokens;
,

geo_concat as (
select
    /*+ materialize */
    uprn,
    geo_address_token || '*' || rank() over (partition by uprn, geo_address_token order by geo_row_num) || '*' || uprn as geo_token_count,
    geo_row_num
from
    geo_tokens
)
--select * from geo_concat;
,

dpa_concat as (
select
    /*+ materialize */
    uprn,
    dpa_address_token || '*' || rank() over (partition by uprn, dpa_address_token order by dpa_row_num) || '*' || uprn as dpa_token_count,
    dpa_row_num
from
    dpa_tokens
)
--select * from dpa_concat order by uprn, dpa_row_num;
,

dpa_geo as (
select
    /*+ materialize */
    coalesce(dt.uprn, gt.uprn)  as  uprn,
    dt.dpa_row_num,
    dt.dpa_token_count,
    gt.geo_token_count,
    gt.geo_row_num,
    lead(dt.dpa_token_count ignore nulls) over (partition by coalesce(dt.uprn, gt.uprn) order by geo_row_num) as lead_token
from
    dpa_concat  dt
full outer join
    geo_concat  gt
    on gt.geo_token_count = dt.dpa_token_count
)
--select * from dpa_geo order by uprn, dpa_row_num;
,

insert_tokens as (
select
    /*+ materialize */
    distinct
    uprn,
    lead_token as token_one,
    lead_token as token_two,
    null as geo_row_num
from
    dpa_geo
where
    dpa_token_count is null

union all

select
    uprn,
    lead_token  as  token_one,
    geo_token_count  as  token_two,
    geo_row_num
from
    dpa_geo
where
    dpa_token_count is null
)
--select * from insert_tokens order by uprn, geo_row_num;
,

final_tokens as (
select
    /*+ materialize */
    dg.uprn,
    dg.dpa_row_num,
    itk.geo_row_num as token_row,
    substr(coalesce(itk.token_two, dg.dpa_token_count), 1, instr(coalesce(itk.token_two, dg.dpa_token_count), '*') -1) as final_token
from
    dpa_geo  dg
full outer join
    insert_tokens  itk
    on itk.token_one = dg.dpa_token_count
where
    1=1
    and coalesce(itk.token_two, dg.dpa_token_count) is not null
order by
    dpa_row_num, token_row
)
--select * from final_tokens order by uprn, dpa_row_num, token_row;
,

core as (
select
    /*+ materialize */
    uprn,
    LISTAGG(final_token, ', ') within group (order by dpa_row_num, token_row) as core_single_line_address
from
    final_tokens
group by
    uprn
)
--select * from core;
,

core_edit as (
select
    /*+ materialize */
    se.uprn,
    class,
    addressbase_postal,
    ab_postcode,
    geo_single_line_address,
    dpa_single_line_address,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(core_single_line_address, '[,][^,]+$'),             --split postcode (!!!)
                                                                                                                    '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                                    '&', ' AND '),                      --replace the ampersand character with the string "and"
                                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                                    ) as core_single_line_address
from
    sla_edit  se
left join
    core
    on core.uprn  =  se.uprn
)
--select * from core_edit order by uprn;
,

uprn_distinct as (
select
    ce.*,
    dense_rank() over (partition by ab_postcode, geo_single_line_address order by uprn) as  geo_uprn_rank,
    dense_rank() over (partition by ab_postcode, dpa_single_line_address order by uprn) as  dpa_uprn_rank,
    dense_rank() over (partition by ab_postcode, core_single_line_address order by uprn) as  core_uprn_rank
from
    core_edit  ce
)
--select * from uprn_distinct order by geo_uprn_rank desc;
,

core_stack as (
select uprn, class, ab_postcode, geo_single_line_address as ab_address from core_edit where geo_single_line_address is not null and geo_uprn_rank = 1
union all

select uprn, class, ab_postcode, dpa_single_line_address as ab_address from core_edit where dpa_single_line_address is not null and dpa_uprn_rank = 1
union all

select uprn, class, ab_postcode, core_single_line_address as ab_address from core_edit where core_single_line_address is not null and core_uprn_rank = 1
)
--select * from ore_stack;

select
    distinct
    uprn,
    class,
    case when class = 'RI01' then 1 else 0 end as carehome_flag,
    ab_postcode,
    ab_address,
    dense_rank() over (order by uprn, ab_address)  uprn_id
from
    core_stack
order by
    uprn_id;


--select * from int615_ab_plus_base;
--select count(*) from int615_ab_plus_base;
--------------------------------------------------------------------------------
-- PART TWO: TOKENISE AB_PLUS DATA ---------------------------------------------


--drop table int593_ab_core_tokens;
create table int615_ab_plus_tokens compress for query high as

with

plus_tokens as (
select
    /*+ materialize */
    uprn,
    uprn_id,
    ab_postcode,
    trim(regexp_substr(ab_address,'[^ ]+', 1, lines.column_value))  as  ab_address,
    carehome_flag
from
    int615_ab_plus_base,
    table(cast(multiset(select level from dual connect by instr(ab_address, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
order by
    uprn_id
)
--select * from plus_tokens;
,

-- CALCULATE THE NUMBER OF NUMERICAL AND NON-NUMERICAL TOKNENS PER UPRN APPEARING WITHIN AB_CORE (AFTER EARLIER CLASS FILTER)

plus_count as (
select
    /*+ materialize */
    sum(case when regexp_like(pt.ab_address, '[0-9]') then 1 else 0 end)  as  ab_int_count,
    sum(case when regexp_like(pt.ab_address, '[0-9]') then 0 else 1 end)  as  ab_char_count,
    pt.uprn_id
from
    plus_tokens  pt
where
    pt.ab_address is not null
group by
    pt.uprn_id
)
--select * from plus_count;

select
    pt.*,
    pc.ab_int_count,
    pc.ab_char_count,
    pc.ab_int_count + pc.ab_char_count  as  ab_total,
    case when regexp_like(pt.ab_address, '[0-9]') then 1 else 0 end as  int_flag
from
    plus_tokens  pt
inner join
    plus_count  pc
    on pc.uprn_id  =  pt.uprn_id
where
    1=1
    and pt.ab_address is not null;


--select * from int615_ab_plus_tokens;
--select count(*) from int615_ab_plus_tokens;
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------