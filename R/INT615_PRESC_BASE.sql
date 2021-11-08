--------------------------------------------------------------------------------
-- PART ONE: PRESC_BASE_TABLE --------------------------------------------------
-- NOTE: RUNS IN ~30 MINUTES ---------------------------------------------------

--drop table int615_presc_base
create table int615_presc_base compress for query high as 

with

ab_postcodes as (
select
    /*+ materialize */
    distinct
    ab_postcode
from
    int615_ab_plus_base
)
--select count(*) from ab_postcodes;
,

pds_data as (
select
    year_month,
    pf_id,
    nhs_no_pds  as  nhs_no,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(address, '[,][^,]+$'),              --split postcode (!!!)
                                                                                                            '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                                            '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                            '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                            '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                            '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                            ' - ','-')                          --remove any spaces around a hyphen
                                                                                                            ) as pat_address,
    regexp_replace(upper(postcode),'[^A-Z0-9]','')  as pat_postcode
from
    DALL_REF.int615_paper_pfid_address_20_21
where
    1=1
    and year_month in (202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104)
    and regexp_replace(upper(postcode), '[^A-Z0-9]', '') in (select ab_postcode from ab_postcodes)
)
--select * from pds_data;
,

etp_edit as (
select
    epmd.epm_id,
    epmd.part_date,
    upper(trim(epmd.pat_address_line1)) || ' ' || upper(trim(epmd.pat_address_line2)) || ' ' || upper(trim(epmd.pat_address_line3)) || ' ' || upper(trim(epmd.pat_address_line4))   as  pat_address,
    regexp_replace(upper(pat_address_postcode),'[^A-Z0-9]','')  as  pat_postcode
from
    SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb  epmd
where
    1=1
    and floor((to_date(part_date, 'YYYYMMDD') - patient_dob)/365) >= 64
    and substr(epmd.part_date, 1, 6) in (202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104)
    and regexp_replace(upper(pat_address_postcode),'[^A-Z0-9]','') in (select ab_postcode from ab_postcodes)
)
--select * from etp_edit;
,

pds_fact as (
select
    pt.year_month,
    pt.pf_id,
    pt.nhs_no,
    pt.pat_address,
    pt.pat_postcode
from
    pds_data  pt
inner join
    SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT  fact  
    on fact.pf_id  =  pt.pf_id
where
    1=1
    and fact.year_month in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and fact.patient_identified = 'Y'
	and fact.pay_da_end   = 'N' -- excludes disallowed items
	and fact.pay_nd_end   = 'N' -- excludes not dispensed items
	and fact.pay_rb_end   = 'N' -- excludes referred back items
	and fact.cd_req       = 'N' -- excludes controlled drug requisitions 
	and fact.oohc_ind     = 0   -- excludes out of hours dispensing
	and fact.private_ind  = 0   -- excludes private dispensers
	and fact.ignore_flag  = 'N' -- excludes LDP dummy forms
    and fact.eps_flag     = 'N'
    and fact.calc_age     >= 65
)
--select * from pds_fact;
,

etp_fact as (
select
    fact.year_month,
    fact.pf_id,
    fact.nhs_no,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(ee.pat_address, '[,][^,]+$'),              --split postcode (!!!)
                                                                                                                '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                                                '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                                '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                                '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                                '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                                ' - ','-')                          --remove any spaces around a hyphen
                                                                                                                ) as pat_address,
    ee.pat_postcode
from
    etp_edit  ee
inner join
    SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT  fact   
    on fact.epm_id  =  ee.epm_id
    and fact.eps_part_date  =  ee.part_date
where
    1=1
    and fact.year_month in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and fact.patient_identified = 'Y'
	and fact.pay_da_end   = 'N' -- excludes disallowed items
	and fact.pay_nd_end   = 'N' -- excludes not dispensed items
	and fact.pay_rb_end   = 'N' -- excludes referred back items
	and fact.cd_req       = 'N' -- excludes controlled drug requisitions 
	and fact.oohc_ind     = 0   -- excludes out of hours dispensing
	and fact.private_ind  = 0   -- excludes private dispensers
	and fact.ignore_flag  = 'N' -- excludes LDP dummy forms
    and fact.eps_flag = 'Y'
    and fact.calc_age >= 65
)
--select * from etp_fact;
,

pds_etp as (
select
    distinct
    year_month,
    pf_id,
    nhs_no,
    pat_address,
    pat_postcode
from
    etp_fact

union all

select
    distinct
    year_month,
    pf_id,
    nhs_no,
    pat_address,
    pat_postcode
from
    pds_fact
)

select
    year_month,
    nhs_no,
    pf_id,
    pat_postcode,
    pat_address,
    dense_rank() over(order by pat_address, pat_postcode)  as  address_record_id
from
    pds_etp;


--select * from int615_presc_base;
--select count(*) from int615_presc_base;
--------------------------------------------------------------------------------
-- PART TWO: PRESC TOKENS -----------------------------------------------------


--drop table int615_presc_records;
create table int615_presc_tokens compress for query high as 

with

distinct_records as (
select
    /*+ materialize */
    distinct
    address_record_id,
    pat_postcode,
    pat_address
from
    int615_presc_base
)
--select * distinct_records;
,

-- TOKENISE

pat_tokens as (
select
    /*+ materialize */
    dr.address_record_id,
    dr.pat_postcode,
    trim(regexp_substr(dr.pat_address,'[^ ]+', 1, lines.column_value))  as  pat_address
from
    distinct_records  dr,
    table(cast(multiset
    (select level from dual connect by instr(dr.pat_address, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
    order by dr.address_record_id
)
--select * from etp_tokens;
,

-- CALCULATE THE NUMBER OF NUMERICAL AND NON-NUMERICAL TOKNENS PER UPRN APPEARING WITHIN AB_CORE (AFTER EARLIER CLASS FILTER)

pat_count as (
select
    /*+ materialize */
    sum(case when regexp_like(pat_address, '[0-9]') then 1 else 0 end)  as  pat_int_count,
    sum(case when regexp_like(pat_address, '[0-9]') then 0 else 1 end)  as  pat_char_count,
    address_record_id
from
    pat_tokens
where
    pat_address is not null
group by
    address_record_id
)

select
    pt.*,
    pc.pat_int_count,
    pc.pat_char_count,
    pc.pat_int_count + pc.pat_char_count  as  pat_total,
    case when regexp_like(pt.pat_address, '[0-9]') then 1 else 0 end as  int_flag
from
    pat_tokens  pt
inner join
    pat_count  pc
    on pc.address_record_id  =  pt.address_record_id
where
    1=1
    and pt.pat_address is not null;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------