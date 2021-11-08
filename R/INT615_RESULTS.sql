--------------------------------------------------------------------------------
-- PART ONE: FINAL BASE TABLE --------------------------------------------------
-- NOTE: TAKES ~10 MINS --------------------------------------------------------


--drop table int615_results;
create table int615_results compress for query high as

-- NOTE: CHILDREN RATHER THAN JUST 'CHILD' USED DUE TO CHILDWELL PLACE NAME ----
-- NOTE: REGEX_INSTR MUST BE >= 1 (DUE TO MULTIPLE OCCURENCES) -----------------

with

address_pf as (
select
    pb.pf_id,
    1  as  ch_flag,
    mt.uprn,
    mt.match_type
from
    int615_presc_base  pb
inner join
    int615_match  mt
    on mt.address_record_id  =  pb.address_record_id
where
    1=1
    and carehome_flag = 1
    and regexp_instr(pb.pat_address, 'CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE') = 0
)
--select count(*) from address_match;
,

pat_match as (
select
    pb.year_month,
    pb.pat_address,
    count(distinct pb.nhs_no)  as  pat_count
from
    int615_presc_base  pb
inner join
    int615_match  mt
    on mt.address_record_id  =  pb.address_record_id
where
    1=1
    and pf_id not in (select pf_id from address_pf)
    and regexp_instr(pb.pat_address, 'CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE') = 0
group by
    pb.year_month,
    pb.pat_address
having
    count(distinct pb.nhs_no) >= 5
)
--select count(*) from pat_match;
,

pat_pf as (
select
    pb.pf_id,
    1  as  ch_flag,
    null  as  uprn,
    'PATIENT_COUNT'  as  match_type
from
    int615_presc_base  pb
inner join
    pat_match  pm
    on pm.year_month  =  pb.year_month
    and pm.pat_address  =  pb.pat_address
)
--select count(*) from pat_pf;
,

word_pf as (
select
    pf_id,
    1  as  ch_flag,
    null  as  uprn,
    'KEY_WORD'  as  match_type
from
    int615_presc_base
where
    1=1
    and pf_id not in (select pf_id from address_pf)
    and pf_id not in (select pf_id from pat_pf)
    and regexp_instr(pat_address, 'NURSING|RESIDENTIAL HOME|RESPITE|ELDERLY|CONVALESCENT|REST HOME|CARE HOME') != 0
    and regexp_instr(pat_address, 'CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE') = 0
)
--select count(*) from word_pf;
,

non_pcd_word_pf as (
select
    pf_id,
    1  as  ch_flag,
    null  as  uprn,
    'NON_PCD_KEY_WORD'  as  match_type
from
    int615_non_postcode_kw
)
--select count(*) from non_pcd_word_pf;
,

all_pf as (
select * from address_pf
union all
select * from pat_pf
union all
select * from word_pf
union all
select * from non_pcd_word_pf
)
--select count(*) from all_pf;
,

ft as (
select
    year_month,
    eps_part_date,
    epm_id,
    pf_id,
    presc_type_prnt,
    presc_id_prnt,
    item_count,
    item_pay_dr_nic,
    item_calc_pay_qty,
    pds_gender,
    calc_age,
    nhs_no,
    patient_addr_postcode,
    calc_prec_drug_record_id,
    eps_flag
from
    SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT  fact  
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
    and fact.calc_age >= 65
)

select
    ft.*,
    pf.uprn,
    nvl(pf.ch_flag, 0)  as  ch_flag,
    pf.match_type
from
    ft
left join
    all_pf  pf
    on pf.pf_id  =  ft.pf_id;


--------------------------------------------------------------------------------
-- PART TWO: MANUAL VALIDATION DATA --------------------------------------------
-- NOTE: TAKES ~5 MINS


--drop table int615_results_validation;
create table int615_results_validation compress for query high as

with

pb as (
select
    pb.pat_postcode,
    pb.pat_address,
    pb.address_record_id,
    pf.match_type,
    count(distinct pb.pf_id)  as  form_count
from
    int615_presc_base  pb
left join
    int615_results  pf
    on pf.pf_id  =  pb.pf_id
where
    pf.ch_flag = 1
group by
    pb.pat_postcode,
    pb.pat_address,
    pb.address_record_id,
    pf.match_type
)
--select count(*) from pb;

select
    pb.pat_address,
    mt.ab_address,
    pb.pat_postcode,
    pb.address_record_id,
    pb.match_type,
    pb.form_count
from
    pb
left join
    (select distinct address_record_id, ab_address from int615_match)  mt
    on mt.address_record_id  =  pb.address_record_id;


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------