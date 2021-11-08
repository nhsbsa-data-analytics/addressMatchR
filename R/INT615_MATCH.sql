--------------------------------------------------------------------------------
-- PART ONE: RESULTS BASE TABLE --------------------------------------------------


--drop table int615_match;
create table int615_match compress for query high as

with

results_base as (
select
    distinct
    pa.address_record_id,
    pa.pat_postcode,
    pa.pat_address,
    tk.pat_int_count,
    tk.pat_char_count,
    tk.pat_total
from
    int615_presc_base  pa
left join
    (select distinct address_record_id, pat_int_count, pat_char_count, pat_total from int615_presc_tokens)  tk
    on tk.address_record_id  =  pa.address_record_id
)
--select count(*) from results_base;
--select * from results_base;
,

--------------------------------------------------------------------------------
-- PART 1.1: EXACT MATCHES -----------------------------------------------------

exact_matches as (
select
    -- AB INFO --------------------
    abc.uprn,
    abc.uprn_id,
    abc.class,
    abc.carehome_flag,
    abc.ab_address,
    -- ETP INFO -------------------
    rb.address_record_id,
    -- MATCH INFO -----------------
    (rb.pat_int_count * 4) + (rb.pat_char_count)  as  jw_score,
    (rb.pat_int_count * 4) + (rb.pat_char_count)  as  total_score,
    1                                             as  match_score,
    'EXACT'                                       as  match_type
from
    results_base  rb
inner join
    int615_ab_plus_base  abc
    on abc.ab_address  =  rb.pat_address
)
--select count(*) from exact_matches;
,

--------------------------------------------------------------------------------
-- PART 1.2: JW MATCHES --------------------------------------------------------

pat_tokens as (
select
    /*+ materialize */
    distinct
    address_record_id,
    pat_postcode,
    pat_address,
    int_flag,
    pat_int_count,
    pat_char_count,
    pat_total
from
    int615_presc_tokens
where
    address_record_id not in (select address_record_id from exact_matches)
)                             
--select * from pat_tokens;      
,

plus_tokens as (
select
    /*+ materialize */
    uprn,
    uprn_id,
    ab_postcode,
    ab_address,
    int_flag
from
    int615_ab_plus_tokens
where
    ab_postcode in (select distinct pat_postcode from pat_tokens)
)
--select * from plus_tokens;                                 
,

cross_join_exact as (
select
    /*+ materialize */
    ab.uprn,
    ab.uprn_id,
    pat.address_record_id,
    pat.pat_postcode,
    ab.ab_address,
    pat.pat_address,
    pat.int_flag,
    case when pat.int_flag = 1 then 4 else 1 end    as  jw    
from
    pat_tokens  pat
inner join
    plus_tokens  ab      
    on  pat.pat_postcode    =   ab.ab_postcode
    and pat.pat_address     =   ab.ab_address
    and pat.int_flag        =   ab.int_flag
)                                        
--select * from cross_join_exact;
,

cross_join_diff as (
select 
    /*+ materialize */   
    ab.uprn,
    ab.uprn_id,
    pat.address_record_id,
    pat.pat_postcode,
    ab.ab_address,
    pat.pat_address,
    pat.int_flag
from
    pat_tokens  pat
inner join
    plus_tokens  ab      
    on  pat.pat_postcode     =   ab.ab_postcode
    and pat.pat_address     !=   ab.ab_address
    and pat.int_flag         =   ab.int_flag    
where
    1=1
    and pat.int_flag = 0
    and ab.int_flag = 0
    and     (   substr(ab.ab_address, 1, 1) = substr(pat.pat_address, 1, 1)
            or  substr(ab.ab_address, 2, 1) = substr(pat.pat_address, 2, 1)
            or  substr(ab.ab_address, length(ab.ab_address), 1)  =  substr(pat.pat_address, length(pat.pat_address), 1)
            or  instr(ab.ab_address, pat.pat_address) > 1
            or  instr(pat.pat_address, ab.ab_address) > 1
            )
)                             
--select * from cross_join_diff;
,

jw_union as (
select
    /*+ materialize */
    uprn,
    uprn_id,
    address_record_id,
    pat_postcode,
    ab_address,
    pat_address,
    int_flag,
    utl_match.jaro_winkler(pat_address, ab_address)  as  jw
from
    cross_join_diff 
where
    utl_match.jaro_winkler(pat_address, ab_address) >= 0.8
    
union all
select * from cross_join_exact
)
--select * from jw_union;                                   
,

uprn_per_id as (
select
    /*+ materialize */
    uprn,
    count(distinct uprn_id)  as  uprn_count
from
    jw_union
group by
    uprn
)
--select * from uprn_per_id;
,

-- NOTE: NHS_TOKENS WITHIN GROUP_BY, PLUS_TOKEN NOT WITHIN GROUP_BY
-- THIS CALCULATES THE MAXIMUM AB-TOKEN SCORE, AGAINST EACH PAT-TOKEN, FOR EACH UPRN & UPRN_ID
-- NOTE: DON'T NEED POSTCODE AS BOTH UPRN AND RECORD_ID SHARE POSTCODE ALREADY

agg_one as (
select
    /*+ materialize */
    upi.uprn_count,
    upi.uprn,
    uprn_id,
    address_record_id,
    pat_address,
    max(jw)  as  max_val
from
    jw_union  jwu
left join
    uprn_per_id  upi
    on upi.uprn  =  jwu.uprn
group by
    upi.uprn_count,
    upi.uprn,
    uprn_id,
    address_record_id,
    pat_address
)
--select * from agg_one;
,

-- FOR EACH UPRN, THIS SUMS UP ALL OF THE ABOVE MAXIMUM VALUES
-- THIS MEANS THAT EACH UPRN HAS A SUMMED TOTAL FOR EACH PAT-ADDRESS THAT IT SHARES A POSTCODE WITH
-- NOTE: REMOVE TOKENS (IE 'ETP_ADDRESS' AS THESE ARE WHAT ARE BEING SUMMED)

agg_two as (
select
    /*+ materialize */
    ao.uprn_count,
    ao.uprn,
    ao.uprn_id,
    ao.address_record_id,    
    sum(ao.max_val)                                                                               as  jw_score,
    ((et.pat_int_count * 4) + et.pat_char_count)                                                  as  total_score,
    round(sum(ao.max_val) / ((et.pat_int_count * 4) + et.pat_char_count), 4)                      as  match_score,
    rank() over (partition by ao.address_record_id order by sum(ao.max_val) desc)                 as  score_rank,
    row_number() over (partition by ao.address_record_id, sum(ao.max_val) order by ao.uprn_id desc)  as  desc_top_rank,
    row_number() over (partition by ao.address_record_id, sum(ao.max_val) order by ao.uprn_id asc)   as  asc_top_rank
from
    agg_one  ao
inner join
    (select distinct address_record_id, pat_postcode, pat_char_count, pat_int_count, pat_total from pat_tokens)  et
    on et.address_record_id  =  ao.address_record_id
group by
    ao.uprn_count,
    ao.uprn,
    ao.uprn_id,
    ao.address_record_id,
    ((et.pat_int_count * 4) + et.pat_char_count)
)
--select * from agg_two where score_rank = 1;
,

-- SCORE_RANK. DESC_TOP_RANK AND ASC_TOP_RANK ENABLE TOP_RANKING MATCH TO BE FILTERED

jw_matches as (
select
    /*+ materialize */
    -- AB_CORE INFO ---------------
    agt.uprn,
    agt.uprn_id,
    abc.class,
    abc.carehome_flag,
    abc.ab_address,
    -- ETP INFO -------------------
    agt.address_record_id,
    -- SCORE INFO -----------------
    agt.jw_score,
    agt.total_score,
    agt.match_score,
    'JW'  as  match_type
from
    agg_two  agt
inner join
    -- MUST JOIN ON UPRN_ID AND NOT UPRN 
    int615_ab_plus_base  abc
    on abc.uprn_id  = agt.uprn_id
where
    1=1
    and score_rank = 1
    and desc_top_rank = 1
    -- THIS RESOLVES UPRN_ID DRAWS WHERE EQUAAL SCORES ARE SAME UNDERLYING UPRN
    and asc_top_rank <= uprn_count
)
--select * from jw_matches;
,

no_matches as (
select
    /*+ materialize */
    -- AB_CORE INFO ---------------
    null  as  uprn,
    null  as  uprn_id,
    null  as  class,
    0     as  carehome_flag,
    null  as  ab_address,
    -- ETP INFO -------------------
    rb.address_record_id,
    -- SCORE INFO -----------------
    0  as  jw_score,
    0  as  total_score,
    0  as  match_score,
    'NONE'  as  match_type
from
    results_base  rb
where
    1=1
    and address_record_id not in (select address_record_id from jw_matches)
    and address_record_id not in (select address_record_id from exact_matches)
)
--select count(*) from exact_matches;               -- 47,202
--select count(*) from jw_matches;                  --392,451
--select count(*) from no_matches;                  -- 39,172

select
    rb.*,
    au.uprn,
    au.uprn_id,
    au.class,
    au.carehome_flag,
    au.ab_address,
    au.jw_score,
    au.total_score,
    au.match_score,
    au.match_type
from
    results_base  rb
inner join
        (
        select * from no_matches
        union all
        select * from jw_matches
        union all
        select * from exact_matches
        )  au
        on au.address_record_id  =  rb.address_record_id;


--select count(*) from int615_match where match_type = 'EXACT';
--select count(*) from int615_match where match_type = 'JW';
--select count(*) from int615_match where match_type = 'NONE';
--select sum(pat_total) from int615_match where carehome_flag = 1;
--select sum(pat_total) from int615_match where carehome_flag = 0;
--------------------------------------------------------------------------------
-- PART TWO: KEY-WORD MATCHES FROM NON-CH POSTCODE PAT RECORDS  ----------------
-- NOTE: TAKES ~50 MINS --------------------------------------------------------


--drop table int615_NON_POSTCODE_KW;
create table int615_NON_POSTCODE_KW compress for query high as

with

pds as (
select
    pds.pf_id,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(pds.ADDRESS, '[,.();:#'']', ' '),                  --replace special characters with a single space
                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                    '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                    ) as pat_address
from
    DALL_REF.int615_paper_pfid_address_20_21  pds
inner join
    SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT  fact 
    on fact.pf_id  =  pds.pf_id
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
    and fact.eps_flag = 'N'
    and fact.calc_age >= 65
    and upper(replace(pds.postcode, ' ', '')) not in (select distinct ab_postcode from int615_ab_plus_base)
)
--select count(*) from pds;
,

etp as (
select
    fact.pf_id,
    upper(etp.PAT_ADDRESS_LINE1||', '||etp.PAT_ADDRESS_LINE2||', '||etp.PAT_ADDRESS_LINE3||', '||etp.PAT_ADDRESS_LINE4) as PAT_ADDRESS
from
    SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT  fact 
inner join
    SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb  etp
    on etp.part_date  =  fact.eps_part_date
    and etp.epm_id    =  fact.epm_id
where
    1=1
    and substr(etp.part_date, 1, 6) in (202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104)
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
    and floor((to_date(etp.part_date, 'YYYYMMDD') - etp.patient_dob)/365) >= 64
    and upper(replace(fact.patient_addr_postcode, ' ', '')) not in (select distinct ab_postcode from int615_ab_plus_base)
)
--select count(*) from etp_pcd;
,

pds_etp as (
select
    pf_id,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(PAT_ADDRESS, '[,.();:#'']', ' '),                  --replace special characters with a single space
                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                    '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                    ) as PAT_ADDRESS
from
    etp

union all

select pf_id, pat_address from pds
)
--select count(*) from pds_etp;
,

address_records as (
select
    /*+ materialize */
    pf_id,
    pat_address,
    dense_rank() over (order by pat_address)   as  address_record_id
from
    pds_etp
)
--select count(*) from pds_etp;
,

address_records_rank as (
select
    pf_id,
    pat_address,
    address_record_id,
    row_number() over (partition by address_record_id order by pf_id)  as  address_rank
from
    address_records
order by
    address_record_id
)
--select * from address_records_rank;
,

results as (
select
    address_record_id
from
    address_records_rank
where
    1=1
    and address_rank = 1
    and regexp_instr(pat_address, 'NURSING|RESIDENTIAL HOME|RESPITE|ELDERLY|CONVALESCENT|REST HOME|CARE HOME') != 0
    and regexp_instr(pat_address, 'CHILD|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE') = 0
)

select
    ar.pf_id,
    ar.pat_address,
    1  as  carehome_flag
from
    address_records  ar
where
    ar.address_record_id in (select address_record_id from results);


--select * from int615_non_postcode_kw;
--select count(distinct pf_id) from int615_non_postcode_kw;
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------