/*
INT615 Care Home Insight (AddressMatch)
Addressbase Plus one line address tokenise by DPA and GEO address and outer join to create full address token

Version 1.0

Created by Kayoung Goffe
Created on 28-10-2021


AMENDMENTS:
	date : details


DESCRIPTION:


DEPENDENCIES:
	KAYGO.INT_615_AB_PLUS
*/


with dpa_tokenise as (

    select
        /*+ materialize */
        UPRN,
        DPA_SINGLE_LINE_ADDRESS,
        regexp_replace(regexp_replace(trim(regexp_substr(DPA_SINGLE_LINE_ADDRESS,'[^ ]+', 1, LINES.COLUMN_VALUE)), '[,]+$',''),'^[,]+','') as DPA_ADDRESS_TOKEN,
        row_number() over (partition by UPRN order by LINES.COLUMN_VALUE) as dpa_row_num

    from
        KAYGO.INT_615_AB_PLUS,
        table(cast(multiset(select level from DUAL connect by instr(DPA_SINGLE_LINE_ADDRESS, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  LINES

        --order by UPRN,
        --LINES.COLUMN_VALUE

)

--select * from dpa_tokenise;
,

geo_tokenise as (
    select
        /*+ materialize */
        UPRN,
        GEO_SINGLE_LINE_ADDRESS,
        --trim(regexp_substr(GEO_SINGLE_LINE_ADDRESS,'[^ ]+', 1, LINES.COLUMN_VALUE))  as  GEO_SINGLE_LINE_ADDRESS_TOKEN,
        regexp_replace(regexp_replace(trim(regexp_substr(GEO_SINGLE_LINE_ADDRESS,'[^ ]+', 1, LINES.COLUMN_VALUE)), '[,]+$',''),'^[,]+','') as GEO_ADDRESS_TOKEN,
        row_number() over (partition by UPRN order by LINES.COLUMN_VALUE) as geo_row_num


    from
        KAYGO.INT_615_AB_PLUS,
        table(cast(multiset(select level from DUAL connect by instr(GEO_SINGLE_LINE_ADDRESS, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  LINES

        --order by UPRN,
        --LINES.COLUMN_VALUE
)

--select * from geo_tokenise;
,

token_union as (

select UPRN,
       DPA_SINGLE_LINE_ADDRESS as SINGLE_LINE_ADDRESS,
       DPA_ADDRESS_TOKEN as TOKENS,
       DPA_ROW_NUM as ROW_NUM
from dpa_tokenise
union
select
       UPRN,
       GEO_SINGLE_LINE_ADDRESS as SINGLE_LINE_ADDRESS,
       GEO_ADDRESS_TOKEN as TOKENS,
       GEO_ROW_NUM as ROW_NUM
from geo_tokenise
order by UPRN,
ROW_NUM
)

--select * from token_union



select DISTINCT UPRN,
ROW_NUM,
TOKENS
from token_union
where TOKENS is not null
order by UPRN
;



