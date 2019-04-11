-- Appeals, Included Students, and Summer Graduates
-- Evan Kramer
-- 8/13/2018

-- Test appeals
select student_key, completion_type from studentcohortdata where student_key in (3292772, 3292785);

-- Appeals
-- Obion County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        3292772, 
        3292785
    ) AND cd_status = 'A'
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT;

-- Maury County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        3250636
    ) AND cd_status = 'A'
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT; 

-- Lawrence County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        3286474
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT; 

-- Washington County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        4043146 
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT; 

-- Clay County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        3260233, 
        3172094
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT; 

-- Chester County 
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        3259835
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT; 

-- Jefferson County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        3219285
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT;

-- Sullivan County
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date, isp.withdrawal_reason
    FROM studentcohortdata scd
    LEFT OUTER JOIN completion_document cd ON scd.student_key = cd.student_key
    LEFT OUTER JOIN instructional_service_period isp ON scd.isp_id = isp.isp_id
    WHERE scd.student_key IN (
        4443220
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period, scd.withdrawal_reason = src.withdrawal_reason;
COMMIT;

-- Marion County
UPDATE studentcohortdata scd
SET scd.school_no = 75
WHERE scd.student_key IN (
4419362,
3179717,
3443010,
3120197,
3282900,
4661859,
3120378,
3990877,
3046942,
3120426,
3120357,
3179665,
4443434,
4433735,
3120435,
2968104,
3259420,
3046858
);

MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date
    FROM completion_document cd
    LEFT OUTER JOIN studentcohortdata scd ON scd.student_key = cd.student_key
    WHERE cd.cd_status = 'A' AND cd.student_key IN (
        3120197,
        3120378,
        3179665,
        3179717,
        3259420,
        3282900,
        3443010,
        3990877,
        4419362,
        4661859
    )
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN
UPDATE
SET scd.completion_type = src.completion_type, scd.completion_date = src.completion_date, scd.completion_period = src.completion_period;
COMMIT;

-- From appeals tracker
-- District changes
UPDATE studentcohortdata SET district_no = 750, school_no = 72 WHERE student_key = 3337462;
UPDATE studentcohortdata SET district_no = 500, school_no = 40 WHERE student_key = 3156614;
UPDATE studentcohortdata SET district_no = 271, school_no = 20 WHERE student_key = 3164342;
UPDATE studentcohortdata SET district_no = 60, school_no = 78 WHERE student_key = 3258309;
UPDATE studentcohortdata SET district_no = 620, school_no = 40 WHERE student_key = 3126895;
UPDATE studentcohortdata SET district_no = 840, school_no = 11 WHERE student_key = 3051367;
UPDATE studentcohortdata SET district_no = 570, school_no = 53 WHERE student_key = 2989568;
UPDATE studentcohortdata SET district_no = 390, school_no = 15 WHERE student_key = 3399416;
UPDATE studentcohortdata SET district_no = 792, school_no = 2180 WHERE student_key = 3329823;
UPDATE studentcohortdata SET district_no = 630, school_no = 68 WHERE student_key = 3284851;
UPDATE studentcohortdata SET district_no = 950, school_no = 30 WHERE student_key = 3250337;
UPDATE studentcohortdata SET district_no = 440, school_no = 5 WHERE student_key = 3300772;
UPDATE studentcohortdata SET district_no = 210, school_no = 25 WHERE student_key = 3111368;
UPDATE studentcohortdata SET district_no = 190, school_no = 358 WHERE student_key = 3314604;
UPDATE studentcohortdata SET district_no = 792, school_no = 2660 WHERE student_key = 3557074;
UPDATE studentcohortdata SET district_no = 985, school_no = 35 WHERE student_key = 3084711;
UPDATE studentcohortdata SET district_no = 792, school_no = 8125 WHERE student_key = 2966412;
UPDATE studentcohortdata SET district_no = 330, school_no = 137 WHERE student_key = 3074731;
UPDATE studentcohortdata SET district_no = 970, school_no = 140 WHERE student_key = 3154285;
UPDATE studentcohortdata SET district_no = 950, school_no = 73 WHERE student_key = 3256467;
UPDATE studentcohortdata SET district_no = 160, school_no = 5 WHERE student_key = 3931805;
COMMIT;

-- Exclude from cohort
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3226488;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3219886;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3231975;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4090886;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 2988309;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3165931;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3127050;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3141694;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3141696;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3141759;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3141826;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3245347;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3245404;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3245416;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3294221;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4028574;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4334865;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3141752;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3141718;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4315485;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3230929;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3263459;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3245916;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3247437;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4127567;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3855661;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3036738;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3111020;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3111488;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3111546;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3112053;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3269702;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3451906;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3111513;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3238988;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3087976;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4299810;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3233552;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3236016;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3234652;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3137978;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3238652;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4567761;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 4559361;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3215776;
UPDATE studentcohortdata SET included_in_cohort = 'N' WHERE student_key = 3156614;
COMMIT; 

-- Summer graduates
MERGE INTO studentcohortdata scd
USING (
    SELECT scd.student_key, cd.completion_type, cd.completion_period, cd.completion_date
    FROM studentcohortdata scd, completion_document cd
    WHERE scd.isp_id = cd.isp_id
    AND cd.cd_status = 'A' AND scd.cohortyear = 2014
    AND TO_DATE(cd.completion_date, 'DD-MON-YY') >= TO_DATE('08-JUN-18', 'DD-MON-YY') 
) src
ON (scd.student_key = src.student_key)
WHEN MATCHED THEN 
UPDATE 
SET scd.completion_type = src.completion_type, scd.completion_period = src.completion_period, scd.completion_date = src.completion_date;
COMMIT;

-- Finalize included_in_cohort variable 
-- N/Y
UPDATE studentcohortdata scd
SET scd.included_in_cohort = (
    SELECT doc.revised_included_in_cohort
    FROM studentcohortdata scd, studentcohortdocs doc
    WHERE scd.student_key = doc.student_key
    AND scd.included_in_cohort = 'N' AND doc.revised_included_in_cohort = 'Y'
)
WHERE scd.student_key IN (
    SELECT DISTINCT scd.student_key
    FROM studentcohortdata scd, studentcohortdocs doc
    WHERE scd.student_key = doc.student_key
    AND scd.included_in_cohort = 'N' AND doc.revised_included_in_cohort = 'Y'
);
COMMIT;

-- P/N
UPDATE studentcohortdata scd
SET scd.included_in_cohort = 'N'
WHERE scd.student_key IN (
    SELECT scd.student_key
    FROM studentcohortdata scd, studentcohortdocs doc
    WHERE scd.student_key = doc.student_key
    AND scd.cohortyear = 2014
    AND scd.included_in_cohort = 'P' AND doc.revised_included_in_cohort = 'N'
);
COMMIT;

-- P/Y or NULL
UPDATE studentcohortdata scd
SET scd.included_in_cohort = 'Y'
WHERE scd.student_key IN (
    SELECT scd.student_key
    FROM studentcohortdata scd
    LEFT OUTER JOIN studentcohortdocs doc ON doc.student_key = scd.student_key
    WHERE scd.cohortyear = 2014
    AND scd.included_in_cohort = 'P' AND 
        (doc.revised_included_in_cohort = 'Y' OR doc.revised_included_in_cohort IS NULL)
);
COMMIT;

-- Y/N
UPDATE studentcohortdata scd
SET scd.included_in_cohort = 'N'
WHERE scd.student_key IN (
    SELECT scd.student_key
    FROM studentcohortdata scd
    LEFT OUTER JOIN studentcohortdocs doc ON doc.student_key = scd.student_key
    WHERE scd.cohortyear = 2014
    AND scd.included_in_cohort = 'Y' AND doc.revised_included_in_cohort = 'N'
);
COMMIT;