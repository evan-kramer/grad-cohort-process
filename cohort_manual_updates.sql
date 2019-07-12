--Cohort Manual Updates (Weekly)
--Evan Kramer
--7/12/2019

-- Manual updates
-- Verify changes
select distinct student_key 
from student_concentrators
union
select student_key
from sde_dir.person@sde_dir_link.world per,
    voc_ed.ve_concentrator@sde_dir_link.world vc
where sde_dir.per.person_id = voc_ed.vc.person_id;   

select * 
from voc_ed.ve_concentrator@sde_dir_link.world;

-- CTE manual update
UPDATE studentcohortdata
SET cte = 'Y'
WHERE student_key in (
    SELECT DISTINCT student_key 
    FROM student_concentrators
    UNION
    SELECT sde_dir.per.student_key
        from sde_dir.person@sde_dir_link.world per,
             voc_ed.ve_concentrator@sde_dir_link.world vc
       where sde_dir.per.person_id = voc_ed.vc.person_id) and 
    cohortyear = extract(year from sysdate) - 4;
COMMIT;
UPDATE studentcohortdata
SET cte = 'N'
WHERE cte IS NULL;
COMMIT;