-- Checks for <60 day appeals
-- Evan Kramer

select *
from instructional_service_period
inner join scal_id_days 
using (school_bu_id, school_year, instructional_program_num)
where student_key in (
    select student_key
    from studentcohortdata
    where cohortyear = extract(year from sysdate) - 4
); 
