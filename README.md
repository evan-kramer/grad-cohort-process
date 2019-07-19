# Graduation Rate Process
Last updated: July 19, 2019

## Contents
- [Purpose](#purpose)
- [Background](#background)
- [Phases](#phases)
- [Code](#code)

## Purpose
This document outlines the internal actions required to calculate graduation rates. The following laws and policies govern the calculation of graduation rates:
- Federal law
	- [Title VIII Section 8101(25)(A) et seq.](https://legcounsel.house.gov/Comps/Elementary%20And%20Secondary%20Education%20Act%20Of%201965.pdf)
- Federal guidance
	- [Graduation rate guidance](https://www2.ed.gov/policy/elsec/leg/essa/essagradrateguidance.pdf) 
- State law
	- [TCA §49-1-6001](https://advance.lexis.com/documentpage/?pdmfid=1000516&crid=328d7d28-8611-47ab-920f-29c28e8046b7&nodeid=ABXAAGABSAAB&nodepath=%2fROOT%2fABX%2fABXAAG%2fABXAAGABS%2fABXAAGABSAAB&title=49-6-6001.+Graduation+requirements.&config=025054JABlOTJjNmIyNi0wYjI0LTRjZGEtYWE5ZC0zNGFhOWNhMjFlNDgKAFBvZENhdGFsb2cDFQ14bX2GfyBTaI9WcPX5&pddocfullpath=%2fshared%2fdocument%2fstatutes-legislation%2furn%3acontentItem%3a4WM8-8GN0-R03M-R25V-00008-00&ecomp=kgc_kkk&prid=7ab08748-1c91-4e6c-b71b-5a9c67da2464)
	- [TCA §49-1-601](https://advance.lexis.com/documentpage/?pdmfid=1000516&crid=a720e344-eb11-4014-be9c-245c1223ee7e&nodeid=ABXAABAAGAAB&nodepath=%2FROOT%2FABX%2FABXAAB%2FABXAABAAG%2FABXAABAAGAAB&level=4&haschildren=&populated=false&title=49-1-601.+Assignment+of+student+for+purpose+of+calculating+graduation+rate.&config=025054JABlOTJjNmIyNi0wYjI0LTRjZGEtYWE5ZC0zNGFhOWNhMjFlNDgKAFBvZENhdGFsb2cDFQ14bX2GfyBTaI9WcPX5&pddocfullpath=%2Fshared%2Fdocument%2Fstatutes-legislation%2Furn%3AcontentItem%3A4X55-GP80-R03K-M3TX-00008-00&ecomp=k3v8kkk&prid=1edc03cc-d4e5-48fe-8ea7-06ac45a3e9f6)
- State policy
	- [High School Policy 2.103](https://www.tn.gov/content/dam/tn/stateboardofeducation/documents/2.103_High_School_Policy_10-20-17.pdf)

Graduation rates are or may be used in: 
- Accountability
- [Federal](https://www2.ed.gov/about/inits/ed/edfacts/sy-18-19-nonxml.html) and [state](https://www.tn.gov/education/data/department-reports.html) reporting
- [Teacher evaluation](https://team-tn.org/data/achievement-measures/)
- [Research](https://www.tn.gov/education/data/research-and-policy-briefs.html) 

## Background
Districts submit and interact with their graduation cohort data in two ways:
1. Districts enter graduation-related data (e.g., completion types, withdrawal codes, etc. into their __Student Information Systems__ (SIS).
2. These data feed into the tables in EIS that are the backend for the __Cohort application__, where districts submit documentation to request certain students exclusion from the cohort.

Please note that the department must make manual updates to EIS data tables in rare cases. Please consult [Appendix B](#appendix B) for more details on these edge cases.

## Phases 
The annual graduation cohort process occurs in four phases.
- Phase I
	- At the start of Phase I
		- Conduct [initial data quality checks](https://github.com/evan-kramer/grad-cohort-process/blob/master/initial_cohort_data_check.R).
	- Weekly
		- [Pull data and run status updates](https://github.com/evan-kramer/grad-cohort-process/blob/master/graduation_cohort_weekly_update.Rmd).
	- Monthly
		- Run [change request process](https://github.com/evan-kramer/grad-cohort-process/blob/master/graduation_cohort_change_requests.R) (increase to weekly when closer to deadline).
			- The `tracker` switch compiles all district submissions into a single file when set to `T`.
			- The `output` switch saves the output to the server.
			- The `changes` switch actually executes the approved changes in the EIS tables.
	- One week before Phase I deadline
		- Compile data for [cohort reminder emails](https://github.com/evan-kramer/grad-cohort-process/blob/master/graduation_cohort_emails.R) and send using VBA macro.
- Phase II
	- Before the start of Phase II
		- [Update CTE data](https://github.com/evan-kramer/grad-cohort-process/blob/master/cohort_manual_updates.sql#L18).
		- [Finalize the values of the `included_in_cohort` field](https://github.com/evan-kramer/grad-cohort-process/blob/master/appeals_included_summer_graduates.sql#L20).
	- Daily
		- Include [summer graduates](https://github.com/evan-kramer/grad-cohort-process/blob/master/appeals_included_summer_graduates.sql#L5).
- Phase III 
	- Between Phase II and Phase III
		- Create appeals tracker and [compile appeals](https://github.com/evan-kramer/grad-cohort-process/blob/master/graduation_cohort_appeals.R).
		- [Complete appeals](https://github.com/evan-kramer/grad-cohort-process/blob/master/appeals_included_summer_graduates.sql#L77) by updating data in EIS tables.
			- __Note__: This could be added to the code to [compile the appeals tracker](https://github.com/evan-kramer/grad-cohort-process/blob/master/graduation_cohort_appeals.R).

## Code
Below is a list of other scripts in this repository and their intended purpose.
- [`docs_audit`](https://github.com/evan-kramer/grad-cohort-process/blob/master/docs_audit.R) attempts to extract doc BLOBs (i.e., PDF document uploads) stored in EIS tables.
- [`less_than_60_days`](https://github.com/evan-kramer/grad-cohort-process/blob/master/less_than_60_days.R) tries to predict which students will be eligible for appeal because they were enrolled for less than 60 days in the most recent school year.
- [`students_in_wrong_cohort`](https://github.com/evan-kramer/grad-cohort-process/blob/master/students_in_wrong_cohort.R) tries to predict which students are in the wrong cohort (i.e., should actually graduate next year or have already graduated).