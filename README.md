# stats_20_final
Link to database to edit (has been privately shared with you): https://docs.google.com/spreadsheets/d/1i7SVE19UeWHvE241scrbnHkf0KlPYcEAe6FjaxyIcjs/edit?usp=sharing

Key points for editing the sheet:
- big_total is the final page that we can download the CSV for to upload to the project. avoid editing solely directly on it, make edits on both the main and the major sheet. Once you add the courses for a major onto its' special page, I've been pasting that into the bottom of big_total (if there's a better google sheets way to do this, please help)
- The pages after big_total are the ways that UCLA has divided their physical sciences. Reference this: https://physicalsciences.ucla.edu/academic-units/
- TAB: the name of the larger department. Should be IDENTICAL for every major within a sheet. These are identical to the list created in server() (dept_majors <- list(...))
- NAME: the name of the COURSE that is required for a major (which is in the last column). Put the name of the subject beforehand (shortened or not) because lots of departments share values
- DEPARTMENT: department of the COURSE within that row.
- DIVISION: what UCLA college division (Life Sciences, Physical Sciences, Social Sciences, or Humanities) the course is from
- VALUE: whether the class is lower division or upper division
- UNITS: unit value of the course
- MAJOR: what major this course requirement corresponds to (I haven't accounted for the situation where one class fits several majors, if anyone wants to figure that out go for it)

  Key points for the actual repo:
  - functioning_notab -> my main working version of this project where the whole thing is on a single tab, and departments are sorted with a dropdown menu. I encorporated a second tab if any of you have any ideas of what to do with it but I don't. 
  - functioning_solotab -> a version of this project where the departments should switch depending on tab. One tab works, but I haven't figured out how to make the ID stuff work across tabs. Lowkey a dumped version of this
    
