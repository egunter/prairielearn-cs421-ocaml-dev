#!/bin/bash

testsdir="/grade/tests"
shareddir="/grade/shared/ocaml-grader"
handindir="/grade/student"
graderdir="/grade/run"
resultsdir="/grade/results"
student_results="/grade/run/student_results.txt"
result_json_file="/grade/results/results.json"

mkdir $graderdir $resultsdir
cp -pr $shareddir/* $testsdir/* $handindir/* $graderdir

cd $graderdir
make dist-clean

touch student.output
echo "grader failed to build." > ${student_results}
echo "Total: [0 / 1]" >> ${student_results}
touch conflicts.txt
make 2> conflicts.txt

if [ -e "/grade/run/grader" ]
then ./grader > ${student_results}
fi

if [ -e "/grade/run/student.mll" ]
then stufile="/grade/run/student.mll"
else if
    [ -e "/grade/run/student.mly" ]
then stufile="/grade/run/student.mly"
else stufile="/grade/run/student.ml"
fi
fi

scriptscorefield=`grep "Total:" ${student_results} | grep  -o "\[[0-9]* / [0-9]*\]"`
totalfield=`grep "Total:" ${student_results} | grep  -o "\[[0-9]*"`
totalfield=${totalfield:1}
maxscore=`grep "Total:" ${student_results} | grep -o "\\ [0-9]*\]" | cut -d "]" -f -1`
percentscore=`echo "let out_strm = open_out \"tmp_score\" in let sc = string_of_float((float_of_int $totalfield) /. (float_of_int $maxscore)) in let sc0 = (if String.length sc = 2 then sc^\"0\" else sc) in let _ = output_string out_strm sc0 in flush out_strm" > tmp_score.ml; ocaml tmp_score.ml; cat tmp_score`


echo "
Grade report for `ls /grade/student`
=====================================

*Regular Problems:
Grading script score  => $scriptscorefield
Total                 => $totalfield
Score                 => $percentscore


grader program output
---------------------

" > grade.report
  cat ${student_results} >> grade.report
  echo "
Additional output from compiling students code (may be empty)
----------------------------------------------------

" >> grade.report
  cat conflicts.txt >> grade.report
  echo ".................................................." >> grade.report
#  cat student.output >> grade.report # '\001' can't be tolerated
  echo "
Student's code 
----------------------------------------------------
" >> grade.report
  cat $stufile >> grade.report

  echo "


Stdout
----------------------------------------------------
" >> grade.report
  grep -v "\[" ${student_results} >> grade.report

cp grade.report /grade/run/tmp_unescaped
ocaml escape.ml

echo "{" > $result_json_file
echo "\"succeeded\": true," >> $result_json_file
echo "\"message\": \"Testing completed.\"," >> $result_json_file
cat tmp_escaped >> $result_json_file
echo "\"score\": $percentscore" >> $result_json_file
echo "}" >> $result_json_file

# make dist-clean

#  mv grade.report ${resultdir}
#  make clean
#  rm -f outdiff
#  rm -f $.out
#  rm -f $stufile ${student_results} student.*
#  rm -f conflicts.txt


## To run docker:
## docker run -it --rm -p 3000:3000 -v /Users/elsa/courses/cs421/fa2017/pl-cs421:/course -v /Users/elsa/courses/cs421/fa2017/pl-cs421/jobs:/jobs -e HOST_JOBS_DIR=/Users/elsa/courses/cs421/fa2017/pl-cs421/jobs -v /var/run/docker.sock:/var/run/docker.sock prairielearn/prairielearn
