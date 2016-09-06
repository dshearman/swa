#!/bin/sh

(head -n 2 company_name2015.txt && tail -n +3 company_name2016.txt | sort) > company_name2016s.txt
scp company_name2016s.txt staff.scem.uws.edu.au:~/Homepage/300958/project/company_name2016.txt
