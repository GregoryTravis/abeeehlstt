#rm -r notes
#mkdir notes
(stack build abeeehlstt && stack exec abeeehlstt) 2>&1 | tee out
#(stack build --library-profiling --executable-profiling --profile autobeat) 2>&1 | tee out
