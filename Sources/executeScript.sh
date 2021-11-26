#!/bin/bash

echo "Submit NEF Jobs" 
core_number=50
walltime=9:00
let spawnSlaves=$core_number-1

for rat in   1 2 3 4 5 6 7
do
 let seed1=$rat*150
 let seed2=$rat*210
 let seed3=$rat*330
 let seed4=$rat*420
 let seed5=$rat*560
 echo $seed1
 oarsub -p  "cputype='xeon'" -l core=$core_number,walltime=$walltime  --stdout='logs/test12.%jobid%.stdout' --stderr='logs/test12.%jobid%.stderr' -S "./ratscript.sh $rat $seed1 $spawnSlaves"
 oarsub -p  "cputype='xeon'" -l core=$core_number,walltime=$walltime  --stdout='logs/test12.%jobid%.stdout' --stderr='logs/test12.%jobid%.stderr' -S "./ratscript.sh $rat $seed2 $spawnSlaves"
 #oarsub -p  "cputype='xeon'" -l core=$core_number,walltime=$walltime  --stdout='logs/test12.%jobid%.stdout' --stderr='logs/test12.%jobid%.stderr' -S "./ratscript.sh $rat $seed3 $spawnSlaves"
 #oarsub -p  "cputype='xeon'" -l core=$core_number,walltime=$walltime  --stdout='logs/test12.%jobid%.stdout' --stderr='logs/test12.%jobid%.stderr' -S "./ratscript.sh $rat $seed4 $spawnSlaves"
 #oarsub -p  "cputype='xeon'" -l core=$core_number,walltime=$walltime  --stdout='logs/test12.%jobid%.stdout' --stderr='logs/test12.%jobid%.stderr' -S "./ratscript.sh $rat $seed5 $spawnSlaves"
done
