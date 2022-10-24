#!/bin/bash
source /etc/profile.d/modules.sh
module load mpi/openmpi-x86_64
select_rat=$1
seed=$2
count=$3
currentTest=$4
start_idx=$5
end_idx=$6
testSuite=$7
mpirun -mca btl_openib_pkey 0x8108 -mca plm_rsh_agent oarsh --prefix $MPI_HOME  -machinefile $OAR_NODEFILE -np 1 R --slave -f executeNefJobs2.R  --args $select_rat $seed $count $currentTest $start_idx $end_idx $testSuite
