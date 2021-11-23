#!/bin/bash
source /etc/profile.d/modules.sh
module load mpi/openmpi-x86_64
select_rat=$1
seed=$2
count=$3
mpirun -mca btl_openib_pkey 0x8108 -mca plm_rsh_agent oarsh --prefix $MPI_HOME  -machinefile $OAR_NODEFILE -np 1 R --slave -f RatModels.R  --args $select_rat $seed $count
