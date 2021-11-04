#!/bin/bash
source /etc/profile.d/modules.sh
module load mpi/openmpi-x86_64
mpirun -mca btl_openib_pkey 0x8108 -mca plm_rsh_agent oarsh --prefix $MPI_HOME  -machinefile $OAR_NODEFILE Rscript RatModels.R
