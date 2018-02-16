#!/bin/sh
#
#MSUB -M jiacheng.he@ku.edu
#MSUB -N XGB
#MSUB -q crmda
#MSUB -l nodes=10:ppn=20,pmem=6gb
#MSUB -l walltime=24:00:00
#MSUB -m bea


mpirun -np 1 R CMD BATCH --no-restore --no-save Titanic.R Titanic_log.txt