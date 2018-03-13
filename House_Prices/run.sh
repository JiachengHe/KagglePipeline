#!/bin/sh
#
#MSUB -M jiacheng.he@ku.edu
#MSUB -N House_Prices
#MSUB -q sixhour
#MSUB -l nodes=5:ppn=20,pmem=6gb
#MSUB -l walltime=6:00:00
#MSUB -m bea


mpirun -np 1 R CMD BATCH --no-restore --no-save House_Prices.R cluster_log.txt
