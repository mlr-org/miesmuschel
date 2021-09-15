#!/bin/bash

if [ -z "$4" ] ; then
  echo "need [hb|smashy|fixmu] [rs|all] [siman|nosiman] [seed]" >&2
  exit 1
fi


for objective in  rbv2_super ; do
  name="mbo_${objective}_"
  if [[ $2 == "rs" ]] ; then
    name=${name}rs_
  fi
  if [[ $3 == "nosiman" ]] ; then
    name=${name}nosiman_
  fi
  name=${name}${1}_$4
  FILE=results/${objective}/
  RDS=run_s${4}_${objective}_
  if [[ $1 == "fixmu" ]] ; then
    FILE=${FILE}$1/smashy/
    RDS=${RDS}${1}_smashy_
    MPC=--mem-per-cpu=8G
  else
    FILE=${FILE}varymu/$1/
    RDS=${RDS}varymu_${1}_
    MPC=--mem-per-cpu=2G
  fi
  FILE=${FILE}$2/$3/$4/
  RDS=${RDS}${2}_${3}.rds
  if [ ! -d $FILE ] ; then
    echo "directory $FILE not found." >&2
    exit 2
  fi
  FILE=${FILE}${RDS}
  if [ ! -r $FILE ]  ; then
    echo "file $FILE not found." >&2
    exit 3
  fi
  if [[ $objective == "lcbench" ]] ; then
    cores=8
  else
    cores=30
  fi
  export FILE
  call="sbatch -A mallet --exclude=tknl[01-12] -c $cores $MPC -J $name" 
  jobno=$( ${call}-1 runsingle.sbatch | grep -o '[0-9]\+')
  if [ $? -eq 0 ] ; then
    jobno=$( ${call}-2 --dependency=afterany:$jobno runsingle.sbatch | grep -o '[0-9]\+')
    if [ $? -eq 0 ] ; then
      ${call}-3 --dependency=afterany:$jobno runsingle.sbatch
      if [ $? -ne 0 ] ; then
        echo "couldn't submit 3rd" >&2
        exit 6
      fi
    else
      echo "couldn't submit 2nd" >&2
      exit 5
    fi
  else
    echo "couldn't submit" >&2
    exit 4
  fi
done

