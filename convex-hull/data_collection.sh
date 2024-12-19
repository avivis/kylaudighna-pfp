#!/bin/bash

# declare -a algs=("chans2" "chans2Par" "quickHull2" "quickHull2Par")
declare -a seq_algs=("chans2" "quickHull2")
declare -a parallel_algs=("chans2Par" "quickHull2Par")

:> data_par_cores_2M.csv
echo "algorithm,cores,total_0,elapsed_0,total_1,elapsed_1,total_2,elapsed_2,total_3,elapsed_3,total_4,elapsed_4" >> data_par_cores_2M.csv
points=$((2**21))
for par_alg in "${parallel_algs[@]}"; do
  for threads in {1..22}; do
    echo -n "$par_alg,$threads"
    echo -n "$par_alg,$threads" >> data_par_cores_2M.csv
    for it in {1..5}; do
      total_elapsed=$(stack run $((points)) "$par_alg" no-print --rts-options "-N$((threads)) -s -A64M" |& grep 'Total' | grep -oP '\d+\.\d+(?=s)')
      total=$(echo "$total_elapsed" | awk 'NR==1')
      elapsed=$(echo "$total_elapsed" | awk 'NR==2')
      echo -n ",$total,$elapsed"  >> data_par_cores_2M.csv
      echo -n "."
    done
    echo "" >> data_par_cores_2M.csv
    echo "done!"
  done
done

:> data_par_cores_points.csv
echo "algorithm,cores,points,total_0,elapsed_0,total_1,elapsed_1,total_2,elapsed_2,total_3,elapsed_3,total_4,elapsed_4" >> data_par_cores_points.csv
points=$((2**21))
for par_alg in "${parallel_algs[@]}"; do
  for threads in {1..22}; do
    echo -n "$par_alg,$threads"
    echo -n "$par_alg,$threads" >> data_par_cores_points.csv
    for it in {1..5}; do
      total_elapsed=$(stack run $((points)) "$par_alg" no-print --rts-options "-N$((threads)) -s -A64M" |& grep 'Total' | grep -oP '\d+\.\d+(?=s)')
      total=$(echo "$total_elapsed" | awk 'NR==1')
      elapsed=$(echo "$total_elapsed" | awk 'NR==2')
      echo -n ",$total,$elapsed"  >> data_par_cores_points.csv
      echo -n "."
    done
    echo "" >> data_par_cores_points.csv
    echo "done!"
  done
done



:> data_points.csv
echo "algorithm,points,total_0,elapsed_0,total_1,elapsed_1,total_2,elapsed_2,total_3,elapsed_3,total_4,elapsed_4" >> data_points.csv
for seq_alg in "${seq_algs[@]}"; do
  for points_pow in {19..24}; do
    points=$((2 ** points_pow))
    echo -n "$seq_alg,$points"
    echo -n "$seq_alg,$points" >> data_points.csv
    for it in {1..5}; do
      total_elapsed=$(stack run $((points)) "$seq_alg" no-print --rts-options "-N1 -s -A64M" |& grep 'Total' | grep -oP '\d+\.\d+(?=s)')
      total=$(echo "$total_elapsed" | awk 'NR==1')
      elapsed=$(echo "$total_elapsed" | awk 'NR==2')
      echo -n ",$total,$elapsed"  >> data_points.csv
      echo -n "."
    done
    echo "" >> data_points.csv
    echo "done!"
  done
done
for par_alg in "${parallel_algs[@]}"; do
  for points_pow in {19..24}; do
    points=$((2 ** points_pow))
    echo -n "$par_alg,$points"
    echo -n "$par_alg,$points" >> data_points.csv
    for it in {1..5}; do
      total_elapsed=$(stack run $((points)) "$par_alg" no-print --rts-options "-s -A64M" |& grep 'Total' | grep -oP '\d+\.\d+(?=s)')
      total=$(echo "$total_elapsed" | awk 'NR==1')
      elapsed=$(echo "$total_elapsed" | awk 'NR==2')
      echo -n ",$total,$elapsed" >> data_points.csv
      echo -n "."
    done
    echo "" >> data_points.csv
    echo "done!"
  done
done
