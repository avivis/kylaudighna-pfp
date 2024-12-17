# convex-hull

Kyle Edwards (kje2115)

Avighna Suresh (as6469)

Claudia Cortell (ccc2223)


All commands should be run from the `convex-hull/` directory.


## How to run: ##

```stack run <num-points> <algorithm> <print-flag> --rts-options "-s -A64M -N<number-cores> -ls"```

The options for 2D algorithms are: `grahamScan`, `quickHull2`, `quickHull2Par`, `chans`, and `chansPar`.
The 3D options are: `quickHull3` and `quickHull3Par`.

The options for `<print-flag>` are either `print` or `no-print`, which will determine whether the initial generated points are printed. You should usually have this set to `no-print` to avoid excessive output.

The `-s` and `ls` flags in `--rts-options` are not necessary unless you want to time or view the activity log on Threadscope, respectively.

## How to run visualizations: ##

### 2D ####
Make sure that you have `matplotlib` installed before running this.

```python3 visualizations/2dhull_visualizer.py <2d-algorithm>```
### 3D ###
```python3 visualizations/3dhull_visualizer.py```

