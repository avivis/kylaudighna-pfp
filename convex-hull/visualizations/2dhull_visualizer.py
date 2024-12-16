import subprocess
import json
import os
import re
import webbrowser
import random
from datetime import datetime
import matplotlib.pyplot as plt
import sys

def parse_points(output, label):
    """Extracts points from the given labeled section of the output."""
    match = re.search(f"{label}:\\n(.*?)(?:\\n\\n|$)", output, re.DOTALL)
    if not match:
        raise ValueError(f"Could not find the {label} section in the output.")

    points_str = match.group(1).strip()
    points = []

    for line in points_str.split("\n"):
        _, sX, sY = line.split()
        x = float(sX)
        y = float(sY)
        points.append((x, y))

    return points

def run_convex_hull_algorithm(num_points, algorithm):
    """
    Run the convex hull algorithm and capture its output.
    
    Args:
        num_points (int): Number of points to generate
    
    Returns:
        tuple: List of all points, list of hull vertices
    """
    command = f"stack run {num_points} {algorithm} print --rts-options \"-ls -N11 -s\""
    
    try:
        result = subprocess.run(command, shell=True, capture_output=True, text=True)
        output = result.stdout
        print(output)
    except Exception as e:
        print(f"Error running convex hull algorithm: {e}")
        return None, None

    all_points = parse_points(output, "Original points")
    hull_vertices = parse_points(output, "Convex hull")
    return all_points, hull_vertices

def plot_convex_hull(all_points, hull_vertices, algorithm):
    """
    Plot the points and the convex hull on a graph with labeled axes.
    
    Args:
        all_points (list): List of all generated points
        hull_vertices (list): List of convex hull vertices
    """

    plt.figure(figsize=(8, 8))

    all_x, all_y = zip(*all_points)
    plt.scatter(all_x, all_y, color='blue', label='Internal Points', s=10)

    hull_x, hull_y = zip(*hull_vertices + [hull_vertices[0]]) #close hull
    plt.plot(hull_x, hull_y, color='red', label='Convex Hull', linewidth=2)
    plt.scatter(hull_x[:-1], hull_y[:-1], color='red', label='Hull Vertices', s=30)


    plt.xlim(0, 1.1)
    plt.ylim(0, 1.1)
    plt.xlabel("X-axis")
    plt.ylabel("Y-axis")
    plt.title(f"{algorithm} 2D Convex Hull Visualization")
    plt.legend()

    plt.grid(color='gray', linestyle='--', linewidth=0.5, alpha=0.7)

    plt.subplots_adjust(bottom=0.15)
    plt.figtext(0.5, 0.05, f"Total Points: {len(all_points)} | Hull Vertices: {len(hull_vertices)}",
                ha="center", va="center", fontsize=12)

    plt.show()


algs = ["grahamScan", "quickHull2", "quickHull2Par", "chans", "chansPar"]

def main():
    if len(sys.argv) == 0:
        print("Usage: python3 visualizations/2dhull_visualizer.py <algorithm> <num_points>")
        sys.exit(1) 

    algorithm = sys.argv[1]
    if algorithm not in algs:
        print(f"Please choose one of the following algorithms: {', '.join(algs)}")
        sys.exit(1)
    
    if (len(sys.argv) == 3):
        num_points = int(sys.argv[2])
    else:
        num_points = random.randint(15, 100)
   
    all_points, hull_vertices = run_convex_hull_algorithm(num_points, algorithm)
    if all_points and hull_vertices:
        plot_convex_hull(all_points, hull_vertices, algorithm)

if __name__ == "__main__":
    main()