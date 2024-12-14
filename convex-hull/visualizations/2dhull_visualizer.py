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
    except Exception as e:
        print(f"Error running convex hull algorithm: {e}")
        return None, None

    all_points = parse_points(output, "Original points")
    hull_vertices = parse_points(output, "Convex hull")
    return all_points, hull_vertices


def generate_html_visualizer(all_points, hull_vertices, num_points):
    """
    Generate an HTML file with a 2D Convex Hull visualization.
    Args:
        all_points (list): List of all generated points
        hull_vertices (list): List of convex hull vertices
        num_points (int): Number of points generated
    """
    html_template = f"""<!DOCTYPE html>
<html>
<head>
    <title>2D Convex Hull Visualization</title>
    <style>
        html, body {{
            height: 100%;
            margin: 0;
            padding: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            background-color: #f0f0f0;
            font-family: Arial, sans-serif;
        }}
        .container {{
            display: flex;
            flex-direction: column;
            align-items: center;
            width: 100%;
            max-width: 600px;
        }}
        canvas {{
            border: 1px solid #ccc;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            background-color: white;
            margin-bottom: 20px;
        }}
        .legend {{
            display: flex;
            align-items: center;
            gap: 20px;
            margin-bottom: 10px;
        }}
        .legend-item {{
            display: flex;
            align-items: center;
            gap: 8px;
        }}
        .legend-dot {{
            width: 12px;
            height: 12px;
            border-radius: 50%;
        }}
        .red-dot {{ background-color: red; }}
        .blue-dot {{ background-color: blue; }}
        .info {{
            font-size: 14px;
            color: #666;
        }}
    </style>
</head>
<body>
    <div class="container">
        <canvas id="canvas" width="600" height="450"></canvas>
        <div class="legend">
            <div class="legend-item">
                <div class="legend-dot red-dot"></div>
                <span>Hull Vertices</span>
            </div>
            <div class="legend-item">
                <div class="legend-dot blue-dot"></div>
                <span>Internal Points</span>
            </div>
        </div>
        <div class="info">
            Total Points: {num_points} | Hull Vertices: {len(hull_vertices)}
        </div>
    </div>

    <script>
        // All points
        const allPoints = {json.dumps(all_points)};

        // Hull vertices
        const hullVertices = {json.dumps(hull_vertices)};

        const canvas = document.getElementById('canvas');
        const ctx = canvas.getContext('2d');

        // Normalize points to fit within the canvas
        function normalizePoints(points) {{
            const mins = [
                Math.min(...points.map(p => p[0])),
                Math.min(...points.map(p => p[1]))
            ];
            const maxs = [
                Math.max(...points.map(p => p[0])),
                Math.max(...points.map(p => p[1]))
            ];
            const scales = maxs.map((max, i) => max - mins[i] || 1);
            return points.map(point => [
                ((point[0] - mins[0]) / scales[0]) * 580 + 10,
                ((point[1] - mins[1]) / scales[1]) * 430 + 10
            ]);
        }}

        const normalizedAllPoints = normalizePoints(allPoints);
        const normalizedHull = normalizePoints(hullVertices);

        function draw() {{
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            // Draw internal points
            ctx.fillStyle = '#0000ff';
            normalizedAllPoints.forEach(p => {{
                ctx.beginPath();
                ctx.arc(p[0], p[1], 3, 0, Math.PI * 2);
                ctx.fill();
            }});

            // Draw hull edges
            ctx.strokeStyle = '#ff0000';
            ctx.lineWidth = 2;
            ctx.beginPath();
            normalizedHull.forEach((p, i) => {{
                const next = normalizedHull[(i + 1) % normalizedHull.length];
                ctx.moveTo(p[0], p[1]);
                ctx.lineTo(next[0], next[1]);
            }});
            ctx.stroke();

            // Draw hull vertices
            ctx.fillStyle = '#ff0000';
            normalizedHull.forEach(p => {{
                ctx.beginPath();
                ctx.arc(p[0], p[1], 6, 0, Math.PI * 2);
                ctx.fill();
            }});
        }}

        // Initial draw
        draw();
    </script>
</body>
</html>"""
    
    os.makedirs('convex_hull_visualizations', exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f'convex_hull_visualizations/convex_hull_viz_2d_{timestamp}.html'
    with open(filename, 'w') as f:
        f.write(html_template)
    print(f"Visualization saved to {filename}")
    return filename

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
        num_points = random.randint(10, 100)
    
   
    all_points, hull_vertices = run_convex_hull_algorithm(num_points, algorithm)
    if all_points and hull_vertices:
        plot_convex_hull(all_points, hull_vertices, algorithm)

        # generated_file = generate_html_visualizer(all_points, hull_vertices, num_points)
        # webbrowser.open('file://' + os.path.realpath(generated_file))

if __name__ == "__main__":
    main()