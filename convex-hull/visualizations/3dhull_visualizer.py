import subprocess
import json
import os
import re
import webbrowser
import random
from datetime import datetime

def run_convex_hull_algorithm(num_points):
    """
    Run the convex hull algorithm and capture its output.
    
    Args:
        num_points (int): Number of points to generate
    
    Returns:
        tuple: List of all points, list of hull vertices
    """
    # Assuming the Haskell executable is named 'convex-hull-exe'
    command = f"stack exec convex-hull-exe -- {num_points} quickHullPar 3d"
    
    try:
        result = subprocess.run(command, shell=True, capture_output=True, text=True)
        output = result.stdout
    except Exception as e:
        print(f"Error running convex hull algorithm: {e}")
        return None, None
    
    all_points_match = re.findall(r'V3 ([\d.-]+) ([\d.-]+) ([\d.-]+)', output)
    all_points = [list(map(float, point)) for point in all_points_match]
    hull_match = re.findall(r'V3 ([\d.-]+) ([\d.-]+) ([\d.-]+)', output.split('Found')[1].split(']')[0])
    hull_vertices = [list(map(float, vertex)) for vertex in hull_match]
    return all_points, hull_vertices

def generate_html_visualizer(all_points, hull_vertices, num_points):
    """
    Generate an HTML file with a 3D Convex Hull visualization.
    Args:
        all_points (list): List of all generated points
        hull_vertices (list): List of convex hull vertices
        num_points (int): Number of points generated
    """
    # HTML template with embedded JavaScript for visualization
    html_template = f"""<!DOCTYPE html>
<html>
<head>
    <title>3D Convex Hull Visualization</title>
    <style>
        body {{
            display: flex;
            flex-direction: column;
            align-items: center;
            background-color: #f0f0f0;
            font-family: Arial, sans-serif;
        }}
        canvas {{
            border: 1px solid #ccc;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            background-color: white;
            margin-top: 20px;
        }}
        .legend {{
            margin-top: 20px;
            display: flex;
            align-items: center;
            gap: 20px;
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
            margin-top: 10px;
            font-size: 14px;
            color: #666;
        }}
    </style>
</head>
<body>
    <canvas id="canvas" width="800" height="600"></canvas>
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

    <script>
        // All points
        const allPoints = {json.dumps(all_points)};

        // Hull vertices
        const hullVertices = {json.dumps(hull_vertices)};

        // Generate triangular faces
        const faces = [];
        for (let i = 0; i < hullVertices.length - 2; i++) {{
            for (let j = i + 1; j < hullVertices.length - 1; j++) {{
                for (let k = j + 1; k < hullVertices.length; k++) {{
                    faces.push([i, j, k]);
                }}
            }}
        }}

        const canvas = document.getElementById('canvas');
        const ctx = canvas.getContext('2d');
        let rotation = 0;

        // Project 3D point to 2D
        function project(point, rotation) {{
            const scale = 400;
            const [x, y, z] = point;
            
            // Rotate around Y axis
            const rotatedX = x * Math.cos(rotation) - z * Math.sin(rotation);
            const rotatedZ = x * Math.sin(rotation) + z * Math.cos(rotation);
            
            // Simple perspective projection
            const distance = 4;
            const perspective = distance / (distance + rotatedZ);
            
            return {{
                x: (rotatedX * perspective * scale) + 400,
                y: (y * perspective * scale) + 300,
                z: rotatedZ
            }};
        }}

        function calculateFaceDepth(face, projectedPoints) {{
            const [i, j, k] = face;
            return (projectedPoints[i].z + projectedPoints[j].z + projectedPoints[k].z) / 3;
        }}

        function draw() {{
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            // Draw background grid
            ctx.strokeStyle = '#eee';
            ctx.lineWidth = 1;
            for (let i = 0; i < canvas.width; i += 50) {{
                ctx.beginPath();
                ctx.moveTo(i, 0);
                ctx.lineTo(i, canvas.height);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(0, i);
                ctx.lineTo(canvas.width, i);
                ctx.stroke();
            }}

            // Project all points
            const projectedHull = hullVertices.map(p => project(p, rotation));
            const projectedInternal = allPoints.filter(point => 
                !hullVertices.some(hp => 
                    hp[0] === point[0] && hp[1] === point[1] && hp[2] === point[2]
                )
            ).map(p => project(p, rotation));

            // Sort faces by depth
            const facesWithDepth = faces.map(face => ({{
                faceIndices: face,
                depth: calculateFaceDepth(face, projectedHull)
            }}));
            facesWithDepth.sort((a, b) => a.depth - b.depth);

            // Draw faces with transparency
            ctx.globalAlpha = 0.2;
            facesWithDepth.forEach((faceData) => {{
                const [i, j, k] = faceData.faceIndices;
                ctx.beginPath();
                ctx.moveTo(projectedHull[i].x, projectedHull[i].y);
                ctx.lineTo(projectedHull[j].x, projectedHull[j].y);
                ctx.lineTo(projectedHull[k].x, projectedHull[k].y);
                ctx.closePath();
                ctx.fillStyle = '#ff6666';
                ctx.fill();
            }});
            ctx.globalAlpha = 1.0;

            // Draw hull edges
            ctx.strokeStyle = '#ff0000';
            ctx.lineWidth = 2;
            for (let i = 0; i < projectedHull.length; i++) {{
                for (let j = i + 1; j < projectedHull.length; j++) {{
                    const p1 = projectedHull[i];
                    const p2 = projectedHull[j];
                    ctx.beginPath();
                    ctx.moveTo(p1.x, p1.y);
                    ctx.lineTo(p2.x, p2.y);
                    ctx.stroke();
                }}
            }}

            // Draw hull vertices
            ctx.fillStyle = '#ff0000';
            projectedHull.forEach(p => {{
                ctx.beginPath();
                ctx.arc(p.x, p.y, 6, 0, Math.PI * 2);
                ctx.fill();
            }});

            // Draw internal points
            ctx.fillStyle = '#0000ff';
            projectedInternal.forEach(p => {{
                ctx.beginPath();
                ctx.arc(p.x, p.y, 6, 0, Math.PI * 2);
                ctx.fill();
            }});

            // Update rotation
            rotation = (rotation + 0.01) % (Math.PI * 2);
            requestAnimationFrame(draw);
        }}

        // Start animation
        draw();
    </script>
</body>
</html>"""
    
    os.makedirs('convex_hull_visualizations', exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f'convex_hull_visualizations/convex_hull_viz_{timestamp}.html'
    with open(filename, 'w') as f:
        f.write(html_template)
    print(f"Visualization saved to {filename}")
    return filename

def main():
    num_points = random.randint(10, 100)
    all_points, hull_vertices = run_convex_hull_algorithm(num_points)
    if all_points and hull_vertices:
        generated_file = generate_html_visualizer(all_points, hull_vertices, num_points)
        webbrowser.open('file://' + os.path.realpath(generated_file))

if __name__ == "__main__":
    main()