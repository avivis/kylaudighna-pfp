import subprocess
import json
import os
import re
import webbrowser
import random
from datetime import datetime

def parse_points(output, label):
    """Extracts points from the given labeled section of the output."""
    match = re.search(f"{label}:\\n(.*?)(?:\\n\\n|$)", output, re.DOTALL)
    if not match:
        raise ValueError(f"Could not find the {label} section in the output.")

    points_str = match.group(1).strip()
    points = []

    for line in points_str.split("\n"):
        _, sX, sY, sZ = line.split()
        x = float(sX)
        y = float(sY)
        z = float(sZ)
        points.append((x, y, z))

    return points

def run_convex_hull_algorithm(num_points):
    """
    Run the convex hull algorithm and capture its output.
    
    Args:
        num_points (int): Number of points to generate
    
    Returns:
        tuple: List of all points, list of hull vertices
    """


    command = f"stack run {num_points} quickHull3 print --rts-options \"-ls -N11 -s\""
    
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

def generate_html_visualizer(all_points, hull_vertices, num_points):
    """
    Generate an HTML file with a 3D Convex Hull visualization.
    Args:
        all_points (list): List of all generated points
        hull_vertices (list): List of convex hull vertices
        num_points (int): Number of points generated
    """
    html_template = '''<!DOCTYPE html>
<html>
<head>
    <title>3D Convex Hull Visualization (Improved)</title>
    <style>
        html, body {
            height: 100%;
            margin: 0;
            padding: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            background-color: #f0f0f0;
            font-family: Arial, sans-serif;
        }
        .container {
            display: flex;
            flex-direction: column;
            align-items: center;
            width: 100%;
            max-width: 800px;
        }
        canvas {
            border: 1px solid #ccc;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            background-color: white;
            margin-bottom: 20px;
        }
        .legend {
            display: flex;
            align-items: center;
            gap: 20px;
            margin-bottom: 10px;
        }
        .legend-item {
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .legend-shape {
            width: 20px;
            height: 20px;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .circle {
            width: 12px;
            height: 12px;
            border-radius: 50%;
            background-color: rgba(255, 0, 0, 0.4);
        }
        .square {
            width: 10px;
            height: 10px;
            background-color: blue;
            transform: rotate(45deg);
        }
        .info {
            font-size: 14px;
            color: #666;
        }
    </style>
</head>
<body>
    <div class="container">
        <canvas id="canvas" width="800" height="600"></canvas>
        <div class="legend">
            <div class="legend-item">
                <div class="legend-shape"><div class="circle"></div></div>
                <span>Hull Vertices</span>
            </div>
            <div class="legend-item">
                <div class="legend-shape"><div class="square"></div></div>
                <span>Internal Points</span>
            </div>
        </div>
        <div class="info">
            Total Points: ''' + str(num_points) + ''' | Hull Vertices: ''' + str(len(hull_vertices)) + '''
        </div>
    </div>

    <script>
        // All points and hull vertices
        const allPoints = ''' + json.dumps(all_points) + ''';
        const hullVertices = ''' + json.dumps(hull_vertices) + ''';

        // Generate triangular faces
        const faces = [];
        for (let i = 0; i < hullVertices.length - 2; i++) {
            for (let j = i + 1; j < hullVertices.length - 1; j++) {
                for (let k = j + 1; k < hullVertices.length; k++) {
                    faces.push([i, j, k]);
                }
            }
        }

        const canvas = document.getElementById('canvas');
        const ctx = canvas.getContext('2d');
        let rotation = 0;

        function normalizePoints(points) {
            const mins = [
                Math.min(...points.map(p => p[0])),
                Math.min(...points.map(p => p[1])),
                Math.min(...points.map(p => p[2]))
            ];
            const maxs = [
                Math.max(...points.map(p => p[0])),
                Math.max(...points.map(p => p[1])),
                Math.max(...points.map(p => p[2]))
            ];

            const scales = maxs.map((max, i) => max - mins[i] || 1);
            const maxScale = Math.max(...scales);

            return points.map(point => 
                point.map((coord, i) => (coord - mins[i]) / maxScale * 2 - 1)
            );
        }

        const normalizedAllPoints = normalizePoints([...allPoints, ...hullVertices]);
        const normalizedPoints = normalizedAllPoints.slice(0, allPoints.length);
        const normalizedHull = normalizedAllPoints.slice(allPoints.length);

        function project(point, rotation) {
            const scale = 350; // Increased scale for larger view
            const [x, y, z] = point;
            
            const rotatedX = x * Math.cos(rotation) - z * Math.sin(rotation);
            const rotatedZ = x * Math.sin(rotation) + z * Math.cos(rotation);
            
            const distance = 3;
            const perspective = distance / (distance + rotatedZ + 1 + 2);
            
            return {
                x: (rotatedX * perspective * scale) + 400,
                y: (y * perspective * scale) + 300, 
                z: rotatedZ
            };
        }

        function calculateFaceDepth(face, projectedPoints) {
            const [i, j, k] = face;
            return (projectedPoints[i].z + projectedPoints[j].z + projectedPoints[k].z) / 3;
        }

        function drawSquare(ctx, x, y, size) {
            ctx.save();
            ctx.translate(x, y);
            ctx.rotate(Math.PI / 4); // 45-degree rotation
            ctx.fillRect(-size/2, -size/2, size, size);
            ctx.restore();
        }

        function draw() {
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            // Draw background grid
            ctx.strokeStyle = '#eee';
            ctx.lineWidth = 1;
            for (let i = 0; i < canvas.width; i += 50) {
                ctx.beginPath();
                ctx.moveTo(i, 0);
                ctx.lineTo(i, canvas.height);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(0, i);
                ctx.lineTo(canvas.width, i);
                ctx.stroke();
            }

            const projectedHull = normalizedHull.map(p => project(p, rotation));
            const projectedInternal = normalizedPoints.map(p => project(p, rotation));

            // Sort faces by depth
            const facesWithDepth = faces.map(face => ({
                faceIndices: face,
                depth: calculateFaceDepth(face, projectedHull)
            }));
            facesWithDepth.sort((a, b) => a.depth - b.depth);

            // Draw faces with increased transparency
            ctx.globalAlpha = 0.08; // Made more transparent
            facesWithDepth.forEach((faceData) => {
                const [i, j, k] = faceData.faceIndices;
                ctx.beginPath();
                ctx.moveTo(projectedHull[i].x, projectedHull[i].y);
                ctx.lineTo(projectedHull[j].x, projectedHull[j].y);
                ctx.lineTo(projectedHull[k].x, projectedHull[k].y);
                ctx.closePath();
                ctx.fillStyle = '#ff6666';
                ctx.fill();
            });

            // Draw hull edges with transparency
            ctx.globalAlpha = 0.04; // Made more transparent
            ctx.strokeStyle = '#ff0000';
            ctx.lineWidth = 2;
            for (let i = 0; i < projectedHull.length; i++) {
                for (let j = i + 1; j < projectedHull.length; j++) {
                    const p1 = projectedHull[i];
                    const p2 = projectedHull[j];
                    ctx.beginPath();
                    ctx.moveTo(p1.x, p1.y);
                    ctx.lineTo(p2.x, p2.y);
                    ctx.stroke();
                }
            }

            // Draw internal points as with depth-based size
            ctx.globalAlpha = 0.8;
            ctx.fillStyle = '#0000ff';
            projectedInternal.forEach(p => {
                const size = 3;
                ctx.beginPath();
                ctx.arc(p.x, p.y, size, 0, Math.PI * 2); // Draw circle for internal points
                ctx.fill();
            });

            // Draw hull vertices as opaque red circles
            ctx.globalAlpha = 0.8; // Less transparent for better visibility
            ctx.fillStyle = '#ff0000'; // Explicit red color for hull points
            projectedHull.forEach(p => {
                ctx.beginPath();
                ctx.arc(p.x, p.y, 6, 0, Math.PI * 2); // Circle with radius 6
                ctx.fill();
            });

            rotation = (rotation + 0.01) % (Math.PI * 2);
            requestAnimationFrame(draw);
        }

        draw();
    </script>
</body>
</html>'''

    os.makedirs('convex_hull_visualizations', exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f'convex_hull_visualizations/convex_hull_viz_{timestamp}.html'
    with open(filename, 'w') as f:
        f.write(html_template)
    print(f"Visualization saved to {filename}")
    return filename

def main():
    num_points = random.randint(13, 25)
    all_points, hull_vertices = run_convex_hull_algorithm(num_points)
    if all_points and hull_vertices:
        generated_file = generate_html_visualizer(all_points, hull_vertices, num_points)
        webbrowser.open('file://' + os.path.realpath(generated_file))

if __name__ == "__main__":
    main()