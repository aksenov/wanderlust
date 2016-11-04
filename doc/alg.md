# Interesting algorithms

Delaunay triangulation (DT) - connect all locations with shortest paths
https://github.com/trystan/delaunay-triangulation

Voronoi diagram (VD) - building terrain basis
https://github.com/trystan/voronoi-diagram

Minimal spanning tree (MST) - minimize loops in graph - maze building tool
http://aysy.lu/loom/loom.alg.html#var-prim-mst

Dijkstra's algorithm (DA) - find all routes from point
http://aysy.lu/loom/loom.alg.html#var-dijkstra-path

A* path (A*) - find shortes path from-to with heuristic

Thoughts
Throw away longest paths from Delaunay triangulation - they are probably borders and looks bad at map.
Use random weight and length combination on DT for MST.