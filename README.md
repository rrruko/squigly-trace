# squigly-trace
A cute ray tracer written in Haskell.

# to do
+ ~~Load textures from mtl files.~~
+ ~~Add antialiasing.~~
+ ~~Change to a realistic color mapping that doesn't distort hues.~~
+ Maybe make geometric functions more generic e.g. to support different partition systems.
+ Improve space partitioning and partition traversal.

This project has been moving slowly because I can't be bothered to fix its mysteriously slow execution time.
Despite the use of an acceleration structure (specifically a bounding interval hierarchy), it currently takes about 40 seconds to render one 540x540 sample with -O2.
Profiling a single sample indicates that too many intersections are done per ray, so the acceleration structure is probably not implemented right.

