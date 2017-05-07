# squigly-trace
A cute ray tracer written in Haskell.

# to do
+ ~~Load textures from mtl files.~~
+ Add antialiasing.
+ Improve space partitioning and partition traversal.
+ Change to a realistic color mapping that doesn't distort hues.
+ Maybe make geometric functions more generic e.g. to support different partition systems.

This project has been moving slowly because I can't be bothered to fix its mysteriously slow execution time.
Despite the use of an acceleration structure (specifically a bounding interval hierarchy), it currently takes about 70 seconds to render one 540x540 sample. Profiling unhelpfully reveals that ~99% of the time is spent intersecting the boxes of the hierarchy, but I haven't determined whether they are being intersected too often (and if so, why) or the intersection code just takes ages per call.
