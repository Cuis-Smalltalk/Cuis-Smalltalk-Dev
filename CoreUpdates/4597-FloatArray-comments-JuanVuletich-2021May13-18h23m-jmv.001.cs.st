'From Cuis 5.0 [latest update: #4596] on 13 May 2021 at 6:24:06 pm'!

!Float32Array commentStamp: 'jmv 5/13/2021 17:12:39' prior: 0!
Float32rrays store 32bit IEEE floating point numbers, i.e. what usually called float in the C world.

A possible way to create literal Float32Arrays is by using backticks:
`#[ 1.0 2.0 3.0 ] asFloat32Array`!


!Float64Array commentStamp: 'jmv 5/13/2021 17:11:37' prior: 0!
Float64Arrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.
Some support is included for subclasses in the style of Balloon3D-Math.

Uses the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits. See #floatAt: and #floatAt:put:

Can be created as literals like:
#[ 1.0 2.0 3.0 ]!

