'From Cuis7.5 [latest update: #7703] on 20 November 2025 at 8:05:02 am'!

!Float32Array methodsFor: 'accessing' stamp: 'jmv 11/19/2025 18:19:18'!
length
	"Return the length of the receiver"
	<primitive: 'primitiveLength' module: 'FloatArrayPlugin'>
	^super length! !


!Float64Array methodsFor: 'accessing' stamp: 'jmv 11/19/2025 18:22:16'!
length
	"Return the length of the receiver"
	<primitive: 'primitiveLength' module: 'Float64ArrayPlugin'>
	^super length! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 11/19/2025 18:26:55'!
primFromFloat32Array: aFloat32Array
"
self 
(Float64Array new:3) primFromFloat32Array: #[1.0 2.0 3.0] asFloat32Array
"
	<primitive: 'primitiveFromFloatArray' module: 'Float64ArrayPlugin'>
	^self fillWith: aFloat32Array! !

