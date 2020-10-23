'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:20:00 pm'!
'Description '!
!provides: 'Tests-OpenCL' 1 1!
!requires: 'OpenCL' 1 50 nil!
SystemOrganization addCategory: 'Tests-OpenCL'!


!classDefinition: #OpenCLTest category: 'Tests-OpenCL'!
TestCase subclass: #OpenCLTest
	instanceVariableNames: 'context q1 q2'
	classVariableNames: ''
	poolDictionaries: 'OpenCLConstants'
	category: 'Tests-OpenCL'!
!classDefinition: 'OpenCLTest class' category: 'Tests-OpenCL'!
OpenCLTest class
	instanceVariableNames: ''!


!OpenCLTest commentStamp: '<historical>' prior: 0!
Test and document usage of OpenCL framework.

Things we still need to test:

- test whether event is actually returned when attempt to enqueue a command fails (i.e. do we have to release the event).  This is about verifying the behavior that we expect from OpenCL implementations, to verify that the assumptions that we are making are safe.
For example: 
evt := [commandQueue someCommandThatWillFailToEnqueue] 
		on: OpenCLError 
		do: [:err | err resume].
self should: [evt getHandle = #[0 0 0 0] ].

Additionally, try:
	OpenCLPlatform allPlatforms do: [ :platform | platform reportOn: Transcript ].!

!OpenCLTest methodsFor: 'running' stamp: 'jmv 1/31/2018 15:35:51'!
setUp
	| devices |
	devices := OpenCLPlatform allPlatforms collect: [ :platform |
		platform allGPUs, platform allCPUs ].
	devices := devices concatenation.
	context := OpenCLContext newForDevices: devices.
	q1 := context commandQueueOn: devices first.
	q2 := context commandQueueOn: devices first.! !

!OpenCLTest methodsFor: 'running' stamp: 'jcg 1/14/2010 23:10'!
tearDown
	q1 destroy.
	q2 destroy.
	context destroy.
	! !

!OpenCLTest methodsFor: 'running' stamp: 'jcg 1/15/2010 00:07'!
testCopyToAndFromBuffer
	| bytes1 bytes2 buf |
	
	bytes1 := ByteArray new: 255.
	bytes2 := ByteArray new: 255.
	1 to: 255 do: [:i | bytes1 at: i put: i].
	self shouldnt: [bytes1 = bytes2 "obviously"].
	
	"Create memory buffer to write to and read from."
	buf := context bufferOfSize: 255.
	
	"We must block, since we're copying to/from ByteArrays that might move during GC.  Note that if we didn't block, the read-buffer would have to wait on the event from the write-buffer command."
	q1 writeBuffer: buf block: true offset: 0 size: 255 pointer: bytes1 waitOn: nil.
	q2 readBuffer: buf block: true offset: 0 size: 255 pointer: bytes2 waitOn: nil.
	
	"Now that we copied to the buffer and back, the two ByteArrays should be equivalent."
	self should: [bytes1 = bytes2].
	
	"Free the OpenCL memory."
	buf destroy.! !

!OpenCLTest methodsFor: 'running' stamp: 'jcg 1/15/2010 01:56'!
testFinalization
	| buf refCounter |
	buf := context bufferOfSize: 1000.
	
	"There's no accessor, because users never need to touch it."
	refCounter := buf instVarNamed: 'refCounter'.
	
	self should: [refCounter count = 1].
	buf := nil.
	Smalltalk garbageCollectMost.
	self should: [refCounter count = 0].
! !

!OpenCLTest methodsFor: 'running' stamp: 'jmv 1/31/2018 15:44:12'!
testMapAndCopyImage
	
	| inputForm outputForm mapForm imageFormat image1 image2 ptr map1evt map2evt unmap1evt unmap2evt copyEvt |
	
	inputForm := (Form fromDisplay: (0@0 corner: 200@200)) asFormOfDepth: (Smalltalk isLittleEndian ifTrue: [ -32 ] ifFalse: [ 32 ]).
	outputForm := Form extent: 200@200 nativeDepthFrom: 32.
	mapForm := ExternalForm extent: 200@200 nativeDepthFrom: 32.
	
	imageFormat := CLImageFormatStruct rgba8888.
	image1 := context 
					newImage2D: CL_MEM_WRITE_ONLY
					format: imageFormat
					width: 200
					height: 200
					rowPitch: nil
					hostPointer: nil.
	image2 := context
					newImage2D: CL_MEM_READ_ONLY
					format: imageFormat
					width: 200
					height: 200
					rowPitch: nil
					hostPointer: nil.
		
	map1evt := q1 
					mapImage: image1 
					block: true 
					flags: CL_MAP_WRITE 
					origin: #(0 0 0) 
					region: #(200 200 1) 
					waitOn: nil.
	ptr := map1evt pointer.
	mapForm setManualSurfacePointer: ptr.
	inputForm displayOn: mapForm.
	mapForm setManualSurfacePointer: nil.
	unmap1evt := q1 unmap: image1 pointer: ptr waitOn: map1evt.
	
	copyEvt := q2 
					copyImage: image1 
					origin: #(0 0 0) 
					into: image2 
					origin: #(0 0 0) 
					region: #(200 200 1) 
					waitOn: unmap1evt.

	map2evt := q1 
					mapImage: image2
					block: true 
					flags: CL_MAP_READ 
					origin: #(0 0 0) 
					region: #(200 200 1) 
					waitOn: copyEvt.
	ptr := map2evt pointer.
	mapForm setManualSurfacePointer: ptr.
	mapForm displayOn: outputForm.
	mapForm setManualSurfacePointer: nil.
	unmap2evt := q1 unmap: image2 pointer: ptr waitOn: map2evt.
	
	self should: [inputForm bits = outputForm bits].
	
	mapForm destroySurface.
! !
