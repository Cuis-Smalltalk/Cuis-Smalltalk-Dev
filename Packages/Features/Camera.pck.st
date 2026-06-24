'From Cuis7.9 [latest update: #8014] on 23 June 2026 at 8:26:35 pm'!
'Description Basic Webcam support'!
!provides: 'Camera' 1 3!
SystemOrganization addCategory: 'Camera'!


!classDefinition: #CameraInterface category: 'Camera'!
Object subclass: #CameraInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Camera'!
!classDefinition: 'CameraInterface class' category: 'Camera'!
CameraInterface class
	instanceVariableNames: ''!

!classDefinition: #CameraMorph category: 'Camera'!
BoxMorph subclass: #CameraMorph
	instanceVariableNames: 'cameraNumber resolution orientation cameraIsOn displayForm captureDelayMs showFPS framesSinceLastDisplay lastDisplayTime fps'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Camera'!
!classDefinition: 'CameraMorph class' category: 'Camera'!
CameraMorph class
	instanceVariableNames: ''!


!CameraInterface commentStamp: '<historical>' prior: 0!
CameraInterface: Simple cross-platform webcam access interface adapted from MIT Scratch. Changes made so that different cameras can be tested when more than one is connected, and so that the interface is simpler and may be interrupt-driven.

[| form |
form := Form extent: 352 @ 288 depth: 32.
CameraInterface
	openCamera: 1 width: form width height: form height;
	trySetSemaphoreForCamera: 1.
[Sensor noButtonPressed] whileTrue:
	[CameraInterface
		waitForNextFrame: 1 timeout: 2000;
		getFrameForCamera: 1 into: form bits.
	 form displayAt: Sensor cursorPoint]] ensure: [CameraInterface closeCamera: 1]

Copyright (c) 2009 Massachusetts Institute of Technology

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.!

!CameraInterface class methodsFor: 'utilities' stamp: 'len 23/Jun/2026, 1:24:36 pm (UTC)'!
camera: cameraNum framesDo: aBlock while: whileBlock
	"Evaluate aBlock every time a frame becomes available.  Answer a tuple of frames per second
	 and the number of 16ms delays per second if polling is used, plus indications of which schemes
	 were used. Be destructive; use only one bitmap, overwriting its contents with each successive frame.
	 Use the buffered interface if possible. It is the sender's responsibility to open and close the camera."
	| form bitmap schemes delay start duration frameCount delayCount semaphore  |
	form := Form
				extent: (self frameExtent: cameraNum)
				depth: 32.
	bitmap := form bits.
	bitmap pin.
	schemes := Array new writeStream.
	[self camera: cameraNum setFrameBuffer: bitmap.
	 schemes nextPut: 'buffered']
		on: Error
		do: [:err|
			bitmap unpin.
			schemes nextPut: 'copied'].
	semaphore := Semaphore new.
	[self camera: cameraNum setSemaphore: (Smalltalk registerExternalObject: semaphore).
	 schemes nextPut: 'interrupt driven']
		on: Error
		do: [:err|
			Smalltalk unregisterExternalObject: semaphore.
			semaphore := nil.
			schemes nextPut: 'polling'].
	delay := Delay forMilliseconds: (1000 / 60) asInteger. "60 fps is fast"

	start := Time localMicrosecondClock.
	frameCount := delayCount := 0.
	[semaphore ifNotNil:
		[semaphore wait].
	[(self getFrameForCamera: cameraNum into: bitmap) <= 0] whileTrue:
		[delay wait. delayCount := delayCount + 1].
	 frameCount := frameCount + 1.
	 aBlock value: form.
	 whileBlock value] whileTrue.
	^{ frameCount * 1.0e6 / (duration := Time localMicrosecondClock - start).
		delayCount * 1.0e6 / duration },
	 schemes contents

	"| cameraNum |
	 self openCamera: (cameraNum := 1) width: 640 height: 480.
	 self waitForCameraStart: cameraNum.
	 [self camera: cameraNum framesDo: [:bitmap| bitmap display] while: [Sensor noButtonPressed]] ensure:
		[self closeCamera: cameraNum]"! !

!CameraInterface class methodsFor: 'utilities' stamp: 'eem 14/Mar/2021, 7:51:00 am (UTC)'!
cameraDevices
	"CameraInterface cameraDevices"
	^Array streamContents:
		[:s| | i |
		i := 1.
		[(self cameraName: i)
			ifNotNil: [:cameraName| s nextPut: cameraName. true]
			ifNil: [false]] whileTrue: [i := i + 1]]! !

!CameraInterface class methodsFor: 'utilities' stamp: 'eem 16/Oct/2020, 3:33:00 am (UTC)'!
trySetSemaphoreForCamera: camNum
	"Attempt to set a semaphore to be signalled when a frame is available.
	 Fail silently.  Use e.g. waitForCameraStart: or waitForNextFrame: to
	 access the semaphore if available."
	| semaphore |
	Smalltalk registerExternalObject: (semaphore := Semaphore new).
	[CameraInterface camera: camNum setSemaphore: semaphore]
		on: Error
		do: [:err|
			Smalltalk unregisterExternalObject: semaphore]! !

!CameraInterface class methodsFor: 'utilities' stamp: 'len 23/Jun/2026, 7:54:47 am (UTC)'!
waitForCameraStart: camNum
	"Wait for the camera to get it's first frame (indicated by a non-zero frame extent. Timeout after two seconds."
	"self waitForCameraStart: 1"

	| startTime |
	(self cameraGetSemaphore: camNum) ifNotNil:
		[:semaphoreIndex|
		(Smalltalk externalObjects at: semaphoreIndex ifAbsent: [self error: 'seriously?!!?!!?!!?']) wait.
		^self].
	startTime := Time localMicrosecondClock.
	[(self packedFrameExtent: camNum) > 0 ifTrue: [^ self].
	 (Time localMicrosecondClock - startTime) < 2000000] whileTrue:
		[(Delay forMilliseconds: 50) wait]! !

!CameraInterface class methodsFor: 'utilities' stamp: 'len 23/Jun/2026, 1:24:04 pm (UTC)'!
waitForNextFrame: camNum timeout: timeoutms
	"Wait for the camera to get it's first frame (indicated by a non-zero frame extent. Timeout after two seconds."
	"self waitForNextFrame: 1 timeout: 2000"

	| now timeoutusecs |
	(self cameraGetSemaphore: camNum) ifNotNil:
		[:semaphoreIndex|
		(Smalltalk externalObjects at: semaphoreIndex ifAbsent: [self error: 'seriously?!!?!!?!!?']) waitTimeoutMSecs: timeoutms.
		^self].
	now := Time localMicrosecondClock.
	timeoutusecs := timeoutms * 1000.
	[(self camera: camNum getParam: 1) > 0 ifTrue: [^self].
	 (Time localMicrosecondClock - now) < timeoutusecs] whileTrue:
		[(Delay forMilliseconds: 50) wait]! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 13/Feb/2023, 5:19:00 am (UTC)'!
camera: cameraNum getParam: paramNum
	"Answer the requested parameter value of the given camera.
		param 1 is the frame count, the number of frames grabbed since the last send of getFrameForCamera:into:
		param 2 is the size of the bitmap in bytes required for an image
		param 3 is whether the image is being mirrored or not"

	<primitive: 'primGetParam' module: 'CameraPlugin' error: ec>
	self primitiveFailed! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 12/Apr/2021, 6:33:00 am (UTC)'!
camera: cameraNum setFrameBuffer: frameBuffer
	"Set a pinned non-pointer object as the frame buffer for the camera.
	 Fail if cameraNum does not reference an open camera, or if the buffer is not large enough."
	<primitive: 'primSetCameraBuffers' module: 'CameraPlugin' error: ec>
	^self primitiveFailed! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 12/Apr/2021, 6:35:00 am (UTC)'!
camera: cameraNum setFrameBufferA: frameBufferA B: frameBufferBOrNil
	"Set a pair of pinned non-pointer objects as the frame buffers for the camera.
	 If both are non-nil the plugin will fill them alternating between first frameBufferA and second frameBufferBOrNil.
	 Fail if frameBufferBOrNil is not nil and a different size from frameBufferA.
	 Fail if cameraNum does not reference an open camera, or if the buffers are not large enough."
	<primitive: 'primSetCameraBuffers' module: 'CameraPlugin' error: ec>
	^self primitiveFailed! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 13/Feb/2023, 5:20:00 am (UTC)'!
camera: cameraNum setParam: paramNum to: anIntegerOrBoolean
	"Set the requested parameter value of the given camera, and answer its previous value.
		param 1 cannot be set; it is the frame count, the number of frames grabbed since the last send of getFrameForCamera:into:
		param 2 cannot be set; it is the size of the bitmap in bytes required for an image
		param 3 is whether the image is being mirrored or not"

	<primitive: 'primSetParam' module: 'CameraPlugin' error: ec>
	self primitiveFailed! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 7/Oct/2020, 8:35:00 am (UTC)'!
camera: cameraNum setSemaphore: semaphoreIndex
	"Set an external semaphore index through which to signal that a frame is available.
	 Fail if cameraNum does not reference an open camera, or if the platform does not
	 support interrupt-driven frame receipt."
	<primitive: 'primSetCameraSemaphore' module: 'CameraPlugin' error: ec>
	^self primitiveFailed! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 13/Feb/2023, 5:27:00 am (UTC)'!
cameraGetLatestBufferIndex: cameraIndex
	"If double-buffering is in effect (set via primSetCameraBuffers) answer
	 which buffer contains the freshest data, either A (1) or B (2). If no buffer
	 has been filled yet, answer nil.  Otherwise fail with an appropriate error code."
	<primitive: 'primGetLatestBufferIndex' module: 'CameraPlugin' error: ec>
	^self primitiveFailed

	"CameraInterface cameraGetLatestBufferIndex: 1"! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 16/Oct/2020, 3:19:00 am (UTC)'!
cameraGetSemaphore: cameraNum
	"Answer the external semaphore index through which to signal that a frame is available.
	 Fail if cameraNum has not had a semaphore index set, or is otherwise invalid.
	 Answer nil on failure for convenience."
	<primitive: 'primGetCameraSemaphore' module: 'CameraPlugin' error: ec>
	^nil! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'kfr 14/Oct/2011, 4:58:00 pm (UTC)'!
cameraIsAvailable
	"Answer true if at least one camera is available."

	^(self cameraName: 1) notNil
! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'jm 9/Sep/2004, 7:51:00 am (UTC)'!
cameraIsOpen: cameraNum
	"Answer true if the camera is open."

	^ (self packedFrameExtent: cameraNum) > 0
! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'jm 1/Sep/2004, 9:37:00 am (UTC)'!
cameraName: cameraNum
	"Answer the name of the given camera. Answer nil if there is no camera with the given number."

	<primitive: 'primCameraName' module: 'CameraPlugin'>
	^ nil
! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 15/Oct/2020, 12:33:00 pm (UTC)'!
cameraUID: cameraNum
	"Answer the unique ID of the given camera. Answer nil if there is no camera with the given number."

	<primitive: 'primCameraUID' module: 'CameraPlugin'>
	^ nil

	"CameraInterface cameraUID: 1"! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'eem 16/Oct/2020, 3:48:00 am (UTC)'!
closeCamera: cameraNum
	"Close the camera. Do nothing if it was not open. Unregister any associated semaphore."

	(self cameraGetSemaphore: cameraNum) ifNotNil:
		[:semaphoreIndex|
		Smalltalk unregisterExternalObject: (Smalltalk externalObjectAt: semaphoreIndex ifAbsent: nil)].
	self primitiveCloseCamera: cameraNum! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'jm 31/Aug/2004, 5:54:00 am (UTC)'!
frameExtent: cameraNum
	"Answer the frame extent of the currently open camera, or zero if the camera isn't open."

	| packedExtent |
	packedExtent := self packedFrameExtent: cameraNum.
	^ (packedExtent bitShift: -16) @ (packedExtent bitAnd: 16rFFFF) ! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'jm 13/May/2010, 2:09:00 am (UTC)'!
getFrameForCamera: cameraNum into: aBitmap
	"Copy a camera frame into the given Bitmap. The Bitmap should be a Form of depth 32 that is the same width and height as the current camera frame. Fail if the camera is not open or if the bitmap is not the right size. If successful, answer the number of frames received from the camera since the last call. If this is zero, then there has been no change."

	<primitive: 'primGetFrame' module: 'CameraPlugin'>
	^ 0! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'jm 5/Sep/2005, 8:02:00 am (UTC)'!
openCamera: cameraNum width: frameWidth height: frameHeight
	"Open the given camera requesting the given frame dimensions. The camera number is usually 1 since you typically have only one camera plugged in. If the camera does not support the exact frame dimensions, an available frame size with width >= the requested width is selected."

	<primitive: 'primOpenCamera' module: 'CameraPlugin'>
	^ nil
! !

!CameraInterface class methodsFor: 'camera ops' stamp: 'jm 31/Aug/2004, 5:53:00 am (UTC)'!
packedFrameExtent: cameraNum
	"Answer the extent of the currently open camera packed in an integer. The top 16 bits are the width, the low 16 bits are the height. Answer zero if the camera isn't open."

	<primitive: 'primFrameExtent' module: 'CameraPlugin'>
	^ 0
! !

!CameraInterface class methodsFor: 'test' stamp: 'eem 12/Apr/2021, 7:39:00 am (UTC)'!
bufferedInterruptDrivenVideoTest: camNum
	"A quick test of video input. Displays video on the screen until the mouse is pressed.
	 Answer nil if the interrupt-driven interface is unavailable."
	"self bufferedInterruptDrivenVideoTest: 1"
	"self bufferedInterruptDrivenVideoTest: 2"
	"[self bufferedInterruptDrivenVideoTest: 2] fork.
	  self bufferedInterruptDrivenVideoTest: 1"

	| semaphore height frameExtent frameBuffer |
	height := 16.
	1 to: camNum - 1 do:
		[:camIndex| "N.B. the extent of an unopened camera is 0@0"
		height := height + (self frameExtent: camIndex) y + 16].
	(self cameraIsOpen: camNum) ifFalse:
		[(self openCamera: camNum width: 352 height: 288) ifNil:
			[self inform: 'no camera'.
			 ^nil]].
	frameExtent := self frameExtent: camNum.
	frameBuffer := Form extent: frameExtent depth: 32.
	frameBuffer bits pin.
	self camera: camNum setFrameBuffer: frameBuffer bits.
		 
	semaphore := Semaphore new.
	[self camera: camNum setSemaphore: (Smalltalk registerExternalObject: semaphore)]
		on: Error
		do: [:err|
			Smalltalk unregisterExternalObject: semaphore.
			self inform: 'interrupt-driven camera interface unavailable: ', err messageText.
			^nil].
	[| n startTime frameCount msecs fps |
	 [semaphore wait.
	  startTime ifNil:
		[frameCount := 0.
		 frameExtent := self frameExtent: camNum.
		"N.B. the actual frame size may not be determined until delivery of the first frame.
		 So resize the form if necessary."
		 frameExtent ~= frameBuffer extent ifTrue:
			[frameBuffer := Form extent: frameExtent depth: 32 bits: frameBuffer bits].
		 startTime := Time millisecondClockValue].
	  Sensor anyButtonPressed] whileFalse:
		[n := self getFrameForCamera: camNum into: frameBuffer bits.
		n > 0 ifTrue:
			[frameCount := frameCount + 1.
			 frameBuffer displayAt: 16 @ height]].
	 msecs := Time millisecondClockValue - startTime.
	 fps := (frameCount * 1000) // msecs.
	 ^(self cameraName: camNum), ': ', frameExtent printString, ' ', frameCount printString, ' frames at ', fps printString, ' frames/sec']
		ensure:
			[self closeCamera: camNum.
			 Smalltalk unregisterExternalObject: semaphore.
			 Sensor waitNoButton]! !

!CameraInterface class methodsFor: 'test' stamp: 'eem 12/Apr/2021, 7:28:00 am (UTC)'!
interruptDrivenVideoTest: camNum
	"A quick test of video input. Displays video on the screen until the mouse is pressed.
	 Answer nil if the interrupt-driven interface is unavailable."
	"self interruptDrivenVideoTest: 1"
	"self interruptDrivenVideoTest: 2"
	"[self interruptDrivenVideoTest: 2] fork.
	  self interruptDrivenVideoTest: 1"

	| semaphore height frameExtent |
	height := 16.
	1 to: camNum - 1 do:
		[:camIndex| "N.B. the extent of an unopened camera is 0@0"
		height := height + (self frameExtent: camIndex) y + 16].
	(self cameraIsOpen: camNum) ifFalse:
		[(self openCamera: camNum width: 352 height: 288) ifNil:
			[self inform: 'no camera'.
			 ^nil]].
	semaphore := Semaphore new.
	[self camera: camNum setSemaphore: (Smalltalk registerExternalObject: semaphore)]
		on: Error
		do: [:err|
			Smalltalk unregisterExternalObject: semaphore.
			self inform: 'interrupt-driven camera interface unavailable: ', err messageText.
			^nil].
	[| f n startTime frameCount msecs fps |
	 [semaphore wait.
	 "N.B. the frame extent may not be known until the delivery of the first frame.
	  So we have to delay initialization."
	  startTime ifNil:
		[(frameExtent := self frameExtent: camNum) x = 0 ifTrue: [self inform: 'no camera'. ^nil].
		 f := Form extent: (self frameExtent: camNum) depth: 32.
		 frameCount := 0.
		 startTime := Time millisecondClockValue].
	  Sensor anyButtonPressed] whileFalse:
		[n := self getFrameForCamera: camNum into: f bits.
		n > 0 ifTrue:
			[frameCount := frameCount + 1.
			 f displayAt: 16 @ height]].
	 msecs := Time millisecondClockValue - startTime.
	 fps := (frameCount * 1000) // msecs.
	 ^(self cameraName: camNum), ': ', frameExtent printString, ' ', frameCount printString, ' frames at ', fps printString, ' frames/sec']
		ensure:
			[self closeCamera: camNum.
			 Smalltalk unregisterExternalObject: semaphore.
			 Sensor waitNoButton]! !

!CameraInterface class methodsFor: 'test' stamp: 'len 23/Jun/2026, 7:56:12 am (UTC)'!
videoTest: camNum
	"A quick test of video input. Displays video on the screen until the mouse is pressed."
	"self videoTest: 1"
	"self videoTest: 2"

	| frameExtent f n startTime frameCount msecs fps |
	(self cameraIsOpen: camNum) ifFalse:
		[(self openCamera: camNum width: 320 height: 240) ifNil:
			[self inform: 'no camera'.
			 ^nil]].
	self waitForCameraStart: camNum.
	(frameExtent := self frameExtent: camNum) x = 0 ifTrue: [^ self inform: 'no camera'].
	f := Form extent: (self frameExtent: camNum) depth: 32.
	frameCount := 0.
	startTime := nil.
	[Sensor isAnyButtonPressed] whileFalse:
		[n := self getFrameForCamera: camNum into: f bits.
		n > 0 ifTrue:
			[startTime ifNil: [startTime := Time millisecondClockValue].
			frameCount := frameCount + 1.
			f display]].
	Sensor waitNoButton.
	msecs := Time millisecondClockValue - startTime.
	self closeCamera: camNum.
	fps := frameCount * 1000 // msecs.
	^(self cameraName: camNum), ': ', frameExtent printString, ' ', frameCount printString, ' frames at ', fps printString, ' frames/sec'! !

!CameraInterface class methodsFor: 'private' stamp: 'eem 16/Oct/2020, 3:45:00 am (UTC)'!
primitiveCloseCamera: cameraNum
	"Close the camera. Do nothing if it was not open except answering nil."

	<primitive: 'primCloseCamera' module: 'CameraPlugin'>
	^nil! !

!CameraMorph methodsFor: 'accessing' stamp: 'len 23/Jun/2026, 8:44:04 am (UTC)'!
cameraNumber
	^ cameraNumber! !

!CameraMorph methodsFor: 'accessing' stamp: 'len 23/Jun/2026, 8:47:43 am (UTC)'!
cameraNumber: anInteger
	cameraNumber = anInteger ifTrue: [^ self].
	cameraNumber := anInteger.
	self initializeDisplayForm! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 12:52:42 pm (UTC)'!
getMenu
	| menu submenu |
	menu := MenuMorph new defaultTarget: self.
	submenu := MenuMorph new defaultTarget: self.
	menu add: (cameraIsOn ifTrue: ['Turn off'] ifFalse: ['Turn on']) action: #toggleCameraOnOff.
	menu add: (orientation = #natural ifTrue: ['<on>'] ifFalse: ['<off>']), 'Natural Orientation' action: #toggleOrientation.
	menu add: (showFPS ifTrue: ['<on>'] ifFalse: ['<off>']), 'Show FPS' action: #toggleShowFPS.
	#(low medium high hd) with: #('Low (160×120)' 'Medium (320×240)' 'High (640×480)' 'HD (1280×960)')
		do: [:each :label|
			submenu add: (resolution = each ifTrue: ['<yes>'] ifFalse: ['<no>']), label action: #setResolution: argument: each].
	menu add: 'Resolution' subMenu: submenu.
	^ menu! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 11:13:01 am (UTC)'!
off
	self stopStepping.
	cameraIsOn := false.
	"Be careful not to close the camera if any other morphs are using the same camera."
	(self class allSubInstances anySatisfy: [:camera| camera cameraNumber = cameraNumber and: [camera isOn]]) ifFalse: [CameraInterface closeCamera: cameraNumber].
	self redrawNeeded! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 12:21:06 pm (UTC)'!
on
	cameraIsOn ifTrue: [^true].
	(CameraInterface cameraIsOpen: cameraNumber) ifFalse:
		[(CameraInterface openCamera: cameraNumber width: extent x height: extent y) ifNil: [^ false]].
	"The plugin/camera subsystem may end up choosing a different width and height.
	 So use the width and height it has selected; it may not be what was asked for."
	self initializeDisplayForm.
	CameraInterface waitForCameraStart: cameraNumber.
	cameraIsOn := true.
	self startStepping! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 12:53:19 pm (UTC)'!
setResolution: aSymbol
	resolution := aSymbol.
	self initializeDisplayForm! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 8:51:55 am (UTC)'!
toggleCameraOnOff
	cameraIsOn ifTrue:[self off] ifFalse:[self on]! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 12:57:00 pm (UTC)'!
toggleOrientation
	orientation := orientation = #natural ifTrue: [#original] ifFalse: [#natural].
	self redrawNeeded! !

!CameraMorph methodsFor: 'actions' stamp: 'len 23/Jun/2026, 12:20:31 pm (UTC)'!
toggleShowFPS
	showFPS := showFPS not.
	self redrawNeeded! !

!CameraMorph methodsFor: 'drawing' stamp: 'len 23/Jun/2026, 1:20:08 pm (UTC)'!
drawCameraImageOn: aCanvas
	| scale offset |
	scale := 1 @ 1.
	offset := 0 @ 0.
	extent = displayForm extent ifFalse: [scale := extent / displayForm extent].
	orientation == #natural
		ifTrue: [scale := scale x negated @ scale y.
			offset := extent x @ 0].
	1 @ 1 = scale
		ifTrue: [aCanvas image: displayForm at: offset]
		ifFalse:
			[| originalTransformation |
			originalTransformation := aCanvas currentTransformation copy.
			aCanvas geometryTransformation: (aCanvas currentTransformation translatedBy: offset; scaledBy: scale).
			aCanvas image: displayForm at: 0 @ 0.
			aCanvas geometryTransformation: originalTransformation]! !

!CameraMorph methodsFor: 'drawing' stamp: 'len 23/Jun/2026, 1:08:45 pm (UTC)'!
drawFPSOn: aCanvas
	| font |
	showFPS ifFalse: [^ self].
	font := Preferences windowTitleFont.
	aCanvas
		drawString: 'FPS: ', fps asString
		at: 4 @ (extent y - 2 - font lineSpacing)
		font: font
		color: `Color white alpha: 0.6`! !

!CameraMorph methodsFor: 'drawing' stamp: 'len 23/Jun/2026, 1:21:54 pm (UTC)'!
drawOn: aCanvas 
	cameraIsOn ifFalse:
		[| turnOn |
		 turnOn := displayForm isNil.
		 self initializeDisplayForm.
		 turnOn ifTrue: [self on]].
	cameraIsOn ifTrue:
		[(CameraInterface frameExtent: cameraNumber) ~= displayForm extent ifTrue:
			[self initializeDisplayForm]].
	displayForm ifNil: [self initializeDisplayForm].
	cameraIsOn
		ifTrue: [self drawCameraImageOn: aCanvas; drawFPSOn: aCanvas]
		ifFalse:
			[aCanvas
				fillRectangle: self localBounds color: (Color gray alpha: 0.5);
				drawString: 'Off' at: 4 @ 2 font: Preferences windowTitleFont color: Color white]! !

!CameraMorph methodsFor: 'drawing' stamp: 'len 23/Jun/2026, 1:02:12 pm (UTC)'!
requiresVectorCanvas
	^ true ! !

!CameraMorph methodsFor: 'event handling testing' stamp: 'len 23/Jun/2026, 1:03:54 pm (UTC)'!
handlesMouseDown: aMouseButtonEvent
	^ aMouseButtonEvent mouseButton2Pressed! !

!CameraMorph methodsFor: 'initialization' stamp: 'len 23/Jun/2026, 11:21:38 am (UTC)'!
initialize
	super initialize.
	cameraNumber := 1.
	cameraIsOn := false.
	showFPS := false.
	captureDelayMs := 16. "stepTime"	
	fps := 60. "guess."
	lastDisplayTime := 0.
	framesSinceLastDisplay := 0.
	resolution := #medium.
	orientation := #natural.
	self morphExtent: (self extentForResolutionName: resolution)! !

!CameraMorph methodsFor: 'initialization' stamp: 'len 23/Jun/2026, 8:48:45 am (UTC)'!
intoWorld: aWorld
	super intoWorld: aWorld.
	cameraIsOn ifTrue: [self on] ifFalse:[self off].
"	self removeActionsForEvent: #aboutToEnterWorld.
	aWorld
		when: #aboutToLeaveWorld
		send: #outOfWorld:
		to: self
		with: aWorld."! !

!CameraMorph methodsFor: 'stepping' stamp: 'len 23/Jun/2026, 11:11:11 am (UTC)'!
step
	cameraIsOn ifFalse: [self stopStepping].
	self updateDisplay! !

!CameraMorph methodsFor: 'stepping' stamp: 'len 23/Jun/2026, 7:50:37 am (UTC)'!
stepTime
	"Answer the desired time between steps in milliseconds"
	^ captureDelayMs! !

!CameraMorph methodsFor: 'stepping' stamp: 'len 23/Jun/2026, 8:36:11 am (UTC)'!
wantsSteps
	^ true! !

!CameraMorph methodsFor: 'structure' stamp: 'len 23/Jun/2026, 7:41:22 am (UTC)'!
delete
	self off.
	super delete! !

!CameraMorph methodsFor: 'testing' stamp: 'len 23/Jun/2026, 11:13:01 am (UTC)'!
isOn
	^ cameraIsOn! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 8:45:36 am (UTC)'!
decreaseCaptureDelay
	captureDelayMs := captureDelayMs - 1 max: 10! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 8:18:46 am (UTC)'!
extentForResolutionName: aSymbol
	^ {160 @ 120. 320 @ 240. 640 @ 480. 1280 @ 960} at: (#(low medium high hd) indexOf: aSymbol ifAbsent: [^ 320@240])! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 8:45:23 am (UTC)'!
increaseCaptureDelay
	captureDelayMs := captureDelayMs + 1 min: 200! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 12:12:44 pm (UTC)'!
initializeDisplayForm
	| displayExtent cameraExtent formExtent |
	displayExtent := self extentForResolutionName: resolution.
	cameraExtent := CameraInterface frameExtent: cameraNumber.
	cameraExtent isZero 
		ifTrue: [formExtent := displayExtent]
		ifFalse: [ | camRatio frameRatio |
			formExtent := cameraExtent.
			camRatio := cameraExtent x / cameraExtent y.
			frameRatio := displayExtent x / displayExtent y.
			camRatio ~= frameRatio ifTrue: [displayExtent := displayExtent x @ (displayExtent x * camRatio reciprocal)]].
	displayForm := Form extent: formExtent depth: 32.
	self morphExtent: displayExtent! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 8:40:02 am (UTC)'!
nextFrame
	| frameCount |
	frameCount := CameraInterface getFrameForCamera: cameraNumber into: displayForm bits.
	frameCount = 0 ifTrue: [self increaseCaptureDelay].
	frameCount > 2 ifTrue: [self decreaseCaptureDelay].
	framesSinceLastDisplay := framesSinceLastDisplay + frameCount! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 8:19:15 am (UTC)'!
outOfWorld: aWorld

	super outOfWorld: aWorld.
	cameraIsOn ifTrue: [self off. cameraIsOn := true].
	aWorld
		when: #aboutToEnterWorld
		send: #intoWorld:
		to: self
		with: aWorld.! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 8:37:12 am (UTC)'!
updateDisplay
	cameraIsOn ifTrue:[self nextFrame].
	self updateFPS.
	self redrawNeeded! !

!CameraMorph methodsFor: 'private' stamp: 'len 23/Jun/2026, 7:51:06 am (UTC)'!
updateFPS
	| now mSecs |
	now := Time millisecondClockValue.
	mSecs := now - lastDisplayTime.
	(mSecs > 500 or: [mSecs < 0 "clock wrap-around"])
		ifTrue: [
			fps := (framesSinceLastDisplay * 1000) // mSecs.
			lastDisplayTime := now.
			framesSinceLastDisplay := 0]! !

!CameraMorph class methodsFor: 'as yet unclassified' stamp: 'len 23/Jun/2026, 8:11:18 am (UTC)'!
allOff
	self allInstancesDo: [:each | each off]! !
