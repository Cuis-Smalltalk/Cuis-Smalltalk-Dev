'From Cuis7.9 [latest update: #8310] on 3 November 2026 at 5:25:59 pm'!
!classDefinition: #Duration category: #'Kernel-Chronology'!
Magnitude subclass: #Duration
	instanceVariableNames: 'nanos seconds '
	classVariableNames: 'Zero '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type '
	classVariableNames: 'NilObject '
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'jmv 3/Nov/2026, 7:53:45 pm (UTC)'!
shouldKeepSelection

	^selectionToKeep ~= (0 to: 0)! !


!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'jmv 3/Nov/2026, 7:53:54 pm (UTC)'!
isLookingInMethodNode

	^ (selectionToKeep = (0 to: 0)) not ! !


!Cursor class methodsFor: 'instance creation' stamp: 'jmv 3/Nov/2026, 7:42:42 pm (UTC)'!
resizeLeftCursor
	^ (Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0000010010000000
		2r0000010010000000
		2r0000010010000000
		2r0001010010100000
		2r0011010010110000
		2r0111010010111000
		2r1111110011111100
		2r0111010010111000
		2r0011010010110000
		2r0001010010100000
		2r0000010010000000
		2r0000010010000000
		2r0000010010000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 3/Nov/2026, 7:42:16 pm (UTC)'!
resizeTopCursor
	^ (Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000100000000
		2r0000001110000000
		2r0000011111000000
		2r0000111111100000
		2r0000000100000000
		2r0111111111111100
		2r0000000000000000
		2r0000000000000000
		2r0111111111111100
		2r0000000100000000
		2r0000111111100000
		2r0000011111000000
		2r0000001110000000
		2r0000000100000000
		2r0000000000000000
		2r0000000100000000)
	offset: `-7@ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 3/Nov/2026, 7:42:09 pm (UTC)'!
resizeTopLeftCursor
	^ (Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0111110000010000
		2r0111100000100000
		2r0111000001000100
		2r0110100010001000
		2r0100010100010000
		2r0000001000100000
		2r0000010001000000
		2r0000100010000000
		2r0001000101000100
		2r0010001000101100
		2r0000010000011100
		2r0000100000111100
		2r0000000001111100
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask.! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 3/Nov/2026, 7:42:02 pm (UTC)'!
resizeTopRightCursor
	^ (Cursor
	small1BitExtent: `16@16`
	pixelBits: #(
		2r0000000000000000
		2r0001000001111100
		2r0000100000111100
		2r0100010000011100
		2r0010001000101100
		2r0001000101000100
		2r0000100010000000
		2r0000010001000000
		2r0000001000100000
		2r0100010100010000
		2r0110100010001000
		2r0111000001000000
		2r0111100000100000
		2r0111110000000000
		2r0000000000000000
		2r0000000000000000)
	offset: `-7@ -7`) withMask.! !


!Timespan class methodsFor: 'instance creation' stamp: 'jmv 3/Nov/2026, 7:44:36 pm (UTC)'!
includingDateAndTime: aDateAndTime

	^ self starting: aDateAndTime duration: Duration zero! !


!CompiledMethod methodsFor: 'method node' stamp: 'jmv 3/Nov/2026, 7:54:04 pm (UTC)'!
sourceRangesOfNodesEquivalentTo: nodesToLookFor 
	
	^self sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: (0 to: 0)! !

!CompiledMethod methodsFor: 'method node' stamp: 'jmv 3/Nov/2026, 7:54:11 pm (UTC)'!
sourceRangesOfParameterizableNodesOf: nodesToLookFor rejectingIntervals: rejectingIntervals
	
	^ self 
		sourceRangesOfParameterizableNodesOf: nodesToLookFor 
		keepingSelection: (0 to: 0)
		rejectingIntervals: rejectingIntervals! !


!Transcripter class methodsFor: 'utilities' stamp: 'jmv 3/Nov/2026, 8:06:48 pm (UTC)'!
emergencyEvaluator
	(Transcripter newInFrame: (0@0 corner: 320@200))
		show: 'Type ''exit'' to exit the emergency evaluator.';
		readEvalPrint! !


!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 3/Nov/2026, 7:44:55 pm (UTC)'!
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.
	tag := self.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | 
			e tag == tag 
				ifTrue:[ timeoutBlock value ]
				ifFalse:[ e pass]].! !


!DateAndTime class methodsFor: 'constants' stamp: 'jmv 3/Nov/2026, 7:44:48 pm (UTC)'!
unixEpoch
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	(DateAndTime now - DateAndTime unixEpoch) days / 365.25
	"
	^ self
		julianDayNumber: 2440588
		seconds: 0
		nanoseconds: 0
		offset: Duration zero! !


!Duration class methodsFor: 'ansi protocol' stamp: 'jmv 3/Nov/2026, 7:44:13 pm (UTC)'!
zero
	Zero ifNil: [
		Zero := self basicNew seconds: 0 nanoSeconds: 0 ].
	^Zero
! !


!Number methodsFor: 'truncation and round off' stamp: 'jmv 3/Nov/2026, 7:51:34 pm (UTC)'!
roundedHTE
	"Answer the integer nearest the receiver.
		(-3/2) roundedHTE. -2 .
		(-5/2) roundedHTE. -2 .
		(3/2) roundedHTE. 2 .
		(5/2) roundedHTE. 2 .
	See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even
	
	See more detailed comment at Float >> #roundedHTE"

	| truncated fractionPartAbs |
	truncated := self truncated.
	fractionPartAbs := (self-truncated) abs.
	fractionPartAbs = (1/2)
		ifTrue: [ truncated even ifTrue: [^truncated] ifFalse: [^truncated + self sign]].
	fractionPartAbs < (1/2)
		ifTrue: [^ truncated]
		ifFalse: [^ truncated + self sign]! !


!InputSensor methodsFor: 'keyboard' stamp: 'jmv 3/Nov/2026, 8:06:24 pm (UTC)'!
keyboardTest
	"
	Sensor keyboardTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char := nil.
	[ char == $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char := self keyboard.
		Display fill: (5@5 extent: 600@50) fillColor: `Color white`.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				nextPutAll: char printString;
				space;
				nextPut: $(;
				print: char codePoint;
				nextPut: $);
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ].
			Time now printOn: s
			])
		displayAt: 10 @ 10 ].! !


!Morph methodsFor: 'drawing' stamp: 'jmv 3/Nov/2026, 8:06:00 pm (UTC)'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: (-75 @ -70 corner: 75 @ 70)
		color: `Color blue`! !

!Morph methodsFor: 'geometry' stamp: 'jmv 3/Nov/2026, 8:05:45 pm (UTC)'!
localBoundsForError
	"Only to be used for drawing in an error condition. See senders."

	^(0@0 corner: 75@70).! !


!Sample11BarnsleyFern methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:02:54 pm (UTC)'!
initialize
	"See https://en.wikipedia.org/wiki/Barnsley_fern "
	super initialize.
	
	probabilities := #[ 0.01 0.85 0.07 0.07 ].
	affineTransformations := {
		"Stem"
		AffineTransformation from:
			#[ 	0.00 	0.00 	0.00
				0.00 	0.16 	0.00 ].
		"Successively smaller leaflets"
		AffineTransformation from:
			#[ 	0.85 	0.04 	0.00
				-0.04 	0.85 	1.60 ].
		"Largest left-hand leaflet"
		AffineTransformation from:
			#[ 	0.20 	-0.26 	0.00
				0.23 	0.22 	1.60 ].
		"Largest right-hand leaflet "
		AffineTransformation from:
			#[ 	-0.15 	0.28 	0.00
				0.26 	0.24 	0.44 ] }.! !


!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/Nov/2026, 8:06:11 pm (UTC)'!
fullyCoveredRectangle
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	This implementation is a good optimization for many common cases."

	| b e |
	"If we can't know better."
	self isOrAnyOwnerIsRotated ifTrue: [
		^ nil ].
	color mightBeTranslucent ifTrue: [
		^ nil ].
	b := self displayBounds.
	b isNil ifTrue: [
		^nil ].
	b := b insetBy: (2@2 corner: 1@1).
	Theme current roundWindowCorners ifFalse: [
		^ b ].
	e := (self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling.
	^ b origin + e corner: b corner! !


!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 7:59:43 pm (UTC)'!
initializeBottom
	selector := #windowBottom:.
	cursorKey := #resizeBottomCursor.
	self setBalloonText: 

'Bottom resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s bottom edge.
	
', 'Resize to bottom screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom half.

', 'Move to bottom screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom edge.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 7:59:52 pm (UTC)'!
initializeBottomLeft
	selector := #windowBottomLeft:.
	cursorKey := #resizeBottomLeftCursor.
	self setBalloonText: 

'Bottom Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to bottom left corner.
	
', 'Resize to bottom left screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom left quadrant.

', 'Move to bottom left screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom left corner.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 7:59:57 pm (UTC)'!
initializeBottomRight
	selector := #windowBottomRight:.
	cursorKey := #resizeBottomRightCursor.
	self setBalloonText: 

'Bottom Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to bottom right corner.
	
', 'Resize to bottom right screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom right quadrant.

', 'Move to bottom right screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom right corner.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:00:01 pm (UTC)'!
initializeLeft
	selector := #windowLeft:.
	cursorKey := #resizeLeftCursor.
	self setBalloonText: 

'Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s left edge.
	
', 'Resize to left screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screens''s left half.

', 'Move to left screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s left edge.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:00:06 pm (UTC)'!
initializeRight
	selector := #windowRight:.
	cursorKey := #resizeRightCursor.
	self setBalloonText: 

'Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to right edge.
	
', 'Resize to right screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s right half.

', 'Move to right screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s right edge.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:00:10 pm (UTC)'!
initializeTop
	selector := #windowTop:.
	cursorKey := #resizeTopCursor.
	self setBalloonText: 

'Top resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s top edge.
	
', 'Resize to top screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screens''s top half.

', 'Move to top screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top edge.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:00:15 pm (UTC)'!
initializeTopLeft
	selector := #windowTopLeft:.
	cursorKey := #resizeTopLeftCursor.
	self setBalloonText: 

'Top Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to top left corner.
	
', 'Resize to top left screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s top left quadrant.

', 'Move to top left screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top left corner.'! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:00:19 pm (UTC)'!
initializeTopRight
	selector := #windowTopRight:.
	cursorKey := #resizeTopRightCursor.
	self setBalloonText: 

'Top Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to top right corner.
	
', 'Resize to top right screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s top right quadrant.

', 'Move to top right screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top right corner.'! !


!IconMorph methodsFor: 'initialization' stamp: 'jmv 3/Nov/2026, 8:06:37 pm (UTC)'!
initialize
	super initialize.
	viewBox := (0.0 @ 0.0 corner: 24.0 @ 24.0).
	path := #().
	color := Color white! !


!LayoutEdgesSpec methodsFor: 'updating' stamp: 'jmv 3/Nov/2026, 8:02:39 pm (UTC)'!
updateHorizontalWeightsAndOffsetsFor: targetBounds in: availableSpace

	| newLeftOffset newLeftWeight newRightOffset newRightWeight |

	((#[0.0 0.5 1.0 ] includes: topLeftEdgesWeight x) or: [topLeftOffset x ~= 0])
		ifTrue: [				"Keep weight, adjust offset."
			newLeftWeight := topLeftEdgesWeight x.
			newLeftOffset := targetBounds left - (availableSpace left + (availableSpace width * newLeftWeight)) ]
		ifFalse: [				"Keep offset, adjust weight."
			newLeftOffset := topLeftOffset x.
			newLeftWeight := (targetBounds left - (availableSpace left + newLeftOffset)) / availableSpace width ].

	(bottomRightEdgesWeight x = topLeftEdgesWeight x)
		ifTrue: [				"Use left weight, adjust offset"
			newRightWeight := newLeftWeight.
			newRightOffset := targetBounds right - (availableSpace left + (availableSpace width * newRightWeight)) ]
		ifFalse: [
			((#[0.0 0.5 1.0 ] includes: bottomRightEdgesWeight x) or: [bottomRightOffset x ~= 0])
				ifTrue: [		"Keep weight, adjust offset"
					newRightWeight := bottomRightEdgesWeight x.
					newRightOffset := targetBounds right - (availableSpace left + (availableSpace width * newRightWeight)) ]
				ifFalse: [		"Keep offset, adjust weight."
					newRightOffset := bottomRightOffset x.
					newRightWeight := (targetBounds right - (availableSpace left + newRightOffset)) / availableSpace width ]].

	topLeftEdgesWeight := newLeftWeight @  topLeftEdgesWeight y.
	topLeftOffset := newLeftOffset @topLeftOffset y.
	bottomRightEdgesWeight := newRightWeight @bottomRightEdgesWeight y.
	bottomRightOffset := newRightOffset @ bottomRightOffset y.! !

!LayoutEdgesSpec methodsFor: 'updating' stamp: 'jmv 3/Nov/2026, 8:03:13 pm (UTC)'!
updateVerticalWeightsAndOffsetsFor: targetBounds in: availableSpace

	| newTopOffset newTopWeight newBottomOffset newBottomWeight |

	((#[0.0 0.5 1.0 ] includes: topLeftEdgesWeight y) or: [topLeftOffset y ~= 0])
		ifTrue: [				"Keep weight, adjust offset."
			newTopWeight := topLeftEdgesWeight y.
			newTopOffset := targetBounds top - (availableSpace top + (availableSpace height * newTopWeight)) ]
		ifFalse: [				"Keep offset, adjust weight."
			newTopOffset := topLeftOffset y.
			newTopWeight := (targetBounds top - (availableSpace top + newTopOffset)) / availableSpace height ].

	(bottomRightEdgesWeight y = topLeftEdgesWeight y)
		ifTrue: [				"Use left weight, adjust offset"
			newBottomWeight := newTopWeight.
			newBottomOffset := targetBounds bottom - (availableSpace top + (availableSpace height * newBottomWeight)) ]
		ifFalse: [
			((#[0.0 0.5 1.0 ] includes: bottomRightEdgesWeight y) or: [bottomRightOffset y ~= 0])
				ifTrue: [		"Keep weight, adjust offset"
					newBottomWeight := bottomRightEdgesWeight y.
					newBottomOffset := targetBounds bottom - (availableSpace top + (availableSpace height * newBottomWeight)) ]
				ifFalse: [		"Keep offset, adjust weight."
					newBottomOffset := bottomRightOffset y.
					newBottomWeight := (targetBounds bottom - (availableSpace top + newBottomOffset)) / availableSpace height ]].

	topLeftEdgesWeight := topLeftEdgesWeight x @ newTopWeight.
	topLeftOffset := topLeftOffset x @ newTopOffset.
	bottomRightEdgesWeight := bottomRightEdgesWeight x @ newBottomWeight.
	bottomRightOffset := bottomRightOffset x @ newBottomOffset.! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 3/Nov/2026, 8:05:36 pm (UTC)'!
boundingRectOfCurrentIfTrivial
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	currentMorph knowsOwnLocalBounds ifTrue: [
		"Include an extra pixel to cover possible anti aliasing."
		^(currentTransformation externalBoundingRectOf: currentMorph localBounds)
			encompassingIntegerRectangle outsetBy: (0@0 corner: 1@1) ].
	^nil.! !


!PseudoClass class methodsFor: 'categories' stamp: 'jmv 3/Nov/2026, 8:00:37 pm (UTC)'!
removedCategoryName
	^ Text string: '*** removed methods ***'  attribute: TextColor red! !


!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'jmv 3/Nov/2026, 7:53:33 pm (UTC)'!
completeSourceRangeOf: aNode

	| nodeRange nodeRanges |
	
	"Some nodes do not have ranges at all like BlockNode - Hernan"
	nodeRanges := sourceMethodNode completeSourceRanges at: aNode ifAbsent: [ #() ].
	nodeRange := nodeRanges at: (sourceRangeIndexByNode occurrencesOf: aNode) ifAbsent: [ 0 to: 0 ].
	nodeRange := nodeRange asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	
	^nodeRange
		! !


!SHRange class methodsFor: 'instance creation' stamp: 'jmv 3/Nov/2026, 7:35:11 pm (UTC)'!
nilObject

	NilObject ifNil: [
		NilObject := SHRange start: 0 end: 0 type: nil ].
	^NilObject! !

!methodRemoval: NodesFinder class #noSelection stamp: 'jmv 3/Nov/2026, 7:54:18 pm (UTC)'!
NodesFinder class removeSelector: #noSelection!
!classDefinition: #Duration category: #'Kernel-Chronology'!
Magnitude subclass: #Duration
	instanceVariableNames: 'nanos seconds'
	classVariableNames: 'Zero'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type'
	classVariableNames: 'NilObject'
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
