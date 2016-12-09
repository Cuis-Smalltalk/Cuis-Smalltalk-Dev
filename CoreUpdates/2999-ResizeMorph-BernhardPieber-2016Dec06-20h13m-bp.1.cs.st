'From Cuis 5.0 of 7 November 2016 [latest update: #2995] on 6 December 2016 at 8:16:54 pm'!
!classDefinition: #ResizeMorph category: #'Morphic-Views'!
RectangleLikeMorph subclass: #ResizeMorph
	instanceVariableNames: 'gridLineWidth gridColor selectionColor outlineMorph grid from to action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!SystemWindow methodsFor: 'resize/collapse' stamp: 'bp 10/18/2015 12:18'!
resize
	| resizeMorph |
	resizeMorph _ ResizeMorph new morphExtent: 200@150.
	resizeMorph action: [self resize: (resizeMorph selectionRectangle: Display extent)].
	resizeMorph morphPosition: self world activeHand morphPosition.
	resizeMorph openInWorld
	! !


!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 22:22'!
action: aBlock
	action _ aBlock! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/18/2015 18:00'!
drawGridOn: aCanvas
	0 to: grid x do: [:i |
		| x |
		x _ i * (extent x - gridLineWidth) / grid x.
		aCanvas line: x @ 0 to: x @ (extent y - 2) width: gridLineWidth color: gridColor].
	0 to: grid y do: [:i |
		| y |
		y _ i * (extent y - gridLineWidth) / grid y.
		aCanvas line: 0 @ y to: (extent x - 2) @ y width: gridLineWidth color: gridColor]! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 23:02'!
drawOn: aCanvas
	super drawOn: aCanvas.
	from ifNotNil: [aCanvas fillRectangle: (self selectionRectangle: extent) color: selectionColor].
	self drawGridOn: aCanvas! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 19:00'!
grid: aPoint
	grid _ aPoint! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 19:00'!
handlesMouseDown: aMouseButtonEvent
	^true! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/17/2015 15:51'!
initialize
	super initialize.
	extent _ 400@300.
	color _ Color white.
	grid _ 8@6.
	gridLineWidth _ 2.
	gridColor _ Color black.
	selectionColor _ Color red! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 23:18'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	from _ self toGridPoint: localEventPosition.
	outlineMorph _ BorderedRectMorph new
		borderColor: Color black;
		color: Color transparent;
		openInWorld;
		hide.
	self selectTo: localEventPosition! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 23:17'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	self selectTo: localEventPosition.
	outlineMorph delete.
	action ifNotNil: [
		action value.
		self delete]! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 21:32'!
mouseMove: aMouseButtonEvent localPosition: localEventPosition
	self selectTo: localEventPosition! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 21:25'!
printOn: aStream
	super printOn: aStream.
	aStream space; print: from; space; print: to! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 23:14'!
selectTo: localEventPosition
	| newTo |
	newTo _ self toGridPoint: localEventPosition.
	newTo ~= to ifTrue: [
		to _ newTo.
		self redrawNeeded.
		self updateOutlineMorph]! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/17/2015 15:35'!
selectionRectangle: aRectangle
	^(from corner: to + 1) scaledBy: aRectangle // grid! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/17/2015 15:34'!
toGridPoint: aPoint
	^(aPoint min: extent - 1) // (extent // grid)! !

!ResizeMorph methodsFor: 'as yet unclassified' stamp: 'bp 10/11/2015 23:15'!
updateOutlineMorph
	| rectangle |
	rectangle _ self selectionRectangle: Display extent.
	outlineMorph
		morphPosition: rectangle origin extent: rectangle extent;
		show! !


!SystemWindow methodsFor: 'menu' stamp: 'bp 10/11/2015 21:42'!
buildWindowMenu

	| aMenu |

	aMenu _ MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' action: #relabel;
		add: 'window color...' action: #setWindowColor;
		addLine;
		add: 'send to back' action: #sendToBack;
		add: 'make next-to-topmost' action: #makeSecondTopmost;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ]) action: #toggleStickiness;
		addLine;
		add: 'close' action: #delete;
		add: 'collapse' action: #collapse;
		add: 'expand / contract' action: #expandBoxHit;
		addLine;
		add: 'resize...' action: #resize;
		add: 'resize full' action: #resizeFull;
		add: 'resize top' action: #resizeTop;
		add: 'resize left' action: #resizeLeft;
		add: 'resize bottom' action: #resizeBottom;
		add: 'resize right' action: #resizeRight;
		add: 'resize top left' action: #resizeTopLeft;
		add: 'resize top right' action: #resizeTopRight;
		add: 'resize bottom left' action: #resizeBottomLeft;
		add: 'resize bottom right' action: #resizeBottomRight.

	^ aMenu! !

