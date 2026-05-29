'From Cuis7.7 [latest update: #7970] on 29 May 2026 at 12:58:46 pm'!

!Parser methodsFor: 'primitives' stamp: 'eem 1/Dec/2008 09:18:00'!
primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
	"Create named primitive with optional error code."
	
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: { 
		aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ].
		aNameString asSymbol.
		0. 0}.
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117! !


!MessageNode methodsFor: 'printing' stamp: 'jmv 1/Mar/2010 10:20:00'!
printWithClosureAnalysisToDoOn: aStream indent: level

	| limitNode |
	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	limitNode := (arguments last == nil
				or: [arguments last isAssignmentNode not])
					ifTrue: [arguments first]
					ifFalse: [arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printWithClosureAnalysisKeywords: #to:do:
					arguments: {limitNode. (arguments at: 3)}
					on: aStream indent: level]
		ifFalse: [self printWithClosureAnalysisKeywords: selector key
					arguments: {limitNode} , arguments allButFirst
					on: aStream indent: level]! !


!Form methodsFor: 'transitions' stamp: 'jmv 26/Jun/2025 17:51:56'!
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d delay |
	delay := Delay forSeconds: 0.1.
	pageRect := otherImage boundingBox.
	oldPage := self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: `Color black`.
	oldBottom := self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps := 8.
	buffer := Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d := pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
					p := pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: `0@0` rule: Form over.
					p := pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect := pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad := {
			pageRect topLeft.
			pageRect bottomLeft + (0@p y).
			pageRect bottomRight.
			pageRect topRight - (0@p y)}.
		warp := (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceToScreen: leafRect.
		delay wait.
		].

	buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceToScreen.
	delay wait.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"! !


!Rectangle methodsFor: 'accessing' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
corners
	"Return an array of corner points in the order of a quadrilateral spec for WarpBlt"
	^ {self topLeft. self bottomLeft. self bottomRight. self topRight}! !

!Rectangle methodsFor: 'accessing' stamp: 'jmv 12/Mar/2018 15:55:55'!
innerCorners
	"Return an array of inner corner points,
	ie, the most extreme pixels included,
	in the order of a quadrilateral spec for WarpBlt"
	| r1 |
	r1 := self topLeft corner: self bottomRight - `1@1`.
	^ {r1 topLeft. r1 bottomLeft. r1 bottomRight. r1 topRight}! !

