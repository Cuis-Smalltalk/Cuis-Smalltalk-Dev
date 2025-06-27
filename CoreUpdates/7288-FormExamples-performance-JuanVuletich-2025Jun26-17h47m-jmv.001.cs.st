'From Cuis7.5 [latest update: #7285] on 26 June 2025 at 5:52:31 pm'!

!Form methodsFor: 'transitions' stamp: 'jmv 6/26/2025 17:51:56'!
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
		sourceQuad := Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
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

!Form methodsFor: 'transitions' stamp: 'jmv 6/26/2025 17:49:40'!
slideImage: otherImage at: topLeft delta: delta
	"
	Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@ -4
	"
	| bb nSteps clipRect |
	bb := otherImage boundingBox.
	clipRect := topLeft extent: otherImage extent.
	nSteps := 1.
	delta x = 0 ifFalse: [nSteps := nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps := nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do: [ :i | 
			self 
				copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
			Display forceToScreen: clipRect.
		]! !

!Form methodsFor: 'transitions' stamp: 'jmv 6/26/2025 17:49:16'!
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectsOrNil waitTime |
	i := 0.
	clipRect := topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect := clipRect intersect: clipBox ].
	[ rectsOrNil := rectForIndexBlock value: (i := i + 1) ]
		whileNotNil: [
			t := Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over ].
			Display forceToScreen: clipRect.
			waitTime := 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]! !

