'From Cuis 6.0 [latest update: #5549] on 3 November 2022 at 3:24:37 pm'!

!AbstractVectorCanvas methodsFor: 'morphic' stamp: 'jmv 11/3/2022 15:18:45'!
drawCurrentAsOutline

	^false! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/3/2022 15:23:31'!
drawCurrentAsOutline

	currentMorph visible ifTrue: [
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph.
		self privateFrameReverseGlobalRect: currentMorph displayBounds borderWidth: 2 ].
	^true! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 11/3/2022 15:24:04'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"The main canvas should only be drawn by the UI process.
	If some other process has been ser as the UI process, don't continue, as breakage is likely."
	world ifNotNil: [
		world canvas == self ifTrue: [
			UISupervisor isUIProcessRunning ifFalse: [^self]]].
	aMorph visible ifFalse: [^ self].

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
				self canvasToUse drawCurrentAsOutline ifTrue: [
					self outOfMorph.
					^ self]].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !


!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 11/3/2022 15:21:59'!
privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	"
	(VectorCanvas onForm: Display)
		privateFrameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color yellow
		borderWidth: 20
		color: Color green.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect _ auxBitBltEngine clipRect.
	auxBitBltEngine clipRect: rect.
	auxBitBltEngine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
	auxBitBltEngine clipRect: prevClipRect.! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 11/3/2022 15:22:08'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor
	"
	(VectorCanvas onForm: Display)
		privateFrameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect _ auxBitBltEngine clipRect.
	auxBitBltEngine clipRect: rect.
	auxBitBltEngine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.
	auxBitBltEngine clipRect: prevClipRect.! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 11/3/2022 15:22:18'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(VectorCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect _ auxBitBltEngine clipRect.
	auxBitBltEngine clipRect: rect.
	auxBitBltEngine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth.
	auxBitBltEngine clipRect: prevClipRect.! !

