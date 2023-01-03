'From Cuis 6.0 [latest update: #5614] on 3 January 2023 at 3:23:35 pm'!

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 1/3/2023 15:22:03'!
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
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
	auxBitBltEngine clipRect: prevClipRect.! !

!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 1/3/2023 15:22:17'!
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
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.
	auxBitBltEngine clipRect: prevClipRect.! !


!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 1/3/2023 15:21:20'!
privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		privateFrameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color green
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect _ engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
	engine clipRect: prevClipRect.
		! !

!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 1/3/2023 15:22:35'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		privateFrameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect _ engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.
	engine clipRect: prevClipRect.! !

!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 1/3/2023 15:21:39'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect _ engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth.
	engine clipRect: prevClipRect.! !


"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
CodePackage installedPackages removeKey: 'VectorEngineInSmalltalk'!
