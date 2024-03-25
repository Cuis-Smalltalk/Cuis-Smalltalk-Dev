'From Cuis6.3 [latest update: #6294] on 25 March 2024 at 11:56:19 am'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 3/25/2024 11:39:11'!
displayUnfinishedEntryOnDisplay

	| font count string x y fh r innerR |
	self displayEngine.
	innerR := bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font := FontFamily defaultFamilyAndPointSize.
		fh := font lineSpacing.
		count := innerR height // fh-1.
		x := innerR left.
		string := unfinishedEntry contents.
		y := ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font lineSpacing + innerR top.
		r := innerR left: lastDisplayPosition.
		displayEngine clipRect: r.
		(self drawString: string at: x@y font: font color: `Color darkGray`) ifNotNil: [ :lastPoint |
			lastDisplayPosition := lastPoint x.
			^r ]].
	^nil! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 3/25/2024 11:44:23'!
isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled := aBoolean.
	self color: (aBoolean ifTrue: [`Color black`] ifFalse: [`Color lightGray`]).! !


!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 3/25/2024 11:39:03'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr := self valueOfProperty: #matchString.
	allItems := self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches := allItems select: [ :m |
		isMatch := matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph := self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph := LabelMorph new color: `Color darkGray`.
		self addMorphBack: feedbackMorph lock position: `0@ -20`.
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !


!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 3/25/2024 11:50:07'!
drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness
	
	| height top bottom |

	height := extent y - (2 * scrollbarThickness).
	top := (1.0 * self firstVisible-1 / self entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom := (1.0 * self lastVisible / self entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray`! !


!Sample03Smiley methodsFor: 'drawing' stamp: 'jmv 3/25/2024 11:30:22'!
drawOn: aCanvas
	"Example of the #pathSequence drawing operation, that can represent many drawing operations
			in one compact literal, and draw them at once.
	0.0		x	y									Move pen to position x@y
	1.0		x	y									Draw a line to position x@y
	2.0		toX	toY	controlX		controlY						Draw a Quadratic Bezier Curve
	3.0		toX	toY	control1X	control1Y			control2X	control2Y			Draw a Cubic Bezier Curve
	See #circleApproximationExamples
	Sample03Smiley new openInHand
	"

	aCanvas strokeWidth: 30 color: `Color gray alpha: 0.8` fillColor: `Color lightGreen alpha: 0.7` do: [
		aCanvas
				pathSequence: `#[
					0.0  150.0  150.0
					3.0  300.0  300.0  150.0  233.0  217.0  300.0
					3.0  450.0  150.0  380.3  300.0  450.0  233.0
					3.0  300.0  0.0      450.0  67.0    383.0  0.0
					3.0  150.0  150.0  210.7  0.0      150.0  67.0
					] asFloat32Array`
		].

	aCanvas strokeWidth: 30 color: `Color gray` do: [
		aCanvas
				pathSequence: `#[
					0.0 220.0 190.0
					2.0 300.0 240.0 245.0 240.0
					2.0 380.0 190.0 355.0 240.0
					0.0 250.0 100.0
					1.0 250.0 120.0
					0.0 350.0 100.0
					1.0 350.0 120.0
					] asFloat32Array`
		].! !


!Sample09Clock methodsFor: 'drawing' stamp: 'jmv 3/25/2024 11:53:09'!
drawOn: aCanvas

	| f |
	aCanvas
		strokeWidth: 10 color: Color lightCyan
		fillColor: Color veryVeryLightGray lighter do: [ :canvas |
			canvas circleCenter:  0@0 radius: 100 ].
	f := FontFamily familyName: 'DejaVu Sans' pointSize: 14.
	aCanvas drawString: 'XII' at: -13 @ 58 font: f color: Color brown.
	aCanvas drawString: 'III' at: 73 @ -24 font: f color: Color brown.
	aCanvas drawString: 'VI' at: -9 @ -106 font: f color: Color brown.
	aCanvas drawString: 'IX' at: -90 @ -24 font: f color: Color brown.

	f := FontFamily familyName: 'Alex Brush' pointSize: 16.
	aCanvas drawString: 'Chaque Heure' atCenterX: 0@ 18 font: f color: Color black.
	f := FontFamily familyName: 'Alex Brush' pointSize: 8.
	aCanvas drawString: 'pour la Minorie' atCenterX: 0@ -60 font: f color: Color black.! !


!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:35:11'!
drawCloseIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.996 g: 0.329 b: 0.329`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonClosePathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:36:27'!
drawCollapseIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.988 g: 0.557 b: 0.102`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.988 g: 0.557 b: 0.102) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonCollapsePathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:32:16'!
drawDownIcon
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollDownPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:36:31'!
drawExpandIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.337 g: 0.604 b: 0.161`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.337 g: 0.604 b: 0.161) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self strokeWidth: (size/10.0 max: 1.0) color: s do: [
		self pathSequence: (self class windowButtonExpandPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:46:26'!
drawExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	| halfWidth width |
	width := h * 3/4.
	halfWidth := width / 2.
	self fillColor: `Color lightGray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: (h/8) @ (h/4) negated;
			rel_LineTo: halfWidth @ width;
			rel_LineTo: halfWidth @ width negated;
			rel_LineTo: width negated @ 0 ].
		! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:32:30'!
drawLeftIcon
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollLeftPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:36:34'!
drawMenuIcon

	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `Color r: 0.212 g: 0.439 b: 0.784`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.212 g: 0.439 b: 0.784) * 0.85`.
			s := `Color black` ].

	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: `Color gray` fillColor: c do: [
		self pathSequence: (self class windowButtonPathData: size) ].

	self fillColor: s do: [
		self pathSequence: (self class windowButtonMenuPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:46:16'!
drawNotExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	| halfWidth width |
	width := h * 3/4.
	halfWidth := width / 2.
	self fillColor: `Color lightGray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: (h/4) @ halfWidth negated;
			rel_LineTo: width @ halfWidth;
			rel_LineTo: width negated @ halfWidth;
			rel_LineTo: 0 @ width negated.
		].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:36:38'!
drawPushPinIcon
	"For Menu stay-up button"
	| c s size |
	(currentMorph isPressed or: [ currentMorph mouseIsOver ])
		ifFalse: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329)`.
			s := `Color darkGray` ]
		ifTrue: [
			c := `(Color r: 0.996 g: 0.329 b: 0.329) * 0.85`.
			s := `Color black` ].
	size := currentMorph morphWidth.
	self strokeWidth: size/30.0 color: s fillColor: c do: [
		self pathSequence: (self class pushPinIconData: size) ]! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:32:36'!
drawRightIcon
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollRightPathData: size) ].! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/25/2024 11:32:42'!
drawUpIcon
	| c size |
	c := currentMorph isPressed
		ifFalse: [`Color lightGray`]
		ifTrue: [`Color darkGray`].
	size := currentMorph morphWidth.
	self strokeWidth: size/7.2 color: c do: [
		self pathSequence: (self class scrollUpPathData: size) ].! !


!VectorCanvas methodsFor: 'private-framework' stamp: 'jmv 3/25/2024 11:41:34'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(VectorCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect := auxBitBltEngine clipRect.
	auxBitBltEngine clipRect: rect.
	auxBitBltEngine
		sourceForm: nil;
		fillColor: `Color gray: 0.5`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth.
	auxBitBltEngine clipRect: prevClipRect.! !


!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 3/25/2024 11:41:51'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect := engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		fillColor: `Color gray: 0.5`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth.
	engine clipRect: prevClipRect.! !


!Theme methodsFor: 'menu colors' stamp: 'jmv 3/25/2024 11:42:16'!
menuTitleBar
	Display depth = 1 ifTrue: [^ `Color white`].
	Display depth = 2 ifTrue: [^ `Color gray: 0.5`].
	^ self menu darker! !

