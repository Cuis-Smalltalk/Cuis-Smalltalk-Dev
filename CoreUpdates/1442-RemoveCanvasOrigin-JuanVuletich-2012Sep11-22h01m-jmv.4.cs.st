'From Cuis 4.0 of 21 April 2012 [latest update: #1441] on 11 September 2012 at 10:42:02 pm'!
!classDefinition: #FormCanvas category: #'Morphic-Support'!
Object subclass: #FormCanvas
	instanceVariableNames: 'origin clipRect form port shadowColor transformations currentTransformation cti '
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/11/2012 22:22'!
canvasOrigin
	"Return the current origin for drawing operations"
	self flag: #jmvVer2. "shouldn't be needed I believe."
	^ transformations first position! !


!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/11/2012 22:26'!
clipRect
	"Return the currently active clipping rectangle"
	^ clipRect translatedBy: self canvasOrigin negated! !

!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/11/2012 22:26'!
contentsOfArea: aRectangle into: aForm
	| bb o |
	bb _ BitBlt toForm: aForm.
	o _ self canvasOrigin.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: (aRectangle left + o x); sourceY: (aRectangle top + o y);
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^aForm! !

!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/11/2012 22:24'!
formWithOffset

	^ form offset: self canvasOrigin negated! !

!FormCanvas methodsFor: 'copying' stamp: 'jmv 9/11/2012 22:29'!
copyClipRect: aRectangle
	| o |
	o _ self canvasOrigin.
	^ self copyOrigin: o clipRect: (aRectangle translatedBy: o)
! !

!FormCanvas methodsFor: 'drawing' stamp: 'jmv 9/11/2012 22:27'!
line: pt1 to: pt2 width: w color: c
	| offset p1 p2 |
	p1 _ currentTransformation transform: pt1.
	p2 _ currentTransformation transform: pt2.
	offset _ w // 2.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: (p1 rounded + offset) to: (p2 rounded + offset)! !

!FormCanvas methodsFor: 'drawing-images' stamp: 'jmv 9/11/2012 22:15'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ currentTransformation transform: aPoint.
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!FormCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 9/11/2012 22:20'!
fillOval: r color: fillColor borderWidth: borderWidth borderColor: borderColor

	| displayRectangle |
	displayRectangle _ (currentTransformation displayBoundsOfTransformOf: r) truncated.
	"draw the border of the oval"
	(borderWidth = 0 or: [borderColor isTransparent]) ifFalse:[
		self setPaintColor: borderColor.
		port frameOval: displayRectangle borderWidth: borderWidth].
	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillOval: (displayRectangle insetBy: borderWidth)].
! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/11/2012 22:26'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: borderWidth ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: borderWidth) ]! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 9/11/2012 22:17'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red
	"
	| rect |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	self setPaintColor: borderColor.
	port
		frameRect: rect
		borderWidth: borderWidth.! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/11/2012 22:16'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c
	| font p1 |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	font _ fontOrNil ifNil: [StrikeFont default].
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		kern: font baseKern negated! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/11/2012 22:28'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: font color: c kern: kern

	| p1 |
	p1 _ currentTransformation transform: aPoint.
	port colorMap: nil.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		strikeFont: font
		kern: kern! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/11/2012 22:28'!
drawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: c
	| font portRect bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [port clipRect: portRect. ^self].
	port clipWidth = 0 ifTrue: [port clipRect: portRect. ^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	port installStrikeFont: font foregroundColor: (shadowColor ifNil: [ c ]).
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/11/2012 22:16'!
drawStringEmbossed: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor
	| font portRect insideColor bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	"Slight optimization when there's nothing to do."
	port clipHeight = 0 ifTrue: [^self].
	port clipWidth = 0 ifTrue: [^self].
	font _ fontOrNil ifNil: [StrikeFont default].
	insideColor _ shadowColor ifNil: [ aColor ].
	insideColor = Color black ifFalse: [ | topColor |
		topColor _ insideColor alphaMixed: 0.25 with: Color black.
		port installStrikeFont: font foregroundColor: topColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: bounds topLeft
			strikeFont: font
			kern: font baseKern negated ].
	insideColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ insideColor alphaMixed: 0.22 with: Color white.
		port installStrikeFont: font foregroundColor: bottomColor.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: bounds topLeft + (0@2)
			strikeFont: font
			kern: font baseKern negated ].
	port installStrikeFont: font foregroundColor: insideColor.
	port
		displayString: aString asString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft + (0@1)
		strikeFont: font
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'initialization' stamp: 'jmv 9/11/2012 22:11'!
initialize
	super initialize.

	"We currently set up these only in #initialize.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	each time the world is redrawn. See #drawInvalidAreasWorld:submorphs:
	Maybe this cleanup should be in an aux method that can be called each time on an existing instance..."
	currentTransformation _ MatrixTransform2x3 identity.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !

!FormCanvas methodsFor: 'testing' stamp: 'jmv 9/11/2012 22:18'!
isFullyVisible: aRectangle
	"Optimization"
	aRectangle right > clipRect right		ifTrue: [^ false].
	aRectangle left < clipRect left			ifTrue: [^ false].
	aRectangle bottom > clipRect bottom	ifTrue: [^ false].
	aRectangle top < clipRect top			ifTrue: [^ false].
	^ true
! !

!FormCanvas methodsFor: 'testing' stamp: 'jmv 9/11/2012 22:40'!
isVisible: aRectangle

| o |
self flag: #jmvVer2.
"should receive local coordinates, and do the conversion here!!!!!!!!!!"
"This goes together with removing #morphFullBoundsInWorld and #morphBoundsInWorld"

	o _ self canvasOrigin.
	"Optimization"
	(aRectangle right + o x) < clipRect left		ifTrue: [^ false].
	(aRectangle left + o x) > clipRect right		ifTrue: [^ false].
	(aRectangle bottom + o y) < clipRect top	ifTrue: [^ false].
	(aRectangle top + o y) > clipRect bottom	ifTrue: [^ false].
	^ true
! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/11/2012 22:19'!
fillRectangle: rInPortTerms tilingWith: aForm sourceRect: patternBox rule: aCombinationRule
	"We assume that aForm is part of an InfiniteForm.
	aRectangle is in form coordinates, no transformation is done."
	| additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	ex _ patternBox extent.
	additionalOffset _ 0@0.
	clippedPort _ port clippedBy: rInPortTerms.
	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.
	clipOffset _ rInPortTerms topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedPort clipRect.
	savedMap _ clippedPort colorMap.
	clippedPort sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedPort destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedPort destOrigin: x@y; copyBits]].
	clippedPort colorMap: savedMap! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/11/2012 22:17'!
frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"
	rect and borderWidth are in form coordinates. No transformation is done.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white
	"
	| w h |
	self setPaintColor: topLeftColor.

	port frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w _ h _ borderWidth]
		ifFalse: [w _ borderWidth x.   h _ borderWidth y].
	self setPaintColor: bottomRightColor.
	port 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/11/2012 22:28'!
image: aForm at: aPoint sourceRect: sourceRect rule: rule 
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: aPoint sourceRect: sourceRect rule: rule.! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/11/2012 22:41'!
setForm: aForm

	form _ aForm.
	port _ self portClass toForm: form.

	"this was the contents of the #reset method"

	"origin of the top-left corner of this cavas"
	transformations first setTranslation: 0@0.
	clipRect _ (0@0 corner: form extent).	"default clipping rectangle"
	shadowColor _ nil! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/11/2012 22:41'!
setOrigin: aPoint

	transformations first setTranslation: aPoint.! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/11/2012 22:41'!
setOrigin: aPoint clipRect: aRectangle

	transformations first setTranslation: aPoint.
	clipRect _ aRectangle.
	port clipRect: aRectangle.
! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 9/11/2012 22:40'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	"
	This is starting to work:
		| c |
		c _ Display getCanvas initTransformationsFor: World.
		World submorphsDo: [ :m | c fullDraw: m ].
	"

	"To replace #fullDrawOn:"
	self flag: #jmvVer3.

	aMorph visible ifFalse: [^ self].

	self into: aMorph.

	aMorph layoutSubmorphsIfNeeded.
	(self isVisible: aMorph morphFullBoundsInWorld) ifFalse: [
		self outOf: aMorph.
		^ self].

	aMorph isKnownFailing 
		ifTrue: [ aMorph drawErrorOn: self ]
		ifFalse: [ aMorph fullDrawOn: self ].

	self outOf: aMorph! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/11/2012 22:40'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		aCanvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas.
	self drawDropHighlightOn: aCanvas.
	self drawMouseDownHighlightOn: aCanvas! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/11/2012 22:38'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Should implement in some reasonable way... including submorphs?"

	self flag: #jmvVer2.
	"IF I remove this, then layout of buttons in FileList breaks when selecting / deselecting code files. Besides, ProgressMorph example breaks too"
	self layoutSubmorphsIfNeeded.

	self flag: #jmvVer2.	"consider submorphs!!!!!!!!!!"
	^self morphBoundsInWorld! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/11/2012 22:33'!
browseHierarchy
	"Create and schedule a new hierarchy browser on the currently selected class or meta."

	model hierarchyBrowser ifNotNil: [ :newBrowser |
		HierarchyBrowserWindow
			openNoSysCat: newBrowser
			label: newBrowser labelString.
		newBrowser assureSelectionsShow ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/11/2012 22:23'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| ownBnds fullBnds bw |
	ownBnds _ self morphBoundsInWorld.
	fullBnds _ self morphFullBoundsInWorld.
	(savedPatch isNil or: [savedPatch extent ~= fullBnds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBnds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: (fullBnds translatedBy: aCanvas canvasOrigin)
		into: savedPatch.
	savedPatch offset: fullBnds topLeft.
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth.
		aStream nextPut: ownBnds.
		prevBounds ifNotNil: [ aStream nextPut: prevBounds ].
		(fullBnds areasOutside: (fullBnds insetBy: bw)) do: [ :r |
			aStream nextPut: r ].
		prevFullBounds ifNotNil: [
			(prevFullBounds areasOutside: (prevFullBounds insetBy: bw)) do: [ :r |
				aStream nextPut: r ]]]
	ifFalse: [
		prevFullBounds ifNil: [
			aStream nextPut: fullBnds ]
		ifNotNil: [
			aStream nextPut: (fullBnds merge: prevFullBounds)]].
	prevBounds _ ownBnds.
	prevFullBounds _ fullBnds! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/11/2012 22:40'!
drawLinesOn: aCanvas 
	| lColor |
	lColor _ Theme current line.

	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(aCanvas isVisible: submorph morphBoundsInWorld) or: [
				submorph nextSibling notNil and: [
					aCanvas isVisible:
						submorph nextSibling morphBoundsInWorld ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 9/11/2012 22:01'!
buildMorphicWindow

	| textMorph |
	listMorph _ HierarchicalListMorph
			model: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #genericMenu
			keystrokeAction: #explorerKey:from:.
	listMorph autoDeselect: false.
	textMorph _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: (model rootObject printStringLimitedTo: 64)! !

!methodRemoval: FormCanvas #origin!
FormCanvas removeSelector: #origin!
!methodRemoval: FormCanvas #zzisVisible:!
FormCanvas removeSelector: #zzisVisible:!
!classDefinition: #FormCanvas category: #'Morphic-Support'!
Object subclass: #FormCanvas
	instanceVariableNames: 'clipRect form port shadowColor transformations currentTransformation cti'
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms'
	poolDictionaries: ''
	category: 'Morphic-Support'!
