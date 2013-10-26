'From Cuis 4.0 of 21 April 2012 [latest update: #1407] on 25 August 2012 at 2:26:30 pm'!

!Morph methodsFor: 'layout' stamp: 'jmv 8/25/2012 13:50'!
layoutSubmorphsIfNeeded
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	layoutNeeded ifNil: [ layoutNeeded _ true ].
	layoutNeeded ifTrue: [
		self layoutSubmorphs.
		layoutNeeded _ false ]! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 8/25/2012 13:50'!
privateInvalidateMorph: aMorph
	"Private. Invalidate the given morph after adding or removing.
	This method is private because a) we're invalidating the morph 'remotely'
	and b) it forces a fullBounds computation which should not be necessary
	for a general morph c) the morph may or may not actually invalidate
	anything (if it's not in the world nothing will happen) and d) the entire
	mechanism should be rewritten."

	"Is this needed at all?"
	self flag: #jmvVer2.
	aMorph layoutSubmorphsIfNeeded.

	aMorph redrawNeeded! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/25/2012 14:24'!
clipSubmorphs: aBoolean
	"Drawing specific. If this property is set, clip the receiver's submorphs to the receiver's clipping bounds."
	self redrawNeeded.
	aBoolean
		ifTrue: [ self setProperty: #clipSubmorphs toValue: true ]
		ifFalse: [ self removeProperty: #clipSubmorphs ].
	self redrawNeeded! !

!Morph methodsFor: 'fileIn/out' stamp: 'jmv 8/25/2012 14:07'!
prepareToBeSaved
	"Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session."

	self releaseCachedState! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 13:50'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Should implement in some reasonable way... including submorphs?"
	self flag: #jmvVer2.
	self layoutSubmorphsIfNeeded.

	self flag: #jmvVer2.
	^self morphBoundsInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 13:50'!
morphPositionInOwner: newPositionInOwner
	"Change the position of this morph."
"Ojo aca. Ponemos position a nil pero es solo por el problema de bounds desenganchado de position..."
	| newPositionInWorld |
	self flag: #jmvVer2.

	newPositionInWorld _ owner
		ifNotNil: [ owner externalizeToWorld: newPositionInOwner ]
		ifNil: [ newPositionInOwner ].

	position = newPositionInOwner ifTrue: [
		newPositionInWorld = self morphPositionInWorld ifFalse: [
			'---------feote2' print
		].
		^ self ].		"Null change"

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsIfNeeded; redrawNeeded.


	"Maybe we don't really need an owner to run this method..."
	self validateOwnerNotNil.

	position _ newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 13:50'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (MatrixTransform2x3) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"
	self validateOwnerNotNil.

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].


	position = newPositionInOwner ifTrue: [
		"Al menos en algunos casos esto pasa porque al poner owner, no reajustamos bounds...
		Es necesario hacer que los globales sigan a los locales (manteniendo locales, moviendo morph)
		o que las locales sigan a las globales (manteniendo posicion en el mundo... si es que estabamos en un mundo!!)"
		self flag: #jmvVer2.
		"
		newPositionInWorld - bounds topLeft = (0@0) ifFalse: [
			'-----feote' print.
			newPositionInWorld print.
			bounds print.
			position print.
			self print.
			owner print.
			owner morphPosition print.
			'--' print.
		].
		"
		^ self ].		"Null change"

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsIfNeeded; redrawNeeded.

	position _ newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/25/2012 14:20'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	layoutNeeded _ true! !

!Morph methodsFor: 'layout-properties' stamp: 'jmv 8/25/2012 14:25'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	self layoutSpec == aLayoutSpec ifTrue:[^self].
	aLayoutSpec morph: self.
	self assureExtension layoutSpec: aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/25/2012 14:01'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	
	"This method is the only real use of ivar fullBounds, other than senders of #fullBounds"

	self layoutSubmorphsIfNeeded.
	self invalidRect: self morphFullBoundsInWorld! !

!Morph methodsFor: 'private' stamp: 'jmv 8/25/2012 14:08'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner _ owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition _ self morphPositionInWorld ].
	owner _ aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					position _ owner internalizeFromWorld: oldGlobalPosition.
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]]! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 14:22'!
basicExtent: aPoint

	self flag: #jmvVer2.
	"Lo dejo un rato porque se llama dde initialize... De ultima, es un problema? Es cierto que no se la escala aun..."
	"
	self validateOwnerNotNil.
	"
	
	"ver senders. Es en owner's o en world's ???"
	extent = aPoint ifTrue: [^ self].
	self redrawNeeded.
	extent _ aPoint.
	self someSubmorphPositionOrExtentChanged.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2012 13:43'!
addHandles
	| box |
	target isWorldMorph ifTrue: [ ^ self addHandlesForWorldHalos ].

	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target worldBoundsForHalo.  "update my size"
	box _ self basicBox.

	target addHandlesTo: self box: box.

	self addName.
	growingOrRotating _ false.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2012 13:43'!
addHandlesForWorldHalos
	"Add handles for world halos, like the man said"

	| box w |
	w _ self world ifNil: [target world].
	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target morphBoundsInWorld.
	box _ w morphBoundsInWorld insetBy: 9.
	target addWorldHandlesTo: self box: box.

	self
		addNameBeneath: (box insetBy: (0@0 corner: 0@10))
		string: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2012 13:43'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtent: newExtent.
	growHandle morphPosition: evt eventPosition - (growHandle morphExtent // 2)! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/25/2012 13:43'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	degrees _ (evt eventPosition - target referencePosition) degrees.
	degrees _ degrees - angleOffset degrees.
	degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false.
	degrees = 0.0
		ifTrue: [rotHandle color: Color lightBlue]
		ifFalse: [rotHandle color: Color blue].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	self removeAllHandlesBut: rotHandle.

	target rotationDegrees: degrees.

	rotHandle morphPosition: evt eventPosition - (rotHandle morphExtent // 2)! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/25/2012 14:03'!
updateFromParagraph
	"A change has taken place in my paragraph, as a result of editing and I must be updated. "

	paragraph ifNotNil: [
		editor storeSelectionInParagraph.
		self fit ].

	owner
		updateScrollBarsBounds;
		setScrollDeltas! !


!LayoutAdjustingMorph methodsFor: 'adjusting' stamp: 'jmv 8/25/2012 01:01'!
adjustIndicatorAt: aPoint
	owner direction = #vertical
		ifTrue: [
			indicator morphPosition: indicator morphPosition x @ (aPoint y-(indicator morphBoundsInWorld height//2)) ]
		ifFalse: [
			indicator morphPosition: (aPoint x-(indicator morphBoundsInWorld width//2)) @ indicator morphPosition y ]! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 8/25/2012 11:14'!
padding: aSymbolOrNumber
	"This sets how extra space is used when doing layout. For example, a column might have extra , unneded vertical space. #top means widgets are set close to the top, and extra space is at bottom. Conversely, #bottom means widgets are set close to the bottom, and extra space is at top. Valid values include #left and #right (for rows) and #center. Alternatively, any number between 0.0 and 1.0 might be used.
	self new padding: #center
	self new padding: 0.9
	"
	padding _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/25/2012 14:11'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		layoutNeeded _ false.
		^self].

	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	layoutNeeded _ false! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/25/2012 11:13'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth widths l usableHeight boundsTop boundsRight r t b |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableWidth _ boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth _ usableWidth - sumOfFixed.
	normalizationFactor _ self proportionalWidthNormalizationFactor.
	availableForPropWidth _ availableForPropWidth * normalizationFactor.
	widths _ submorphs collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	l _ ((usableWidth - widths sum) * (padding ifNil: [0]) + xSep max: 0) +  boundsForLayout left.
	usableHeight _ boundsForLayout height - (2*ySep) max: 0.
	boundsTop _ boundsForLayout top.	
	boundsRight _ boundsForLayout right.
	submorphs size to: 1 by: -1 do: [ :index | | m w h ls |
		m _ submorphs at: index.
		w _ widths at: index.
		"major direction"
		r _ l + w min: boundsRight.
		"minor direction"
		ls _ m layoutSpec.
		h _ (ls heightFor: usableHeight) min: usableHeight.
		t _ (usableHeight - h) * ls minorDirectionPadding + ySep + boundsTop.
		b _ t + h.
		"Set bounds and adjust major direction for next step"
		m morphBoundsInWorld: (l rounded @ t rounded corner: r rounded @ b rounded).
		w > 0 ifTrue: [
			l _ r + xSep min: boundsRight ]]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/25/2012 11:14'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight heights t usableWidth boundsLeft boundsBottom b l r |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableHeight _ boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight _ usableHeight - sumOfFixed.
	normalizationFactor _ self proportionalHeightNormalizationFactor.
	availableForPropHeight _ availableForPropHeight * normalizationFactor.
	heights _ submorphs collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	t _ ((usableHeight - heights sum) * (padding ifNil: [0]) + ySep max: 0) +  boundsForLayout top.
	usableWidth _ boundsForLayout width - (2*xSep) max: 0.
	boundsLeft _ boundsForLayout left.	
	boundsBottom _ boundsForLayout bottom.
	submorphs size to: 1 by: -1 do: [ :index | | m h w ls |
		m _ submorphs at: index.
		h _ heights at: index.
		"major direction"
		b _ t + h min: boundsBottom.
		"minor direction"
		ls _ m layoutSpec.
		w _ (ls widthFor: usableWidth) min: usableWidth.
		l _ (usableWidth - w) * ls minorDirectionPadding + xSep + boundsLeft.
		r _ l + w.
		"Set bounds and adjust major direction for next step"
		m morphBoundsInWorld: (l rounded @ t rounded corner: r rounded @ b rounded).
		h > 0 ifTrue: [
			t _ b + ySep min: boundsBottom ]]! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 8/25/2012 14:14'!
adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth doNotResizeBelow |
	doNotResizeBelow _ self minPaneWidthForReframe.
	i _ submorphs indexOf: aLayoutAdjustMorph.
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ aPoint x - aLayoutAdjustMorph referencePosition x.
	delta _ delta max: doNotResizeBelow - lCurrentWidth.
	delta _ delta min: rCurrentWidth - doNotResizeBelow.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [	"If both proportional, update them"
			ls setProportionalWidth: (1.0 * lNewWidth / lCurrentWidth * ls proportionalWidth).
			rs setProportionalWidth: (1.0 * rNewWidth / rCurrentWidth * rs proportionalWidth) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 8/25/2012 14:11'!
adjustVerticallyBy: aLayoutAdjustMorph at: aPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight doNotResizeBelow |
	doNotResizeBelow _ self minPaneHeightForReframe.
	i _ submorphs indexOf: aLayoutAdjustMorph.
	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ aPoint y - aLayoutAdjustMorph referencePosition y.
	delta _ delta max: doNotResizeBelow - tCurrentHeight.
	delta _ delta min: bCurrentHeight - doNotResizeBelow.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [	"If both proportional, update them"
			ts setProportionalHeight: (1.0 * tNewHeight / tCurrentHeight * ts proportionalHeight).
			bs setProportionalHeight: (1.0 * bNewHeight / bCurrentHeight * bs proportionalHeight) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 8/25/2012 14:20'!
morphExtent: aPoint

	self flag: #jmvVer2.
	extent = aPoint ifFalse: [
		self redrawNeeded.
		extent _ aPoint.
		self buildMagnifiedBackgroundImage.
		self redrawNeeded ].

	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox = self morphBoundsInWorld ifFalse: [
				worldState canvas: nil.
				worldState viewBox: self morphBoundsInWorld ]]]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/25/2012 14:20'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	self updateScrollBarsBounds! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 8/25/2012 14:12'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	aWorld addMorph: self.
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphBoundsInWorld: (Display boundingBox center - (w@h//2) extent: w@h).
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.
	aWorld startSteppingSubmorphsOf: self.! !


!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 8/25/2012 14:23'!
collapseOrExpand
	"Collapse or expand the window, depending on existing state"

	isCollapsed
		ifTrue: [ self expand ]
		ifFalse: [ self collapse]! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 8/25/2012 13:51'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateMode worldDamageRects handsToDraw allDamage |
	submorphs do: [ :m | m layoutSubmorphsIfNeeded].  "force re-layout if needed"
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateMode _ self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [ self assuredNonDisplayCanvas ].

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"draw hands onto world canvas"
	handsToDraw reverseDo: [ :h | self drawHand: h ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: allDamage ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [ self forceDamageToScreen: allDamage ]
		ifFalse: [ canvas showAt: aWorld viewBox origin invalidRects: allDamage ].
	"restore world canvas under hands"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!methodRemoval: PluggableScrollPane #mustLayout!
PluggableScrollPane removeSelector: #mustLayout!
!methodRemoval: LayoutMorph class #example4!
LayoutMorph class removeSelector: #example4!
!methodRemoval: LayoutMorph class #example5!
LayoutMorph class removeSelector: #example5!
!methodRemoval: Morph #computeFullBounds!
Morph removeSelector: #computeFullBounds!
!methodRemoval: Morph #layoutSubmorphsAndComputeFullBounds!
Morph removeSelector: #layoutSubmorphsAndComputeFullBounds!
!methodRemoval: Morph #mustLayout!
Morph removeSelector: #mustLayout!
