'From Cuis 4.0 of 21 April 2012 [latest update: #1351] on 5 August 2012 at 10:59:33 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:32'!
morphBoundsInWorld: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.
		
	oldExtent _ self morphExtentInWorld.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPositionInWorld: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPositionInWorld: newBounds topLeft; morphExtent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self morphExtent: newExtent; morphPositionInWorld: newBounds topLeft.
	].! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 8/5/2012 22:18'!
morphBoundsInWorld: aRectangle
	innerRectangle _ aRectangle insetBy: self borderWidth + self padding.! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 8/5/2012 22:17'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	"

	| resizeFactor outerBox arrowMorph resizedForm f |
	resizeFactor _ 4.
	outerBox _ RectangleMorph new.
	outerBox
		morphExtent: finalSizeInteger asPoint * resizeFactor;
		borderWidth: 0;
		color: Color transparent.
	
	arrowMorph _ self buildArrowIn: outerBox morphBoundsInWorld.
	outerBox addMorphFront: arrowMorph.
	arrowMorph morphPositionInOwner: 12@8.	"not a clue why these numbers work..."
	
	
	f _ outerBox imageForm: 32.
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !

!FormCanvas class methodsFor: 'private' stamp: 'jmv 8/5/2012 22:17'!
buildArrowIn: aRectangle 
	"PRIVATE - create an arrow bounded in aRectangle"
	"
	(self buildArrowOfDirection: #up size: 20) display
	"

	| arrow vertices |
	vertices _ self verticesForSimpleArrow: aRectangle.
	arrow _ PolygonMorph 
		vertices: vertices
		color: Color darkGray
		borderWidth: 0
		borderColor: Color black.
	"arrow bounds: (arrow bounds insetBy: (aRectangle width / 6) rounded)."
	^arrow! !


!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 8/5/2012 22:22'!
testLayout2
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 _ RectangleMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (c2 _ RectangleMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ RectangleMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = (pane morphHeight - 10 * 0.9) rounded.
	self assert: c1 morphBoundsInWorld bottom = (row morphBoundsInWorld bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = (row morphHeight - 10 * 0.8) rounded.
	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 morphBoundsInWorld top - row morphBoundsInWorld top) - (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (c1 morphHeight / 0.8 * 0.7) rounded.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 8/5/2012 22:24'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 _ RectangleMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ RectangleMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ RectangleMorph new  borderWidth: 0)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (c2 _ RectangleMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ RectangleMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld; morphExtent: 400@300.
	World doOneCycleNow.

	self assert: row morphBoundsInWorld left = (pane morphBoundsInWorld left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow morphBoundsInWorld left = (row morphBoundsInWorld left + 5).
	self assert: (innerRow morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - innerRow morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: innerRow morphHeight = 30.

	self assert: i1 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 5).
	self assert: (i1 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i1 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 20).
	self assert: (i2 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i2 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 35).
	self assert: (i3 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: c2 morphHeight = 40.
	self assert: (c3 morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !


!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:19'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

"would be better In own coordinates!!"
	self flag: #jmvVer2.
	^ self morphBoundsInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:30'!
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
	self validateOwnerNotNil.

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].


	position = newPositionInOwner ifTrue: [
		"Al menos en algunos casos esto pasa porque al poner owner, no reajustamos bounds...
		Es necesario hacer que los globales sigan a los locales (manteniendo locales, moviendo morph)
		o que las locales sigan a las globales (manteniendo posicion en el mundo... si es que estabamos en un mundo!!)"
		self flag: #jmvVer2.
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
"Quizas no un null change del todo... Es posible que bounds y position se desincronicen, al cambiar el owner (o cualquier owner en el medio). Todo esto es fulerote. espero sacarlo pronto!!"
		self privateFullMoveBy: newPositionInWorld - self morphPositionInWorld.
		^ self ].		"Null change"

	"May need to compute fullBounds, but I see no reason to layout submorphs!!"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds; redrawNeeded.

"This won't be needed once we remove the bounds ivar in global coordinates!!
With this, we also delete implementors of #privateFullMoveBy: and #privateMoveBy: "
self privateFullMoveBy: newPositionInWorld - self morphPositionInWorld.

	position _ newPositionInOwner.
	self validatePositionAndBounds.
	self validateExtentAndBounds.

	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 8/5/2012 22:25'!
addHalo: evt
	| halo prospectiveHaloClass |
	prospectiveHaloClass _ Smalltalk at: self haloClass ifAbsent: [HaloMorph].
	halo _ prospectiveHaloClass new.
	halo popUpFor: self event: evt.
	halo morphBoundsInWorld: self worldBoundsForHalo.
	^halo! !

!Morph methodsFor: 'menus' stamp: 'jmv 8/5/2012 22:37'!
changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self activeHand;
		target: self;
		selector: #color:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 8/5/2012 22:37'!
changeColorTarget: anObject selector: aSymbol originalColor: aColor hand: aHand
	"Put up a color picker for changing some kind of color.  May be modal or modeless, depending on #modalColorPickers setting"
	self flag: #arNote. "Simplify this due to anObject == self for almost all cases"
	"^ ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: aHand;
		target: anObject;
		selector: aSymbol;
		originalColor: aColor;
		putUpFor: anObject near: ((anObject is: #Morph)
					ifTrue: [ Rectangle center: self morphPosition extent: 20 ]
					ifFalse: [ anObject == self world
								ifTrue: [ anObject viewBox bottomLeft + (20@-20) extent: 200 ]
								ifFalse: [ anObject morphFullBoundsInWorld ]]);
		yourself"! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 8/5/2012 22:31'!
resizeMorph
	| handle |
	handle := HandleMorph new 
				forEachPointDo: [:newPoint | self morphExtent: newPoint - self morphPositionInWorld].
	self activeHand attachMorph: handle.
	handle startStepping! !


!BorderedMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:41'!
innerBounds
	"Return the inner rectangle enclosed by the bounds of this morph excluding the space taken by its borders. For an unbordered morph, this is just its bounds."

	^ self morphBoundsInWorld insetBy: borderWidth! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 22:25'!
createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	self addMorph: result.
	result morphBoundsInWorld: (29@90 corner: 122@117).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 22:25'!
createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	self addMorph: result.
	result morphBoundsInWorld: (149@90 corner: 242@117).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 22:25'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	self addMorph: result.
	result morphBoundsInWorld: ( 30@7 corner: 269@22).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 22:25'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |
	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result morphBoundsInWorld: (14@25 corner: 257@84).
	^ result! !


!HaloHandleMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:41'!
drawOn: aCanvas
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: (color alpha: 0.57)
		at: self morphPositionInWorld! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 8/5/2012 22:20'!
step
	| newBounds |
	target
		ifNil: [^ self].
	newBounds _ target isWorldMorph
				ifTrue: [target morphBoundsInWorld]
				ifFalse: [target worldBoundsForHalo truncated].
	newBounds = self morphBoundsInWorld
		ifTrue: [^ self].
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newBounds extent = extent
		ifTrue: [^ self morphPosition: newBounds origin].
	growingOrRotating ifFalse: [
		submorphs size > 1
			ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self morphBoundsInWorld: newBounds! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:25'!
addHandle: handleSpec on: eventName send: selector to: recipient 
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: HandleSize asPoint).
	(iconName _ handleSpec iconSymbol) ifNotNil: [
			| form |
			form _ Icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon.
				icon morphPositionInOwner: 0@0 ]].
	handle 
		on: #mouseUp
		send: #endInteraction
		to: self.
	handle 
		on: eventName
		send: selector
		to: recipient.
	handle 
		setBalloonText: (target balloonHelpTextForHandle: handle).
	^handle! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:25'!
addHandles
	| box |
	target isWorldMorph ifTrue: [ ^ self addHandlesForWorldHalos ].

	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target worldBoundsForHalo.  "update my size"
	box _ self basicBox.

	target addHandlesTo: self box: box.

	self addName.
	growingOrRotating _ false.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:18'!
addHandlesForWorldHalos
	"Add handles for world halos, like the man said"

	| box w |
	w _ self world ifNil:[target world].
	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target morphBoundsInWorld.
	box _ w morphBoundsInWorld insetBy: 9.
	target addWorldHandlesTo: self box: box.

	self
		addNameBeneath: (box insetBy: (0@0 corner: 0@10))
		string: (innerTarget printStringLimitedTo: 40).
	growingOrRotating _ false.
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:18'!
addNameBeneath: outerRectangle string: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition w nameBackground |
	w _ self world ifNil: [ target world ].
	nameBackground _ RectangleMorph new
		borderWidth: 0;
		color: (Color lightBlue alpha: 0.9).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: Color magenta.
	self addMorph: nameBackground.
	self addMorph: nameMorph.
	namePosition _ outerRectangle bottomCenter - ((nameMorph morphWidth // 2) @ (self handleSize negated // 2 - 1)).
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph morphExtent y + 5).
	nameBackground morphBoundsInWorld: (nameMorph morphBoundsInWorld outsetBy: 2).
	^nameMorph! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:57'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self handleSize.
	anExtent _ ((self morphWidth + self handleSize + 8) max: minSide) @
				((self morphHeight + self handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld extent: anExtent.
	w _ self world ifNil: [ target outermostWorldMorph ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: 8@8) ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:19'!
startGrow: evt with: growHandle
	"Initialize resizing of my target.  Launch a command representing it, to support Undo"

	| botRt |
	self obtainHaloForEvent: evt andRemoveAllHandlesBut: growHandle.
	botRt _ target morphPositionInWorld + target morphExtentInWorld.
	positionOffset _ (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [0@0]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:20'!
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
		contentsOfArea: (fullBnds translateBy: aCanvas origin)
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

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 8/5/2012 22:19'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self addMorphBack: m.
	delta _ m morphExtentInWorld // 2.
	m morphPosition: (self morphPosition - delta).
	targetOffset _ m morphPosition - self morphPosition.! !


!HandleMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/5/2012 22:41'!
justDroppedInto: aMorph event: anEvent
	"So that when the hand drops me (into the world) I go away"
	lastPointBlock ifNotNil: [lastPointBlock value: self morphBoundsInWorld center].
	self flag: #arNote. "Probably unnecessary"
	anEvent hand releaseKeyboardFocus: self.
	self redrawNeeded.
	self delete! !

!HandleMorph methodsFor: 'stepping and presenter' stamp: 'jmv 8/5/2012 22:41'!
step
	pointBlock value: self morphBoundsInWorld center! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:25'!
adjustExtent
	"And reposition submorphs"
	| w p0 h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	p0 _ self morphPositionInWorld.
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m morphBoundsInWorld: (p0 + (0@y) extent: w@h).
		y _ y + h ].
	self morphExtent: w@y! !


!LayoutAdjustingMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 22:25'!
mouseDown: anEvent
	super mouseDown: anEvent.
	self cursor show.
	hand _ anEvent hand.
	self startStepping.
	Preferences fastDragWindowForMorphic ifTrue: [
		indicator _ RectangleIndicatorMorph new.
		indicator morphBoundsInWorld: self initialIndicatorBounds.
		indicator openInWorld ]! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/5/2012 22:25'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth widths l usableHeight boundsTop boundsRight r t b |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableWidth _ boundsForLayout width - ((submorphs size + 1) * xSep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth _ usableWidth - sumOfFixed.
	padding ifNil: [	"shrink"
		availableForPropWidth = 0 ifFalse: [
			self flag: #jmvVer2.
			self width: self width - availableForPropWidth.
			^ self layoutSubmorphsAndComputeFullBounds ]].
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

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 8/5/2012 22:25'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight heights t usableWidth boundsLeft boundsBottom b l r |
	xSep _ self xSeparation.
	ySep _ self ySeparation.
	usableHeight _ boundsForLayout height - ((submorphs size + 1) * ySep).
	sumOfFixed _ submorphs sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight _ usableHeight - sumOfFixed.
	padding ifNil: [	"shrink"
		availableForPropHeight = 0 ifFalse: [
			self flag: #jmvVer2.
			self height: self height - availableForPropHeight.
			^ self layoutSubmorphsAndComputeFullBounds ]].
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


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 8/5/2012 22:27'!
example6
	"
	Useful example contributed by Ken Dickey
	All these should look the same, right? (mmmh this should be a test...)
	self example6
	"
| pane rect1 rect2 |
pane _ LayoutMorph newRow separation: 5. "1"
pane color: Color lightGreen; morphBoundsInWorld: (120 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '1').

rect1 := RectangleMorph new color: (Color lightOrange); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect1.
rect2 := RectangleMorph new color: (Color cyan); 
	morphWidth: 20; morphHeight: 30.
pane addMorph: rect2.
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "2"
pane color: Color lightGreen; morphBoundsInWorld: (320 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '2').

rect1 := RectangleMorph new color: (Color lightOrange);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect1.
rect2 := RectangleMorph new color: (Color cyan).
pane addMorph: rect2
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane openInWorld.


pane _ LayoutMorph newRow separation: 5. "3"
pane color: Color lightGreen; morphBoundsInWorld: (520 @ 50 extent: 180 @ 100).
pane addMorph: (StringMorph contents: '3').

rect1 := RectangleMorph new color: (Color lightOrange).
pane addMorph: rect1 
         layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
rect2 := RectangleMorph new color: (Color cyan);
	layoutSpec: (LayoutSpec  fixedWidth: 20 fixedHeight: 30 minorDirectionPadding: #center).
pane addMorph: rect2.
pane openInWorld.! !


!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 8/5/2012 22:47'!
sourcePoint
	"If we are being dragged use our center, otherwise use pointer position"
	^ (trackPointer not or: [owner notNil and: [owner is: #HandMorph]])
		ifTrue: [ self morphBoundsInWorld center ]
		ifFalse: [ self activeHand morphPosition ]! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 8/5/2012 22:33'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker.
	marker morphPosition: self morphPositionInWorld + (0@2)! !

!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:58'!
drawOn: aCanvas 
	| stringColor stringBounds leftEdge b |

	b _ self morphBoundsInWorld.
	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: b colorOrInfiniteForm: Theme current menuHighlight].
	leftEdge := 0.
	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas image: iconForm at: b left+1 @ (b top + (b height - iconForm height // 2)).
			leftEdge _ iconForm width + self iconSeparation].

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + self submorphBounds width + 8 ].

	stringBounds _ b left + leftEdge @ (b top + 1) corner: b corner.

	aCanvas
		drawString: contents
		in: stringBounds
		font: self fontToUse
		color: stringColor.
	subMenu
		ifNotNil: [ aCanvas image: SubMenuMarker at: b right - 8 @ (b top + b bottom - SubMenuMarker height // 2) ]! !

!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 8/5/2012 22:33'!
aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu box |
	(owner notNil and: [ owner submorphs size = 1]) ifTrue:[
		"I am a lonely menuitem already; just grab my owner"
		owner stayUp.
		^owner aboutToBeGrabbedBy: aHand ].
	box _ self morphBoundsInWorld.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu morphBoundsInWorld: box.
	menu stayUp.
	self isSelected: false.
	^menu! !

!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 8/5/2012 22:33'!
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu morphBoundsInWorld: self morphBoundsInWorld.
	menu stayUp.
	evt hand grabMorph: menu from: owner. "duplicate was ownerless so use #grabMorph:from: here"
	^menu! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 8/5/2012 22:34'!
select: evt
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphPositionInWorld topRight + (10@0)
									with: self morphPositionInWorld topLeft)
			forHand: evt hand
			from: self.
		subMenu selectItem: nil event: evt].! !


!MenuMorph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:42'!
moveRight: aNumber
	self morphPosition: ((aNumber - self morphExtentInWorld x) @ self morphPositionInWorld y)! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:48'!
characterIndexAtPoint: aPoint

	| line block f |
	f _ self fontToUse.
	
	line _ TextLine 
		start: 1
		stop: contents size
		internalSpaces: 0
		paddingWidth: 0.
	line
		rectangle: self morphBoundsInWorld;
		lineHeight: f height baseline: f ascent.
		
	block _ (CharacterBlockScanner new text: 
			(contents asText font: font))
		characterBlockAtPoint: aPoint index: nil
		in: line.

	^ block stringIndex! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:51'!
displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas
	| caretColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	caretColor _ Theme current insertionPoint.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!!"
			d _ isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0 < halfW ifTrue: [
		x1 _ x1 - x0 + halfW.
		x0 _ halfW ].
	r _ self morphBoundsInWorld right-halfW-1.
	r < x1 ifTrue: [
		x0 _ x0 + r - x1.
		x1 _ r ].
	caretRect _ x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		line: x0@(bottom-halfW) to: x1@(top+halfW)
		width: w color: caretColor! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:48'!
drawCaretOn: aCanvas
	"Essentially copied from #displayInsertionMarkAtX:top:bottom:emphasis:on:"
	|  top bottom x tl |

	showCaret ifTrue: [
		tl _ self morphPositionInWorld.
		top _ tl y.
		bottom _ top + self baseFont height.
		x _ (self fontToUse widthOfString: contents from: 1 to: editor startIndex-1) + tl x.
		self displayInsertionMarkAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas ]! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 8/5/2012 22:43'!
invalidRect: damageRect from: aMorph
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self outermostWorldMorph 
                ifTrue: [worldState recordDamagedRect: (damageRect intersect: self morphBoundsInWorld )]
                ifFalse: [super invalidRect: damageRect from: aMorph]
! !

!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:57'!
drawOn: aCanvas

	"draw background image."
	| b |
	b _ self morphBoundsInWorld.
	backgroundImage
		ifNotNil: [
			"self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage multipliedBy: color at: bounds topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage multipliedBy: color at: bounds topLeft ]"
			self clipsSubmorphs ifTrue: [
				aCanvas clipBy: self clippingBounds
					during: [ :canvas | canvas image: backgroundImage at: b topLeft ]]
				ifFalse: [ aCanvas image: backgroundImage at: b topLeft ]]

		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [aCanvas drawsOnDisplay] and: [color class == TranslucentColor])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Squeak Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt current toForm: Display)
						clipRect: aCanvas clipRect;
						copy: b
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/5/2012 22:43'!
acceptDroppingMorph: dropped event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	| aMorph |
	aMorph := self morphToDropFrom: dropped.
	self isWorldMorph 
		ifTrue: [	"Add the given morph to this world and start stepping it if it wants to be."

			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Beeper beep.
					aMorph morphPosition: self morphBoundsInWorld center]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m isKindOf: HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !

!PasteUpMorph methodsFor: 'project state' stamp: 'jmv 8/5/2012 22:42'!
viewBox: newViewBox
	self flag: #jmvVer2.
	self isWorldMorph ifTrue: [
		(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
			ifTrue: [ worldState canvas: nil ].
		worldState viewBox: newViewBox ].
"	super morphPosition: newViewBox topLeft."
	bounds _ newViewBox.
	position _ owner
		ifNil: [ newViewBox topLeft ]
		ifNotNil: [ owner internalizeFromWorld: newViewBox topLeft ].
	extent _ newViewBox extent.
	self validatePositionAndBounds.
	self validateExtentAndBounds.! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 8/5/2012 22:43'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphFullBoundsInWorld extent.
	delta _ trialRect amountToTranslateWithin: self morphBoundsInWorld.
	self addMorph: aMorph.
	aMorph morphPosition: trialRect origin + delta.! !

!PasteUpMorph methodsFor: 'private' stamp: 'jmv 8/5/2012 22:43'!
privateMoveBy: delta

	super privateMoveBy: delta.
	worldState ifNotNil: [
		worldState viewBox ifNotNil: [
			worldState viewBox: self morphBoundsInWorld 
		].
	].! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:44'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphBoundsInWorld
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:55'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y b |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		b _ self morphBoundsInWorld.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ b center.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ b width-labelMargin-labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ b left + labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f height // 2).
		aCanvas
			drawStringEmbossed: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: colorForLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:54'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin b |

	f _ self fontToUse.
	b _ self morphBoundsInWorld.
	center _ b center.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ b width-labelMargin-labelMargin-1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ b left + labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			in: (x@y extent: extent - (labelMargin*2-2@4))
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2012 22:44'!
containsPoint: aPoint

	| iconOrigin |
	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ self morphBoundsInWorld center - (magnifiedIcon extent // 2).
			(magnifiedIcon isTransparentAt: aPoint - iconOrigin) not ]]! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:44'!
hScrollBarWidth
	"Return the width of the horizontal scrollbar"

	| w |	
	w _ self morphExtentInWorld x - (2 * borderWidth).
	self vIsScrollbarShowing
		ifTrue: [ w _ w - self scrollBarClass scrollbarThickness ].
	^w! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:54'!
updateScrollBarsBounds
	
	| t topLeft b |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.

	b _ self morphBoundsInWorld.

	topLeft _ b topRight + (0-t-borderWidth @ borderWidth).
	scrollBar morphBoundsInWorld: (topLeft extent: t @ self vScrollBarHeight).

	topLeft _ b bottomLeft + (borderWidth @ (t + borderWidth) negated).
	hScrollBar morphBoundsInWorld: (topLeft extent: self hScrollBarWidth@ t)! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:44'!
vScrollBarHeight
	^self morphExtentInWorld y - (2 * borderWidth)! !


!PluggableListMorphOfMany methodsFor: 'event handling' stamp: 'jmv 8/5/2012 22:53'!
mouseMove: event 
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row b |
	b _ self morphBoundsInWorld.
	row _ (event eventPosition y < b top and: [ scrollBar value > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: b topLeft) + 2 ]
		ifFalse: [
			(event eventPosition y > b bottom and: [ scrollBar value < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: b bottomLeft) - 3 ]
				ifFalse: [ self rowAtLocation: event eventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	(self ownerThatIsA: SystemWindow) ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self changeModelSelection: row ]
		ifFalse: [ self changeModelSelection: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !


!PolygonMorph methodsFor: 'editing' stamp: 'jmv 8/5/2012 22:26'!
addHandles
	| handle newVert tri |
	self removeHandles.
	handles _ OrderedCollection new.
	tri _ Array with: 0@-4 with: 4@3 with: -3@3.
	vertices withIndexDo:
		[ :vertPt :vertIndex |
		handle _ EllipseMorph new.
		handle on: #mouseMove send: #dragVertex:event:fromHandle:
				to: self withValue: vertIndex.
		handle on: #mouseUp send: #dropVertex:event:fromHandle:
				to: self withValue: vertIndex.
		self addMorph: handle.
		handle
			morphBoundsInWorld: (Rectangle center: vertPt extent: 8@8);
			color: Color yellow.
		handles addLast: handle.
		(closed or: [vertIndex < vertices size]) ifTrue: [
			newVert _ PolygonMorph
					vertices: (tri collect: [:p | p + (vertPt + (vertices atWrap: vertIndex+1) // 2)])
					color: Color green borderWidth: 1 borderColor: Color black.
			newVert on: #mouseDown send: #newVertex:event:fromHandle:
					to: self withValue: vertIndex.
			self addMorph: newVert.
			handles addLast: newVert]].
	smoothCurve ifTrue: [self updateHandles; someSubmorphPositionOrExtentChanged].
	self redrawNeeded! !

!PolygonMorph methodsFor: 'editing' stamp: 'jmv 8/5/2012 22:21'!
updateHandles
	| newVert oldVert midPts nextVertIx tweens |
	smoothCurve
		ifTrue: [
			handles first referencePosition: vertices first.
			handles last referencePosition: vertices last.
			midPts _ OrderedCollection new.
			nextVertIx _ 2.
			tweens _ OrderedCollection new.
			self
				lineSegmentsDo: [:p1 :p2 | 
					tweens addLast: p2 asIntegerPoint.
					p2 = (vertices atWrap: nextVertIx)
						ifTrue: ["Found endPoint."
							midPts addLast: (tweens at: tweens size // 2)
									+ (tweens at: tweens size + 1 // 2) // 2.
							tweens _ OrderedCollection new.
							nextVertIx _ nextVertIx + 1]].
			midPts withIndexDo: [:midPt :vertIndex |
				(closed or: [vertIndex < vertices size]) ifTrue: [
					newVert _ handles at: vertIndex * 2.
					newVert referencePosition: midPt ]]]
		ifFalse: [
			vertices
				withIndexDo: [ :vertPt :vertIndex | 
					oldVert _ handles at: vertIndex * 2 - 1.
					oldVert referencePosition: vertPt.
					(closed or: [vertIndex < vertices size])
						ifTrue: [
							newVert _ handles at: vertIndex * 2.
							newVert morphPosition: vertPt
									+ (vertices atWrap: vertIndex + 1) - newVert morphExtentInWorld // 2 + (1 @ -1)]]]! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 22:26'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	aWorld addMorph: self.
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphBoundsInWorld: (0@0 extent: w@h).
	labelMorph fitContents.
	subLabelMorph fitContents.
	self layoutSubmorphs.
	self align: self morphBoundsInWorld center with: Display boundingBox center.
	aWorld startSteppingSubmorphsOf: self.! !


!RectangleIndicatorMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:49'!
drawOn: aCanvas
	| bw b |
	bw _ self defaultBorderWidth.
	b _ self morphBoundsInWorld.
	aCanvas frameRectangle: b borderWidth: bw color: Color black.
	aCanvas frameRectangle: (b insetBy: bw) borderWidth: bw color: Color white! !


!StarMorph methodsFor: 'handles' stamp: 'jmv 8/5/2012 22:27'!
addHandles
	| center handle1 handle2 |
	self removeHandles.
	center _ vertices sum // vertices size.   "Average vertices to get the center"
	handle1 _ EllipseMorph new.
	handle1
		on: #mouseDown send: #dragVertex:event:fromHandle: to: self withValue: #center;
		on: #mouseMove send: #dragVertex:event:fromHandle: to: self withValue: #center.
	self addMorph: handle1.
	handle1 morphBoundsInWorld: (Rectangle center: center extent: 8@8); color: Color yellow.			
	handle2 _ EllipseMorph new.
	handle2
		on: #mouseDown send: #dragVertex:event:fromHandle: to: self withValue: #outside;
		on: #mouseMove send: #dragVertex:event:fromHandle: to: self withValue: #outside.
	self addMorph: handle2.
	handle2 morphBoundsInWorld: (Rectangle center: vertices second extent: 8@8); color: Color yellow.	
	handles _ { handle1 . handle2 }.
	self redrawNeeded! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 8/5/2012 22:39'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	ClassicTheme beCurrent.
	World backgroundImageData: nil.
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	World submorphsDo: [ :a | a delete ].
	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	"????
	Smalltalk organization removeCategoriesMatching: 'Signal Processing*'.
	SystemOrganization removeSystemCategory: 'LinearAlgebra'.
	Smalltalk organization removeCategoriesMatching: 'Sound-*'
	"

	Beeper setDefault: nil.
	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(ZipConstants GZipConstants ZipFileConstants ChronologyConstants SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSorter zapAllChangeSets.
	Smalltalk garbageCollect.


	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:47'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."
	 | r |
	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: self morphBoundsInWorld do: [ :rr |  aCollection add: rr ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas"
	r _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (self morphBoundsInWorld insetBy: 0@r) do: [ :rr |  aCollection add: rr ]! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:45'!
drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: self morphBoundsInWorld colorOrInfiniteForm: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	aCanvas fillRectangle: self titleAreaInnerRect colorOrInfiniteForm: titleColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:46'!
drawRoundedFrameOn: aCanvas color: widgetsColor
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	aCanvas
		windowFrame: self morphBoundsInWorld 
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: color! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:47'!
makeMeFullyVisible 

	self world extent > (0@0) ifFalse: [^ self].

	(self position >= (0@0) and: [ self position < (self world extent-self extent)]) ifTrue: [
		^ self "OK -- visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self isCollapsed
		ifTrue: [self position: (RealEstateAgent assignCollapsePointFor: self)]
		ifFalse: [self position: (RealEstateAgent initialFrameFor: self initialExtent: self morphBoundsInWorld world: self world) topLeft].

! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:45'!
innerBounds
	"Exclude the label area"

	^ self morphBoundsInWorld insetBy: (borderWidth @ (self labelHeight+borderWidth) corner: borderWidth @ borderWidth)! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:46'!
labelRectangle
	"Actually the whole label area"

	| tl br e x0 y0 x1 y1 b |
	b _ self morphBoundsInWorld.
	tl _ b topLeft.
	br _ b bottomRight.
	e _ self boxExtent.
	x0 _ tl x + ( e x * 4 + 14).
	y0 _ tl y + 2.
	x1 _ br x - 1.
	y1 _ tl y + e y + 1.
	^x0@y0 corner: x1@y1
	
	
	
! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:46'!
labelRectangleForEmbossed
	"Actually the whole label area"

	| tl br e x0 y0 x1 y1 b |
	b _ self morphBoundsInWorld.
	tl _ b topLeft.
	br _ b bottomRight.
	e _ self boxExtent.
	x0 _ tl x + ( e x * 4 + 14).
	y0 _ tl y + 1.
	x1 _ br x - 1.
	y1 _ tl y + e y + 2.
	^x0@y0 corner: x1@y1
	
	
	
! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/5/2012 22:45'!
titleAreaInnerRect
	"Assumes a border will be drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	^ (self morphBoundsInWorld insetBy: borderWidth) withHeight: self labelHeight! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 8/5/2012 22:37'!
changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu.  This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 8/5/2012 22:26'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	"SystemWindow fullScreen"

	| left right possibleBounds |
	left := right := 0.
	possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
	Preferences fullScreenLeavesDeskMargins 
		ifTrue: [ possibleBounds := possibleBounds insetBy: 22 ].
	self morphBoundsInWorld: possibleBounds! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 8/5/2012 22:37'!
setWindowColor
	"Allow the user to select a new basic color for the window"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self widgetsColor;
		putUpFor: self near: self morphFullBoundsInWorld"! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 8/5/2012 22:38'!
setWindowColor: incomingColor
	| existingColor aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor _ incomingColor asNontranslucentColor.
	aColor = Color black ifTrue: [^ self].
	existingColor _ self widgetsColor.
	existingColor ifNil: [^ Beeper beep].
	self widgetsColor: aColor.
	self redrawNeeded! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 8/5/2012 22:26'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphBoundsInWorld: (RealEstateAgent initialFrameFor: self world: aWorld).
	self activate.
	aWorld startSteppingSubmorphsOf: self.! !

!SystemWindow methodsFor: 'top window' stamp: 'jmv 8/5/2012 22:46'!
activateAndForceLabelToShow
	self activate.
	self morphPositionInWorld y < 0 ifTrue: [
		self morphPosition: (self morphPosition x @ 0)]! !

!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 8/5/2012 22:56'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(#titleAreaRect and #titleAreaInnerRect)"
	self invalidRect: (self morphBoundsInWorld withHeight: self labelHeight + borderWidth)! !

!SystemWindow methodsFor: 'layout' stamp: 'jmv 8/5/2012 22:52'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| bl br h thickness tl tr w cornerExtent b |
	thickness _ 4.
	cornerExtent _ 20.
	b _ self morphBoundsInWorld.
	tl _ b topLeft.
	tr _ b topRight.
	bl _ b bottomLeft.
	br _ b bottomRight.
	w _ b width - cornerExtent - cornerExtent.
	h _ b height - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphBoundsInWorld: (tl + (cornerExtent@0) extent: w@thickness).
	(adjusters at: #bottomAdjuster) morphBoundsInWorld: (bl+ (cornerExtent @ thickness negated) extent: w@thickness).
	(adjusters at: #leftAdjuster) morphBoundsInWorld: (tl+ (0@cornerExtent) extent: thickness@h).
	(adjusters at: #rightAdjuster) morphBoundsInWorld: (tr + (thickness negated@ cornerExtent) extent: thickness@h).
	(adjusters at: #topLeftAdjuster) morphBoundsInWorld: (tl extent: cornerExtent@cornerExtent).
	(adjusters at: #bottomLeftAdjuster) morphBoundsInWorld: (bl-(0@cornerExtent) extent: cornerExtent@cornerExtent).
	(adjusters at: #topRightAdjuster) morphBoundsInWorld: (tr+(cornerExtent negated@0) extent: cornerExtent@cornerExtent).
	(adjusters at: #bottomRightAdjuster) morphBoundsInWorld: (br-cornerExtent extent: cornerExtent@cornerExtent).

	layoutMorph ifNotNil: [
		layoutMorph morphBoundsInWorld: self layoutBounds ]! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:26'!
windowBottom: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld bottom: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:26'!
windowBottomLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (aPoint x @ self morphBoundsInWorld top corner: self morphBoundsInWorld right @ aPoint y)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:49'!
windowBottomRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (self morphPositionInWorld corner: aPoint)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:27'!
windowLeft: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld left: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:27'!
windowRight: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld right: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:27'!
windowTop: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld top: aNumber)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:27'!
windowTopLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (aPoint corner: self morphBoundsInWorld corner)! !

!SystemWindow methodsFor: 'resizing' stamp: 'jmv 8/5/2012 22:45'!
windowTopRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld left @ aPoint y corner: aPoint x @ self morphBoundsInWorld bottom)! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 8/5/2012 22:28'!
step

	"My dimensions are constrained live."
	| r |
	r _ World morphBoundsInWorld.
	r _ r left @ (r bottom -18) extent: r width@18.
	self morphBoundsInWorld = r ifFalse: [
		self morphBoundsInWorld: r]! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 8/5/2012 22:38'!
chooseColor
	"Make a new Text Color Attribute, let the user pick a color, and return the attribute"

	| |
	"(ColorPickerMorph new)
		choseModalityFromPreference;
		sourceHand: morph activeHand;
		target: (attribute := TextColor color: Color black);
		selector: #color:;
		originalColor: Color black;
		putUpFor: morph near: morph morphFullBoundsInWorld.
	^attribute"! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 22:29'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: true;
		bounds: bounds;
		displayOn: aCanvas form.
	"
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	Transcript
		showOnDisplay: true;
		morphBoundsInWorld: (0@0 extent: self morphExtentInWorld);
		displayOn: form;
		morphBoundsInWorld: self morphBoundsInWorld.
	aCanvas image: form at: self morphPositionInWorld! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2012 22:51'!
containsPoint: aPoint
	| sensitiveBorder b |
	b _ self morphBoundsInWorld.
	(b containsPoint: aPoint) ifFalse: [ ^false ].
	sensitiveBorder _ 4.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aPoint x - b left < sensitiveBorder or: [ aPoint y - b top < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ b right - aPoint x <= sensitiveBorder or: [ aPoint y - b top < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aPoint x - b left < sensitiveBorder or: [ b bottom - aPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ b right - aPoint x <= sensitiveBorder or: [ b bottom - aPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !

!methodRemoval: Transcript class #bounds:!
Transcript class removeSelector: #bounds:!
!methodRemoval: Morph #bounds!
Morph removeSelector: #bounds!
!methodRemoval: Morph #bounds:!
Morph removeSelector: #bounds:!
!classRemoval: #ColorPickerMorph!
Smalltalk removeClassNamed: #ColorPickerMorph!
!classRemoval: #SketchMorph!
Smalltalk removeClassNamed: #SketchMorph!
