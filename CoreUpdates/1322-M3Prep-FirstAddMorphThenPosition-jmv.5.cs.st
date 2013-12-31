'From Cuis 4.0 of 3 April 2012 [latest update: #1255] on 10 April 2012 at 3:03:39 pm'!

!InnerTextMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/13/2011 23:05'!
addMorphFrontFromWorldPosition: aMorph
	"Overridden for more specific re-layout and positioning"
	| positionInWorld |
	positionInWorld _ aMorph morphPosition.
	^self anchorMorph: aMorph at: positionInWorld! !


!MenuMorph methodsFor: 'private' stamp: 'jmv 12/13/2011 20:42'!
positionAt: aPoint relativeTo: aMenuItem
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |
	self adjustSubmorphsLayout.
	i _ 0.
	yOffset _ 0.
	[(sub _ self submorphs at: (i _ i + 1)) == aMenuItem]
		whileFalse: [ yOffset _ yOffset + sub morphHeight ].

	self morphPosition: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	bounds right > owner world bounds right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ bounds amountToTranslateWithin:
		(owner world bounds withHeight: ((owner world bounds height - 18) max: (ActiveHand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 23:36'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	aWorld addMorph: self.
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self bounds: (0@0 extent: w@h).
	labelMorph fitContents.
	subLabelMorph fitContents.
	self layoutSubmorphs.
	self align: self fullBounds center with: Display boundingBox center.
	aWorld startSteppingSubmorphsOf: self.! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 12/13/2011 21:12'!
addHalo: evt
	| halo prospectiveHaloClass |
	prospectiveHaloClass _ Smalltalk at: self haloClass ifAbsent: [HaloMorph].
	halo _ prospectiveHaloClass new.
	halo popUpFor: self event: evt.
	halo bounds: self worldBoundsForHalo.
	^halo! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 23:12'!
openInWorld: aWorld
	"Add this morph to the requested World."
	aWorld addMorph: self.
	aWorld startSteppingSubmorphsOf: self! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/13/2011 23:05'!
addMorphFrontFromWorldPosition: aMorph
	| positionInWorld |
	positionInWorld _ aMorph morphPosition.
	self addMorphFront: aMorph.
	aMorph morphPosition: positionInWorld! !


!AutoCompleterMorph methodsFor: 'initialization' stamp: 'jmv 12/14/2011 16:48'!
setCompleter: anAutoCompleter position: aPoint 
	completer _ anAutoCompleter.
	self resetMenu.
	self openInWorld.
	self morphPositionInOwner: aPoint.! !


!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 12/13/2011 19:44'!
addToWorld: world near: box
	| goodLocation |
	goodLocation _ self bestPositionNear: box inWorld: world.
	world allMorphsDo:
		[:p | (p isMemberOf: ColorPickerMorph) ifTrue:
		[(p ~~ self and: [p owner notNil and: [p target == target]]) ifTrue:
			[(p selector == selector and: [p argument == argument])
				ifTrue: [^ p comeToFront  "uncover existing picker"]
				ifFalse: ["place second picker relative to first"
						goodLocation _ self bestPositionNear: p bounds inWorld: world]]]].
	world addMorphFront: self.
	self morphPositionInOwner: goodLocation.
	self redrawNeeded! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 12/13/2011 19:44'!
indicateColorUnderMouse
	"Track the mouse with the special eyedropper cursor, and accept whatever color is under the mouse as the currently-chosen color; reflect that choice in the feedback box, and return that color."

	| pt |
	self pickColorAt: (pt _ World activeHand morphPosition ).
	isModal ifTrue: [
		self world activeHand morphPositionInOwner: pt.
		self world displayWorldSafely; runStepMethods].
	^ selectedColor	! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 12/13/2011 19:45'!
trackColorUnderMouse
	"Track the mouse with the special eyedropper cursor, and accept whatever color is under the mouse as the currently-chosen color; reflect that choice in the feedback box, and return that color."

	| pt |
	selectedColor _ originalColor.
	self trackColorAt: (pt _ World activeHand morphPosition ).
	isModal ifTrue: [
		self world activeHand morphPositionInOwner: pt.
		self world displayWorldSafely; runStepMethods.
		self modalBalloonHelpAtPoint: pt].
	^ selectedColor	! !

!ColorPickerMorph methodsFor: 'private' stamp: 'jmv 12/13/2011 19:44'!
anchorAndRunModeless: aHand
	"If user clicks on the drag-dot of a modal picker,
	anchor it, and change to modeless operation."

	self initializeModal: false; originalColor: originalColor.  "reset as modeless"
	aHand flushEvents.  "Drop any events gathered during modal loop"
	aHand morphPositionInOwner: Sensor mousePoint; grabMorph: self.  "Slip into drag operation"
! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 4/10/2012 14:59'!
createAcceptButton
	"create the [accept] button"
	| result buttonColor theme |
	theme _ Theme current.
	theme useUniformColors
		ifTrue: [ buttonColor _ theme buttonColorFrom: theme defaultWindowColor ]
		ifFalse: [ buttonColor _ theme acceptButton].
	result _ PluggableButtonMorph new
		 model: self;
		 color: buttonColor;
		 label: 'Accept';
		 action: #acceptClicked.
	self addMorph: result.
	result bounds: (29@90 corner: 122@117).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 4/10/2012 14:59'!
createCancelButton
	"create the [cancel] button"
	| result buttonColor theme |
	theme _ Theme current.
	theme useUniformColors
		ifTrue: [ buttonColor _ theme buttonColorFrom: theme defaultWindowColor ]
		ifFalse: [ buttonColor _ theme buttonColorFrom: theme cancelButton ].
	result _ PluggableButtonMorph new
		 model: self;
		 color: buttonColor;
		 label: 'Cancel';
		 action: #cancelClicked.
	self addMorph: result.
	result bounds: (149@90 corner: 242@117).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 22:52'!
createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	self addMorph: result.
	result bounds: ( 30@7 corner: 269@22).
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 22:52'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |
	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval.
	result extent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result bounds: (14@25 corner: 257@84).
	^ result! !


!HaloMorph methodsFor: 'events' stamp: 'jmv 12/13/2011 21:12'!
popUpFor: aMorph event: evt
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ evt
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastEvent ]
				ifNotNil: [
					hand _ evt hand.
					evt ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPosition.
	self startStepping! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/31/2011 00:15'!
addHandle: handleSpec on: eventName send: selector to: recipient 
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse icon |
	aPoint := self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle bounds: (Rectangle center: aPoint extent: HandleSize asPoint).
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

!HaloMorph methodsFor: 'private' stamp: 'jmv 12/13/2011 20:24'!
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
	nameMorph morphPosition: (namePosition min: w viewBox bottomRight - nameMorph extent y + 5).
	nameBackground bounds: (nameMorph bounds outsetBy: 2).
	^nameMorph! !


!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/13/2011 20:28'!
attachMorph: m
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."
	| delta |
	self releaseMouseFocus. "Break focus"
	self addMorphBack: m.
	delta _ m bounds extent // 2.
	m morphPosition: (self morphPosition - delta).
	m formerPosition: m morphPosition.
	targetOffset _ m morphPosition - self morphPosition.! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 12/13/2011 20:29'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| xcess |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	aHand world addMorphFront: self.
	self morphPosition: aHand morphPosition + (-6@20).
	xcess _ bounds right - aHand world bounds right.
	xcess > 0 ifTrue: [
		self morphPosition: self morphPosition - (xcess@0) ].
	aHand balloonHelp: self! !


!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 12/14/2011 18:14'!
adjustExtent
	"And reposition submorphs"
	| w p0 h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	p0 _ bounds topLeft..
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m bounds: (p0 + (0@y) extent: w@h).
		y _ y + h ].
	self extent: w@y! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 12/13/2011 20:30'!
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
	marker morphPosition: bounds left @ (bounds top + 2).! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 12/13/2011 23:07'!
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition - self morphPosition.
	sourceItem owner owner addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self fullBounds amountToTranslateWithin: sourceItem world bounds.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPoint first value: false;
		value: rightOrLeftPoint last - (self width @ 0) value: false;
		value: rightOrLeftPoint first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 12/13/2011 20:42'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [:m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [:m | m updateContents].
	aWorld addMorphFront: self.
	self 
		positionAt: aPoint
		relativeTo: (selectedItem ifNil: [self items first]).
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [hand newKeyboardFocus: self].
	evt := hand lastEvent.
	(evt isKeyboard or: [evt isMouse and: [evt anyButtonPressed not]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 12/13/2011 22:55'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m isKindOf: MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock.
		feedbackMorph morphPosition: self morphPosition - (0@20).
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self submorphs last delete.
		self removeProperty: #feedbackMorph ].
	matches size = 1 ifTrue: [
		self
			selectItem: matches first
			event: evt ].! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 12/13/2011 23:34'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ ActiveWorld.
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString width + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w displayWorld		 "show myself"
	]. 
	self delete.
	w displayWorld! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 12/13/2011 23:36'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					paraTopLeft ]
		ifFalse: [
			destY _ lineY.
			runX _ destX.
			anchoredFormOrMorph 
				displayOn: canvas grafPort destForm 
				at: destX - anchoredFormOrMorph morphWidth @ (destY + line baseline - anchoredFormOrMorph morphHeight)
				clippingBox: canvas grafPort clipRect
				rule: Form blend
				fillColor: nil ].
	^ true! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/13/2011 22:53'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph fullBounds extent.
	delta _ trialRect amountToTranslateWithin: bounds.
	self addMorph: aMorph.
	aMorph morphPosition: trialRect origin + delta.! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 12/13/2011 23:56'!
addToScroller: aMorph

	scroller addMorph: aMorph.
	aMorph morphPosition: scroller morphPosition! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 12/31/2011 00:18'!
hHideScrollBar
	hScrollBar hide.
	scroller adjustExtent! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 12/31/2011 00:18'!
hShowScrollBar

	hScrollBar show.
	scroller adjustExtent! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 12/31/2011 00:19'!
vHideScrollBar
	scrollBar hide.
	scroller adjustExtent! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 12/31/2011 00:19'!
vShowScrollBar

	scrollBar show.
	scroller adjustExtent! !


!PolygonMorph methodsFor: 'editing' stamp: 'jmv 12/13/2011 23:58'!
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
									+ (vertices atWrap: vertIndex + 1) - newVert bounds extent // 2 + (1 @ -1)]]]! !


!ProgressMorph methodsFor: 'accessing' stamp: 'jmv 12/13/2011 23:20'!
label: aString subLabel: otherString
	labelMorph contents: aString.
	subLabelMorph contents: otherString! !


!ProgressMorph class methodsFor: 'example' stamp: 'jmv 12/13/2011 23:38'!
example
	"
	ProgressMorph example
	"
	| progress |
	progress _ ProgressMorph label: 'Test progress' subLabel: 'this is the subheading'.
	progress openInWorld.
	[
		100 timesRepeat: [
			(Delay forMilliseconds: 20) wait.
			progress incrDone: 0.01 ].
		progress delete] fork! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/14/2011 18:24'!
createCloseBox
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		icon: Theme current closeIcon;
		setBalloonText: 'close this window'! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/14/2011 18:24'!
createCollapseBox
	^(PluggableButtonMorph model: self action: #collapseOrExpand)
		icon: Theme current collapseIcon;
		setBalloonText: 'collapse this window'! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/14/2011 18:24'!
createExpandBox
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		icon: Theme current expandIcon;
		setBalloonText: 'expand to full screen'! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/14/2011 18:24'!
createMenuBox
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		icon: Theme current windowMenuIcon;
		setBalloonText: 'window menu'! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 12/14/2011 18:24'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| spacing box |
	spacing _ self boxExtent x + 2.

	box _ self createCloseBox.
	self addMorph: box.
	box morphPosition: 2@2.
	box extent: self boxExtent.

	box _ self createCollapseBox.
	self addMorph: box.
	box morphPosition: spacing+2@2.
	box extent: self boxExtent.

	box _ self createExpandBox.
	self addMorph: box.
	box morphPosition: spacing*2+2@2.
	box extent: self boxExtent.

	box _ self createMenuBox.
	self addMorph: box.
	box morphPosition: spacing*3+2@2.
	box extent: self boxExtent.! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 12/13/2011 21:06'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self bounds: (RealEstateAgent initialFrameFor: self world: aWorld).
	self activate.
	aWorld startSteppingSubmorphsOf: self.! !

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 12/13/2011 21:07'!
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self morphPosition: (RealEstateAgent initialFrameFor: self world: aWorld) topLeft; extent: extent.
	self activate.
	aWorld startSteppingSubmorphsOf: self.! !

!methodRemoval: SystemWindow #openAsIsIn:!
SystemWindow removeSelector: #openAsIsIn:!
!methodRemoval: ProgressMorph #setupMorphs!
ProgressMorph removeSelector: #setupMorphs!
!methodRemoval: MenuMorph #positionAt:relativeTo:inWorld:!
MenuMorph removeSelector: #positionAt:relativeTo:inWorld:!
!methodRemoval: InnerTextMorph #addMorphFront:fromWorldPosition:!
InnerTextMorph removeSelector: #addMorphFront:fromWorldPosition:!
!methodRemoval: Morph #addMorphFront:fromWorldPosition:!
Morph removeSelector: #addMorphFront:fromWorldPosition:!
!methodRemoval: Morph #privateBounds:!
Morph removeSelector: #privateBounds:!
