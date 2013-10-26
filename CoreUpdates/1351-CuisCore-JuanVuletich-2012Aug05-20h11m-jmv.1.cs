'From Cuis 4.0 of 21 April 2012 [latest update: #1350] on 5 August 2012 at 8:15:48 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 19:32'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Should implement in some reasonable way... including submorphs?"
	self flag: #jmvVer2.
	self layoutSubmorphsAndComputeFullBounds.

	self flag: #jmvVer2.
	^self morphBoundsInWorld! !


!HandMorph methodsFor: 'layout' stamp: 'jmv 8/5/2012 19:42'!
morphFullBoundsInWorld
	"Extend my bounds by the shadow offset when carrying morphs."
"
	self layoutSubmorphsAndComputeFullBounds.
	^submorphs isEmpty
		ifTrue: [ fullBounds ]
	
		ifFalse: [ fullBounds topLeft corner: fullBounds bottomRight + self shadowOffset ].
"

	"Morphs should know nothing about absolute coordinates..."
	"In addition, considering just first submorph... should include all of them"
	| r |
	self flag: #jmvVer2.
	^submorphs isEmpty
		ifTrue: [ super morphFullBoundsInWorld ]
		ifFalse: [
			r _ super morphFullBoundsInWorld merge: submorphs first morphFullBoundsInWorld.
			r origin corner: r corner + self shadowOffset ]! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:37'!
drawDropHighlightOn: aCanvas
	self highlightedForDrop ifTrue: [
		aCanvas frameRectangle: self morphFullBoundsInWorld borderWidth: 1 color: self dropHighlightColor ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:37'!
drawMouseDownHighlightOn: aCanvas
	self highlightedForMouseDown ifTrue: [
		aCanvas frameRectangle: self morphFullBoundsInWorld borderWidth: 1 color: self color darker darker ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:38'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(aCanvas isVisible: self morphFullBoundsInWorld) ifFalse:[^self].		"Needs fullBounds 'in owner' if inside a scroller"
	self isKnownFailing ifTrue: [^self drawErrorOn: aCanvas].

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		aCanvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas.
	self drawDropHighlightOn: aCanvas.
	self drawMouseDownHighlightOn: aCanvas! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:38'!
imageForm

	^ self imageForm: Display depth forRectangle: self morphFullBoundsInWorld! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:38'!
imageForm: depth

	^ self imageForm: depth forRectangle: self morphFullBoundsInWorld! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:38'!
shadowForm
	"Return a form representing the 'shadow' of the receiver - e.g., all pixels that are occupied by the receiver are one, all others are zero."
	| bnds canvas |
	bnds _ self morphFullBoundsInWorld.
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	self fullDrawOn: canvas.
	^ canvas formWithOffset! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/5/2012 19:37'!
containsPoint: aPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [ ^false ].
	(self containsPoint: aPoint) ifTrue: [ ^true ].
	self submorphsDo: [ :m |
		(m isLocked and: [ m fullContainsPoint: aPoint ]) ifTrue: [ ^true ]].
	^false! !

!Morph methodsFor: 'geometry' stamp: 'jmv 8/5/2012 19:39'!
worldBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ Preferences haloEnclosesFullBounds
		ifFalse: [ self morphBoundsInWorld ]
		ifTrue: [ self morphFullBoundsInWorld ]! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 8/5/2012 19:37'!
fullContainsPoint: aPoint
"
	This alternative implementation is included in this comment because it could be useful someday.
	If we start to rely heavily on the use of #ownShadowForm in #containsPoint, this could be cheaper.
	
	| shadow |
	self clipSubmorphs
		ifTrue: [ ^self containsPoint: aPoint ]
		ifFalse: [
			(self fullBounds containsPoint: aPoint) ifFalse: [^ false].
			(self containsPoint: aPoint) ifTrue: [^ true].
			shadow _ self shadowForm.
			^(shadow pixelValueAt: aPoint - shadow offset) > 0 ]
"
	
	self flag: #jmvVer2.
	"Is the comment relevant now?"
	
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [ ^ false ].  "quick elimination"
	(self containsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	submorphs do: [:m | (m fullContainsPoint: aPoint) ifTrue: [ ^ true ]].
	^ false
! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/5/2012 16:40'!
layoutSubmorphsAndComputeFullBounds
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	fullBounds ifNotNil: [ ^self ].

	self layoutSubmorphs.
	fullBounds _ self computeFullBounds.
	fullBounds _ bounds! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/5/2012 19:38'!
submorphBounds
	"Private. Compute the actual full bounds of the receiver"

	"Remove when removing morphFullBoundsInWorld? Reimplement?"
	self flag: #jmvVer2.

	^submorphs inject: nil into: [ :prevBox :m |
		m visible
			ifTrue: [ m morphFullBoundsInWorld quickMerge: prevBox ]
			ifFalse: [ prevBox ] ]! !

!Morph methodsFor: 'menus' stamp: 'jmv 8/5/2012 19:37'!
changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu"

	ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self activeHand;
		target: self;
		selector: #color:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 8/5/2012 19:37'!
changeColorTarget: anObject selector: aSymbol originalColor: aColor hand: aHand
	"Put up a color picker for changing some kind of color.  May be modal or modeless, depending on #modalColorPickers setting"
	self flag: #arNote. "Simplify this due to anObject == self for almost all cases"
	^ ColorPickerMorph new
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
		yourself! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 8/5/2012 19:38'!
morphsAt: aPoint behind: aMorph unlocked: aBool 
	"Return all morphs at aPoint that are behind frontMorph; if aBool is true return only unlocked, visible morphs."

	| isBack found all |
	all _ (aMorph isNil or: [owner isNil]) 
				ifTrue: [
					"Traverse down"
					(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [^#()].
					(aBool and: [self isLocked or: [self visible not]]) ifTrue: [^#()].
					nil]
				ifFalse: ["Traverse up"
					all _ owner 
								morphsAt: aPoint
								behind: self
								unlocked: aBool.
					WriteStream with: all].
	isBack _ aMorph isNil.
	self submorphsDo: [ :m |
			isBack 
				ifTrue: [
					found _ m 
								morphsAt: aPoint
								behind: nil
								unlocked: aBool.
					found notEmpty 
						ifTrue: 
							[all ifNil: [all _ WriteStream on: #()].
							all nextPutAll: found]].
			m == aMorph ifTrue: [isBack _ true]].
	(isBack and: [self containsPoint: aPoint]) 
		ifTrue: 
			[all ifNil: [^Array with: self].
			all nextPut: self].
	^all ifNil: [#()] ifNotNil: [all contents]! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 8/5/2012 19:38'!
morphsAt: aPoint unlocked: aBool do: aBlock
	"Evaluate aBlock with all the morphs starting at the receiver which appear at aPoint. If aBool is true take only visible, unlocked morphs into account."
	| |
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse:[^self].
	(aBool and:[self isLocked or:[self visible not]]) ifTrue:[^self].
	self submorphsDo: [ :m |
		m morphsAt: aPoint unlocked: aBool do: aBlock].
	(self containsPoint: aPoint) ifTrue:[aBlock value: self].! !

!Morph methodsFor: 'private' stamp: 'jmv 8/5/2012 20:15'!
privateMoveBy: delta
	"Private!! Use 'position:' instead."

	"All these will die soon!!"

	self flag: #jmvVer2.
	self validateOwnerNotNil.

	bounds _ bounds translateBy: delta.
	fullBounds ifNotNil: [ 
		fullBounds _ fullBounds translateBy: delta.
		fullBounds _ bounds ]! !


!ColorPickerMorph methodsFor: 'menu' stamp: 'jmv 8/5/2012 19:35'!
pickUpColorFor: aMorph
	"Show the eyedropper cursor, and modally track the mouse through a mouse-down and mouse-up cycle"

      | aHand localPt oldCursor |
	aHand _ aMorph isNil
		ifTrue: [self world activeHand] 
		ifFalse: [ aMorph activeHand].
	self addToWorld: aHand world near: (aMorph ifNil: [aHand world]) morphFullBoundsInWorld.
	self owner ifNil: [^ self].

	oldCursor _ Sensor currentCursor.
	ColorPickerMorph eyeDropperCursor show.

	self updateContinuously: false.
	[Sensor anyButtonPressed]
		whileFalse: 
			 [self trackColorUnderMouse].
	self deleteAllBalloons.

	localPt _World activeHand morphPosition - bounds topLeft.
	self inhibitDragging ifFalse: [
		(DragBox containsPoint: localPt) ifTrue:
			["Click or drag the drag-dot means to anchor as a modeless picker"
			^ self anchorAndRunModeless: aHand].
	].
	(clickedTranslucency _ TransparentBox containsPoint: localPt)
		ifTrue: [selectedColor _ originalColor].

	self updateContinuously: true.
	[Sensor anyButtonPressed]
		whileTrue:
			 [self updateTargetColorWith: self indicateColorUnderMouse].
	aHand newMouseFocus: nil;
		flushEvents.
	oldCursor show.
	self delete.
		 
 ! !

!ColorPickerMorph methodsFor: 'other' stamp: 'jmv 8/5/2012 19:35'!
putUpFor: aMorph near: aRectangle
	"Put the receiver up on the screen.   Note highly variant behavior depending on the setting of the #modalColorPickers preference"
	| layerNumber |
	(aMorph is: #Morph) ifTrue: [
		layerNumber _ aMorph morphicLayerNumber.
		aMorph allOwnersDo: [ : m|
			layerNumber _ layerNumber min: m morphicLayerNumber].
		self setProperty: #morphicLayerNumber toValue: layerNumber - 0.1 ].

	isModal == true "backward compatibility"
		ifTrue: [
			self pickUpColorFor: aMorph]
		ifFalse: [
			self addToWorld:
				((aMorph notNil and: [aMorph world notNil])
					ifTrue: [ aMorph world ]
					ifFalse: [ self currentWorld ])
		  		near:
					(aRectangle ifNil:
						[aMorph ifNil: [100@100 extent: 1@1] ifNotNil: [ aMorph morphFullBoundsInWorld ]])]! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 8/5/2012 19:35'!
containsPoint: aPoint event: anEvent
	"mouseButton3 events are handled by the halo"
	(anEvent isMouse and: [ anEvent isMouseDown and: [ anEvent mouseButton3Pressed ]])
		ifFalse:  [^super containsPoint: aPoint event: anEvent ].
	^self morphFullBoundsInWorld containsPoint: anEvent eventPosition! !

!HaloMorph methodsFor: 'updating' stamp: 'jmv 8/5/2012 19:36'!
redrawNeeded
	"Quicker to invalidate handles individually if target is large (especially the world)"

	self validatePositionAndBounds.
	self validateExtentAndBounds.
	extent > (200@200)
		ifTrue: [(target notNil and: [target ~~ self world]) ifTrue: [
					"Invalidate 4 outer strips first, thus subsuming separate damage."
					(self morphFullBoundsInWorld areasOutside: target morphBoundsInWorld) do:
						[ :r | self invalidRect: r ]].
				self submorphsDo: [:m | m redrawNeeded]]
		ifFalse: [ super redrawNeeded ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:36'!
fullDrawOn: aCanvas 
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."

	self visible ifFalse: [^self].
	(aCanvas isVisible: self morphFullBoundsInWorld) ifFalse: [^self].
	self nonCachingFullDrawOn: aCanvas.
! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:40'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| ownBnds fullBnds bw |
	ownBnds _ self bounds.
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

!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:36'!
shadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	self drawSubmorphsOn: canvas.
	^ canvas formWithOffset! !


!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 8/5/2012 19:36'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	scroller hasSubmorphs ifFalse: [ ^nil ].
	(scroller morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	scroller firstSubmorph morphPositionInWorld y > ptY ifTrue: [ ^nil ].
	last _ scroller lastSubmorph.
	last morphPositionInWorld y + last morphExtentInWorld y < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^scroller 
		findSubmorphBinary: [ :m |
			(m morphPositionInWorld y <= ptY and: [ m morphPositionInWorld y + m morphExtentInWorld y >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPositionInWorld y + (m morphExtentInWorld y // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 8/5/2012 19:37'!
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition - self morphPosition.
	sourceItem owner owner addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPoint first value: false;
		value: rightOrLeftPoint last - (self morphWidth @ 0) value: false;
		value: rightOrLeftPoint first value: true! !


!MorphicEventDispatcher methodsFor: 'dispatching' stamp: 'jmv 8/5/2012 19:39'!
dispatchDefault: anEvent with: aMorph
	"Dispatch the given event. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| inside |
	"See if we're fully outside aMorphs bounds"
	(aMorph morphFullBoundsInWorld containsPoint: anEvent eventPosition) ifFalse: [ ^#rejected ]. "outside"

	"Traverse children"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild processEvent: anEvent using: self) == #rejected ifFalse: [
				"Not rejected. The event was in some submorph of the receiver"
				inside _ true
			]]].

	"Check for being inside the receiver"
	inside ifFalse: [ inside _ aMorph containsPoint: anEvent eventPosition event: anEvent ].
	inside ifTrue: [ ^aMorph handleEvent: anEvent ].
	^ #rejected! !

!MorphicEventDispatcher methodsFor: 'dispatching' stamp: 'jmv 8/5/2012 19:39'!
dispatchDropEvent: anEvent with: aMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside |

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: anEvent eventPosition)
		ifFalse: [ ^#rejected ].

	"Give aMorph a chance to repel the dropping morph"
	aMorph rejectDropEvent: anEvent.
	anEvent wasHandled ifTrue:[^self].

	"Go looking if any of our submorphs wants it"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild processEvent: anEvent using: self) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: anEvent eventPosition event: anEvent ].
	inside ifTrue: [ ^aMorph handleEvent: anEvent ].
	^#rejected! !

!MorphicEventDispatcher methodsFor: 'dispatching' stamp: 'jmv 8/5/2012 19:39'!
dispatchMouseDown: anEvent with: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
"
	| globalPt handler inside lastHandler |
	"Try to get out quickly"
	globalPt _ anEvent eventPosition.
	(aMorph morphFullBoundsInWorld containsPoint: globalPt) ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	lastHandler _ anEvent handler. "in case the mouse wasn't even in the receiver"
	handler _ aMorph handlerForMouseDown: anEvent.
	handler ifNotNil: [ anEvent handler: handler ].

	"Now give our submorphs a chance to handle the event"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			(eachChild processEvent: anEvent using: self) == #rejected ifFalse: [
				"Some child did contain the point so we're part of the top-most chain."
				inside _ true.
			]]].

	(inside or: [ aMorph containsPoint: anEvent eventPosition event: anEvent ]) ifTrue:[
		"Receiver is in the top-most unlocked, visible chain."
		handler ifNotNil: [ handler handleEvent: anEvent ].
		"Note: Re-installing the handler is not really necessary but good style."
		anEvent handler: lastHandler.
		^self ].

	"Mouse was not on receiver nor any of its children"
	anEvent handler: lastHandler.
	^#rejected! !


!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 8/5/2012 19:39'!
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
					aMorph morphPosition: bounds center]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m isKindOf: HaloMorph) ifTrue: [ m delete ]].
	self world startSteppingSubmorphsOf: aMorph! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 8/5/2012 19:39'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphFullBoundsInWorld extent.
	delta _ trialRect amountToTranslateWithin: bounds.
	self addMorph: aMorph.
	aMorph morphPosition: trialRect origin + delta.! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 8/5/2012 19:39'!
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
	self align: self morphBoundsInWorld center with: Display boundingBox center.
	aWorld startSteppingSubmorphsOf: self.! !


!SystemWindow methodsFor: 'menu' stamp: 'jmv 8/5/2012 19:39'!
changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu.  This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu"

	ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld! !

!SystemWindow methodsFor: 'menu' stamp: 'jmv 8/5/2012 19:39'!
setWindowColor
	"Allow the user to select a new basic color for the window"

	ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self widgetsColor;
		putUpFor: self near: self morphFullBoundsInWorld! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 8/5/2012 19:39'!
chooseColor
	"Make a new Text Color Attribute, let the user pick a color, and return the attribute"

	| attribute |
	(ColorPickerMorph new)
		choseModalityFromPreference;
		sourceHand: morph activeHand;
		target: (attribute := TextColor color: Color black);
		selector: #color:;
		originalColor: Color black;
		putUpFor: morph near: morph morphFullBoundsInWorld.	"default"
	^attribute! !


!WorldState methodsFor: 'hands' stamp: 'jmv 8/5/2012 19:39'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result hBnds |
	result _ OrderedCollection new.
	hands do: [:h |
		h needsToBeDrawn ifTrue: [
			h hasChanged
				ifTrue: [result add: h]
				ifFalse: [
					hBnds _ h morphFullBoundsInWorld.
					(damageList detect: [:r | r intersects: hBnds] ifNone: nil)
						ifNotNil: [result add: h]]]].
	^ result
! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:39'!
drawHand: aHandMorph

	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph morphFullBoundsInWorld.
		canvas frameRectangle: r borderWidth: bw color: Color black.
		canvas frameRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		aHandMorph fullDrawOn: canvas ]! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 8/5/2012 19:39'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ OrderedCollection new.
	(damageRecorder invalidRectsFullBounds: aWorld viewBox) do: [ :r |
		r ifNotNil: [ initialRectsToRepair addLast: r ]].
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _morph morphFullBoundsInWorld.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			(canvas copyClipRect: r) clipBy: aWorld morphBoundsInWorld during: [ :c | aWorld drawOn: c ]]].
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could merge all xrects into just one call... Most likely, that would be slower, though."
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			m fullDrawOn: (canvas copyClipRect: r)
		].
"		(canvas copyClipRect: rr) fullDrawMorph: m "
	].
	
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!methodRemoval: HandMorph #fullBounds!
HandMorph removeSelector: #fullBounds!
!methodRemoval: Morph #fullBounds!
Morph removeSelector: #fullBounds!
!methodRemoval: Morph #nearestOwnerThat:!
Morph removeSelector: #nearestOwnerThat:!
