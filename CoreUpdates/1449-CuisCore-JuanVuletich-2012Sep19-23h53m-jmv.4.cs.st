'From Cuis 4.0 of 21 April 2012 [latest update: #1448] on 20 September 2012 at 12:16:48 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/19/2012 23:38'!
submorphsShadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas into: self.
	self drawSubmorphsOn: canvas.
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 9/20/2012 00:04'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files). If self isStereo is true, both channels are stored, creating a stereo file. Otherwise, only the left channel is stored, creating a mono file."

	| bufSize stereoBuffer reverseBytes  |
	self reset.
	bufSize _ (2 * self samplingRate rounded) min: samplesToStore.  "two second buffer"
	stereoBuffer _ SoundBuffer newStereoSampleCount: bufSize.
	reverseBytes _ bigEndianFlag ~= Smalltalk isBigEndian.

	'Storing audio...' displayProgressAt: World activeHand morphPositionInOwner
		from: 0 to: samplesToStore during: [:bar | | remaining out |
			remaining _ samplesToStore.
			[remaining > 0] whileTrue: [
				bar value: samplesToStore - remaining.
				stereoBuffer primFill: 0.  "clear the buffer"
				self playSampleCount: (bufSize min: remaining) into: stereoBuffer startingAt: 1.
				self isStereo
					ifTrue: [out _ stereoBuffer]
					ifFalse: [out _ stereoBuffer extractLeftChannel].
				reverseBytes ifTrue: [out reverseEndianness].
				(aBinaryStream isKindOf: StandardFileStream)
					ifTrue: [  "optimization for files: write sound buffer directly to file"
						aBinaryStream next: (out size // 2) putAll: out startingAt: 1]  "size in words"
					ifFalse: [  "for non-file streams:"
						1 to: out monoSampleCount do: [:i | aBinaryStream int16: (out at: i)]].
				remaining _ remaining - bufSize]].
! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/19/2012 23:15'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ Display defaultCanvasClass forShadowOver: self morphBoundsInWorld.
	canvas into: self.
	canvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ].
	^ canvas form! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 23:40'!
containsPoint: aPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	(self morphContainsPoint: aPoint) ifTrue: [ ^true ].
	self clipsSubmorphs ifFalse: [
		self submorphsDo: [ :m |
			(m isLocked and: [ m fullContainsPoint: (m internalize: aPoint) ])
				ifTrue: [ ^true ]]].
	^false! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 23:55'!
processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	aMouseEvent hand mouseFocus == self ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: localEventPosition event: aMouseEvent) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/20/2012 00:15'!
rejectDropEvent: aMorphicEvent
	"This hook allows the receiver to repel a drop operation currently executed. The method is called prior to checking children so the receiver must validate that the event was really designated for it.
	Note that the ordering of the tests below is designed to avoid a (possibly expensive) #fullContainsPoint: test. If the receiver doesn't want to repel the morph anyways we don't need to check after all."
	(self repelsMorph: aMorphicEvent contents event: aMorphicEvent) ifFalse: [^self]. "not repelled"
	(self fullContainsPoint: (self internalizeFromWorld: aMorphicEvent eventPosition)) ifFalse: [^self]. "not for me"
	"Throw it away"
	aMorphicEvent wasHandled: true.
	aMorphicEvent contents rejectDropMorphEvent: aMorphicEvent! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 23:39'!
fullContainsPoint: aPoint
"
	This alternative implementation is included in this comment because it could be useful someday.
	If we start to rely heavily on the use of #ownShadowForm in #morphContainsPoint, this could be cheaper.
	
	| shadow |
	self clipSubmorphs
		ifTrue: [ ^self morphContainsPoint: aPoint ]
		ifFalse: [
			(self fullBounds containsPoint: aPoint) ifFalse: [^ false].
			(self morphContainsPoint: aPoint) ifTrue: [^ true].
			shadow _ self shadowForm.
			^(shadow pixelValueAt: aPoint - shadow offset) > 0 ]
"
	
	self flag: #jmvVer2.
	"Is the comment relevant now?"

	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self clipsSubmorphs ifFalse: [
		submorphs do: [ :m |
			(m fullContainsPoint: (m internalize: aPoint)) ifTrue: [ ^ true ]]].
	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 23:16'!
morphContainsPoint: aPoint
	| shadow |
	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ (0@0 extent: self morphExtent) containsPoint: aPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	shadow _ self ownShadowForm.
	^(shadow pixelValueAt: aPoint) > 0! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/20/2012 00:06'!
editBalloonHelpContent: aString
	| reply |
	reply _ FillInTheBlank
		multiLineRequest: 'Edit the balloon help text for ' , (self printStringLimitedTo: 40)
		centerAt: self world activeHand morphPositionInOwner
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		answerHeight: 200.
	reply ifNil: [^ self].  "User cancelled out of the dialog"
	(reply isEmpty or: [reply asString = self noHelpString])
		ifTrue: [self setBalloonText: nil]
		ifFalse: [self setBalloonText: reply]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/19/2012 23:42'!
transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| w eventLocalPos |

	"Never transfer halo to top-most world"
	(self isWorldMorph and:[owner isNil]) ifFalse: [
		(formerHaloOwner ~~ self) 
			ifTrue: [ ^self addHalo: event from: formerHaloOwner ]].

	eventLocalPos _ self internalizeFromWorld: event eventPosition.
	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; throw the event back in to find recipient"
		formerHaloOwner removeHalo.
		^self dispatchEvent: event copy resetHandlerFields localPosition: eventLocalPos.
	].
	self submorphsDo: [ :m |
		(m fullContainsPoint: (m internalize: eventLocalPos)) 
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ].
	].
	"We're at the bottom most level; throw the event back up to the root to find recipient"
	formerHaloOwner removeHalo.
	(w _ self world) ifNil: [ ^self ].
	^w dispatchEvent: event copy resetHandlerFields localPosition: event eventPosition! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/20/2012 00:12'!
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
	(isBack and: [self morphContainsPoint: (self internalizeFromWorld: aPoint)]) 
		ifTrue: 
			[all ifNil: [^Array with: self].
			all nextPut: self].
	^all ifNil: [#()] ifNotNil: [all contents]! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/20/2012 00:12'!
morphsAt: aPoint unlocked: aBool do: aBlock
	"Evaluate aBlock with all the morphs starting at the receiver which appear at aPoint. If aBool is true take only visible, unlocked morphs into account."
	| |
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse:[^self].
	(aBool and:[self isLocked or:[self visible not]]) ifTrue:[^self].
	self submorphsDo: [ :m |
		m morphsAt: aPoint unlocked: aBool do: aBlock].
	(self morphContainsPoint: (self internalizeFromWorld: aPoint)) ifTrue:  [ aBlock value: self ]! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 23:58'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			self selected: (localEventPosition y // self class itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 23:17'!
morphContainsPoint: aPoint

	| radius other delta xOverY e |
	((0@0 extent: self morphExtent) containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	e _ self morphExtent.
	e > (1@1)
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ e y asFloat / 2.
	other _ e x asFloat / 2.
	delta _ aPoint - (other@radius).
	xOverY _ e x asFloat / e y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!FillInTheBlankMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 23:58'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	(self morphContainsPoint: localEventPosition) ifFalse: [
		^ Beeper beep]. "sent in response to outside modal click"
	aMouseButtonEvent hand grabMorph: self. "allow repositioning"! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/20/2012 00:04'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph request: 'What is your favorite color?'"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: World activeHand morphPositionInOwner! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/20/2012 00:05'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: World activeHand morphPositionInOwner! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 23:34'!
containsPoint: aPoint event: aMorphicEvent
	"mouseButton3 events are handled by the halo"

	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ (0@0 extent: extent) containsPoint: aPoint ].

	^false! !

!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 23:43'!
rejectsEvent: anEvent
	"Return true to reject the given event. Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it."
	(super rejectsEvent: anEvent) ifTrue: [^true].
	anEvent isDropEvent ifTrue: [^true]. "never attempt to drop on halos"
	^false! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2012 00:00'!
doRecolor: evt with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	evt hand obtainHalo: self.
	(aHandle morphContainsPoint: (aHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: evt]
		ifTrue: [
			target changeColor]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2012 00:01'!
maybeCollapse: evt with: collapseHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	evt hand obtainHalo: self.
	self delete.
	(collapseHandle morphContainsPoint: (collapseHandle internalizeFromWorld: evt eventPosition)) 
		ifFalse: [
			target addHalo: evt ]
		ifTrue: [
			target collapse ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2012 00:01'!
maybeDismiss: evt with: dismissHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	evt hand obtainHalo: self.
	(dismissHandle morphContainsPoint: (dismissHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [
			self delete.
			target addHalo: evt]
		ifTrue: [
			target resistsRemoval ifTrue: [
				(PopUpMenu
					confirm: 'Really throw this away'
					trueChoice: 'Yes'
					falseChoice: 'Um, no, let me reconsider') ifFalse: [^ self]].

			self delete.
			target dismissViaHalo]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/20/2012 00:01'!
setDismissColor: evt with: dismissHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	evt hand obtainHalo: self.
	colorToUse _  (dismissHandle morphContainsPoint:  (dismissHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [ Color red muchLighter ]
		ifTrue: [ Color lightGray ].
	dismissHandle color: colorToUse! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/19/2012 23:38'!
nonCachingFullDrawOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	"Note: We use a shadow form here to prevent drawing
	overlapping morphs multiple times using the transparent
	shadow color."
	shadowForm _ self submorphsShadowForm.

	"draw shadows"
	aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5).
	
	"draw morphs in front of shadows"
	self drawSubmorphsOn: aCanvas.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 9/20/2012 00:05'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| xcess |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	aHand world addMorphFront: self.
	self morphPosition: aHand morphPositionInOwner + (-6@20).
	xcess _ self morphPositionInWorld x + self morphExtentInWorld x - aHand world morphWidth.
	xcess > 0 ifTrue: [
		self morphPosition: self morphPosition - (xcess@0) ].
	aHand balloonHelp: self! !


!LayoutAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 9/20/2012 00:05'!
handPoint

	^ hand morphPositionInOwner adhereTo: owner morphBoundsInWorld! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 9/20/2012 00:10'!
step
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		indicator ifNotNil: [
			indicator delete.
			indicator _ nil ].
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ self handPoint.
	hand lastEvent mouseButton1Pressed
		ifTrue: [
			indicator
				ifNil: [ self adjustOwnerAt: p ]
				ifNotNil: [ self adjustIndicatorAt: p ]]
		ifFalse: [
			indicator ifNotNil: [
				indicator delete.
				indicator _ nil.
				self adjustOwnerAt: p ].
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: hand morphPositionInOwner)) ifFalse: [
				hand _ nil.
				Cursor normal show.
				self stopStepping ]]! !


!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 9/20/2012 00:05'!
chooseMagnification: evt
	| handle origin aHand currentMag |
	currentMag _ magnification.
	aHand _ evt ifNil: [ self world activeHand ] ifNotNil: [evt hand].
	origin _ aHand morphPositionInOwner y.
	handle _ HandleMorph new forEachPointDo:
		[ :newPoint | self magnification: (newPoint y - origin) / 8.0 + currentMag ].
	aHand attachMorph: handle.
	handle startStepping.
	self redrawNeeded. ! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 9/20/2012 00:05'!
sourcePoint
	"If we are being dragged use our center, otherwise use pointer position"
	^ (trackPointer not or: [owner notNil and: [owner is: #HandMorph]])
		ifTrue: [ self morphBoundsInWorld center ]
		ifFalse: [ self activeHand morphPositionInOwner ]! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/20/2012 00:14'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner morphContainsPoint: (owner internalizeFromWorld: evt eventPosition))
		ifFalse: [ ^false ].
	owner activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/20/2012 00:15'!
activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil: [ ^false ]. "not applicable"
	(subMenu morphContainsPoint: (subMenu internalizeFromWorld: evt eventPosition)) ifFalse:[^false].
	subMenu activate: evt.
	self removeAlarm: #deselectTimeOut:.
	^true! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 9/20/2012 00:05'!
popUpForHand: hand in: aWorld
	| p |
	"Present this menu under control of the given hand."

	p _ hand morphPositionInOwner truncated.
	^self popUpAt: p forHand: hand in: aWorld
! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/20/2012 00:06'!
popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	^self
		popUpAt: aWorld activeHand morphPositionInOwner
		forHand: aWorld activeHand
		in: aWorld
! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 23:40'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsPoint:localEventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"
	self isSticky ifTrue: [ ^self ].
	"Grab the menu and drag it to some other place"
	aMouseButtonEvent hand grabMorph: self! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 23:40'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: localEventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand releaseMouseFocus: self.
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/20/2012 00:06'!
sightTarget: event 
	| bullseye menu newTarget |
	owner
		ifNil: [^ self ].
	bullseye _ Point fromUserWithCursor: Cursor target.
	ActiveHand morphPositionInOwner: bullseye.
	menu _ CustomMenu new.
	(owner morphsAt: bullseye) do: [ :m |
		menu add: m printString action: m ].
	menu title: self printString, ' targets... '.
	newTarget _ menu startUp.
	newTarget
		ifNil: [^ self].
	self target: newTarget! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/20/2012 00:05'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu"

	^ self invokeModalAt: ActiveHand morphPositionInOwner in: ActiveWorld allowKeyboard: allowKeyboardControl! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 9/20/2012 00:06'!
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
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight: ((owner world morphExtentInWorld y - 18) max: (ActiveHand morphPositionInOwner y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/20/2012 00:05'!
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
		self morphPositionInOwner: w activeHand morphPositionInOwner.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w displayWorld		 "show myself"
	]. 
	self delete.
	w displayWorld! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2012 23:55'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| inside eventPositionInChild |
	"See if we're fully outside aMorphs bounds"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition) ifFalse: [ ^#rejected ]. "outside"

	"Traverse children"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Not rejected. The event was in some submorph of the receiver"
				inside _ true
			]]].

	"Check for being inside the receiver"
	inside ifFalse: [ inside _ aMorph containsPoint: positionInAMorph event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].
	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2012 23:54'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. The dispatch is similar to the default dispatch with one difference: Morphs are given the chance to reject an entire drop operation. If the operation is rejected, no drop will be executed."
	| inside eventPositionInChild |

	"Try to get out quickly"
	(aMorph morphFullBoundsInWorld containsPoint: self eventPosition)
		ifFalse: [ ^#rejected ].

	"Give aMorph a chance to repel the dropping morph"
	aMorph rejectDropEvent: self.
	self wasHandled ifTrue: [^self ].

	"Go looking if any of our submorphs wants it"
	inside _ false.
	aMorph submorphsDo: [ :eachChild |
		inside ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				inside _ true
			]]].

	inside ifFalse: [ inside _ aMorph containsPoint: positionInAMorph event: self ].
	inside ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph].
	^#rejected! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/19/2012 23:55'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| globalPt aMorphHandlesIt handledByInner lastHandler answer eventPositionInChild |

	"Only for MouseDown"
	self isMouseDown
		ifFalse: [ ^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	globalPt _ self eventPosition.
	(aMorph morphFullBoundsInWorld containsPoint: globalPt) ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	lastHandler _ eventHandler.
	aMorphHandlesIt _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isPlayfieldLike or: [ self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self)
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue:[
			"aMorph is in the top-most unlocked, visible morph in the chain."
			aMorphHandlesIt ifTrue: [ self sentTo: aMorph localPosition: positionInAMorph ].
			answer _ self ]
		ifFalse: [
			"Mouse was not on aMorph nor any of its children"
			answer _ #rejected ].

	eventHandler _ lastHandler.
	^answer! !


!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 9/19/2012 23:43'!
morphToGrab: event
	"Return the morph to grab from a mouse down event. If none, return nil."
	| p |
	p _ event eventPosition.
	self submorphsDo: [ :m |
		((m rejectsEvent: event) not and: [
			m fullContainsPoint: (m internalize: p) ])
		ifTrue: [ ^m ]].
	^nil! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 23:59'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ self morphContainsPoint: localEventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 23:17'!
morphContainsPoint: aPoint

	| iconOrigin |
	((0@0 extent: self morphExtent) containsPoint: aPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ self morphExtent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: aPoint - iconOrigin) not ]]! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 9/20/2012 00:06'!
startUpWithoutKeyboard
	"Display and make a selection from the receiver as long as the button  is pressed. Answer the current selection.  Do not allow keyboard input into the menu"
	
	^ self startUpWithCaption: nil at: ActiveHand morphPositionInOwner allowKeyboard: false! !


!WindowEdgeAdjustingMorph methodsFor: 'accessing' stamp: 'jmv 9/20/2012 00:06'!
handPoint
	^ hand morphPositionInOwner! !

!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 23:18'!
morphContainsPoint: aPoint
	| sensitiveBorder b |
	b _ 0@0 extent: self morphExtent.
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

!methodRemoval: WindowEdgeAdjustingMorph #zzmorphContainsPoint:!
WindowEdgeAdjustingMorph removeSelector: #zzmorphContainsPoint:!
!methodRemoval: PluggableButtonMorph #zzmorphContainsPoint:!
PluggableButtonMorph removeSelector: #zzmorphContainsPoint:!
!methodRemoval: HandMorph #shadowForm!
HandMorph removeSelector: #shadowForm!
!methodRemoval: HaloMorph #morphContainsPoint:!
HaloMorph removeSelector: #morphContainsPoint:!
!methodRemoval: HaloMorph #zzcontainsPoint:event:!
HaloMorph removeSelector: #zzcontainsPoint:event:!
!methodRemoval: HaloMorph #zzmorphContainsPoint:!
HaloMorph removeSelector: #zzmorphContainsPoint:!
!methodRemoval: EllipseMorph #zzmorphContainsPoint:!
EllipseMorph removeSelector: #zzmorphContainsPoint:!
!methodRemoval: Morph #zzcontainsPoint:event:!
Morph removeSelector: #zzcontainsPoint:event:!
!methodRemoval: Morph #zzfullContainsPoint:!
Morph removeSelector: #zzfullContainsPoint:!
!methodRemoval: Morph #zzmorphContainsPoint:!
Morph removeSelector: #zzmorphContainsPoint:!
!methodRemoval: Morph #zzownShadowForm!
Morph removeSelector: #zzownShadowForm!
