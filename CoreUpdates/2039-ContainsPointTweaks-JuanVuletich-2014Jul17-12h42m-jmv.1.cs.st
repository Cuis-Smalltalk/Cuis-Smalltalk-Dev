'From Cuis 4.2 of 25 July 2013 [latest update: #2039] on 17 July 2014 at 1:07:33.558204 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 7/17/2014 12:57'!
containsPoint: aLocalPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	self visible ifFalse: [ ^false ].
	(self morphContainsPoint: aLocalPoint) ifTrue: [ ^true ].
	self clipsSubmorphs ifFalse: [
		self submorphsDo: [ :m |
			(m isLocked and: [ m fullContainsPoint: (m internalize: aLocalPoint) ])
				ifTrue: [ ^true ]]].
	^false! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/17/2014 13:01'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Warning:
		Not #visible morphs will not be considered in recursion, but will answer if asked directly"

	| r |
	self flag: #jmvVer2.
	"IF I remove this, then layout of buttons in FileList breaks when selecting / deselecting code files. Besides, ProgressMorph example breaks too"
	self layoutSubmorphsIfNeeded.

	self flag: #jmvVer2.
	r _ self morphBoundsInWorld.
	(self hasSubmorphs and: [self clipsSubmorphs not ])
		ifTrue: [
			submorphs do: [ :m |
				m visible ifTrue: [
					r _ r merge: m morphFullBoundsInWorld ]]].
	^r! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/17/2014 13:04'!
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

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self clipsSubmorphs ifFalse: [
		submorphs do: [ :m |
			(m fullContainsPoint: (m internalize: aPoint)) ifTrue: [ ^ true ]]].
	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/17/2014 13:04'!
morphContainsPoint: aLocalPoint
	| shadow |

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ (0@0 extent: self morphExtent) containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	shadow _ self ownShadowForm.
	^(shadow pixelValueAt: aLocalPoint) > 0! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 7/17/2014 12:05'!
potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	owner ifNil:[^#()].
	"
	^owner morphsAt: self referencePosition behind: self unlocked: true
	"
	"ToDo"
	self flag: #jmvVer2.
	^#()! !


!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 7/17/2014 12:49'!
morphContainsPoint: aLocalPoint

	| radius other delta xOverY e |
	((0@0 extent: self morphExtent) containsPoint: aLocalPoint) ifFalse: [^ false].  "quick elimination"
	e _ self morphExtent.
	e > (1@1)
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ e y asFloat / 2.
	other _ e x asFloat / 2.
	delta _ aLocalPoint - (other@radius).
	xOverY _ e x asFloat / e y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!MenuMorph methodsFor: 'menu' stamp: 'jmv 7/17/2014 12:07'!
sightTarget: event 
	| bullseye menu newTarget |
	owner
		ifNil: [^ self ].
	bullseye _ Point fromUserWithCursor: Cursor target.
	self world activeHand morphPosition: bullseye.
	menu _ CustomMenu new.
	"ToDo"
	self flag: #jmvVer2.
	"
	(owner morphsAt: bullseye) do: [ :m |
		menu add: m printString action: m ].
	menu title: self printString, ' targets... '.
	newTarget _ menu startUp.
	newTarget
		ifNil: [^ self].
	self target: newTarget
	"! !


!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 7/17/2014 12:49'!
morphContainsPoint: aLocalPoint

	| iconOrigin |
	((0@0 extent: self morphExtent) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ self morphExtent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: aLocalPoint - iconOrigin) not ]]! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 7/17/2014 12:58'!
containsPoint: aLocalPoint event: aMorphicEvent

	self visible ifFalse: [ ^false ].

	"mouseButton3 events are handled by the halo"
	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ (0@0 extent: extent) containsPoint: aLocalPoint ].

	^false! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 7/17/2014 12:50'!
morphContainsPoint: aLocalPoint
	| sensitiveBorder b |
	b _ 0@0 extent: self morphExtent.
	(b containsPoint: aLocalPoint) ifFalse: [ ^false ].
	sensitiveBorder _ 4.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x - b left < sensitiveBorder or: [ aLocalPoint y - b top < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ b right - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y - b top < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x - b left < sensitiveBorder or: [ b bottom - aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ b right - aLocalPoint x <= sensitiveBorder or: [ b bottom - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !


!MorphicEvent methodsFor: 'dispatching' stamp: 'jmv 7/17/2014 12:19'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].

	^ #rejected! !


!DropEvent methodsFor: 'dispatching' stamp: 'jmv 7/17/2014 12:18'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."
	| eventPositionInChild dropped |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sentTo: aMorph localPosition: positionInAMorph ]].
	^#rejected! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 7/17/2014 12:19'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner eventPositionInChild |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sentTo: aMorph localPosition: positionInAMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand waitForClicksOrDrag: aMorph event: self dragSel: #dragEvent:localPosition: clkSel: #dragEvent:localPosition:.
					false ifTrue: [ self hand grabMorph: aMorph ].
					self wasHandled: true.
					^self ]]].

	handledByInner ifTrue: [ ^self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !

!methodRemoval: Morph #morphsAt:!
Morph removeSelector: #morphsAt:!
!methodRemoval: Morph #morphsAt:behind:unlocked:!
Morph removeSelector: #morphsAt:behind:unlocked:!
!methodRemoval: Morph #morphsAt:unlocked:!
Morph removeSelector: #morphsAt:unlocked:!
!methodRemoval: Morph #morphsAt:unlocked:do:!
Morph removeSelector: #morphsAt:unlocked:do:!
