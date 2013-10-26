'From Cuis 4.0 of 21 April 2012 [latest update: #1445] on 19 September 2012 at 10:43:58 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:35'!
morphContainsPoint: aPoint
	| shadow |
	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphBoundsInWorld containsPoint: aPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	shadow _ self ownShadowForm.
	^(shadow pixelValueAt: aPoint - shadow offset) > 0! !


!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:35'!
morphContainsPoint: aPoint

	| radius other delta xOverY e |
	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	e _ self morphExtentInWorld.
	e > (1@1)
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ e y asFloat / 2.
	other _ e x asFloat / 2.
	delta _ aPoint - self morphPositionInWorld - (other@radius).
	xOverY _ e x asFloat / e y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared! !


!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:36'!
morphContainsPoint: aPoint 
	"This method is overridden so that, once up, the handles will stay up as long as the mouse is within the box that encloses all the handles even if it is not over any handle or over its owner."

	^target
		ifNil: [ super morphContainsPoint: aPoint ] 
		ifNotNil: [ false ]! !


!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:35'!
morphContainsPoint: aPoint

	| iconOrigin |
	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ self morphBoundsInWorld center - (magnifiedIcon extent // 2).
			(magnifiedIcon isTransparentAt: aPoint - iconOrigin) not ]]! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:35'!
morphContainsPoint: aPoint
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


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 22:37'!
containsPoint: aPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [ ^false ].
	(self morphContainsPoint: aPoint) ifTrue: [ ^true ].
	self submorphsDo: [ :m |
		(m isLocked and: [ m fullContainsPoint: aPoint ]) ifTrue: [ ^true ]].
	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:37'!
fullContainsPoint: aPoint
"
	This alternative implementation is included in this comment because it could be useful someday.
	If we start to rely heavily on the use of #ownShadowForm in #containsPoint, this could be cheaper.
	
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
	
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [ ^ false ].  "quick elimination"
	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	submorphs do: [:m | (m fullContainsPoint: aPoint) ifTrue: [ ^ true ]].
	^ false! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/19/2012 22:37'!
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
	(isBack and: [self morphContainsPoint: aPoint]) 
		ifTrue: 
			[all ifNil: [^Array with: self].
			all nextPut: self].
	^all ifNil: [#()] ifNotNil: [all contents]! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/19/2012 22:37'!
morphsAt: aPoint unlocked: aBool do: aBlock
	"Evaluate aBlock with all the morphs starting at the receiver which appear at aPoint. If aBool is true take only visible, unlocked morphs into account."
	| |
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse:[^self].
	(aBool and:[self isLocked or:[self visible not]]) ifTrue:[^self].
	self submorphsDo: [ :m |
		m morphsAt: aPoint unlocked: aBool do: aBlock].
	(self morphContainsPoint: aPoint) ifTrue:  [aBlock value: self ]! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 22:36'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	(self morphContainsPoint: aMouseButtonEvent eventPosition)
		ifTrue: [
			self selected: (localEventPosition y // self class itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!FillInTheBlankMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 22:37'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	(self morphContainsPoint: aMouseButtonEvent eventPosition) ifFalse: [
		^ Beeper beep]. "sent in response to outside modal click"
	aMouseButtonEvent hand grabMorph: self. "allow repositioning"! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 22:39'!
doRecolor: evt with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	evt hand obtainHalo: self.
	(aHandle morphContainsPoint: evt eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: evt]
		ifTrue: [
			target changeColor]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 22:39'!
maybeCollapse: evt with: collapseHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	evt hand obtainHalo: self.
	self delete.
	(collapseHandle morphContainsPoint: evt eventPosition) 
		ifFalse: [
			target addHalo: evt ]
		ifTrue: [
			target collapse ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 22:39'!
maybeDismiss: evt with: dismissHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	evt hand obtainHalo: self.
	(dismissHandle morphContainsPoint: evt eventPosition)
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

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 22:39'!
setDismissColor: evt with: dismissHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	evt hand obtainHalo: self.
	colorToUse _  (dismissHandle morphContainsPoint: evt eventPosition)
		ifFalse: [ Color red muchLighter ]
		ifTrue: [ Color lightGray ].
	dismissHandle color: colorToUse! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 9/19/2012 22:37'!
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
			(self morphContainsPoint: hand morphPosition) ifFalse: [
				hand _ nil.
				Cursor normal show.
				self stopStepping ]]! !


!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 9/19/2012 22:24'!
morphToGrab: event
	"Return the morph to grab from a mouse down event. If none, return nil."
	self submorphsDo: [ :m |
		((m rejectsEvent: event) not and: [
			m fullContainsPoint: event eventPosition ])
		ifTrue: [ ^m ]].
	^nil! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/19/2012 22:37'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ self morphContainsPoint: aMouseButtonEvent eventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !

!methodRemoval: WindowEdgeAdjustingMorph #containsPoint:!
WindowEdgeAdjustingMorph removeSelector: #containsPoint:!
!methodRemoval: PluggableButtonMorph #containsPoint:!
PluggableButtonMorph removeSelector: #containsPoint:!
!methodRemoval: Paragraph #containsPoint:!
Paragraph removeSelector: #containsPoint:!
!methodRemoval: Paragraph #paragraphContainsPoint:!
Paragraph removeSelector: #paragraphContainsPoint:!
!methodRemoval: HaloMorph #containsPoint:!
HaloMorph removeSelector: #containsPoint:!
!methodRemoval: EllipseMorph #containsPoint:!
EllipseMorph removeSelector: #containsPoint:!
!methodRemoval: Morph #containsPoint:!
Morph removeSelector: #containsPoint:!
