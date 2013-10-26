'From Cuis 4.0 of 21 April 2012 [latest update: #1445] on 19 September 2012 at 10:52:09 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 22:44'!
zzcontainsPoint: aPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	(self morphFullBoundsInWorld containsPoint: aPoint) ifFalse: [ ^false ].
	(self morphContainsPoint: aPoint) ifTrue: [ ^true ].
	self submorphsDo: [ :m |
		(m isLocked and: [ m fullContainsPoint: aPoint ]) ifTrue: [ ^true ]].
	^false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:44'!
zzfullContainsPoint: aPoint
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

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:44'!
zzmorphContainsPoint: aPoint
	| shadow |
	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphBoundsInWorld containsPoint: aPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	shadow _ self ownShadowForm.
	^(shadow pixelValueAt: aPoint - shadow offset) > 0! !


!EllipseMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:44'!
zzmorphContainsPoint: aPoint

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


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 9/19/2012 22:44'!
zzcontainsPoint: aPoint event: aMorphicEvent
	"mouseButton3 events are handled by the halo"

	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ self morphFullBoundsInWorld containsPoint: aMorphicEvent eventPosition ].

	^ super containsPoint: aPoint event: aMorphicEvent! !

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:44'!
zzmorphContainsPoint: aPoint 
	"This method is overridden so that, once up, the handles will stay up as long as the mouse is within the box that encloses all the handles even if it is not over any handle or over its owner."

	^target
		ifNil: [ super morphContainsPoint: aPoint ] 
		ifNotNil: [ false ]! !


!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:44'!
zzmorphContainsPoint: aPoint

	| iconOrigin |
	(self morphBoundsInWorld containsPoint: aPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ self morphBoundsInWorld center - (magnifiedIcon extent // 2).
			(magnifiedIcon isTransparentAt: aPoint - iconOrigin) not ]]! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 9/19/2012 22:44'!
zzmorphContainsPoint: aPoint
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

