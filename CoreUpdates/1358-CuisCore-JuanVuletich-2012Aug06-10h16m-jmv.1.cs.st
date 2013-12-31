'From Cuis 4.0 of 21 April 2012 [latest update: #1357] on 6 August 2012 at 10:17:56 am'!

!HaloMorph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 10:10'!
morphFullBoundsInWorld
	
	"mhhhh not relly nice"
	| r |
	self flag: #jmvVer2.
	^submorphs isEmpty
		ifTrue: [ super morphFullBoundsInWorld ]
		ifFalse: [
			r _ super morphFullBoundsInWorld.
			submorphs do: [ :m |
				r _ r merge: m morphFullBoundsInWorld ].
			r]! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 8/6/2012 10:07'!
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

!HaloMorph methodsFor: 'private' stamp: 'jmv 8/6/2012 09:41'!
basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self handleSize.
	anExtent _ ((self morphWidth + self handleSize + 8) max: minSide) @
				((self morphHeight + self handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target outermostWorldMorph ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: 8@8) ]! !


!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 8/6/2012 09:51'!
select: evt
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + (10@0)
									with: self morphBoundsInWorld topLeft)
			forHand: evt hand
			from: self.
		subMenu selectItem: nil event: evt].! !

