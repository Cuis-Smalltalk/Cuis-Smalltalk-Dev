'From Cuis 4.2 of 25 July 2013 [latest update: #1875] on 14 December 2013 at 8:42:28.519007 am'!
!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'dashBoard viewBox scale '
	classVariableNames: 'SoleInstance '
	poolDictionaries: ''
	category: 'Taskbar-Core'!

!Taskbar commentStamp: '<historical>' prior: 0!
A simple task bar written for Cuis.

dashBoard contains views/controls
viewBox contains graphic buttons of "iconized" windows/morphs.
scale allows 1x 2x 4x tarkbar height. [scale= 1,2,4]!


!Taskbar methodsFor: 'initialization' stamp: 'KenD 12/13/2013 09:54'!
defaultScale

	 ^ 1! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:49'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	"Add morph-specific items to the given menu which was invoked by the given hand.  This method provides is invoked both from the halo-menu and from the control-menu regimes."

	aCustomMenu 
		addLine;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4.! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
normalScale

	self scale: 1.! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:39'!
resize
	"My scale has changed"
	
	self screenSizeChanged. "rescale self"
	viewBox ifNotNil: [ "rescale buttons"
		viewBox submorphs do: [ :button | 
			button layoutSpec fixedWidth: self defaultHeight
		]
	]
! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 09:56'!
scale

	 ^ scale ifNil: [ self defaultScale ] ifNotNil: [ scale ]! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:36'!
scale: anInteger

	(anInteger between: 1 and: 4) ifFalse: [ self error: 'scale should be 1 2 or 4' ].
	 scale := anInteger.
	 self resize.! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
scaleNormal

	self scale: 1! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
scaleX2

	self scale: 2! !

!Taskbar methodsFor: 'resize' stamp: 'KenD 12/13/2013 15:47'!
scaleX4

	self scale: 4! !


!Taskbar methodsFor: 'initialization' stamp: 'KenD 12/13/2013 16:06'!
defaultHeight

	^ Preferences windowTitleFont height * 2 * self scale! !

!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'dashBoard viewBox scale'
	classVariableNames: 'SoleInstance'
	poolDictionaries: ''
	category: 'Taskbar-Core'!
