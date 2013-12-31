'From Cuis 4.0 of 21 April 2012 [latest update: #1433] on 8 September 2012 at 8:17:09 pm'!

!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:16'!
initialize
	super initialize.
	extent _ 200@100! !


!Morph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:10'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MatrixTransform2x3 identity.
	layoutNeeded _ false! !


!RectangleLikeMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:11'!
initialize
	super initialize.
	extent _ 50@40.
	color _ self defaultColor! !


!HandMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:14'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	extent _ CursorWithMask normal extent.
	damageRecorder _ DamageRecorder new.
	self initForEvents.! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:14'!
initialize
	"initialize the state of the receiver"
	super initialize.
	""
	extent _ 10@10.
	contents _ ''.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preferences standardMenuFont! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:13'!
initialize
	super initialize.
	extent _ 50 @ 2! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:15'!
initialize
	super initialize.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!ScrollBar methodsFor: 'initialize' stamp: 'jmv 9/8/2012 20:16'!
initialize
	super initialize.
	extent _ self class scrollbarThickness @ 100.
	value _ 0.0.
	self recreateSubmorphs.
	scrollDelta _ 0.02.
	pageDelta _ 0.2! !

!methodRemoval: ScrollBar #defaultBounds!
ScrollBar removeSelector: #defaultBounds!
!methodRemoval: PluggableMorph #defaultBounds!
PluggableMorph removeSelector: #defaultBounds!
!methodRemoval: MenuItemMorph #defaultBounds!
MenuItemMorph removeSelector: #defaultBounds!
!methodRemoval: Morph #defaultBounds!
Morph removeSelector: #defaultBounds!
