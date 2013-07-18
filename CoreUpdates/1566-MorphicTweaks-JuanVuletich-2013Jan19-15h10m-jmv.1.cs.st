'From Cuis 4.1 of 12 December 2012 [latest update: #1563] on 19 January 2013 at 3:13:36 pm'!

!Morph methodsFor: 'accessing' stamp: 'jmv 1/19/2013 15:12'!
resistsRemoval
	"Answer whether the receiver is marked as resisting removal"

	^ false! !

!Morph methodsFor: 'menus' stamp: 'jmv 1/19/2013 15:12'!
addToggleItemsToHaloMenu: aMenu
	"Add standard true/false-checkbox items to the memu"

	#(
	(stickinessString toggleStickiness 'whether I should be resistant to a drag done by mousing down on me')
	(lockedString lockUnlockMorph 'when "locked", I am inert to all user interactions')
	(hasClipSubmorphsString changeClipSubmorphs 'whether the parts of objects within me that are outside my bounds should be masked.')
	(hasDragAndDropEnabledString changeDragAndDrop 'whether I am open to having objects dropped into me')
	) do: [ :trip | 
			aMenu addUpdating: trip first action: trip second.
			aMenu balloonTextForLastItem: trip third ]! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 1/19/2013 15:13'!
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	isCollapsed _ false.
	
	self initializeLabelArea.
	extent _ 300 @ 200.
	updatablePanes _ #().

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorph: m ].

	"by default"
	self beColumn! !

!methodRemoval: SystemWindow #wantsLabel!
SystemWindow removeSelector: #wantsLabel!
!methodRemoval: Morph #resistsRemovalString!
Morph removeSelector: #resistsRemovalString!
!methodRemoval: Morph #toggleResistsRemoval!
Morph removeSelector: #toggleResistsRemoval!
