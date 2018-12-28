'From Cuis 5.0 of 7 November 2016 [latest update: #3543] on 25 December 2018 at 5:16:22 pm'!
!classDefinition: #DraggingGuideMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #DraggingGuideMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Browser methodsFor: 'message category functions' stamp: 'AY 12/22/2018 11:56:46'!
categorizeUnderCategoryAt: aMessageCategoryListIndex messageAt: aMessageListIndex

	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector messageSelector |
		categorySelector _ self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		messageSelector _ self messageList at: aMessageListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: messageSelector under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]! !


!PluggableListMorph methodsFor: 'accessing' stamp: 'HAW 12/25/2018 12:21:46'!
rowAtLocation: aPoint ifNone: aNoneBlock 
	
	| listMorph |
	
	listMorph _ self listMorph.
	^listMorph rowAtLocation: (listMorph internalize: aPoint) ifNone: aNoneBlock! !

!PluggableListMorph methodsFor: 'drawing' stamp: 'AY 12/25/2018 16:51:46'!
flashRow: aRow

	^self listMorph flashRow: aRow.! !

!PluggableListMorph methodsFor: 'events' stamp: 'AY 12/25/2018 17:02:10'!
dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged |
	
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].

	dragged _ DraggingGuideMorph new.
	dragged addMorph: (StringMorph contents: (self getListItem: row)).
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: row.
	
	aMouseEvent hand attachMorphBeside: dragged.! !

!PluggableListMorph methodsFor: 'event handling testing' stamp: 'AY 12/22/2018 11:50:28'!
allowsMorphDrop

	^self hasProperty: #allowsMorphDrop! !

!PluggableListMorph methodsFor: 'private' stamp: 'AY 12/22/2018 01:26:29'!
itemsAreDraggable
	
	^self hasProperty: #draggableItems! !

!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'AY 12/22/2018 12:26:06'!
acceptDropsFrom: aMorph performing: aDropActionSelector

	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: aDropActionSelector.! !

!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'AY 12/22/2018 12:25:54'!
makeItemsDraggable

	self setProperty: #draggableItems toValue: true! !

!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'AY 12/22/2018 01:26:39'!
makeItemsUndraggable

	self removeProperty: #draggableItems! !

!PluggableListMorph methodsFor: 'accessing - properties' stamp: 'AY 12/22/2018 12:27:38'!
rejectDrops

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.! !

!PluggableListMorph methodsFor: 'layout' stamp: 'AY 12/25/2018 16:52:06'!
acceptDroppingMorph: aMorph event: dropEvent

	| localPosition row dropActionSelector args |
	
	localPosition _ self internalizeFromWorld: dropEvent eventPosition.
	row _ self rowAtLocation: localPosition ifNone: [ ^self ].
	
	self flashRow: row.
	
	dropActionSelector _ self valueOfProperty: #dropActionSelector.
	args _ dropActionSelector numArgs.
	args = 1 ifTrue: [
		model perform: dropActionSelector with: row.
		^self].
	args = 2 ifTrue: [ | dropSelectorArgument |
		dropSelectorArgument _ aMorph
			valueOfProperty: #dropSelectorArgument
			ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
		model perform: dropActionSelector with: row with: dropSelectorArgument.
		^self].
	
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'! !

!PluggableListMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/25/2018 12:22:07'!
wantsDroppedMorph: aMorph event: evt

	^(aMorph is: #DraggingGuideMorph)
		and: [ (aMorph valueOfProperty: #dragSource) = (self valueOfProperty: #acceptedDragSource) ]! !


!DraggingGuideMorph methodsFor: 'initialization' stamp: 'AY 12/21/2018 16:05:14'!
defaultColor

	^Color transparent! !

!DraggingGuideMorph methodsFor: 'initialization' stamp: 'AY 12/21/2018 16:06:08'!
initialize

	super initialize.
	extent _ 0@0.! !

!DraggingGuideMorph methodsFor: 'testing' stamp: 'AY 12/21/2018 16:05:22'!
is: aSymbol

	^aSymbol == #DraggingGuideMorph or: [ super is: aSymbol ]! !

!DraggingGuideMorph methodsFor: 'dropping/grabbing' stamp: 'AY 12/25/2018 16:49:57'!
justDroppedInto: newOwnerMorph event: anEvent 
	
	self delete.
	anEvent hand redrawNeeded.! !


!HandMorph methodsFor: 'double click support' stamp: 'AY 12/21/2018 16:07:39'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel
	
	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'AY 12/25/2018 17:06:43'!
attachMorphBeside: aMorph

	"Position the given morph beside this hand, then grab it."

	aMorph aboutToBeGrabbedBy: self.
	^self grabMorph: aMorph delta: (self morphWidth)@0! !


!InnerListMorph methodsFor: 'list management' stamp: 'HAW 12/25/2018 12:21:42'!
rowAtLocation: aPoint ifNone: aNoneBlock

	| potentialRowNumber |
	
	potentialRowNumber := aPoint y // font height + 1.
	
	^(listItems isInBounds: potentialRowNumber) 
		ifTrue: [ potentialRowNumber ]
		ifFalse: aNoneBlock! !

!InnerListMorph methodsFor: 'drawing' stamp: 'AY 12/25/2018 16:51:08'!
flashRow: aRow

	self world ifNotNil: [ :world | world canvas ifNotNil: [ :canvas | 
		Display flash: (canvas externalizeDisplayBounds: (self drawBoundsForRow: aRow) from: self) ]].
	
! !


!PluggableListMorph methodsFor: 'events' stamp: 'HAW 12/25/2018 12:22:40'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self 
		rowAtLocation: localEventPosition
		ifNone:  [^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view.
	Model update will be done on mouse button up, so this feedback will be visible before that."
	self listMorph highlightedRow: row.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: (self itemsAreDraggable ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'AY 12/22/2018 12:18:24'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList |

	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	messageList makeItemsDraggable.
	messageCatList acceptDropsFrom: messageList performing: #categorizeUnderCategoryAt:messageAt:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicSystemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: self buildMorphicClassColumn proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !

