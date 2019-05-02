'From Cuis 5.0 of 7 November 2016 [latest update: #3721] on 2 May 2019 at 8:31:27 am'!

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:50:29'!
navigateDown
	"move down, wrapping to top if needed"
	| nextSelection |
	nextSelection _ self visualSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection _ self minimumSelection ].
	self changeSelectionTo: nextSelection! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:51:51'!
navigateLeft
	| oldSelection nextSelection |
	oldSelection _ self visualSelectionIndex.
	nextSelection _ oldSelection.
	selectedMorph ifNotNil: [
		selectedMorph isExpanded
			ifTrue: [
				self toggleExpandedState: selectedMorph.
				self scrollSelectionIntoView ]
			ifFalse: [
				oldSelection > self minimumSelection ifTrue: [
					nextSelection _ (oldSelection-1 to: 1 by: -1) 
						detect: [ :i | ( scroller submorphs at: i) indentLevel < selectedMorph indentLevel ] 
						ifNone: [ oldSelection ]].
				]].
	self changeSelectionTo: nextSelection! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:58:27'!
navigateOnePageDown

	self changeSelectionTo: (self visualSelectionIndex + self numSelectionsInView min: self maximumSelection)! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:54:14'!
navigateOnePageUp

	self changeSelectionTo: (self minimumSelection max: self visualSelectionIndex - self numSelectionsInView)! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:51:59'!
navigateRight
	| oldSelection nextSelection |
	oldSelection _ self visualSelectionIndex.
	nextSelection _ oldSelection.
	selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView ]
				ifFalse: [ nextSelection := oldSelection + 1 ]].
	self changeSelectionTo: nextSelection! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:49:21'!
navigateToBottom
	
	self changeSelectionTo: self maximumSelection! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:52:29'!
navigateToTop
	
	self changeSelectionTo: self minimumSelection! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:50:43'!
navigateUp
	"move up, wrapping to bottom if needed"
	| nextSelection |
	nextSelection _ self visualSelectionIndex - 1.
	nextSelection < self minimumSelection ifTrue: [ nextSelection _ self maximumSelection ].
	self changeSelectionTo: nextSelection! !

!HierarchicalListMorph methodsFor: 'private' stamp: 'jmv 5/1/2019 12:37:18'!
changeSelectionTo: nextSelection

	nextSelection = self visualSelectionIndex ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ]].! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 5/1/2019 12:55:50'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	keystrokeActionSelector ifNil: [^self].
	(args _ keystrokeActionSelector numArgs) = 1 
		ifTrue: [^mainView perform: keystrokeActionSelector with: aKeyboardEvent keyCharacter].
	args = 2 
		ifTrue: [
			^mainView 
				perform: keystrokeActionSelector
				with: aKeyboardEvent keyCharacter
				with: self].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:55:36'!
arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil! !


!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:55:33'!
arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent anyModifierKeyPressed ifFalse: [
		aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
		aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
		aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
		aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
		aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
		aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
		aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
		aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ]].
	^ nil! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:50:55'!
navigateDown
	"move down, wrapping to top if needed"
	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection _ self minimumSelection ].
	self changeSelectionTo: nextSelection! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:58:14'!
navigateOnePageDown

	self changeSelectionTo: (self getCurrentSelectionIndex + self numSelectionsInView min: self maximumSelection)! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:52:34'!
navigateToTop
	
	self changeSelectionTo: self minimumSelection! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:51:10'!
navigateUp
	"move up, wrapping to bottom if needed"
	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex - 1.
	nextSelection < self minimumSelection ifTrue: [ nextSelection _ self maximumSelection ].
	self changeSelectionTo: nextSelection! !

!PluggableListMorph methodsFor: 'private' stamp: 'jmv 5/1/2019 12:36:00'!
changeSelectionTo: nextSelection

	nextSelection = self getCurrentSelectionIndex ifFalse: [
		| window |
		window _ self owningWindow.
		(window isNil or: [ window okToChangeDueTo: self ]) ifTrue: [
			"No change if model is locked"
			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.
			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		]
	]! !

