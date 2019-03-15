'From Cuis 5.0 of 7 November 2016 [latest update: #3662] on 15 March 2019 at 5:14:44 pm'!

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 17:13:20'!
goPageDown
	
	| oldEntry newEntry nextEntry |
	
	oldEntry _ self selected.
	newEntry _ oldEntry.
	[nextEntry _ self nextSelectableEntryIndexFrom: newEntry.
	nextEntry > oldEntry and: [nextEntry - oldEntry <= self itemsPerPage]]
		whileTrue: [newEntry _ nextEntry].
	
	self selected: newEntry.
	self firstVisible: newEntry.
	
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 17:09:51'!
goPageUp
	
	| oldEntry newEntry nextEntry |
	
	oldEntry _ self selected.
	newEntry _ oldEntry.
	[nextEntry _ self previousSelectableEntryIndexFrom: newEntry.
	oldEntry > nextEntry and: [oldEntry - nextEntry <= self itemsPerPage]]
		whileTrue: [newEntry _ nextEntry].
	
	self selected: newEntry.
	self firstVisible: newEntry.
	
	self redrawNeeded.! !

!methodRemoval: AutoCompleterMorph #currentPage!
AutoCompleterMorph removeSelector: #currentPage!
!methodRemoval: AutoCompleterMorph #gotoPage:!
AutoCompleterMorph removeSelector: #gotoPage:!
!methodRemoval: AutoCompleterMorph #pageCount!
AutoCompleterMorph removeSelector: #pageCount!

!AutoCompleterMorph reorganize!
('accessing' entryCount itemHeight itemsPerPage selected selected:)
('actions' goDown goHome goPageDown goPageUp goToEnd goUp help resetMenu)
('activity tracking' lastActivity stillActive timeOfLastActivity timeout updateColor)
('drawing' adjust:ifOutOfScreenWith:xOffset:yOffset: calculateItemsPerPage calculateWidth colorOf: downButtonPosition drawContainingRectangle: drawDownArrowOn:thickness: drawItemOf:on:width:top: drawItemsOn:width: drawOn: drawScrollBarMovementBarOn:thickness: drawScrollBarOn: drawScrollBarRectangleOn:thickness: drawUpArrowOn:thickness: maxItemsPerPage setDefaultColors upButtonPosition)
('events' mouseButton1Up:localPosition:)
('event handling testing' handlesMouseDown:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor setCompleter:position:)
('stepping' stepAt: stepTime wantsSteps)
('private' firstSelectableEntryIndex firstVisible firstVisible: lastSelectableEntryIndex lastVisible lastVisible: nextSelectableEntryIndexFrom: nextSelectableEntryIndexFrom:goingForwards: nextSelectableEntryIndexFromAndIncluding: previousSelectableEntryIndexFrom: wrapIndex:by:)
('selector documentation' crPressedOnSelectorDocumentation hideSelectorDocumentation initializeSelectorDocumentation isShowingSelectorDocumentation methodDocumentationSeparator selectorDefaultDocumentationLocation selectorDocumentation selectorDocumentationExtent selectorDocumentationText selectorDocumentationTextForAllI: selectorDocumentationTextForAllImplementorsOf: selectorDocumentationTextOf:forAll: showSelectorDocumentation)
('submorphs-add/remove' delete)
('testing' canSelectEntryAt: isXOutOfScreen:with: isYOutOfScreen:with:)
!

