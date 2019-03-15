'From Cuis 5.0 of 7 November 2016 [latest update: #3661] on 15 March 2019 at 2:26:59 pm'!

!AutoCompleterMorph methodsFor: 'accessing' stamp: 'AY 3/15/2019 11:51:58'!
entryCount
	
	^completer entryCount! !

!AutoCompleterMorph methodsFor: 'accessing' stamp: 'AY 3/15/2019 13:26:28'!
itemsPerPage

	^itemsPerPage! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 13:04:51'!
firstSelectableEntryIndex
	
	^self nextSelectableEntryIndexFrom: 0! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 14:01:03'!
firstVisible: anIndex
	
	firstVisible _ anIndex
		min: self entryCount - self itemsPerPage + 1
		max: 1.! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 14:10:19'!
lastSelectableEntryIndex
	
	^self previousSelectableEntryIndexFrom: 1! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 14:01:42'!
lastVisible: anIndex
	
	self firstVisible: anIndex - self itemsPerPage + 1.! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 13:31:48'!
nextSelectableEntryIndexFrom: anIndex
	
	^self nextSelectableEntryIndexFrom: anIndex goingForwards: true! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 13:31:31'!
nextSelectableEntryIndexFrom: anIndex goingForwards: goingForwardsBoolean
	
	| direction indicesFromAnIndex |
	
	direction _ goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	indicesFromAnIndex _ (1 to: self entryCount)
		collect: [ :offset | self wrapIndex: anIndex + (offset*direction) by: self entryCount ].
	
	^indicesFromAnIndex
		detect: [ :index | self canSelectEntryAt: index ]
		ifNone: [self error: 'there are no selectable entries']! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 14:18:28'!
nextSelectableEntryIndexFromAndIncluding: anIndex
	
	^self nextSelectableEntryIndexFrom: anIndex - 1! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 13:32:02'!
previousSelectableEntryIndexFrom: anIndex
	
	^self nextSelectableEntryIndexFrom: anIndex goingForwards: false! !

!AutoCompleterMorph methodsFor: 'private' stamp: 'AY 3/15/2019 13:25:42'!
wrapIndex: anIndex by: aSize
	
	^anIndex - 1 \\ aSize + 1! !

!AutoCompleterMorph methodsFor: 'testing' stamp: 'AY 3/15/2019 11:55:29'!
canSelectEntryAt: anIndex
	
	^completer canSelectEntryAt: anIndex! !


!AutoCompleter methodsFor: 'entries' stamp: 'AY 3/15/2019 11:47:28'!
entryAt: anIndex
	
	^self entries at: anIndex! !

!AutoCompleter methodsFor: 'testing' stamp: 'AY 3/15/2019 11:47:48'!
canSelectEntryAt: anIndex

	^self canSelect: (self entryAt: anIndex)! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 14:10:45'!
goDown
	
	self selected: (self nextSelectableEntryIndexFrom: self selected).
	(self selected between: self firstVisible and: self lastVisible)
		ifFalse: [self lastVisible: self selected].
	
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 14:03:40'!
goHome

	self selected: self firstSelectableEntryIndex.
	self firstVisible: 1.
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 14:03:32'!
goToEnd
	
	self selected: self lastSelectableEntryIndex.
	self lastVisible: self selected.
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 14:00:18'!
goUp

	(self selected = 0 and: [self firstVisible = 1]) ifTrue: [^ self].
	
	self selected: (self previousSelectableEntryIndexFrom: self selected).
	(self selected between: self firstVisible and: self lastVisible)
		ifFalse: [self firstVisible: self selected].
		
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'AY 3/15/2019 14:25:49'!
resetMenu

	| width newExtent |
	self hideSelectorDocumentation.

	self calculateItemsPerPage.
	
	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.
	
	width _ self calculateWidth.
	newExtent _ width + 4 @ (itemsPerPage * self itemHeight + 2).

	self morphPosition: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !

!AutoCompleterMorph methodsFor: 'paging' stamp: 'AY 3/15/2019 14:18:47'!
gotoPage: anInteger

	| item |

	item := ((anInteger - 1) * itemsPerPage) + 1.
	item >= completer entryCount ifTrue: [^false].

	item < 1 ifTrue: [item := 1].
	firstVisible _ item.
	self selected: (self nextSelectableEntryIndexFromAndIncluding: item).

	^ true! !

