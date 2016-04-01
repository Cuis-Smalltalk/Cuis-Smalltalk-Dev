'From Cuis 4.2 of 25 July 2013 [latest update: #2710] on 1 April 2016 at 2:19:23.301542 pm'!

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 3/27/2016 18:18'!
getListItem: index
	"get the index-th item in the displayed list"
	^list at: index! !

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 3/27/2016 18:18'!
getListSize
	"return the current number of items in the displayed list"
	^list size! !

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 3/27/2016 18:18'!
keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes , aChar asLowercase asString.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	nextSelection _ list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self changeModelSelection: nextSelection! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 3/27/2016 18:16'!
selection: item
	"Called from outside to request setting a new selection."

	self selectionIndex: (list indexOf: item)! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 3/27/2016 18:20'!
expandAll

	| firstSelection list |
	list _ model getList.
	firstSelection _ list isEmpty ifFalse: [ list first ].
	model noteNewSelection: firstSelection.
	listMorph expandAll! !

