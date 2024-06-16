'From Cuis7.1 [latest update: #6470] on 16 June 2024 at 5:11:48 pm'!

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 6/16/2024 17:10:59'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	doubleClickSelector ifNotNil: [
		^ (self window ifNil: [self model]) perform: doubleClickSelector].
	selectedMorph ifNotNil: [
		self toggleExpandedState: selectedMorph.
		self scrollSelectionIntoView ]! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'jmv 6/14/2024 15:32:22'!
doubleClickSelector: aSymbol
	doubleClickSelector := aSymbol.
	self autoDeselect: false. ! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 6/16/2024 17:11:21'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index := self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self visualSelectionIndex
		ifFalse: [ self setSelectionIndex: index ].
	^ (self window ifNil: [self model]) perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 6/14/2024 15:32:25'!
doubleClickSelector: aSymbol
	doubleClickSelector := aSymbol.
	self autoDeselect: false. ! !

