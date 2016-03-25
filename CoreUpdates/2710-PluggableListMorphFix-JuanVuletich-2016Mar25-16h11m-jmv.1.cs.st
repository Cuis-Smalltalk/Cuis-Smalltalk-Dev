'From Cuis 4.2 of 25 July 2013 [latest update: #2709] on 25 March 2016 at 4:18:49.523218 pm'!
!classDefinition: #PluggableListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getListSizeSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 3/25/2016 16:17'!
getListSize
	"return the current number of items in the displayed list"
	list ifNotNil: [ ^list size ].
	^self getList size! !

!PluggableListMorph methodsFor: 'updating' stamp: 'jmv 3/25/2016 16:14'!
updateList
	| index |
	"the list has changed -- update from the model"
	self getList.
	self listMorph listChanged.
	self setScrollDeltas.
	index _ self getCurrentSelectionIndex.
	self selectionIndex: index! !

!classDefinition: #PluggableListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #PluggableListMorph
	instanceVariableNames: 'list getListSelector getIndexSelector setIndexSelector keystrokeActionSelector autoDeselect lastKeystrokeTime lastKeystrokes doubleClickSelector menuGetter mainView'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
