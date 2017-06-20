'From Cuis 5.0 of 7 November 2016 [latest update: #3113] on 19 June 2017 at 3:22:14 pm'!
!classDefinition: #InnerListMorph category: #'Morphic-Views'!
InnerPluggableMorph subclass: #InnerListMorph
	instanceVariableNames: 'listItems font selectedRow selectedRows highlightedRow '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!InnerListMorph methodsFor: 'initialization' stamp: 'jmv 6/19/2017 15:21:44'!
initialize
	super initialize.
	self color: Color black.
	font _ Preferences standardListFont.
	listItems _ #().
	selectedRow _ nil.
	highlightedRow _ nil! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 6/19/2017 15:21:40'!
listChanged
	"set newList to be the list of strings to display"
	listItems _ Array new: self getListSize withAll: nil.
	selectedRow _ nil.
	self adjustExtent! !

!InnerListMorph methodsFor: 'private' stamp: 'jmv 6/19/2017 15:21:48'!
noSelection
	selectedRow _ nil.
	highlightedRow _ nil! !

!classDefinition: #InnerListMorph category: #'Morphic-Views'!
InnerPluggableMorph subclass: #InnerListMorph
	instanceVariableNames: 'listItems font selectedRow highlightedRow'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!
