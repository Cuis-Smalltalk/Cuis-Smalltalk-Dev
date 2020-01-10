'From Cuis 5.0 [latest update: #4008] on 9 January 2020 at 5:43:07 pm'!

!Browser methodsFor: 'message list' stamp: 'jmv 1/9/2020 17:41:56'!
messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!), it is reset to zero."
	| classOrMetaClassOrganizer sel answer |
	classOrMetaClassOrganizer _ self classOrMetaClassOrganizer.
	classOrMetaClassOrganizer isNil ifTrue: [ ^#() ].
	sel _ self messageCategoryListSelection.
	(sel isNil or: [ sel == ClassOrganizer allCategory ]) ifTrue: [
		^ classOrMetaClassOrganizer allMethodSelectors].
	selectedMessageCategory isNil ifTrue: [ ^#() ].
	answer _ classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory.	
	answer isNil ifTrue: [
		selectedMessageCategory _ nil.
		answer _ #() ].
	^answer! !


!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 1/9/2020 17:28:19'!
initialize
	super initialize.
	scroller morphWidth: extent x.! !

!methodRemoval: PluggableListMorph #textColor stamp: 'jmv 1/9/2020 17:28:40'!
PluggableListMorph removeSelector: #textColor!
