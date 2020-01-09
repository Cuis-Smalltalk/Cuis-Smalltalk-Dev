'From Cuis 5.0 [latest update: #4009] on 9 January 2020 at 6:04:38 pm'!

!Browser methodsFor: 'message category functions' stamp: 'jmv 1/9/2020 18:00:54'!
categoryOfCurrentMethod
	"Determine the method category associated with the receiver at the current moment, or nil if none"

	| category |
	^ super categoryOfCurrentMethod ifNil: [
		category _ selectedMessageCategory.
		category == ClassOrganizer allCategory
			ifTrue: [nil]
			ifFalse: [category]]! !

!Browser methodsFor: 'message list' stamp: 'jmv 1/9/2020 18:01:53'!
messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!), it is reset to zero."
	| classOrMetaClassOrganizer answer |
	classOrMetaClassOrganizer _ self classOrMetaClassOrganizer.
	classOrMetaClassOrganizer isNil ifTrue: [ ^#() ].
	(selectedMessageCategory isNil or: [ selectedMessageCategory == ClassOrganizer allCategory ]) ifTrue: [
		^ classOrMetaClassOrganizer allMethodSelectors].
	answer _ classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory.	
	answer isNil ifTrue: [
		selectedMessageCategory _ nil.
		answer _ #() ].
	^answer! !

!methodRemoval: Browser #messageCategoryListSelection stamp: 'jmv 1/9/2020 17:59:37'!
Browser removeSelector: #messageCategoryListSelection!
