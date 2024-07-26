'From Cuis7.1 [latest update: #6533] on 25 July 2024 at 4:15:13 pm'!

!Browser methodsFor: 'message list' stamp: 'jmv 7/25/2024 16:14:48'!
messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!), it is reset to zero."
	| classOrMetaClassOrganizer answer |
	classOrMetaClassOrganizer := self classOrMetaClassOrganizer.
	classOrMetaClassOrganizer isNil ifTrue: [ ^#() ].
	answer := (selectedMessageCategory isNil or: [ selectedMessageCategory == ClassOrganizer allCategory ])
		ifTrue: [classOrMetaClassOrganizer allMethodSelectors]
		ifFalse: [classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory].	
	answer isNil ifTrue: [
		selectedMessageCategory := nil.
		answer := #() ].
	^answer! !

