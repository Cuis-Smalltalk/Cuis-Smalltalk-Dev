'From Cuis 6.0 [latest update: #5802] on 24 May 2023 at 3:25:08 pm'!

!Browser methodsFor: 'message list' stamp: 'len 5/23/2023 16:04:56'!
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
	"Sort message list with binary selectors first (otherwise some unicode binary selectors end up at the end of the list)."
	^answer sort: [:a :b| a isValidBinarySelector ifTrue: [b isValidBinarySelector ifTrue: [a < b] ifFalse: [true]] ifFalse: [b isValidBinarySelector ifTrue: [false] ifFalse: [a < b]]]! !

