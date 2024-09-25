'From Cuis7.1 [latest update: #6748] on 25 September 2024 at 2:43:50 pm'!

!Browser methodsFor: 'system category list' stamp: 'jmv 9/25/2024 14:36:53'!
setSelectedSystemCategory: aStringOrNil
	"Set the selected system category and update all other selections to be deselected."
	selectedSystemCategory := aStringOrNil.
	selectedClassName := nil.
	selectedMessageCategory := nil.
	selectedMessage := nil.
	self editSelection: (aStringOrNil isNil ifTrue: [#none] ifFalse: [#newClass]).
	metaClassIndicated := false.
	self setClassOrganizer.
	self changed: #selectedSystemCategory.
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged! !

