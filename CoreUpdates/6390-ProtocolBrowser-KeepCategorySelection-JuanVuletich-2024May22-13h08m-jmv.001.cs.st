'From Cuis7.1 [latest update: #6391] on 22 May 2024 at 1:30:47 pm'!

!ProtocolBrowser methodsFor: 'private' stamp: 'jmv 5/22/2024 13:29:02'!
selectiveClassListIndex: anObject
	"Set the value of selectiveClassListIndex"

	| hadSelectedCategoryFor |
	selectiveClassListIndex := anObject.
	hadSelectedCategoryFor := categoriesListIndex notNil ifTrue: [selectedMessage].
	categoriesListIndex := nil.
	self protocolFor: selectiveClassListIndex.
	hadSelectedCategoryFor notNil ifTrue: [
		self categoriesListIndex: (categoriesList indexOf: selectedMessage category) ].
! !

