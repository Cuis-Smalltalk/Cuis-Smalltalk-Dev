'From Cuis7.7 [latest update: #7870] on 9 April 2026 at 1:54:37 pm'!

!Browser methodsFor: 'class functions' stamp: 'FJG 7/Apr/2026 16:56:08'!
editComment
	"Retrieve the description of the class comment."

	selectedClassName ifNil: [ ^self ].
	self messageCategoryListIndex: 0.
	metaClassIndicated := false.
	self editSelection: #editComment.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self acceptedContentsChanged
! !

!Browser methodsFor: 'class list' stamp: 'FJG 7/Apr/2026 16:56:04'!
classListIndex: anInteger
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList prevSelectedClassName prevSelectedMessage |
	classList := self classList.
	canSelectClass := classList isInBounds: anInteger.
	prevSelectedClassName := selectedClassName.
	selectedClassName := canSelectClass ifTrue: [
		| newClassName |
		newClassName := classList at: anInteger ifAbsent: nil.
		newClassName := newClassName ifNotNil: [newClassName withoutLeadingBlanks asSymbol].
		newClassName ].
	self setClassOrganizer.
	prevSelectedMessage := selectedMessage.
	selectedMessage := nil.

	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent := self class recentClasses.
		className := self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].

	"Clear selectedMessageCategory if there is no match in the new list of categories"
	(self messageCategoryList indexOf: selectedMessageCategory) = 0 ifTrue: [
		selectedMessageCategory := nil].

	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged.
	prevSelectedClassName = selectedClassName ifFalse: [
		"If clicked on already selected class, deselect selector, so class definition is shown"
		self setSelector: prevSelectedMessage ].! !

!Browser methodsFor: 'message category functions' stamp: 'FJG 7/Apr/2026 16:56:22'!
removeMessageCategory
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [^ self].
	messageCategoryName := self selectedMessageCategoryName.
	(self messageList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?'])
		ifTrue: [
			self selectedClassOrMetaClass removeCategory: messageCategoryName.
			self messageCategoryListIndex: 0].
	self changed: #messageCategoryList.
! !

!Browser methodsFor: 'metaclass' stamp: 'FJG 7/Apr/2026 16:56:10'!
metaClassIndicated: trueOrFalse 
	"Indicate whether browsing instance or class messages."

	metaClassIndicated := trueOrFalse.
	self setClassOrganizer.
	selectedSystemCategory ifNotNil: [
		self editSelection: (selectedClassName
			ifNil: [metaClassIndicated
				ifTrue: [#none]
				ifFalse: [#newClass]]
			ifNotNil: [#editClass])].
	selectedMessageCategory := nil.
	selectedMessage := nil.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged! !

!methodRemoval: CodeFileBrowser #removeMessageCategory stamp: 'FJG 9/Apr/2026 13:54:09'!
CodeFileBrowser removeSelector: #removeMessageCategory!
