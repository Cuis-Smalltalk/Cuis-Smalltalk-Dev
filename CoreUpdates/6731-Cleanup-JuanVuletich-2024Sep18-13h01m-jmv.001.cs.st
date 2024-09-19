'From Cuis7.1 [latest update: #6727] on 18 September 2024 at 1:05:16 pm'!

!Browser methodsFor: 'class list' stamp: 'jmv 9/18/2024 13:05:07'!
classListIndex: anInteger
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList prevSelectedClassName prevSelectedMessage |
	classList := self classList.
	canSelectClass := classList isInBounds: anInteger.
	prevSelectedClassName := selectedClassName.
	selectedClassName := canSelectClass ifTrue: [
		| newClassName |
		newClassName := classList at: anInteger ifAbsent: [ nil ].
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

	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged.
	prevSelectedClassName = selectedClassName ifFalse: [
		"If clicked on already selected class, deselect selector, so class definition is shown"
		self setSelector: prevSelectedMessage ].! !

