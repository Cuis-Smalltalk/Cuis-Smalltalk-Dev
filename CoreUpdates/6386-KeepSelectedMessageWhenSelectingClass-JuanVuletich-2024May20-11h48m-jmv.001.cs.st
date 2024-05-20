'From Cuis7.1 [latest update: #6384] on 20 May 2024 at 11:50:52 am'!

!Browser methodsFor: 'class list' stamp: 'jmv 5/20/2024 11:48:56'!
classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList prevSelectedMessage |

	classList := self classList.
	canSelectClass := classList isInBounds: anInteger.
	selectedClassName := canSelectClass ifTrue: [	| newClassName |
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
		
	"We have to refresh the class list if somebody wants to select a class that does not exist anymore - Hernan"
	anInteger > classList size ifTrue: [ self changed: #classList ].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged.
	self setSelector: prevSelectedMessage.
! !

