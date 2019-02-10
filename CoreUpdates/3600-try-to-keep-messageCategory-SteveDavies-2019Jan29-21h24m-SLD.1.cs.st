'From Cuis 5.0 of 7 November 2016 [latest update: #3597] on 8 February 2019 at 10:16:00 pm'!

!Browser methodsFor: 'class list' stamp: 'SLD 2/8/2019 22:07:43'!
classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList |

	classList _ self classList.
	canSelectClass _ classList isInBounds: anInteger.
	selectedClassName _ canSelectClass ifTrue: [	| newClassName |
		newClassName := classList at: anInteger ifAbsent: [ nil ].
		newClassName := newClassName ifNotNil: [newClassName withoutLeadingBlanks asSymbol].
		newClassName ].
	self setClassOrganizer.
	selectedMessage _ nil.
	
	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent _ self class recentClasses.
		className _ self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].

	"Clear selectedMessageCategory if there is no match in the new list of categories"
	(self messageCategoryList indexOf: selectedMessageCategory) = 0 ifTrue: [
		selectedMessageCategory _ nil].
		
	"We have to refresh the class list if somebody wants to select a class that does not exist anymore - Hernan"
	anInteger > classList size ifTrue: [ self changed: #classList ].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !

