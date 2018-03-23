'From Cuis 5.0 of 7 November 2016 [latest update: #3277] on 22 March 2018 at 12:42:03 pm'!

!Browser methodsFor: 'class list' stamp: 'HAW 3/22/2018 12:40:03'!
classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass |

	canSelectClass _ anInteger between: 1 and: self classList size.
	selectedClassName _ canSelectClass ifTrue: [ self classList at: anInteger ].
	self setClassOrganizer.
	selectedMessageCategory _ nil.
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
	
	"We have to refresh the class list if somebody wants to select a class that does not exist anymore - Hernan"
	anInteger > self classList size ifTrue: [ self changed: #classList ].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !


!SystemDictionary methodsFor: 'class names' stamp: 'HAW 3/22/2018 12:37:37'!
forgetClass: aClass logged: aBool 
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	SystemOrganization removeElement: aClass name.
	aBool ifTrue: [SystemChangeNotifier uniqueInstance classRemoved: aClass fromCategory: aClass category].		
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache! !

