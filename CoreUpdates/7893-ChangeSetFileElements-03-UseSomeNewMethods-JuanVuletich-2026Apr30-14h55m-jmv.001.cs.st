'From Cuis7.7 [latest update: #7890] on 30 April 2026 at 3:11:57 pm'!

!CodeProvider methodsFor: 'misc' stamp: 'jmv 30/Apr/2026 11:36:47'!
priorSourceOrNil
	"If the currently-selected method has a previous version, return its source, else return nil"
	| aClass aSelector changes |
	(aClass := self selectedClassOrMetaClass) ifNil: [^ nil].
	(aSelector := self selectedMessageName) ifNil: [^ nil].
	changes := aClass recordedChangesForSelector: aSelector.
	(changes == nil or: [changes size <= 1]) ifTrue: [^ nil].
	^ (changes at: 2) string 
! !

!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 30/Apr/2026 11:36:30'!
revertToPreviousVersion
	"Revert to the previous version of the current method"
	| aClass aSelector changes |
	aClass := self selectedClassOrMetaClass.
	aClass ifNil: [^ self changed: #flash].
	aSelector := self selectedMessageName.
	changes := aClass recordedChangesForSelector: aSelector.
	(changes == nil or: [changes size <= 1]) ifTrue: [self changed: #flash.  ^ Smalltalk beep].
	changes second fileIn.
	self acceptedContentsChanged! !


!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 30/Apr/2026 11:36:15'!
methodsWithAnyInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one, even historically"
	| slips |
	slips := Set new.
	self changedClasses do: [:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method := aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					(aClass recordedChangesForSelector: mAssoc key) do: [ :chg | | aTimeStamp |
						aTimeStamp := chg stamp.
						(aTimeStamp notNil and: [(aTimeStamp beginsWith: myInits) not])
							ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]]].
	^ slips! !
