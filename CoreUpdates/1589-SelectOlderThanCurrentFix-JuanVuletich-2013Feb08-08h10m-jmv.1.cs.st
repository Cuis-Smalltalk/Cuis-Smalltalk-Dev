'From Cuis 4.1 of 12 December 2012 [latest update: #1588] on 8 February 2013 at 8:11:43 am'!

!ChangeListElement methodsFor: 'testing' stamp: 'jmv 2/8/2013 08:10'!
isMethodDeletion
	^false! !


!MethodDeletionChangeRecord methodsFor: 'testing' stamp: 'jmv 2/8/2013 08:10'!
isMethodDeletion
	^true! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 2/8/2013 08:11'!
selectMethodsOlderThanCurrent
	"Selects all method definitions for which there is some newer counterpart in the current image.
	This is, select methods that installing would be go back in time"

	| cm doSelect |
	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			doSelect _ false.
			((change changeType == #method and: [ change isMethodDeletion not ] ) and: [
				((class _ change changeClass) notNil)]) ifTrue: [
					cm _ class compiledMethodAt: change methodSelector ifAbsent: nil.
					cm ifNotNil: [
						doSelect _ change dateAndTime < cm dateAndTime ].
					].
			listSelections at: i put: doSelect ]].
	self changed: #allSelections.
	self changed: #annotation! !


!MethodDeletionChangeRecord reorganize!
('accessing' changeClass changeClassName changeType doItOnlyIfInBaseSystem: isMetaClassChange methodReference: methodSelector string)
('fileIn/fileOut' fileIn fileOutOn:)
('services' command)
('testing' isMethodDeletion)
!

