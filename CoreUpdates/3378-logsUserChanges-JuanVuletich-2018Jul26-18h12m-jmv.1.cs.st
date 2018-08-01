'From Cuis 5.0 of 7 November 2016 [latest update: #3377] on 26 July 2018 at 6:15:23 pm'!
!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName LastStats LogsUserChanges '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 7/26/2018 18:15:12'!
defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), '.user.changes'! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 7/26/2018 18:15:16'!
logsUserChanges
	LogsUserChanges ifNil: [ LogsUserChanges _ true ].
	^ LogsUserChanges! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 7/26/2018 18:15:19'!
logsUserChanges: aBoolean
	LogsUserChanges _ aBoolean! !

!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName LastStats LogsUserChanges'
	poolDictionaries: ''
	category: 'System-Support'!
