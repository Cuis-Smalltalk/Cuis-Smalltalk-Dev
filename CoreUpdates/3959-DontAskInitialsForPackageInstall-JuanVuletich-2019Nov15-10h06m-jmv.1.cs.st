'From Cuis 5.0 [latest update: #3958] on 15 November 2019 at 10:06:34 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 11/15/2019 10:06:10'!
installingString
	^Installing ! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 11/15/2019 10:02:06'!
authorInitials
	"Answer the initials to be used to identify the current code author.  "
	"
	Utilities clearAuthor; authorInitials
	"
	ChangeSet notInstallOrTestRun ifFalse: [
		^ ChangeSet installingString ].
		
	[AuthorInitials isNil or: [AuthorInitials isEmpty]] whileTrue: [self setAuthor].
	^ AuthorInitials! !

