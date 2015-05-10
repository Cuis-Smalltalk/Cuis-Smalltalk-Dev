'From Cuis 4.2 of 25 July 2013 [latest update: #2284] on 1 May 2015 at 10:15:33.911699 pm'!

!Utilities class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 20:33'!
releaseClassCachedState
	"
	Utilities releaseClassCachedState
	"

	AuthorInitials _ AuthorName _ LastStats _ nil! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 5/1/2015 20:42'!
authorInitials
	"Answer the initials to be used to identify the current code author.  "
	"
	Utilities clearAuthor; authorInitials
	"

	[AuthorInitials isNil or: [AuthorInitials isEmpty]] whileTrue: [self setAuthor].
	^ AuthorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/1/2015 20:42'!
authorName
	"
	Utilities clearAuthor; authorName
	"

	[AuthorName isNil or: [AuthorName isEmpty]] whileTrue: [self setAuthor].
	^ AuthorName! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/1/2015 20:35'!
clearAuthor

	AuthorInitials _ AuthorName _ nil! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/1/2015 20:42'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorName |
	AuthorInitials _ (FillInTheBlankMorph
		request: 'Please type your initials: '
		initialAnswer: (AuthorInitials ifNil: [''])) withBlanksTrimmed.
	authorName _ (Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = AuthorInitials ]
		ifNone: [
			AuthorName _ (FillInTheBlankMorph
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			^ self ]) second withBlanksTrimmed.
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [ AuthorName _ authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ]! !

