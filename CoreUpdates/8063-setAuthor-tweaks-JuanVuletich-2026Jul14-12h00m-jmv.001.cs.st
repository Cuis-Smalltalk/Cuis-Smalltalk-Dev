'From Cuis7.9 [latest update: #8063] on 14 July 2026 at 12:00:41 pm'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 14/Jul/2026, 3:00:16 pm (UTC)'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| initialAnswer authorInitials authorName |

	initialAnswer := self authorInitialsPerSe ifNil: [''].
	authorInitials := self
		request: 'Please type your initials: '
		initialAnswer: initialAnswer
		orCancel: [ initialAnswer ].
	authorInitials := authorInitials withBlanksTrimmed.

	Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = authorInitials ]
		ifFound: [ :pair |
			authorName := pair second withBlanksTrimmed ]
		ifNone: [
			authorName := (self
				request: 'Please type your name:'
				initialAnswer: 'Unknown Name') withBlanksTrimmed.
			self setAuthorInitials: authorInitials name: authorName.
			^ self ].

	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [
			self setAuthorInitials: authorInitials name: authorName ]
		ifFalse: [
			(self confirm: 'Please enter different initials, then') ifFalse: [
				self setAuthorInitials: 'unknown' name: 'Unknown Name'.
				^ self].
			self setAuthor ].! !

