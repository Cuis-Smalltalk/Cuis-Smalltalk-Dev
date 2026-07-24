'From Cuis7.9 [latest update: #8088] on 24 July 2026 at 10:13:56 am'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 24/Jul/2026, 1:06:04 pm (UTC)'!
authorInitials
	"Answer the initials to be used to identify the current code author.  "
	"
	Utilities clearAuthor; authorInitials
	"
	ChangeSet notInstallOrTestRun ifFalse: [
		^ ChangeSet installingString ].

	[(Preferences at: #authorInitials) isEmpty]
		whileTrue: [self setAuthor].
	^ Preferences at: #authorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 24/Jul/2026, 1:06:53 pm (UTC)'!
authorInitialsPerSe
	"Answer the currently-prevailing author initials, just as they are known, empty or not"

	^ Preferences at: #authorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 24/Jul/2026, 1:05:33 pm (UTC)'!
authorName
	"
	Utilities clearAuthor; authorName
	"
	[(Preferences at: #authorName) isEmpty]
		whileTrue: [self setAuthor].
	^ Preferences at: #authorName! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 24/Jul/2026, 1:03:12 pm (UTC)'!
clearAuthor
	"Clear author initials and name.
	Don't save to the user preferences file!!"

	Preferences name: #authorInitials category: #programming value: ''.
	Preferences name: #authorName category: #programming value: ''.! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 24/Jul/2026, 1:07:56 pm (UTC)'!
setAuthorInitials: aStringInitials name: aStringName
	"Set author name and initials programatically.
	Save them to user preferences disk file."

	Preferences name: #authorInitials category: #programming value: aStringInitials.
	Preferences saveToDisk: #authorInitials.
	Preferences name: #authorName category: #programming value: aStringName.
	Preferences saveToDisk: #authorName.! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 24/Jul/2026, 1:04:58 pm (UTC)'!
setAuthorInitials: tempInitials name: tempName while: aBlock
	"Set author name and initials programatically, only during execution of aBlock."

	| authorInitials authorName |
	authorInitials := Preferences at: #authorInitials.
	authorName := Preferences at: #authorName.
	Preferences name: #authorInitials category: #programming value: tempInitials.
	Preferences name: #authorName category: #programming value: tempName.
	aBlock ensure: [
		Preferences name: #authorInitials category: #programming value: authorInitials.
		Preferences name: #authorName category: #programming value: authorName ].
! !

