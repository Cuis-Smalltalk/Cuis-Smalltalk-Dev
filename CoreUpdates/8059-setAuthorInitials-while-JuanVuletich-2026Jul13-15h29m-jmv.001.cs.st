'From Cuis7.9 [latest update: #8056] on 13 July 2026 at 3:37:42 pm'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 13/Jul/2026, 6:37:39 pm (UTC)'!
setAuthorInitials: tempInitials name: tempName while: aBlock
	"Set author name and initials programatically, only during execution of aBlock."

	| authorInitials authorName |
	authorInitials := (Preferences includesKey: #authorInitials) ifTrue: [ Preferences at: #authorInitials ].
	authorName := (Preferences includesKey: #authorName) ifTrue: [ Preferences at: #authorName ].
	Preferences name: #authorInitials category: #programming value: tempInitials.
	Preferences name: #authorName category: #programming value: tempName.
	aBlock ensure: [
		authorInitials
			ifNotNil: [ Preferences name: #authorInitials category: #programming value: authorInitials ]
			ifNil: [ Preferences removeKey: #authorInitials ].
		authorName
			ifNotNil: [ Preferences name: #authorName category: #programming value: authorName ]
			ifNil: [ Preferences removeKey: #authorName ]].! !

