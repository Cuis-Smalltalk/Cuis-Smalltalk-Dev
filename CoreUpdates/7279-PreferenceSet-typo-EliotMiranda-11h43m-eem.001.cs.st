'From Cuis7.5 [latest update: #7265] on 20 June 2025 at 11:40:59 am'!

!PreferenceSet class methodsFor: 'services' stamp: 'eem 6/20/2025 11:40:47'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [ | line parts |
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[ | pref value |
					pref := Preferences instanceAt: parts first asSymbol.
					value := parts second.
					value first == $# ifTrue: [ value := (value withoutPrefix: '#') asSymbol ].
					pref value: value.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefs.txt Error' print.
						error print.
						line print ].
				]]].! !
