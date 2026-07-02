'From Cuis7.9 [latest update: #8040] on 2 July 2026 at 2:07:56 pm'!

!PreferenceSet class methodsFor: 'services' stamp: 'jmv 2/Jul/2026, 6:07:28 pm (UTC)'!
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
					(value size>0 and: [
						value first == $#])
							ifTrue: [ value := (value withoutPrefix: '#') asSymbol ].
					pref value: value.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefs.txt Error' print.
						error print.
						line print ].
				]]].! !

