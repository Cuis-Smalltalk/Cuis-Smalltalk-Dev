'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:43:37 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/15/2023 15:25:24'!
readAndApplyUserPrefs
	"
	Smalltalk readAndApplyUserPrefs
	"
	| file line parts |
	file := DirectoryEntry cuisBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[ Preferences at: parts first asSymbol put: parts second ]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !

