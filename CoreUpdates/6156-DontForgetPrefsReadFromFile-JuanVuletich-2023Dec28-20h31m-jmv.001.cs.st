'From Cuis6.0 [latest update: #6155] on 28 December 2023 at 8:32:54 pm'!

!Preference methodsFor: 'accessing' stamp: 'jmv 12/28/2023 20:31:29'!
justReadFromDisk
	"So that we are included whenever the preferences file is saved again."
	isSavedToDisk := true.! !


!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/28/2023 20:31:46'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file line parts pref |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[
					pref := Preferences instanceAt: parts first asSymbol.
					pref value: parts second.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !

