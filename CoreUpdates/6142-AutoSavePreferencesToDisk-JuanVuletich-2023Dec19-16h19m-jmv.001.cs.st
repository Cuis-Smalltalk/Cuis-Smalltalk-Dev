'From Cuis6.0 [latest update: #6140] on 19 December 2023 at 4:30:45 pm'!
!classDefinition: #Preference category: #'System-Support'!
Object subclass: #Preference
	instanceVariableNames: 'name description category value type isSavedToDisk '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!Preference methodsFor: 'accessing' stamp: 'jmv 12/19/2023 15:34:39'!
isSavedToDisk
	"If this preference was modified by the user, or read from disk, keep the User Preferences file updated."

	^isSavedToDisk == true! !

!Preference methodsFor: 'accessing' stamp: 'jmv 12/19/2023 15:42:06'!
isSavedToDisk: aBoolean
	"True if this preference was modified by the user, or read from disk.
	If so, keep the User Preferences file updated."

	isSavedToDisk := aBoolean.
	isSavedToDisk ifTrue: [
		PreferenceSet saveUserPrefs ].! !


!PreferenceSet methodsFor: 'services' stamp: 'jmv 12/19/2023 15:46:15'!
saveToDisk: nameSymbol
	"Add / update this preference in the User Preferences file."

	(self instanceAt: nameSymbol) isSavedToDisk: true.! !


!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/19/2023 15:27:00'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file line parts pref |
	file := DirectoryEntry projectBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[
					pref := Preferences instanceAt: parts first asSymbol.
					pref value: parts second.
				]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !

!PreferenceSet class methodsFor: 'services' stamp: 'jmv 12/19/2023 16:07:03'!
saveUserPrefs
	"
	PreferenceSet saveUserPrefs
	"
	| file |
	file := DirectoryEntry projectBaseDirectory // 'UserPrefs.txt'.
	file forceWriteStreamDo: [ :stream |
		Preferences allPreferences keysAndValuesDo: [ :symbolName :preference |
			preference isSavedToDisk ifTrue: [
				stream
					nextPutAll: symbolName;
					nextPut: $=.
				preference value printOn: stream.
				stream newLine ]]].! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/19/2023 15:26:50'!
readAndApplyUserPrefs
	"
	Smalltalk readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs.! !

!classDefinition: #Preference category: #'System-Support'!
Object subclass: #Preference
	instanceVariableNames: 'name description category value type isSavedToDisk'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
