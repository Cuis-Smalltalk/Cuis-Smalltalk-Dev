'From Cuis6.3 [latest update: #6280] on 17 March 2024 at 3:30:04 pm'!

!Preference methodsFor: 'accessing' stamp: 'jmv 4/18/2023 10:23:40'!
value: aValueOrString

	| aValue |
	aValue := aValueOrString.
	self isTypeAClass 
		ifTrue: [
			((aValueOrString isKindOf: self type) not and: [ aValueOrString isString ])
				ifTrue: [ aValue := self type readFrom: aValueOrString readStream ].
			(aValue isKindOf: self type) ifFalse: [
				self error: aValue printString, ' is not a ', type printString ].
			value := aValue ]
		ifFalse: [ "Should be handled somehow by the preference editor "
			value := aValue].
	self triggerEvent: #preferenceChanged with: self.! !


!PreferenceSet class methodsFor: 'services' stamp: 'jmv 3/17/2024 15:29:25'!
readAndApplyUserPrefs
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file line parts pref value |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[
					pref := Preferences instanceAt: parts first asSymbol.
					value := parts second.
					value first = $# ifTrue: [ value := (value withoutPrefix: '#') asSymbol ].
					pref value: value.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefts.txt Error' print.
						error print.
						line print ].
				]]].! !

