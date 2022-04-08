'From Cuis 6.0 [latest update: #5104] on 8 April 2022 at 4:07:29 pm'!

!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 4/8/2022 16:05:32'!
addSelector: aSymbol

	self protected: [
		Selectors at: aSymbol ifAbsentPut: 0 ].! !


!Preferences class methodsFor: 'initialization' stamp: 'jmv 4/8/2022 16:06:06'!
compileAccessMethodForPreference: aPreference
	"Compile an accessor method for the given preference"

	self class compileSilently: (aPreference name, '
	^ self valueOfFlag: #', aPreference name, ' ifAbsent: [', aPreference defaultValue storeString, ']') classified: 'standard queries'.
	SmalltalkCompleter addSelector: aPreference name.! !

!Preferences class methodsFor: 'personalization' stamp: 'jmv 4/8/2022 16:05:45'!
compileHardCodedPref: prefName enable: aBoolean
	"Compile a method that returns a simple true or false (depending on the value of aBoolean) when Preferences is sent prefName as a message"

	self class compileSilently: (prefName asString, '
	"compiled programatically -- return hard-coded preference value"
	^ ', aBoolean storeString) classified: 'hard-coded prefs'.
	SmalltalkCompleter addSelector: prefName.
	
"Preferences compileHardCodedPref: #testing enable: false"! !

