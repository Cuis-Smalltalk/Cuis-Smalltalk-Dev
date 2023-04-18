'From Cuis 6.0 [latest update: #5768] on 18 April 2023 at 10:42:17 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/18/2023 10:31:47'!
readAndApplyUserPrefs
	"
	Smalltalk readAndApplyUserPrefs
	"
	| file line parts |
	file := DirectoryEntry smalltalkImageDirectory // 'UserPrefs.txt'.
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


!PreferenceSet methodsFor: 'instance creation' stamp: 'jmv 4/18/2023 10:33:53'!
name: nameSymbol description: aString category: categorySymbol type: aType value: aValue
	"If I exist, I am updated with the newer sent data "

	| pref |
	(nameSymbol isSymbol or: [categorySymbol isSymbol]) ifFalse: [
		self error: 'Preference Name & Category are not valid symbol.' ].
	pref := contents at: nameSymbol ifAbsentPut: [ Preference new ].
	pref
		name: nameSymbol 
		description: aString 
		category: categorySymbol 
		type: aType 
		value: aValue.
	^pref! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 4/18/2023 10:35:31'!
prefFont

	^ #(
		#(defaultFontSize 18 SmallInteger defaultFontSize:)
		#(aaFontsColormapDepth 4)
		#(cacheTrueTypeGlyphs true )
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 4/18/2023 10:32:27'!
init
" Not name #initialize to avoid auto start at class installation "
	#(gui font programming system) with: #(prefGui prefFont prefProgramming prefSystem) do: [ :category :selector |
		(self perform: selector) do: [ :aPrefArray |
			self installDefault: aPrefArray in: category ]].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences.
	self setDefaultFont: FontFamily defaultFamilyName.! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 4/18/2023 10:29:44'!
installDefault: anArray in: category
	"Description of preference in Array: name - value - type (optional) - event handler (optional) "

	| type myPref |
	type := anArray at: 3 ifAbsent: [Preference detectType: anArray second].
	type isSymbol ifTrue: [
		type := Smalltalk at: type ifAbsent: [Preference detectType: anArray second]].
	myPref := self sysPreferences 
		name: anArray first 
		description: '' 
		category: category 
		type: type
		value: anArray second.
	anArray size = 4 ifTrue: [ "fourth record is an event listener hooked to Preference"
		myPref when: #preferenceChanged send: anArray fourth to: PreferenceSet.
		myPref triggerEvent: #preferenceChanged with: myPref ].! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 4/18/2023 10:27:39'!
defaultFontSize: aPreference
	"
	Preferences at: #defaultFontSize put: '12'
	Preferences at: #defaultFontSize put: 14
	Preferences at: #defaultFontSize put: 21
	"
	| pointSize font titleFont |
	pointSize := aPreference value.
	font := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preferences at: #biggerCursors put: (pointSize > 30).
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 4/18/2023 10:32:36'!
setDefaultFontSize: pointSize
	"To help user experiment until happy, reopen menu."

	Preferences at: #defaultFontSize put: pointSize.
	self changeFontSizes.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet init!

