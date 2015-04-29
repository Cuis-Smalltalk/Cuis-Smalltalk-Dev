'From Cuis 4.2 of 25 July 2013 [latest update: #2258] on 20 April 2015 at 4:39:52.672872 pm'!
!classDefinition: #Preferences category: #'System-Support'!
Object subclass: #Preferences
	instanceVariableNames: ''
	classVariableNames: 'DesktopColor DictionaryOfPreferences Parameters '
	poolDictionaries: ''
	category: 'System-Support'!

!Preferences class methodsFor: 'class state access' stamp: 'jmv 4/20/2015 16:10'!
parameters
	Parameters ifNil: [
		Parameters _ Dictionary new ].
	^Parameters! !

!Preferences class methodsFor: 'class state access' stamp: 'jmv 4/20/2015 16:28'!
preferencesDictionary

	DictionaryOfPreferences ifNil: [
		DictionaryOfPreferences _ Dictionary new.
		Preferences cuisDefaults ].
	^DictionaryOfPreferences! !

!Preferences class methodsFor: 'class state access' stamp: 'jmv 4/20/2015 16:31'!
releaseClassState
	"Not just cached state but actual state. User preferences will be forgotten, and defaults will be assumed.
	Preferences releaseClassState
	"
	Parameters _ nil.
	DictionaryOfPreferences _ nil! !


!Preferences class methodsFor: 'add preferences' stamp: 'jmv 4/20/2015 16:28'!
addPreference: prefSymbol categories: categoryList default: aValue balloonHelp: helpString changeInformee: informeeSymbol  changeSelector: aChangeSelector
	"Add or replace a preference as indicated.  Reuses the preexisting Preference object for this symbol, if there is one, so that UI artifacts that interact with it will remain valid."

	| aPreference |
	aPreference _ self preferencesDictionary at: prefSymbol ifAbsent: [Preference new].
	aPreference name: prefSymbol defaultValue: aValue helpString: helpString categoryList: categoryList changeInformee: informeeSymbol changeSelector: aChangeSelector.
	self preferencesDictionary at: prefSymbol put: aPreference.
	self compileAccessMethodForPreference: aPreference! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setButtonFontTo: aFont

	self parameters at: #standardButtonFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setCodeFontTo: aFont
	"Not currently sent, but once protocols are sorted out so that we can discriminate on whether a text object being launched is for code or not, might deserve to be reincorporated"

	self parameters at: #standardCodeFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setListFontTo: aFont
	"Set the list font as indicated"

	self parameters at: #standardListFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setMenuFontTo: aFont

	self parameters at: #standardMenuFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setWindowTitleFontTo: aFont
	"Set the window-title font to be as indicated"

	self parameters at: #windowTitleFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:20'!
standardButtonFont
	"Answer an attractive font to use for buttons"

	^self parameters at: #standardButtonFont ifAbsent: [
		Preferences standardFonts.
		self parameters at: #standardButtonFont ]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:20'!
standardCodeFont
	"Answer the font to be used in code"

	^self parameters at: #standardCodeFont ifAbsent: [
		Preferences standardFonts.
		self parameters at: #standardCodeFont ]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:20'!
standardListFont
	"Answer the font to be used in lists"

	^self parameters at: #standardListFont ifAbsent: [
		Preferences standardFonts.
	 	self parameters at: #standardListFont ]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:20'!
standardMenuFont
	"Answer the font to be used in menus"

	^self parameters at: #standardMenuFont ifAbsent: [
		Preferences standardFonts.
		self parameters at: #standardMenuFont ]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:20'!
windowTitleFont
	"Answer the standard font to use for window titles"

	^self parameters at: #windowTitleFont ifAbsent: [
		Preferences standardFonts.
		self parameters at: #windowTitleFont ]! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 4/20/2015 16:11'!
haloSpecifications
	"Answer a list of HaloSpecs that describe which halos are to be used, what they should look 
like, and where they should be situated"

	^ self parameters at: #HaloSpecs ifAbsent: [
			self installHaloTheme: #iconicHaloSpecifications.
			^ self parameters at: #HaloSpecs]

	"Preferences haloSpecifications"
	"Preferences resetHaloSpecifications"
! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 4/20/2015 16:17'!
installHaloSpecsFromArray: anArray

	| aColor |
	^ self parameters at: #HaloSpecs put: (anArray collect: [ :each |
				aColor _ Color.
				each fourth do: [ :sel | aColor _ aColor perform: sel].
				HaloSpec new 
					horizontalPlacement: each second
					verticalPlacement: each third 
					color: aColor
					iconSymbol: each fifth
					addHandleSelector: each first
					hoverHelp: each sixth])! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 4/20/2015 15:58'!
installHaloTheme: themeSymbol
	self installHaloSpecsFromArray: (self perform: themeSymbol)
	! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 4/20/2015 16:18'!
resetHaloSpecifications
	"Preferences resetHaloSpecifications"

	^ self parameters removeKey: #HaloSpecs ifAbsent: nil! !

!Preferences class methodsFor: 'menu parameters' stamp: 'jmv 4/20/2015 16:17'!
menuBorderWidth

	^self parameters at: #menuBorderWidth ifAbsentPut: [ 2 ]! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 4/20/2015 16:10'!
defaultAnnotationRequests
	^ self parameters at: #MethodAnnotations ifAbsent:
		[self setDefaultAnnotationInfo]
	"Preferences annotationInfo"! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 4/20/2015 16:19'!
setDefaultAnnotationInfo
	"
	Preferences setDefaultAnnotationInfo
	"
	^ self parameters at: #MethodAnnotations put: #(timeStamp messageCategory implementorsCount sendersCount closuresInfo allBaseSystemChangeSets packages)! !

!Preferences class methodsFor: 'preference-object access' stamp: 'jmv 4/20/2015 16:30'!
preferenceAt: aSymbol
	"Answer the Preference object at the given symbol, or nil if not there"

	^ self preferencesDictionary at: aSymbol ifAbsent: nil! !

!Preferences class methodsFor: 'preference-object access' stamp: 'jmv 4/20/2015 16:30'!
preferenceAt: aSymbol ifAbsent: aBlock
	"Answer the Preference object at the given symbol, or the value of aBlock if not present"

	^ self preferencesDictionary at: aSymbol ifAbsent: aBlock! !

!Preferences class methodsFor: 'preferences panel' stamp: 'jmv 4/20/2015 16:30'!
openPreferencesInspector
	"Open a window on the current set of preferences choices, allowing the user to view and change their settings"
	
	self preferencesDictionary inspectWithLabel: 'Preferences'! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 4/20/2015 16:39'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	Taskbar reset; initialize.
	self runningWorld restoreMorphicDisplay.
	
	^ CurrentTheme! !

!methodRemoval: Preferences class #allPreferenceObjects!
Preferences class removeSelector: #allPreferenceObjects!
!methodRemoval: Preferences class #cleanUp!
Preferences class removeSelector: #cleanUp!
!methodRemoval: Preferences class #initialize!
Preferences class removeSelector: #initialize!
!methodRemoval: Preferences class #initializeDictionaryOfPreferences!
Preferences class removeSelector: #initializeDictionaryOfPreferences!
!methodRemoval: Preferences class #initializeParameters!
Preferences class removeSelector: #initializeParameters!
!methodRemoval: Preferences class #inspectParameters!
Preferences class removeSelector: #inspectParameters!
!methodRemoval: Preferences class #inspectUnused!
Preferences class removeSelector: #inspectUnused!
!methodRemoval: Preferences class #inspectUnusedParameters!
Preferences class removeSelector: #inspectUnusedParameters!
!methodRemoval: Preferences class #maxBalloonHelpLineLength!
Preferences class removeSelector: #maxBalloonHelpLineLength!
!methodRemoval: Preferences class #parameterAt:ifAbsent:!
Preferences class removeSelector: #parameterAt:ifAbsent:!
!methodRemoval: Preferences class #parameterAt:ifAbsentPut:!
Preferences class removeSelector: #parameterAt:ifAbsentPut:!
!methodRemoval: Preferences class #removeParameter:!
Preferences class removeSelector: #removeParameter:!
!methodRemoval: Preferences class #removePreference:!
Preferences class removeSelector: #removePreference:!
!methodRemoval: Preferences class #restoreDefaultMenuParameters!
Preferences class removeSelector: #restoreDefaultMenuParameters!
!methodRemoval: Preferences class #setParameter:to:!
Preferences class removeSelector: #setParameter:to:!
Preferences initialize!
!classDefinition: #Preferences category: #'System-Support'!
Object subclass: #Preferences
	instanceVariableNames: ''
	classVariableNames: 'DictionaryOfPreferences Parameters'
	poolDictionaries: ''
	category: 'System-Support'!
