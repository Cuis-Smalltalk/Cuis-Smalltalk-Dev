'From Cuis 6.0 [latest update: #5090] on 3 March 2022 at 9:31:30 am'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:09:42'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences hugeFonts
	"

	self defaultFontSize: 32.
	self setPreference: #guiElementsSize toValue:  #hugeFonts.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:09:46'!
largeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences largeFonts
	"

	self defaultFontSize: 18.
	self setPreference: #guiElementsSize toValue:  #largeFonts.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:09:49'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences smallFonts
	"
	
	self defaultFontSize: 11.
	self setPreference: #guiElementsSize toValue:  #smallFonts.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:09:53'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	self defaultFontSize: 14.
	self setPreference: #guiElementsSize toValue:  #standardFonts.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:09:56'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"

	self defaultFontSize: 7.
	self setPreference: #guiElementsSize toValue:  #tinyFonts.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:10:00'!
veryLargeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences veryLargeFonts
	"

	self defaultFontSize: 24.
	self setPreference: #guiElementsSize toValue:  #veryLargeFonts.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/3/2022 09:10:04'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences verySmallFonts
	"
	
	self defaultFontSize: 9.
	self setPreference: #guiElementsSize toValue:  #verySmallFonts.! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 3/3/2022 09:30:54'!
changeFontSizes

	| options menu currentIndex |
	options _ #(
		#('Huge' #hugeFonts)
		#('Very Large' #veryLargeFonts)
		#('Large' #largeFonts)
		#('Default Size' #standardFonts)
		#('Small' #smallFonts)
		#('Very Small' #verySmallFonts)
		#('Tiny' #tinyFonts)).
	menu _ MenuMorph new defaultTarget: Preferences.
	menu
		addTitle: 'Make GUI elements';
		addStayUpIcons;
		stayUp: true.
	options do: [ :pair |
		menu add: pair first action: pair second ].
	currentIndex _ (options size / 2) ceiling.
	(Preferences preferenceAt: #guiElementsSize) ifNotNil: [ :pref |
		options withIndexDo: [ :pair :i |
			pair second = pref preferenceValue ifTrue: [ currentIndex _ i ]]].
	menu selectItem: (menu items at: currentIndex).
	menu popUpInWorld: self runningWorld.! !

