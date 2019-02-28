'From Cuis 5.0 of 7 November 2016 [latest update: #3633] on 27 February 2019 at 1:27:51 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:20:35'!
bigFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences bigFonts
	"

	self defaultFont17! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:03:25'!
defaultFont05
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont05
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 7)
			(setListFontTo: 5)
			(setMenuFontTo: 5)
			(setWindowTitleFontTo: 7)
			(setCodeFontTo: 5)
			(setButtonFontTo: 5)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:06:29'!
defaultFont06
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont06
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 7)
			(setListFontTo: 6)
			(setMenuFontTo: 6)
			(setWindowTitleFontTo: 8)
			(setCodeFontTo: 6)
			(setButtonFontTo: 6)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:04:07'!
defaultFont07
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont07
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 7)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:02:21'!
defaultFont08
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont08
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 8)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:02:38'!
defaultFont09
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont09
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:05:56'!
defaultFont10
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont10
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 10)
			(setListFontTo: 10)
			(setMenuFontTo: 11)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 10)
			(setButtonFontTo: 10)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:07:19'!
defaultFont12
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont12
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 12)
			(setListFontTo: 12)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 12)
			(setButtonFontTo: 12)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:20:12'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences hugeFonts
	"

	self defaultFont46! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:21:58'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences smallFonts
	"
	
	self defaultFont09! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:20:22'!
veryBigFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences veryBigFonts
	"

	self defaultFont28! !


!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:20:44'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	self defaultFont12! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:03:47'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"

	self defaultFont05! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/27/2019 13:04:24'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences verySmallFonts
	"
	
	self defaultFont07! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 2/27/2019 13:16:39'!
install: aString
"
StrikeFont install: 'DejaVu'.
"
"
StrikeFont install: 'DejaVu Sans Mono'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans Mono'.
Preferences bigFonts.
Character initialize.
"
"
StrikeFont install: 'DejaVu Sans'.
StrikeFont buildLargerPunctuation: 'DejaVu Sans'.
Preferences setDefaultFontFamilyTo: 'DejaVu Sans'.
Preferences standardFonts.
Character initialize.
"
"
StrikeFont install: 'Inconsolata'
StrikeFont install: '#PilGi'
StrikeFont install: 'Optima'
StrikeFont install: 'Herculanum'
StrikeFont install: 'Papyrus'
StrikeFont install: 'Handwriting - Dakota'
StrikeFont install: 'Times New Roman'
StrikeFont install: 'Apple Chancery'
StrikeFont install: 'Cochin'
StrikeFont install: 'Cracked'
StrikeFont install: 'Zapfino'
StrikeFont install: 'Brush Script MT'
StrikeFont install: 'Chalkboard'
"
	| fontDict |
	fontDict _ nil.
	1 to: 200 do: [ :s |
		(self create: aString size: s bold: true italic: true boldItalic: false) ifNotNil: [ :font |
			font print.
			fontDict ifNil: [ fontDict _ AvailableFonts at: aString ifAbsentPut: Dictionary new ].
			fontDict
				at: s
				put: font ]]! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 5/6/2018 16:42:00'!
changeFontSizes

	(MenuMorph new defaultTarget: Preferences)
		addTitle: 'Choose a size';
		addStayUpIcons;
		add: 'Huge Fonts' action: #hugeFonts;
		add: 'Very big Fonts' action: #veryBigFonts;
		add: 'Big Fonts' action: #bigFonts;
		add: 'Standard Fonts' action: #standardFonts;
		add: 'Small Fonts' action: #smallFonts;
		add: 'Very small Fonts'action: #verySmallFonts;
		add: 'Tiny Fonts'action: #tinyFonts;
		popUpInWorld: self runningWorld! !

!methodRemoval: Preferences class #defaultFont8!
Preferences class removeSelector: #defaultFont8!
!methodRemoval: Preferences class #defaultFont9!
Preferences class removeSelector: #defaultFont9!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences standardFonts!

