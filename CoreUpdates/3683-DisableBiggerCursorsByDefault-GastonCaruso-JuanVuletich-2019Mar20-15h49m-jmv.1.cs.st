'From Cuis 5.0 of 7 November 2016 [latest update: #3682] on 20 March 2019 at 3:52:35 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'GC 3/19/2019 21:00:04'!
biggerCursors
	^ self
		valueOfFlag: #biggerCursors
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'themes' stamp: 'GC 3/19/2019 21:02:02'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(biggerCursors false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe false)
		(syntaxHighlightingAsYouType true)
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs false)

		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	).
	self useMenuIcons
	".
	Theme beCurrent.
	Taskbar showTaskbar
	"! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/20/2019 15:49:29'!
defaultFont11
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont11
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 11)
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/20/2019 15:49:26'!
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
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 3/20/2019 15:49:22'!
defaultFont14
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont14
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 14)
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences standardCodeFont pointSize < 14 ifTrue: [Preferences disable: #biggerCursors]!

