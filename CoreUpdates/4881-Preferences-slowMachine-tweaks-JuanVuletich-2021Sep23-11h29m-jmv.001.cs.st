'From Cuis 5.0 [latest update: #4880] on 23 September 2021 at 11:31:18 am'!

!Preferences class methodsFor: 'parameters' stamp: 'jmv 9/23/2021 11:30:01'!
setCheapAnnotationInfo
	"
	Preferences setCheapAnnotationInfo
	"
	^ self parameters at: #MethodAnnotations put: #(timeStamp messageCategory packages changeSets)! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 9/23/2021 11:30:57'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		#(#drawKeyboardFocusIndicator false )
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoMenuIcons.
	self runningWorld backgroundImageData: nil.
	Preferences setCheapAnnotationInfo.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !

