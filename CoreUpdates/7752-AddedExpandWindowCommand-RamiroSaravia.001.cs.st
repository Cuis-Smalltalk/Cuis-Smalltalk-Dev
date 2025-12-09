'From Cuis7.5 [latest update: #7739] on 4 December 2025 at 1:15:27 pm'!

!Theme class methodsFor: 'user interface' stamp: 'rs 4/Dec/2025 13:10:16'!
setDefaultSizeAllWindows

    Preferences name: #expandAllWindows category: #gui value: false.
    Preferences saveToDisk: #expandAllWindows.! !

!Theme class methodsFor: 'user interface' stamp: 'rs 4/Dec/2025 13:10:22'!
setExpandAllWindows

    Preferences name: #expandAllWindows category: #gui value: true.
    Preferences saveToDisk: #expandAllWindows.! !


!SystemWindow methodsFor: 'initialization' stamp: 'rs 4/Dec/2025 13:09:37'!
openInWorld
	"Ensure all widgets have proper colors before opening"
	Theme current widgetsColorFromToolColor ifTrue: [
		self widgetsColor: self windowColor ].
	super openInWorld.
	(Preferences at: #expandAllWindows) ifTrue: [
		self expandBoxHit ].! !


!WorldMorph methodsFor: 'submorphs-menus' stamp: 'rs 4/Dec/2025 13:13:45'!
worldMenuSpec
	"Spec for the menu and shortcut keys for the World.
	See coment at Morph >> #getMenu
	"
	^
	#(
		"Keyboard shortcut only. No menu entry."
		#(991 	nil 	$b 	smalltalk 	browse)
		#(992 	nil 	$f 	smalltalk 	findAndBrowseClass)
		#(993 	nil 	$F 	smalltalk 	findInSourceCode)
		#(994 	nil 	$+ 	theme   	largerDefaultFont)
		#(995 	nil 	$= 	theme   	largerDefaultFont) "See comment at #isCmdAltGreaterThan. This hack is needed for the shortcut to work on Mac."
		#(996 	nil 	$- 	theme   	smallerDefaultFont)
		#(997 	nil 	$/ 	theme 	setExpandAllWindows)
		#(998 	nil 	$* 	theme 	setDefaultSizeAllWindows)
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Theme setDefaultSizeAllWindows
!
