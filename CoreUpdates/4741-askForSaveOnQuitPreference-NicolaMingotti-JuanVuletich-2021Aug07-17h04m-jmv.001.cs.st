'From Cuis 5.0 [latest update: #4740] on 7 August 2021 at 5:06:56 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'NM 8/7/2021 16:46:25'!
askForSaveOnQuit
	^ self
		valueOfFlag: #askForSaveOnQuit
		ifAbsent: [ true ].! !


!Preferences class methodsFor: 'themes' stamp: 'NM 8/7/2021 16:37:36'!
cuisDefaults
	self setPreferencesFrom: #(
	              #(#askForSaveOnQuit true)
			#(#balloonHelpEnabled true )
			#(#biggerCursors false )
			#(#browseWithPrettyPrint false )
			#(#caseSensitiveFinds false )
			#(#checkForSlips true )
			#(#cmdDotEnabled true )
			#(#diffsInChangeList true )
			#(#diffsWithPrettyPrint false )
			#(#menuKeyboardControl true )
			#(#optionalButtons true )
			#(#extraDebuggerButtons true )
			#(#subPixelRenderFonts true )
			#(#thoroughSenders true )
			#(#cheapWindowReframe false )
			#(#syntaxHighlightingAsYouType true )
			#(#tapAndHoldEmulatesButton2 true )
			#(#clickGrabsMorphs false )
			#(#syntaxHighlightingAsYouTypeAnsiAssignment false )
			#(#syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		).
	self useMenuIcons.! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/7/2021 17:06:20'!
quitSession

	| doSaveImage |
	doSaveImage _ Preferences askForSaveOnQuit and: [
		self confirm: 'Save the current image before quitting?' orCancel: [^ self]].
	Smalltalk 
		snapshot: doSaveImage
		andQuit: true
		clearAllClassState: false! !

