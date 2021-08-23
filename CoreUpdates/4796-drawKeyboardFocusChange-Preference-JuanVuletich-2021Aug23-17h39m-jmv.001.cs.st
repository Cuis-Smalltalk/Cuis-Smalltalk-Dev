'From Cuis 5.0 [latest update: #4795] on 23 August 2021 at 5:41:13 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 8/23/2021 15:24:10'!
drawKeyboardFocusIndicator
	^ self
		valueOfFlag: #drawKeyboardFocusIndicator
		ifAbsent: [ true ]! !


!TextModelMorph methodsFor: 'accessing' stamp: 'jmv 8/23/2021 15:04:09'!
drawKeyboardFocusIndicator
	"For InnerTextMorph"

	^drawKeyboardFocusIndicator! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 8/23/2021 15:24:54'!
cuisDefaults
	self setPreferencesFrom: #(
			#(#drawKeyboardFocusIndicator true )
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

!Preferences class methodsFor: 'themes' stamp: 'jmv 8/23/2021 15:25:08'!
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
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 8/23/2021 15:25:36'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak and Cuis in recent years. Caution: turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(drawKeyboardFocusIndicator false )
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders false)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoMenuIcons.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 8/23/2021 15:05:27'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self showsBlinkingCursor ifTrue: [
				editor hasSelection ifFalse: [
					self startBlinking ]]]
		ifFalse: [ self stopBlinking ].
	
	(owner notNil and: [ owner drawKeyboardFocusIndicator ])
		ifTrue: [ owner redrawNeeded ]
		ifFalse: [
			"Selection might be shown differently when focused"
			self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ]]	.! !


!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 8/23/2021 15:23:44'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ Preferences drawKeyboardFocusIndicator.

	scroller _ self innerMorphClass new.
	self addMorph: scroller.
	self scrollerOffset: `0@ 0`.
	self addMorph: scrollBar.
	self addMorph: hScrollBar! !


!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 8/23/2021 15:20:44'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"


	drawKeyboardFocusIndicator
		ifTrue: [self redrawNeeded]
		ifFalse: [
			selectedMorph ifNotNil: [ selectedMorph redrawNeeded ]]! !


!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 8/23/2021 15:33:41'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"

	drawKeyboardFocusIndicator
		ifTrue: [self redrawNeeded]
		ifFalse: [
			scroller selectedRow
				ifNotNil: [ :i |
					scroller selectedRow: nil; selectedRow: i ]]! !

