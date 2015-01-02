'From Cuis 4.2 of 25 July 2013 [latest update: #2141] on 2 January 2015 at 8:35:39.112324 am'!

!Preferences class methodsFor: 'standard queries'!
clickGrabsMorphs
	^ self
		valueOfFlag: #clickGrabsMorphs
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 1/2/2015 08:34'!
forMouse
	"
	Set a few preferences to reasonable values for using with a mouse
	Preferences forMouse
	"
	self setPreferencesFrom:
	#(
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs false)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 1/2/2015 08:34'!
forTouch
	"
	Set a few preferences to reasonable values for using with a touch screen (or pen device)
	Preferences forTouch
	"
	self setPreferencesFrom:
	#(
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs true)
	)! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 1/2/2015 08:27'!
dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner eventPositionInChild |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sentTo: aMorph localPosition: positionInAMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: (Preferences clickGrabsMorphs ifFalse: [#dragEvent:localPosition:])
						clkSel: (Preferences clickGrabsMorphs ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					self wasHandled: true.
					^self ]]].

	handledByInner ifTrue: [ ^self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 1/2/2015 08:32'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe false)
		(syntaxHighlightingAsYouType true)
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs false)
		
		(allowUnderscoreAssignments true)
		(allowUnderscoreSelectors true)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	).
	self useAllIcons.
	Theme beCurrent.
	Taskbar show! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 1/2/2015 08:32'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoIcons.
	self runningWorld backgroundImageData: nil.
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hide! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 1/2/2015 08:32'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak and Cuis in recent years. Caution: turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating false)
		(subPixelRenderFonts true)
		(thoroughSenders false)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoIcons.
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hide! !

Preferences cuisDefaults!
