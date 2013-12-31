'From Cuis 4.2 of 25 July 2013 [latest update: #1910] on 29 December 2013 at 1:20:54.635615 pm'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds morphInHandOrResizing '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 12/30/2013 22:29'!
cheapWindowReframe
	"
	Preferences setPreferencesFrom: {{  #cheapWindowReframe. true}}
	"
	^ self
		valueOfFlag: #cheapWindowReframe
		ifAbsent: [ false ].! !

!Morph methodsFor: 'drawing' stamp: 'jmv 12/29/2013 13:13'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"
	"Draw receiver itself"
	| h |
	h _ self world ifNotNil: [ :w | w activeHand ].
	(owner == h and: [ Preferences cheapWindowReframe ])
		ifTrue: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
				aCanvas
					reverseRectangleBorder: (0 @ 0 extent: self morphExtent)
					borderWidth: 2 ]]
		ifFalse: [
			(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [ self drawOn: aCanvas ].
			self drawSubmorphsOn: aCanvas ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/29/2013 13:13'!
fullDrawOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	Preferences cheapWindowReframe ifFalse: [
		"Note: We use a shadow form here to prevent drawing
		overlapping morphs multiple times using the transparent
		shadow color."
		shadowForm _ self submorphsShadowForm.

		"draw shadows"
		aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5) ].
	
	"draw morphs in front of shadows"
	self drawSubmorphsOn: aCanvas.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 1/27/2013 00:58'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| dropEvent |
	dropEvent _ DropEvent new setPosition: self morphPosition contents: aMorph hand: self.
	owner dispatchEvent: dropEvent localPosition: dropEvent eventPosition.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 2/23/2013 21:57'!
grabMorph: aMorph from: formerOwner at: aPoint
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	self releaseMouseFocus. "Break focus"
	self addMorphBack: aMorph.
	aMorph morphPosition: aPoint.
	aMorph justGrabbedFrom: formerOwner.! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 22:30'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 12/28/2013 22:51'!
mouseEnter: anEvent
	super mouseEnter: anEvent.
	self cursor show.
	hand _ anEvent hand! !

!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 12/28/2013 22:37'!
mouseLeave: anEvent

	super mouseLeave: anEvent.
	hand ifNotNil: [
		hand _ nil.
		Cursor normal show ]! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 12/29/2013 13:13'!
step
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			Preferences cheapWindowReframe ifTrue: [
				owner morphBoundsInWorld newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphBoundsInWorld ]].
			]
		ifFalse: [
			owner redrawNeeded.
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show ]]! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 12/29/2013 13:13'!
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
		(simpleMenus false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(allowUnderscoreAssignments true)
		(allowUnderscoreSelectors true)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
		(cheapWindowReframe false)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 12/29/2013 13:14'!
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
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(simpleMenus false)
		(smartUpdating false)
		(subPixelRenderFonts false)
		(thoroughSenders false)
		(cheapWindowReframe true)
	).
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 12/29/2013 13:15'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak in recent years. Caution: this theme removes the standard Squeak flaps, turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

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
		(simpleMenus false)
		(smartUpdating false)
		(thoroughSenders false)
		(cheapWindowReframe true)
	)! !

!methodRemoval: WorldState #isUpdateCycleSlow!
WorldState removeSelector: #isUpdateCycleSlow!
!methodRemoval: HandMorph #morphInHandOrResizing!
HandMorph removeSelector: #morphInHandOrResizing!
!methodRemoval: HandMorph #morphInHandOrResizing:!
HandMorph removeSelector: #morphInHandOrResizing:!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
RectangleLikeMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent damageRecorder hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!methodRemoval: PasteUpMorph #isUpdateCycleSlow!
PasteUpMorph removeSelector: #isUpdateCycleSlow!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences cuisDefaults!

