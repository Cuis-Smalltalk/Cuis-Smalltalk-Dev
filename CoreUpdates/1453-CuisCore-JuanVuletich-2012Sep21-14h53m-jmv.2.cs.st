'From Cuis 4.0 of 21 April 2012 [latest update: #1450] on 21 September 2012 at 3:18:04 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/21/2012 15:05'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| ownBnds fullBnds |
	ownBnds _ self morphBoundsInWorld.
	fullBnds _ self morphFullBoundsInWorld.
	(savedPatch isNil or: [savedPatch extent ~= fullBnds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBnds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: (fullBnds translatedBy: aCanvas canvasOrigin)
		into: savedPatch.
	savedPatch offset: fullBnds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBnds ]
		ifNotNil: [ aStream nextPut: (fullBnds merge: prevFullBounds)].
	prevBounds _ ownBnds.
	prevFullBounds _ fullBnds! !


!LayoutAdjustingMorph methodsFor: 'events' stamp: 'jmv 9/21/2012 15:06'!
mouseDown: aMouseButtonEvent localPosition: localEventPosition

	super mouseDown: aMouseButtonEvent localPosition: localEventPosition.
	self cursor show.
	hand _ aMouseButtonEvent hand.
	self startStepping! !

!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 9/21/2012 15:17'!
step
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPositionInOwner.
	hand lastEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p ]
		ifFalse: [
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show.
				self stopStepping ]]! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 9/21/2012 15:06'!
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
		(allowUnderscoreAssignments false)
		(allowUnderscoreSelectors false)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 9/21/2012 15:06'!
slowMachine

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
	).
	ClassicTheme beCurrent! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 9/21/2012 15:07'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateMode _ self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [ self assuredNonDisplayCanvas ].

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	handsToDraw reverseDo: [ :h | canvas fullDraw: h ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: allDamage ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [ self forceDamageToScreen: allDamage ]
		ifFalse: [ canvas showAt: aWorld viewBox origin invalidRects: allDamage ].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!methodRemoval: WorldState #drawHand:!
WorldState removeSelector: #drawHand:!
!methodRemoval: WindowEdgeAdjustingMorph #adjustIndicatorAt:!
WindowEdgeAdjustingMorph removeSelector: #adjustIndicatorAt:!
!methodRemoval: WindowEdgeAdjustingMorph #handPoint!
WindowEdgeAdjustingMorph removeSelector: #handPoint!
!methodRemoval: WindowEdgeAdjustingMorph #initialIndicatorBoundsInWorld!
WindowEdgeAdjustingMorph removeSelector: #initialIndicatorBoundsInWorld!
!methodRemoval: Preferences class #fastDragWindowForMorphic!
Preferences class removeSelector: #fastDragWindowForMorphic!
!methodRemoval: LayoutAdjustingMorph #adjustIndicatorAt:!
LayoutAdjustingMorph removeSelector: #adjustIndicatorAt:!
!methodRemoval: LayoutAdjustingMorph #handPoint!
LayoutAdjustingMorph removeSelector: #handPoint!
!methodRemoval: LayoutAdjustingMorph #initialIndicatorBoundsInWorld!
LayoutAdjustingMorph removeSelector: #initialIndicatorBoundsInWorld!
!methodRemoval: HandMorph class #fastDragBorderWidth!
HandMorph class removeSelector: #fastDragBorderWidth!
!classRemoval: #RectangleIndicatorMorph!
Smalltalk removeClassNamed: #RectangleIndicatorMorph!
