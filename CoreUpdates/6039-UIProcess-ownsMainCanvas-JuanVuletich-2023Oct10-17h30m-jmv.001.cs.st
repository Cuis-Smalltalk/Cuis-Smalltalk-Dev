'From Cuis 6.0 [latest update: #6039] on 10 October 2023 at 5:31:42 pm'!

!Morph methodsFor: 'accessing' stamp: 'jmv 10/10/2023 16:45:52'!
mainBoundsFinderCanvas
	"Might answer nil if not in a world!!"

	^ self mainCanvas ifNotNil: [ :c | c boundsFinderCanvas ]! !

!Morph methodsFor: 'accessing' stamp: 'jmv 10/10/2023 16:45:14'!
mainCanvas
	"Might answer nil if not in a world!!"

	^ self topmostWorld ifNotNil: [ :w | w mainCanvas ].! !


!WorldMorph methodsFor: 'canvas' stamp: 'jmv 10/10/2023 16:44:13'!
mainCanvas
	"Answer the main canvas that was last set"

	^ canvas! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/10/2023 16:29:31'!
displayWorldOn: aMorphicCanvas
	"Update this world's display on the argument."

	| allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"Restore world drawing under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: aMorphicCanvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	aMorphicCanvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage := aMorphicCanvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	aMorphicCanvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage := (h savePatchFrom: aMorphicCanvas) quickMerge: allDamage ]
			ifFalse: [ allDamage := h displayFullBoundsForPatch quickMerge: allDamage ].
		aMorphicCanvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	aMorphicCanvas drawsOnDisplay ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			aMorphicCanvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/10/2023 16:41:37'!
doOneCycleNowOn: aMorphicCanvas
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	lastCycleTime := Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldOn: aMorphicCanvas.
	"Run steps, alarms and deferred UI messages."
	self runDeferredUIMessages.
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.
	"Ensure that any affected morph is redrawn ASAP."
	self displayWorldOn: aMorphicCanvas.
	"Process user input events. Run all event triggered code."
	hadAny := false.
	self handsDo: [ :h |
		activeHand := h.
		hadAny := hadAny | h processEventQueue ].
	"The default is the primary hand"
	activeHand := self hands first.
	"Run any deferred UI messages that were enqueued during this cycle, perhaps
	as consequence of event handling or step methods, or sent from other processes,
	before the inter cycle pause."
	self runDeferredUIMessages.
	"Ensure that any affected morph is redrawn ASAP, before the inter cycle pause."
	self displayWorldOn: aMorphicCanvas.
	lastCycleHadAnyEvent := hadAny.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/10/2023 16:36:31'!
doOneCycleOn: aMorphicCanvas delay: delay
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	self doOneCycleNowOn: aMorphicCanvas.

	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause := 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait := 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause := 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause := pause * 21//20 ] ].
			waitUntil := lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil := waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil := waitUntil min: alarms first scheduledTime ]].
			wait := waitUntil - Time localMillisecondClock max: 0 ].
	(Preferences at: #serverMode)
		ifTrue: [ wait := wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			delay setDelay: wait; wait ].! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/10/2023 16:46:36'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Note: Answers an integer rectangle"

	| prevNotVisibleFlag w c answer prevOwner |
	(owner notNil and: [owner isWorldMorph]) ifTrue: [
		w := owner ]
	ifFalse: [
		w := UISupervisor ui ].
	c := w mainBoundsFinderCanvas.
	"Hack owner so #displayBoundsSetFrom: will update privateDisplayBounds"
	prevOwner := owner.
	owner := w.
	prevNotVisibleFlag := self privateFlagAt: 3.
	self privateFlagAt: 3 put: false.
	c fullUpdateProtrudingBounds: self.
	answer := self displayFullBounds.
	"Reset owner and privateDisplayBounds (if needed) so no one finds out what we've just done!!"
	prevOwner == owner ifFalse: [
		owner := prevOwner.
		self world = w ifTrue: [
			self privateFlagAt: 3 put: false.
			self allOwnersReverseDo: [ :m | c into: m ].
			c fullUpdateProtrudingBounds: self.
			self allOwnersDo: [ :m | c outOfMorph ]]].
	self privateFlagAt: 3 put: prevNotVisibleFlag.
	^answer! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/10/2023 16:49:24'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us (or a submorph of ours), even if we are not visible a it
	because of some other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- BoxedMorphs. No special care needed. (Senders in the base Cuis image are of this kind).
		- Morphs drawn by VectorCanvas (i.e. they #requiresVectorCanvas). See notes below.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).
	See #bitMask.

	See #ownsPixel:
	See #includesPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self mainCanvas ifNotNil: [ :mainCanvas |
			mainCanvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]]].

	"If we don't know."
	^ nil.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/10/2023 16:49:33'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxedMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxedMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by VectorCanvas,
	either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].

	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/10/2023 16:49:44'!
ownsPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics, as we need VectorCanvas' morphIdAt: service.

	Valid if running on VectorCanvas.

	In case of running on HybridCanvas, this method is only valid for morphs that
	are drawn by VectorCanvas (i.e. they answer true to #requiresVectorCanvas).

	See #coversPixel:
	See #includesPixel:"

	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].
	^ false.! !


!BoxedMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/10/2023 16:49:51'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self mainCanvas usesVectorEnginePlugin.! !

!BoxedMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/10/2023 16:49:54'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self mainCanvas usesVectorEnginePlugin.! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/10/2023 16:46:46'!
basicDisplayBounds

	^ self mainBoundsFinderCanvas displayBoundsForHand: self! !


!WorldMorph methodsFor: 'caching' stamp: 'jmv 10/10/2023 17:07:28'!
releaseCachedState
	super releaseCachedState.
	self cleanseStepList.
	canvas := nil.
	damageRecorder := nil.
	hands do: [ :h | h releaseCachedState ].! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/10/2023 17:10:15'!
mainLoop

	| morphicCanvas d |
        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	self morphPosition: `0@0` extent: Display extent.
	morphicCanvas := (Display deferUpdates: true)
		ifNotNil: [ Display getMainCanvas ]
		ifNil: [ (Form extent: extent depth: Display depth) getCanvas ].
	morphicCanvas world: self.
	"Set this as the main canvas"
	canvas := morphicCanvas.
	damageRecorder := DamageRecorder new.
	damageRecorder recordInvalidRect: self morphLocalBounds for: nil.
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	d := Delay forMilliseconds: 50.
	[
		self displayWorldOn: morphicCanvas.
		[
			self doOneCycleOn: morphicCanvas delay: d.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				(context receiver is: #Morph) and: [ context  selector = #drawOn: ]])
			ifNotNil: [ :context |
				context receiver drawingFails.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/10/2023 17:07:57'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e d p |
	DisplayScreen isDisplayExtentOk ifFalse: [
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := Display extent.
			d := DisplayScreen hostOSWindowExtent * ScaleFactor - DisplayScreen actualScreenSize.
			p := DisplayScreen hostOSWindowPosition.
			DisplayScreen hostOSWindowExtent: e+d.
			DisplayScreen hostOSWindowPosition: p.
		].
		DisplayScreen setupDisplay.
		UISupervisor newUIProcess.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/10/2023 16:44:27'!
doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldOn: self mainCanvas.

	"Process user input events. Run all event triggered code."
	^activeHand processEventQueue! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/10/2023 16:32:20'!
collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse ].
	self collapseNonWindows.
	self redrawNeeded.! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 10/10/2023 16:44:30'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: true.
		self world ifNotNil: [ :w | w displayWorldOn: w mainCanvas ].  "show myself"
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !


!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 10/10/2023 16:44:34'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		w displayWorldOn: w mainCanvas.  "show myself"
		].
	aBlock value.
	self delete! !

!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 10/10/2023 16:44:38'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString := titleMorph submorphs first.
	self visible: false.
	w := self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorldOn: w mainCanvas	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorldOn: w mainCanvas ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/10/2023 16:58:52'!
displayBoundsSetOnCurrentMorph
	"At time of last draw on world main canvas. nil if unknown.
		(i.e. privateDisplayBounds is not to represent morph bounds in any other context).
	Clipped appropiately if some owner clips currentMorph.
	See also #displayBoundsUpdateOn:"

	currentMorph mainCanvas ifNotNil: [ :c |
		(self isBoundsFinderCanvas or: [c canvasToUse == self canvasToUse]) ifTrue: [
			currentMorph privateDisplayBounds: self canvasToUse boundingRectOfCurrentMorphAfterDraw ]].! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/10/2023 16:59:06'!
displayBoundsUpdateOnCurrentMorph
	"At time of last draw on world main canvas. nil if unknown.
	Only to be used for the world main canvas
		(i.e. privateDisplayBounds is not to represent morph bounds in any other context).
	Rectangle is clipped appropiately if some owner clips currentMorph.
	To be called after #postDrawOn:. See also #displayBoundsSetOn:
	This more complicated update is needed because the contour information from #drawOn: was lost
	when drawing submorphs. So, the contour from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	currentMorph mainCanvas ifNotNil: [ :c |
		(self isBoundsFinderCanvas or: [c canvasToUse == self canvasToUse]) ifTrue: [
			currentMorph privateDisplayBounds: 
				(currentMorph privateDisplayBounds quickMerge:
					self canvasToUse boundingRectOfCurrentMorphAfterDraw) ]].! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 10/10/2023 16:52:19'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"
	aMorph visible ifFalse: [^ self].

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			(Preferences at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self canvasToUse drawCurrentAsOutline ifTrue: [
			self outOfMorph.
			^ self]].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !


!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/10/2023 16:59:14'!
contourSetOnCurrentMorph
	"Set contour.
	To be called immediately after drawing currentMorph."

	currentMorph mainCanvas ifNotNil: [ :c |
		c canvasToUse == self ifTrue: [
			currentMorph wantsContour
				ifTrue: [
					self
						contourOfCurrentMorphAfterDrawInto: (currentMorph valueOfProperty: #contour) 
						into: [ :contourArray :contourY0 :contourY1 |
							currentMorph setProperty: #contour toValue: contourArray.
							currentMorph setProperty: #contourY0 toValue: contourY0.
							currentMorph setProperty: #contourY1 toValue: contourY1 ]]
				ifFalse: [
					self contourClearOnCurrentMorph ]]].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/10/2023 16:59:17'!
contourUpdateOnAllOwners
	"Update contour on all owners of currentMorph.
	To also include the effect of #postDrawOn: and submorphs."

	currentMorph mainCanvas ifNotNil: [ :c |
		c canvasToUse = self ifTrue: [
			1 to: cti-1 do: [ :i |
				(drawingMorphStack at: i) ifNotNil: [ :m |
					m wantsContour ifTrue: [
						self
							contourOfCurrentMorphAfterDrawUpdate: (m valueOfProperty: #contour)
							oldTop: (m valueOfProperty: #contourY0)
							oldBottom: (m valueOfProperty: #contourY1)
							into: [ :contourArray :contourY0 :contourY1 |
								m setProperty: #contour toValue: contourArray.
								m setProperty: #contourY0 toValue: contourY0.
								m setProperty: #contourY1 toValue: contourY1 ]]]]]].! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 10/10/2023 16:59:22'!
contourUpdateOnCurrentMorph
	"Update contour.
	To also include the effect of #postDrawOn: and submorphs."

	currentMorph mainCanvas ifNotNil: [ :c |
		c canvasToUse == self ifTrue: [
			currentMorph wantsContour
				ifTrue: [
					self
						contourOfCurrentMorphAfterDrawUpdate: (currentMorph valueOfProperty: #contour)
						oldTop: (currentMorph valueOfProperty: #contourY0)
						oldBottom: (currentMorph valueOfProperty: #contourY1)
						into: [ :contourArray :contourY0 :contourY1 |
							currentMorph setProperty: #contour toValue: contourArray.
							currentMorph setProperty: #contourY0 toValue: contourY0.
							currentMorph setProperty: #contourY1 toValue: contourY1 ]]]].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| activeProc guiRoot |
(nil confirm: 'After this update,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates] again.') ifFalse: [ self halt ].
[
	ChangeSet installing: '6039-UIProcess-ownsMainCanvas-JuanVuletich-2023Oct10-17h30m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '6039') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Installed ChangeSet: 6039-UIProcess-ownsMainCanvas-JuanVuletich-2023Oct10-17h30m-jmv.001.cs.st' print.
	'Please do World / Changes... / [Install New Updates] again.' print.
] forkAt: 39.
UISupervisor newUIProcess.
Processor activeProcess terminate.
!

