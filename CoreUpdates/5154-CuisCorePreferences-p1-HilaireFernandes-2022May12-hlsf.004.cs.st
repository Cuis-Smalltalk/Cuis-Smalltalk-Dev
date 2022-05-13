'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 9:07:39 am'!

!Theme class methodsFor: 'user interface' stamp: 'hlsf 5/12/2022 08:58:17'!
useMenuIcons
	PreferenceNG at: #wantsMenuIcons put: true! !

!Theme class methodsFor: 'user interface' stamp: 'hlsf 5/12/2022 08:58:40'!
useNoMenuIcons
	PreferenceNG at: #wantsMenuIcons put: false! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'hlsf 5/12/2022 09:00:04'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	PreferenceNG at: #wantsMenuIcons put: false.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Cursor class methodsFor: 'accessing' stamp: 'hlsf 5/12/2022 08:31:36'!
defaultCursor
	"Answer the instance of me that is the shape of an arrow slanted left."
	^ (PreferenceNG at: #biggerCursors)
		ifTrue: [ CursorWithAlpha biggerNormal ]
		ifFalse: [ self cursorAt: #normalCursorWithMask ].! !

!Cursor class methodsFor: 'current cursor' stamp: 'hlsf 5/12/2022 08:31:28'!
currentCursor: aCursor 
	"Make the instance of cursor, aCursor, be the current cursor. Display it. 
	Create an error if the argument is not a Cursor."

	CurrentCursor _ aCursor.
	(PreferenceNG at: #biggerCursors)
		ifTrue: [
			[ ^aCursor asBigCursor installCursor] 
				on: Error do: nil  "fall through "].
	aCursor installCursor! !


!Morph methodsFor: 'events' stamp: 'hlsf 5/12/2022 08:36:15'!
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	(PreferenceNG at: #focusFollowsMouse)
		ifTrue: [evt hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ].! !

!Morph methodsFor: 'geometry' stamp: 'hlsf 5/12/2022 08:39:43'!
displayBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ (PreferenceNG at: #haloEnclosesFullBounds)
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 5/12/2022 09:06:30'!
addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	(PreferenceNG at: #haloSpecifications) do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 5/12/2022 08:45:19'!
haloShowsCoordinateSystem

	^PreferenceNG at: #halosShowCoordinateSystem! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 5/12/2022 08:30:10'!
wantsBalloon
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText notNil) and: [PreferenceNG at: #balloonHelpEnabled]! !

!Morph methodsFor: 'halos and balloon help' stamp: 'hlsf 5/12/2022 08:47:56'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].

	(PreferenceNG at: #selectiveHalos) ifFalse: [
		^true ].

	(#(#addDismissHandle: ) includes: aSelector)
		ifTrue: [ ^ self resistsRemoval not ].
	(#(#addDragHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToBrownDragEasily ].
	(#(#addResizeHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily | self okayToScaleEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addScaleHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToScaleEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true! !


!HandMorph methodsFor: 'double click support' stamp: 'hlsf 5/12/2022 08:49:45'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel

	"Wait until the difference between click, or drag gesture is known, then inform the given morph what transpired." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: (PreferenceNG at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'hlsf 5/12/2022 08:49:57'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (PreferenceNG at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'hlsf 5/12/2022 08:50:06'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel
	
	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (PreferenceNG at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'geometry' stamp: 'hlsf 5/12/2022 08:32:17'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware
			ifTrue: [
				(PreferenceNG at: #cacheDisplayContentWhenMovingMorphs)
					ifTrue: [
						"We are caching whatever is in the Display below us. Thefore, there's no need
						to do an invalidation that would trigger the redraw of everything below us."
						self needsRedraw: true ]
					ifFalse:  [
						"No caching of stuff below us. Just invalidate and redraw."
						self redrawNeeded ]]
			ifFalse: [
				lastPosition _ nil.		"Not nil if carrying morphs at that moment"
				prevFullBounds _ nil "Any saved patch is no longer relevant"]].! !

!HandMorph methodsFor: 'private events' stamp: 'hlsf 5/12/2022 08:34:34'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp _ Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	(evtBuf sixth <= 0 or: [ (keyValue _ Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth) isNil ]) ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	Sensor peekEvent ifNotNil: [ :nxt |
		"start: Combining diacritical marks (i.e. accents in the Linux VM)"
		(nxt fourth = EventSensor eventKeyDown and: [ nxt third > 255 ]) ifTrue: [
			keyValue _ ((Character numericValue: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
			Sensor
				nextEvent;
				nextEvent;
				nextEvent ].
		"end: Combining diacritical marks (i.e. accents in the Linux VM)"
		"start: Spurious LF after CR on Ctrl-Enter on Windows VM"
		((evtBuf fourth = EventSensor eventKeyChar and: [ evtBuf third = 13 ]) and: [
			nxt fourth = EventSensor eventKeyChar and: [ nxt third = 10 ]]) ifTrue: [ Sensor nextEvent
			"print " ]].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [
		(keyValue = 9 and: [(modifiers anyMask: 1) and: [Smalltalk platformName = 'unix']])
			ifTrue: [
				"Linux VMs don't generate shift-tab keystroke. Turn #keyUp into #keystroke"
				pressType _ EventSensor eventKeyChar ]
			ifFalse: [type _ #keyUp ]].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [ 											"Control key pressed"
			keyValue < 27 ifTrue: [ 										"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [   									"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [ 			"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96   						"shift not pressed: conver to lowercase letter" ]]]].
			"On Windows, ctrl-backSpace is reported as ctrl-forwardDelete. But keyDown is ok, so we can know and fix."
			(keyValue = 127 and: [ lastKeyDownValue = 8 ])
				ifTrue: [ keyValue _ 8 ].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers _ modifiers bitOr: 8 ]].
		(modifiers anyMask: 8) ifTrue: [ 									"CmdAlt key pressed (or Control key pressed, and #shouldControlEmulateAltFor: just answered true)"
			(modifiers anyMask: 1) ifTrue: [								"Shift pressed"
				| i |
				"It seems that for ctrl-shifted keys and cmd-shifted keys, the VM incorrectly reports the UNSHIFTED character.
				Correct this, at least for common cmd-shortcuts, and for the US keyboard... Sigh...
				(This has only been observed on Mac VMs, but seems harmless if proper shifted character is reported (as in Linux), as this wil be NOP)
				(On Windows, the situation is even worse: ctrl-{ is not even reported as a keystroke event. Only keyDown and keyUp.)"
				"#($' $, $. $9 $0 $[ $]) -> #($'' $< $> $( $) ${) $}"
				i _ #[39 44 46 57 48 91 93 ] indexOf: keyValue.
				i > 0 ifTrue: [
					keyValue _ #[34 60 62 40 41 123 125] at: i ]]]].
	buttons _ modifiers bitShift: 3.
	"Linux and Windows VM send keyboard ctrl-upArrow and ctrl-downArrow when the user tries to scroll using the mouse wheel
	Mac VM sends cmd-option-ctrl-shift-upArrow and cmd-option-ctrl-shift-downArrow for trackpad vertical scroll gestures,
		and cmd-option-ctrl-shift-leftArrow and cmd-option-ctrl-shift-rightArrow for horizontal scroll gestures.
	This way of reporting scroll events by the VM also enables scrolling using the keyboard (actually, we can't tell if user gesture was on Mouse, Trackpad or Keyboard).
	But ctrl-shift and cmdAlt-shift are needed used for selecting while moving by word, line, etc.
	Additionally, #ctrlArrowsScrollHorizontally allows chosing between keyboard horizontal scroll and moving word by word in text editors."
	mouseScrollDirection _ nil.
	"Ctrl for Keyboard or Mouse wheel gestures. All modifiers for Trackpad gestures."
	(buttons = InputSensor controlKey or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 30
			ifTrue: [mouseScrollDirection _ #up]
		ifFalse: [keyValue = 31
			ifTrue: [mouseScrollDirection _ #down]]].
	"Ctrl for Keyboard or Mouse wheel gestures, only if preference is set. All modifiers for Trackpad gestures."
	((buttons = InputSensor controlKey and: [PreferenceNG at: #ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 28
			ifTrue: [mouseScrollDirection _ #left]
		ifFalse: [keyValue = 29
			ifTrue: [mouseScrollDirection _ #right]]].
	mouseScrollDirection ifNotNil: [
		^ MouseScrollEvent new
			setType: #mouseScroll
			position: self morphPosition
			direction: mouseScrollDirection
			buttons: buttons
			hand: self
			stamp: stamp ].
	^ KeyboardEvent new
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !


!WorldMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 08:32:25'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	Preferences cacheDisplayContentWhenMovingMorphs ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage _ canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(PreferenceNG at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !


!WindowEdgeAdjustingMorph methodsFor: 'adjusting' stamp: 'hlsf 5/12/2022 08:33:10'!
adjustOwnerAt: aGlobalPoint millisecondSinceLast: millisecondSinceLast

	self basicAdjustOwnerAt: aGlobalPoint.

	"If UI is becoming slow or is optimized for slow systems, resize without
	showing window contents, but only edges. But don't do it for rotated Windows!!"
	(owner isOrAnyOwnerIsRotated not and: [
		(PreferenceNG at: #cheapWindowReframe) or: [millisecondSinceLast > 200]]) ifTrue: [
			owner displayBounds newRectFrom: [ :f |
				self basicAdjustOwnerAt: Sensor mousePoint.
				owner morphPosition extent: owner morphExtentInWorld ]].! !


!StringRequestMorph methodsFor: 'private' stamp: 'hlsf 5/12/2022 08:36:26'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w _ self world.
	w isNil ifTrue: [^ response asString].
	done _ false.
	canceled _ false.
	(PreferenceNG at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock _ [:aString| done _ true].
	cancelBlock _ [done _ true. canceled _ true].
	delay _ Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'hlsf 5/12/2022 08:36:31'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	| answer |
	answer _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	(PreferenceNG at: #focusFollowsMouse) ifFalse: [answer textBox focusText].
	^ answer! !


!PluggableScrollPane methodsFor: 'initialization' stamp: 'hlsf 5/12/2022 08:35:19'!
initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ #showIfNeeded.

	"initialize the receiver's scrollBars"
	scrollBar _ ScrollBar new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ ScrollBar new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ PreferenceNG at: #drawKeyboardFocusIndicator.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.
	self updateScrollBarsBounds.
	self innerMorphClass ifNotNil: [ :contentsClass |
		self scroller: contentsClass new ].! !


!HierarchicalListMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 08:38:18'!
drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: (PreferenceNG at: #focusIndicatorWidth)
			color: Theme current focusIndicator ]! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'hlsf 5/12/2022 08:36:37'!
mouseEnter: event
	super mouseEnter: event.
	(PreferenceNG at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !


!PluggableListMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 08:38:23'!
drawOn: aCanvas
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: (PreferenceNG at: #focusIndicatorWidth)
			color: Theme current focusIndicator ].! !

!PluggableListMorph methodsFor: 'events' stamp: 'hlsf 5/12/2022 08:36:41'!
mouseEnter: event
	super mouseEnter: event.
	(PreferenceNG at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: self ]! !


!TextModelMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 08:38:32'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ PreferenceNG at: #focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan ]
			ifFalse: [
				scroller hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						scroller hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ scroller hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc _ Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc _ bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].! !

!TextModelMorph methodsFor: 'events' stamp: 'hlsf 5/12/2022 08:36:45'!
mouseEnter: event
	super mouseEnter: event.
	(PreferenceNG at: #focusFollowsMouse)
		ifTrue: [ event hand newKeyboardFocus: scroller ]! !


!SystemWindow methodsFor: 'menu' stamp: 'hlsf 5/12/2022 08:53:35'!
addTileResizerMenuTo: aMenu
	"We can look at preferences here to decide what too do"
	(PreferenceNG at: #tileResizerInWindowMenu) ifFalse: [
		aMenu add: 'resize full' 		action: #resizeFull icon: #resizeFullIcon;
		add: 'resize top' 				action: #resizeTop icon: #resizeTopIcon;
		add: 'resize left' 				action: #resizeLeft icon: #resizeLeftIcon;
		add: 'resize bottom' 			action: #resizeBottom icon: #resizeBottomIcon;
		add: 'resize right' 				action: #resizeRight icon: #resizeRightIcon;
		add: 'resize top left' 			action: #resizeTopLeft icon: #resizeTopLeftIcon;
		add: 'resize top right' 		action: #resizeTopRight icon: #resizeTopRightIcon;
		add: 'resize bottom left' 		action: #resizeBottomLeft icon: #resizeBottomLeftIcon;
		add: 'resize bottom right' 	action: #resizeBottomRight icon: #resizeBottomRightIcon]
	ifTrue: [ |resizeMorph|
		"Use embedded resize morph"
		resizeMorph _ TileResizeMorph new
							selectionColor: (self widgetsColor adjustSaturation: -0.2 brightness: 0.25) ;
							action: [:resize | |resizeMsg|
								resizeMsg _ ('resize', resize asPlainString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].
	^aMenu.! !


!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 08:46:47'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 08:46:54'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code _ self buildMorphicCodePane.
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons _ LayoutMorph newColumn.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 08:47:00'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 08:47:05'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		h _ self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MenuMorph methodsFor: 'construction' stamp: 'hlsf 5/12/2022 08:55:25'!
addItemsFromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target.
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item wantsIcons |
	wantsIcons _ PreferenceNG at: #wantsMenuIcons.
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ self addLine ]
			ifNotNil: [ | realTarget |
				realTarget _ dict at: #object ifAbsent: [defaultTarget].
				realTarget isSymbol ifTrue: [ realTarget _ defaultTarget perform: realTarget ].
				item _ (dict at: #label) isSymbol
					ifTrue: [
						self
							addUpdating: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						self
							add: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				wantsIcons ifTrue: [
					dict
						at: #icon
						ifPresent: [ :symbolOrFormOrNil |
							item setIcon: symbolOrFormOrNil ]].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ].
			]]! !

!MenuMorph methodsFor: 'construction' stamp: 'hlsf 5/12/2022 08:47:16'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	(PreferenceNG at: #optionalButtons) ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW _ Theme current titleBarButtonsExtent x.
	closeButton _ PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; color: `Color transparent`.
	pinButton _ PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; color: `Color transparent`.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW);
			color: `Color transparent`;
			addMorph: closeButton fixedWidth: buttonHW;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (BoxedMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'control' stamp: 'hlsf 5/12/2022 08:46:05'!
popUpAt: aPoint forHand: hand in: aWorld
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self 
		popUpAt: aPoint 
		forHand: hand 
		in: aWorld 
		allowKeyboard: (PreferenceNG at: #menuKeyboardControl)! !

!MenuMorph methodsFor: 'modal control' stamp: 'hlsf 5/12/2022 08:45:49'!
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: (PreferenceNG at: #menuKeyboardControl)

	"
	| menu sub entry |
	menu _ MenuMorph new.
	1 to: 3 do: [:i |
		entry _ 'Line', i printString.
		sub _ MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				action: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
	"! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'hlsf 5/12/2022 08:33:59'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph ].

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^ #rejected ].

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
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [ aMorph fullIncludesPixel: position ] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: (Preferences clickGrabsMorphs ifFalse: [#dragEvent:localPosition:])
						clkSel: (Preferences clickGrabsMorphs ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					(PreferenceNG at: #clickGrabsMorphs) ifFalse: [
						self shiftPressed
							ifTrue: [ aMorph is: #SystemWindow :: ifTrue: [ aMorph sendToBack ] ]
							ifFalse: [ aMorph activateWindow ] ].
					self wasHandled: true.
					^ self ]]].

	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'hlsf 5/12/2022 08:33:20'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"
	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			(PreferenceNG at: #cheapWindowReframe) and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !


!Theme methodsFor: 'other options' stamp: 'hlsf 5/12/2022 08:39:10'!
fullScreenDeskMargin
	^ (PreferenceNG at: #fullScreenLeavesDeskMargins) ifTrue: [22] ifFalse: [0]! !


!Theme class methodsFor: 'user interface' stamp: 'hlsf 5/12/2022 08:57:48'!
changeIcons

	(MenuMorph new defaultTarget: self)
		addTitle: 'Menu Icons';
		addStayUpIcons;
		add: 'Use icons for menu entries' action: #useMenuIcons;
		add: 'Don''t use icons for menu entries' action: #useNoMenuIcons;
		popUpInWorld: self runningWorld! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 5/12/2022 08:46:18'!
startUpWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (PreferenceNG at: #menuKeyboardControl)! !

!methodRemoval: TileResizeMorph class #initialize stamp: 'hlsf 5/12/2022 08:54:37'!
TileResizeMorph class removeSelector: #initialize!
!methodRemoval: HaloMorph class #initialize stamp: 'hlsf 5/12/2022 08:44:41'!
HaloMorph class removeSelector: #initialize!
HaloMorph initialize!
TileResizeMorph initialize!
