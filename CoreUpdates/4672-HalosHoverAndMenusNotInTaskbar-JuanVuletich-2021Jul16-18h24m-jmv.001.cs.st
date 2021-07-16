'From Cuis 5.0 [latest update: #4670] on 16 July 2021 at 6:27:13 pm'!

!Morph methodsFor: 'testing' stamp: 'jmv 7/16/2021 17:50:02'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ true! !


!HandMorph methodsFor: 'testing' stamp: 'jmv 7/16/2021 18:19:02'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false! !


!HaloHandleMorph methodsFor: 'testing' stamp: 'jmv 7/16/2021 18:18:32'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false! !


!HaloMorph methodsFor: 'testing' stamp: 'jmv 7/16/2021 18:18:39'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false! !


!HoverHelpMorph methodsFor: 'testing' stamp: 'jmv 7/16/2021 18:19:17'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false! !


!MenuMorph methodsFor: 'testing' stamp: 'jmv 7/16/2021 18:20:02'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 7/16/2021 18:17:58'!
wasOpened: aMorph
	"aMorph was added to the world. Add button for aMorph if appropriate."

	aMorph isIncludedInTaskbar ifTrue: [
		self addButtonFor: aMorph ]! !


!HandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('classification' is:)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:dragSel: waitingForMoreClicks)
('drawing' drawOn: fullDrawHandOn: needsToBeDrawn restoreSavedPatchOn: savePatchFrom:appendDamageTo:)
('event handling' createEventFrom:ofType: flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startDropFilesEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusPrevious mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('geometry' displayBounds displayFullBounds)
('geometry testing' submorphsMightProtrude)
('grabbing/dropping' attachMorph: attachMorphBeside: dropMorph:event: dropMorphs: grabMorph: grabMorph:delta: grabMorph:moveUnderHand:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('testing' isIncludedInTaskbar)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating')
('private' forgetGrabMorphDataFor: grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateDropFilesEvent: generateKeyboardEvent: generateMouseEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!


!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary items itemsDo: label lastItem stayUp stayUp:)
('construction' add:action: add:action:balloonText: add:action:icon: add:action:icon:enabled: add:subMenu: add:target:action: add:target:action:argument: add:target:action:argument:icon: add:target:action:argumentList: add:target:action:icon: add:targetHighlight:action:argumentList: addItemsFromDictionaries: addLine addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:action:argumentList: defaultTarget: labels:lines:selections:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:forHand:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' removeStayUpBox)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('testing' isIncludedInTaskbar)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
('modal progress dialog' displayAt:during: informUserAt:during:)
('halos and balloon help' addHalo:)
!

