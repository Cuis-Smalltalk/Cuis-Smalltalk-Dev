'From Cuis6.0 [latest update: #6118] on 14 December 2023 at 10:37:58 am'!

!Debugger class methodsFor: 'gui support' stamp: 'jmv 12/14/2023 10:17:32'!
debuggerWindowsSatisfying: qualifyingBlock do: anotherBlock
	"For all open debugger windows whose model (the Debugger itself) satisfies aBlock,
	evaluate anotherBlock with the window as argument.
	GUI support."

	DebuggerWindow debuggerWindowsSatisfying: qualifyingBlock do: anotherBlock! !

!Debugger class methodsFor: 'gui support' stamp: 'jmv 12/14/2023 10:34:01'!
updateDebuggersLeadingTo: aContext for: anException
	"Update debugger titles to reflect that anException occured.
	For instance, when stepping over some code, if a new halt is found, reflect it on the window title."

	self debuggerWindowsSatisfying: [ :debugger |
		aContext hasSender: debugger interruptedContext ]
		do: [ :debuggerWindow |
			debuggerWindow setLabel: anException description, ' - ', debuggerWindow label ].! !


!DebuggerWindow class methodsFor: 'services' stamp: 'jmv 12/14/2023 10:28:38'!
debuggerWindowsSatisfying: qualifyingBlock do: anotherBlock
	"For all open debugger windows whose model (the Debugger itself) satisfies qualifyingBlock,
	evaluate anotherBlock with the window as argument.
	GUI support."

	UISupervisor ui ifNotNil: [ :world |
		world submorphsDo: [ :morph |
			morph class == DebuggerWindow ifTrue: [
				(qualifyingBlock value: morph model) ifTrue: [
					anotherBlock value: morph ]]]].! !


!ContextPart methodsFor: 'controlling' stamp: 'jmv 12/14/2023 10:36:46'!
runUntilErrorOrReturnFrom: aSenderContext
	"ASSUMES aSenderContext is a sender of self.  Execute self's stack until aSenderContext returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSenderContext returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSenderContext that jumps back to thisContext when evaluated.  We also insert an exception handler under aSenderContext that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error exceptionHandlerContext ensureContext ctxt here topContext |
	aSenderContext method isQuick ifTrue: [ 
		^ {aSenderContext step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSenderContext"
	error := nil.
	exceptionHandlerContext := ContextPart
		contextOn: UnhandledError do: [ :unhandledError |
			error ifNil: [
				error := unhandledError exception.
				topContext := thisContext.
				Debugger updateDebuggersLeadingTo: aSenderContext for: error.
				unhandledError resumeUnchecked: here jump]
			ifNotNil: [unhandledError pass ]].
	ctxt := aSenderContext insertSender: exceptionHandlerContext.
	ensureContext := ContextPart
		contextEnsure: [error ifNil: [
				topContext := thisContext.
				here jump ]].
	ctxt := ctxt insertSender: ensureContext.
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext := topContext stepToCalleeOrNil].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSenderContext terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].! !


!DebuggerWindow class methodsFor: 'services' stamp: 'jmv 12/14/2023 10:27:15'!
closeAllDebuggers
	"DebuggerWindow closeAllDebuggers"

	UISupervisor ui ifNotNil: [ :world |
		world submorphsDo: [ :morph |
			(morph class == DebuggerWindow or: [ morph class == PreDebugWindow ]) ifTrue: [
				morph delete ]]].! !


!Debugger class reorganize!
('instance creation' informExistingDebugger:label: newDebugging:)
('opening' openContext:label:preDebugMessage: openDebugging:label: openDebugging:to:label: openProcess:context:label:)
('constants' defaultDebugStackSize lowSpaceMessage)
('gui support' debuggerWindowsSatisfying:do: updateDebuggersLeadingTo:for:)
!


!WorldMorph reorganize!
('accessing' handlesKeyboard)
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' mainCanvas recordDamagedRect:for:)
('change reporting' addedMorph: invalidateDisplayRect:for: removedMorph:)
('classification' isWorldMorph)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('drawing' displayWorldOn: drawOn:)
('dropping/grabbing' allowsFilesDrop dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('event handling' mouseButton2Activity wantsWindowEvent:)
('events' click:localPosition: keyStroke: windowEvent:)
('geometry' basicDisplayBounds extentChanged: externalizeBoundsToWorld: externalizeDistanceToWorld: externalizeToWorld: internalizeDistanceFromWorld: internalizeFromWorld: morphPositionInWorld viewBox)
('halos and balloon help' deleteBalloonTarget: wantsHaloHandleWithSelector:inHalo:)
('hands' activeHand hands handsDo: handsReverseDo: handsToDrawForDamage:do:)
('initialization' initialize stepListSortBlock)
('interaction loop' mainLoop millisecondsSinceLastCycle runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand:)
('misc' fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runDeferredUIMessages runLocalStepMethods: startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' topmostWorld world)
('submorphs-accessing' allMorphsDo: firstHand)
('submorphs-add/remove' addMorph:centeredNear:)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel:)
('update cycle' checkForNewScreenSize checkIfUpdateNeeded doOneCycleNow doOneCycleNowOn: doOneCycleOn:delay: doOneMinimalCycleNow whenUIinSafeState:)
('windows' bringWindowsFullOnscreen closeUnchangedWindows collapseAll findATranscript findAWindowSatisfying:orMakeOneUsing:)
('world menu' collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphsDo:)
!

