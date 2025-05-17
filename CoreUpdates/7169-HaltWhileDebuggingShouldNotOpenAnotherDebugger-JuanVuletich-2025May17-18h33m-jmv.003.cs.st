'From Cuis7.3 [latest update: #7166] on 17 May 2025 at 7:04:09 pm'!

!Halt methodsFor: 'description' stamp: 'jmv 5/17/2025 18:40:16'!
noHandler
	"If this preference is set, Halt behaves like any other Exception:
		- following any appropriate exception handler
		- throwing the UnhandledError exception if no handler
	This means that Halt can be masked (hidden) by a handler of UnhandledError.
	This is the traditional behavior, but can be a pain if such handler is present."
	(Preferences at: #haltThrowsUnhandledError) ifTrue: [
		super noHandler.
		^self ].

	"Usually Halt should not be caught by an UnhandledError handler, meaning that a Debugger will open
	even in the presence of a handler of UnhandledError that hides most exceptions."
	UnhandledError defaultActionForException: self! !


!ContextPart methodsFor: 'controlling' stamp: 'jmv 5/17/2025 19:03:11'!
runUntilErrorOrReturnFrom: aSenderContext
	"ASSUMES aSenderContext is a sender of self.  Execute self's stack until aSenderContext returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSenderContext returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSenderContext that jumps back to thisContext when evaluated.  We also insert an exception handler under aSenderContext that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error exceptionHandlerContext ensureContext ctxt here topContext |
	aSenderContext method isQuick ifTrue: [ 
		^ {aSenderContext step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSenderContext.
	As Halt may not raise an UnhandledError (see #noHandler), it may need to be handled explicitly here."
	error := nil.
	exceptionHandlerContext := ContextPart
		contextOn: UnhandledError, Halt do: [ :unhandledErrorOrException |
			error ifNil: [
				topContext := thisContext.
				error := (unhandledErrorOrException isKindOf: UnhandledError)
					ifTrue: [ unhandledErrorOrException exception ]
					ifFalse: [ unhandledErrorOrException ].
				Debugger updateDebuggersLeadingTo: aSenderContext for: error.
				unhandledErrorOrException resumeUnchecked: here jump ]
			ifNotNil: [unhandledErrorOrException pass ]].
	ctxt := aSenderContext insertSender: exceptionHandlerContext.
	ensureContext := ContextPart
		contextEnsure: [
			error ifNil: [
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


!Process methodsFor: 'changing suspended state' stamp: 'jmv 5/17/2025 18:51:23'!
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| home |
	home := aContext home.
	[
		suspendedContext := suspendedContext step.
		home == suspendedContext home or: [ suspendedContext sender isNil or: [suspendedContext sender isDead]]
	] whileFalse:
		[
			self ifSuspendedContextIsUnhandledErrorDo:
				[ :anError |
					anError signalerContext ifNotNil:
						[ :unhandledErrorSignalerContext |
							self completeStepUpTo: unhandledErrorSignalerContext.
							"Give a debugger a chance to update its title to reflect the new exception"
							 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
							^unhandledErrorSignalerContext ]]].
		
	^suspendedContext! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 5/17/2025 18:33:33'!
prefSystem
	^ #(
		#(allowNonLocalReturnsInExceptionHandlers true )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(caseSensitiveFinds false )
		#(changeSetVersionNumbers true )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(warnAndOfferLastUserChanges true )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #cuis #(cuis roots) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(useUserPrefsFile true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
		#(saveReleaseOptionsInWorldMenu false)
	)! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 5/17/2025 18:36:20'!
installMiscPreferences
" Preferences with value defined as a closure. We could insert in a dynamic array...
  Preferences with an event handler.
  Preferences with default value of anything but simple literals."
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 6 * 2 max: 16];

		name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];

		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;

		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];

		name: #backgroundEffect
		description: 'determines how backgroundImage is applied'
		category: #gui
		type: Symbol
		value: #stretch;

		name: #classFinder 
		category: #programming 
		value: [ Smalltalk findAndBrowseClass ];

		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName];

		name: #worldMenu
		category: #system
		value: TheWorldMenu;

		name: #browserWindowClass
		category: #programming
		value: BrowserWindow.

	Preferences
		name: #haltThrowsUnhandledError
		description: 'If set, the Halt exception will be catched by an UnhandledError exception handler.'
		category: #system type: Boolean value: false.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Preferences
		name: #haltThrowsUnhandledError
		description: 'If set, the Halt exception will be catched by an UanhandledError exception handler.'
		category: #system type: Boolean value: false.!

