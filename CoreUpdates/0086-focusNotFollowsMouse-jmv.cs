'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 7 November 2008 at 3:25:52 pm'!!LightWidget methodsFor: 'dropping/grabbing' stamp: 'jmv 11/7/2008 13:49'!justDroppedInto: aMorph event: anEvent	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"	self formerOwner: nil.	self formerPosition: nil.	aMorph activateWindow.	(self isInWorld ) ifTrue: [		self world startSteppingSubmorphsOf: self].	"Note an unhappy inefficiency here:  the startStepping... call will often have already been called in the sequence leading up to entry to this method, but unfortunately the isPartsDonor: call often will not have already happened, with the result that the startStepping... call will not have resulted in the startage of the steppage."! !!LightWidget methodsFor: 'events-processing' stamp: 'jmv 11/7/2008 13:50'!handleMouseDown: anEvent	"System level event handling."	anEvent wasHandled ifTrue:[^self]. "not interested"	anEvent hand removePendingBalloonFor: self.	anEvent wasHandled: true.	self activateWindow.	(anEvent controlKeyPressed			and: [Preferences cmdGesturesEnabled])		ifTrue: [^ self invokeMetaMenu: anEvent].	"Make me modal during mouse transitions"	anEvent hand newMouseFocus: self event: anEvent.	anEvent blueButtonChanged ifTrue:[^self blueButtonDown: anEvent].	self mouseDown: anEvent.	anEvent hand removeHaloFromClick: anEvent on: self.	(self handlesMouseStillDown: anEvent) ifTrue:[		self startStepping: #handleMouseStillDown: 			at: Time millisecondClockValue + self mouseStillDownThreshold			arguments: {anEvent copy resetHandlerFields}			stepTime: self mouseStillDownStepRate ].! !!LightWidget methodsFor: 'user interface' stamp: 'jmv 11/7/2008 13:48'!activateWindow	| w |	(w _ self ownerThatIsA: OldSystemWindow) ifNotNil: [		w isTopWindow ifFalse: [			w activate]].! !!OldMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 11/7/2008 13:49'!justDroppedInto: aMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	self formerOwner: nil.
	self formerPosition: nil.	aMorph activateWindow.	(self isInWorld ) ifTrue: [		self world startSteppingSubmorphsOf: self].
	"Note an unhappy inefficiency here:  the startStepping... call will often have already been called in the sequence leading up to entry to this method, but unfortunately the isPartsDonor: call often will not have already happened, with the result that the startStepping... call will not have resulted in the startage of the steppage."! !!OldMorph methodsFor: 'event handling' stamp: 'jmv 11/7/2008 13:51'!handleMouseDown: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue:[^self]. "not interested"
	anEvent hand removePendingBalloonFor: self.
	anEvent wasHandled: true.	self activateWindow.

	(anEvent controlKeyPressed
			and: [Preferences cmdGesturesEnabled])
		ifTrue: [^ self invokeMetaMenu: anEvent].

	"Make me modal during mouse transitions"
	anEvent hand newMouseFocus: self event: anEvent.
	anEvent blueButtonChanged ifTrue:[^self blueButtonDown: anEvent].

	self mouseDown: anEvent.
	anEvent hand removeHaloFromClick: anEvent on: self.

	(self handlesMouseStillDown: anEvent) ifTrue:[
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {anEvent copy resetHandlerFields}
			stepTime: self mouseStillDownStepRate ].
! !!OldMorph methodsFor: 'event handling' stamp: 'jmv 11/7/2008 13:51'!mouseStillDownStepRate	"At what rate do I want to receive #mouseStillDown: notifications?"	^1! !!OldMorph methodsFor: 'user interface' stamp: 'jmv 11/7/2008 13:48'!activateWindow	| w |	(w _ self ownerThatIsA: OldSystemWindow) ifNotNil: [		w isTopWindow ifFalse: [			w activate]].! !!OldHandMorph methodsFor: 'private events' stamp: 'jmv 11/7/2008 14:09'!sendKeyboardEvent: anEvent	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."		| w |	keyboardFocus ifNotNil: [		(w _ keyboardFocus world) ifNil: [			keyboardFocus _ nil.			^self ].		^self sendFocusEvent: anEvent to: keyboardFocus in: w]."Si no hay foco de teclado no hacer nada.En realidad, si el world o la hand quiere los teclazos esta bien. Lo que no hay que hacer es buscar a quien mandarselos""	^self sendEvent: anEvent"! !!OldLazyListMorph methodsFor: 'private' stamp: 'jmv 11/7/2008 15:14'!noSelection	selectedRow := nil.	selectedRows := PluggableSet integerSet! !!OldPluggableListMorph methodsFor: 'events' stamp: 'jmv 11/7/2008 15:16'!mouseDown: evt	| selectors row |	ActiveHand keyboardFocus == self ifFalse: [		evt hand newKeyboardFocus: self.		"If we are focusing, deselect, so that later selection doesn't result in deselect."		self listMorph noSelection].	evt yellowButtonPressed  "First check for option (menu) click"		ifTrue: [^ self yellowButtonActivity: evt shiftPressed].	row _ self rowAtLocation: evt position.	row = 0  ifTrue: [^super mouseDown: evt].	"self dragEnabled ifTrue: [aMorph highlightForMouseDown]."	selectors _ Array 		with: #click:		with: (doubleClickSelector ifNotNil:[#doubleClick:])		with: nil		with: (self dragEnabled ifTrue:[#startDrag:] ifFalse:[nil]).	evt hand waitForClicksOrDrag: self event: evt selectors: selectors threshold: 10 "pixels".! !!OldPluggableTextMorph methodsFor: 'event handling' stamp: 'jmv 11/7/2008 14:06'!mouseEnter: event	super mouseEnter: event.	selectionInterval ifNotNil:		[textMorph editor selectInterval: selectionInterval; setEmphasisHere].	textMorph selectionChanged! !!OldPluggableTextMorph methodsFor: 'event handling' stamp: 'jmv 11/7/2008 14:18'!mouseLeave: event	"The mouse has left the area of the receiver"	textMorph ifNotNil: [selectionInterval _ textMorph editor selectionInterval].	super mouseLeave: event! !!OldSimpleHierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 11/7/2008 14:26'!mouseDown: evt	| aMorph selectors |	evt hand newKeyboardFocus: self.	aMorph _ self itemFromPoint: evt position.	(aMorph notNil and:[aMorph inToggleArea: (aMorph point: evt position from: self)])		ifTrue:[^self toggleExpandedState: aMorph event: evt]. 	evt yellowButtonPressed  "First check for option (menu) click"		ifTrue: [^ self yellowButtonActivity: evt shiftPressed].	aMorph ifNil:[^super mouseDown: evt].	aMorph highlightForMouseDown.	selectors _ Array 		with: #click:		with: nil		with: nil		with: (self dragEnabled ifTrue:[#startDrag:] ifFalse:[nil]).	evt hand waitForClicksOrDrag: self event: evt selectors: selectors threshold: 10 "pixels".! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/7/2008 12:36'!handleListenEvent: evt	evt isMouse ifFalse: [^self].	evt hand hasSubmorphs ifTrue: [^self]. "still dragging"	evt hand removeMouseListener: self.! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/7/2008 12:54'!handlesMouseDown: evt 	"If I am not the topWindow, then I will only respond to dragging by the title bar.	Any other click will only bring me to the top"	(self labelRect containsPoint: evt cursorPoint)		ifTrue: [^ true].	^ false! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/7/2008 13:55'!mouseDown: evt	"If we get #mouseDown:, essentially it means click on the label area. Activate self."	self setProperty: #clickPoint toValue: evt cursorPoint.	TopWindow == self ifFalse: [		evt hand releaseKeyboardFocus.		self activate]! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/7/2008 12:44'!mouseEnterDragging: evt	(self ~~ TopWindow and:[evt hand hasSubmorphs]) ifTrue:[		evt hand addMouseListener: self. "for drop completion on submorph"	].! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/7/2008 12:36'!mouseLeaveDragging: evt	"lock children after drop operations"		(self ~~ TopWindow and: [evt hand hasSubmorphs]) ifTrue:[		evt hand removeMouseListener: self].! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/7/2008 13:28'!mouseMove: evt	"Handle a mouse-move event"	| cp |	cp _ evt cursorPoint.	self valueOfProperty: #clickPoint ifPresentDo: [:firstClick |		((self labelRect containsPoint: firstClick) and: [(cp dist: firstClick) > 3]) ifTrue: [			"If this is a drag that started in the title bar, then pick me up"			^ self isSticky ifFalse: [				self fastFramingOn 					ifTrue: [self doFastFrameDrag: firstClick]					ifFalse: [evt hand grabMorph: self]]]]! !!OldSystemWindow methodsFor: 'label' stamp: 'jmv 11/7/2008 13:21'!setStripeColorsFrom: paneColor 	"Set the stripe color based on the given paneColor"	labelArea		ifNotNil: [			labelArea				color: (Preferences alternativeWindowLook						ifTrue: [paneColor lighter]						ifFalse: [Color transparent])].		self		updateBoxesColor: (self isTopWindow				ifTrue: [paneColor]				ifFalse: [paneColor muchDarker]).		stripes		ifNil: [^ self].		Preferences alternativeWindowLook		ifTrue: [			self isTopWindow				ifTrue: [					stripes first borderColor: paneColor paler;						 color: stripes first borderColor slightlyLighter.					stripes second borderColor: stripes first color slightlyLighter;						 color: stripes second borderColor slightlyLighter]				ifFalse: ["This could be much faster"					stripes first borderColor: paneColor;						 color: paneColor.					stripes second borderColor: paneColor;						 color: paneColor]]		ifFalse: [			self isTopWindow				ifTrue: [					stripes second color: paneColor;						 borderColor: stripes second color darker.					stripes first color: stripes second borderColor darker;						 borderColor: stripes first color darker]				ifFalse: ["This could be much faster"					stripes second color: paneColor;						 borderColor: paneColor.					stripes first color: paneColor;						 borderColor: paneColor]]! !!OldSystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 11/7/2008 12:43'!collapseOrExpand	"Collapse or expand the window, depending on existing state"	| cf |	isCollapsed		ifTrue: [			"Expand -- restore panes to morphics structure"			isCollapsed _ false.			self activate.  "Bring to frint first"			Preferences collapseWindowsInPlace				ifTrue: [					fullFrame := fullFrame align: fullFrame topLeft with: self bounds topLeft]				ifFalse: [					collapsedFrame _ self bounds].			collapseBox ifNotNil: [collapseBox setBalloonText: 'collapse this window' translated].			self bounds: fullFrame.			paneMorphs reverseDo: [ :m |  				self addMorph: m.				self world startSteppingSubmorphsOf: m]]		ifFalse: [			"Collapse -- remove panes from morphics structure"			isCollapsed _ true.			fullFrame _ self bounds.			"First save latest fullFrame"			paneMorphs do: [:m | m delete; releaseCachedState].			model modelSleep.			cf := self getCollapsedFrame.			(collapsedFrame isNil and: [Preferences collapseWindowsInPlace not]) ifTrue:				[collapsedFrame _ cf].			self bounds: cf.			collapseBox ifNotNil: [collapseBox setBalloonText: 'expand this window' translated].			expandBox ifNotNil: [expandBox setBalloonText: 'expand to full screen' translated]].	self layoutChanged! !!OldSystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 11/7/2008 13:23'!spawnOffsetReframeHandle: event divider: divider 
	"The mouse has crossed a secondary (fixed-height) pane divider.  Spawn a reframe handle."

	"Only supports vertical adjustments."

	| siblings topAdjustees bottomAdjustees topOnly bottomOnly resizer pt delta minY maxY cursor |
	owner ifNil: [^self	"Spurious mouseLeave due to delete"].
	self isCollapsed ifTrue: [^self].
	((self world ifNil: [^self]) firstSubmorph isKindOf: OldNewHandleMorph) 
		ifTrue: [^self	"Prevent multiple handles"].
	divider layoutFrame ifNil: [^self].
	(#(#top #bottom) includes: divider resizingEdge) ifFalse: [^self].
	siblings := divider owner submorphs select: [:m | m layoutFrame notNil].
	divider resizingEdge = #bottom 
		ifTrue: 
			[cursor := Cursor resizeTop.
			topAdjustees := siblings select: 
							[:m | 
							m layoutFrame topFraction = divider layoutFrame bottomFraction 
								and: [m layoutFrame topOffset >= divider layoutFrame topOffset]].
			bottomAdjustees := siblings select: 
							[:m | 
							m layoutFrame bottomFraction = divider layoutFrame topFraction 
								and: [m layoutFrame bottomOffset >= divider layoutFrame topOffset]]].
	divider resizingEdge = #top 
		ifTrue: 
			[cursor := Cursor resizeBottom.
			topAdjustees := siblings select: 
							[:m | 
							m layoutFrame topFraction = divider layoutFrame bottomFraction 
								and: [m layoutFrame topOffset <= divider layoutFrame bottomOffset]].
			bottomAdjustees := siblings select: 
							[:m | 
							m layoutFrame bottomFraction = divider layoutFrame topFraction 
								and: [m layoutFrame bottomOffset <= divider layoutFrame bottomOffset]]].
	topOnly := topAdjustees copyWithoutAll: bottomAdjustees.
	bottomOnly := bottomAdjustees copyWithoutAll: topAdjustees.
	(topOnly isEmpty or: [bottomOnly isEmpty]) ifTrue: [^self].
	minY := bottomOnly inject: -9999
				into: [:y :m | y max: m top + (m minHeight max: 16) + (divider bottom - m bottom)].
	maxY := topOnly inject: 9999
				into: [:y :m | y min: m bottom - (m minHeight max: 16) - (m top - divider top)].
	pt := event cursorPoint.
	resizer := OldNewHandleMorph new 
				followHand: event hand
				forEachPointDo: 
					[:p | 
					delta := (p y min: maxY max: minY) - pt y.
					topAdjustees 
						do: [:m | m layoutFrame topOffset: m layoutFrame topOffset + delta].
					bottomAdjustees 
						do: [:m | m layoutFrame bottomOffset: m layoutFrame bottomOffset + delta].
					divider layoutChanged.
					pt := pt + delta]
				lastPointDo: [:p | ]
				withCursor: cursor.
	event hand world addMorphInLayer: resizer.
	resizer startStepping! !!OldSystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 11/7/2008 13:23'!spawnReframeHandle: event 
	"The mouse has crossed a pane border.  Spawn a reframe handle."

	| resizer localPt pt ptName newBounds cursor |
	owner ifNil: [^self	"Spurious mouseLeave due to delete"].
	self isCollapsed ifTrue: [^self].
	((self world ifNil: [^self]) firstSubmorph isKindOf: OldNewHandleMorph) 
		ifTrue: [^self	"Prevent multiple handles"].
	pt := event cursorPoint.
	"prevent spurios mouse leave when dropping morphs"
	owner 
		morphsInFrontOf: self
		overlapping: (pt - 2 extent: 4 @ 4)
		do: [:m | m isHandMorph ifFalse: [(m fullContainsPoint: pt) ifTrue: [^self]]].
	self bounds forPoint: pt
		closestSideDistLen: 
			[:side :dist :len | 
			"Check for window side adjust"

			dist <= 2 ifTrue: [ptName := side]].
	ptName ifNil: 
			["Check for pane border adjust"

			^self spawnPaneFrameHandle: event].
	#(#topLeft #bottomRight #bottomLeft #topRight) do: 
			[:corner | 
			"Check for window corner adjust"

			(pt dist: (self bounds perform: corner)) < 20 ifTrue: [ptName := corner]].
	cursor := Cursor resizeForEdge: ptName.
	resizer := (OldNewHandleMorph new)
				sensorMode: self fastFramingOn;
				followHand: event hand
					forEachPointDo: 
						[:p | 
						localPt := self pointFromWorld: p.
						newBounds := self bounds 
									withSideOrCorner: ptName
									setToPoint: localPt
									minExtent: self minimumExtent.
						self fastFramingOn 
							ifTrue: 
								[Cursor currentCursor == cursor 
									ifFalse: 
										[(event hand)
											visible: false;
											refreshWorld;
											visible: true.
										cursor show].
								self doFastWindowReframe: ptName]
							ifFalse: 
								[self bounds: newBounds]]
					lastPointDo: [:p | ]
					withCursor: cursor.
	event hand world addMorphInLayer: resizer.
	resizer startStepping! !!OldSystemWindow methodsFor: 'top window' stamp: 'jmv 11/7/2008 13:27'!activate
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	self owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop := TopWindow.
	TopWindow := self.
	oldTop ifNotNil: [oldTop passivate].
	self owner firstSubmorph == self 
		ifFalse: [
			"Bring me (with any flex) to the top if not already"
			self owner addMorphFront: self].
	labelArea ifNotNil:  [
			self setStripeColorsFrom: self paneColorToUse].
	self isCollapsed 
		ifFalse: [
			model modelWakeUpIn: self.
			self positionSubmorphs.
			labelArea ifNil: [self adjustBorderUponActivationWhenLabeless]]! !!OldSystemWindow methodsFor: 'top window' stamp: 'jmv 11/7/2008 13:15'!isTopWindow	^ self == TopWindow! !!OldSystemWindow methodsFor: 'top window' stamp: 'jmv 11/7/2008 15:25'!passivate	"Make me unable to respond to mouse and keyboard"	| focus |	focus _ ActiveHand keyboardFocus.	focus ifNotNil: [		(focus ownerThatIsA: OldSystemWindow) == self			ifTrue: [ ActiveHand releaseKeyboardFocus ]].	self setStripeColorsFrom: self paneColorToUse.	model modelSleep.	labelArea ifNil: [ "i.e. label area is nil, so we're titleless"		self adjustBorderUponDeactivationWhenLabeless].		self world ifNotNil:  "clean damage now, so dont merge this rect with new top window"		[self world == World ifTrue: [self world displayWorld]]! !!OldSystemWindow class methodsFor: 'top window' stamp: 'jmv 11/7/2008 13:10'!noteTopWindowIn: aWorld	| newTop |	"TopWindow must be nil or point to the top window in this project."	TopWindow _ nil.	aWorld ifNil: [^ self].	newTop _ aWorld findA: OldSystemWindow.	newTop == nil ifFalse: [newTop activate]! !!Preferences class methodsFor: 'misc' stamp: 'jmv 11/7/2008 14:17'!defaultValueTableForCurrentRelease
	"Answer a table defining default values for all the preferences in the release.  Returns a list of (pref-symbol, boolean-symbol) pairs"

	^  #(
		(abbreviatedBrowserButtons false)
		(alternativeBrowseIt false)
		(alternativeScrollbarLook true)
		(alternativeWindowLook true)
		(annotationPanes false)
		(automaticFlapLayout true)
		(automaticPlatformSettings true)
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(canRecordWhilePlaying false)
		(caseSensitiveFinds false)
		(changeSetVersionNumbers true)
		(checkForSlips true)
		(classicNewMorphMenu false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(confirmFirstUseOfStyle true)
		(conversionMethodsAtFileOut false)
		(cpuWatcherEnabled false)
		(debugHaloHandle true)
		(debugPrintSpaceLog false)
		(debugShowDamage false)
		(decorateBrowserButtons true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(dismissAllOnOptionClose false)
		(fastDragWindowForMorphic true)
		(fullScreenLeavesDeskMargins true)
		(hiddenScrollBars false)
		(higherPerformance false)
		(honorDesktopCmdKeys true)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(logDebuggerStackToFile true)
		(menuButtonInToolPane false)
		(menuColorFromWorld false)
		(menuKeyboardControl false)  
		(modalColorPickers true)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu true)
		(projectsSentToDisk false)
		(propertySheetFromHalo false)
		(restartAlsoProceeds false)
		(reverseWindowStagger true)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(selectiveHalos false)
		(showBoundsInHalo false)
		(simpleMenus false)
		(smartUpdating true)
		(soundQuickStart false)
		(soundStopWhenDone false)
		(soundsEnabled true)
		(systemWindowEmbedOK false)
		(thoroughSenders true)
		(twentyFourHourFileStamps true)
		(uniqueNamesInHalos false)
		(warnIfNoChangesFile true)
		(warnIfNoSourcesFile true))


"
Preferences defaultValueTableForCurrentRelease do:
	[:pair | (Preferences preferenceAt: pair first ifAbsent: [nil]) ifNotNilDo:
			[:pref | pref defaultValue: (pair last == #true)]].
Preferences chooseInitialSettings.
"! !!Preferences class methodsFor: 'themes' stamp: 'jmv 11/7/2008 14:17'!brightSqueak
	"The classic bright Squeak look.  Windows have saturated colors and relatively low contrast; scroll-bars are of the flop-out variety and are on the left.  Many power-user features are enabled."

	self setPreferencesFrom:
	#(
		(alternativeScrollbarLook false)
		(alternativeWindowLook false)
		(annotationPanes true)
		(automaticFlapLayout true)
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(classicNewMorphMenu false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(debugHaloHandle true)
		(debugPrintSpaceLog false)
		(debugShowDamage false)
		(decorateBrowserButtons true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(fullScreenLeavesDeskMargins true)
		(hiddenScrollBars false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars false)
		(logDebuggerStackToFile true)
		(menuButtonInToolPane false)
		(menuColorFromWorld false)
		(menuKeyboardControl true)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu true)
		(propertySheetFromHalo false)
		(restartAlsoProceeds false)
		(reverseWindowStagger true)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(selectiveHalos false)
		(simpleMenus false)
		(smartUpdating true)
		(systemWindowEmbedOK false)
		(thoroughSenders true)
		(warnIfNoChangesFile true)
		(warnIfNoSourcesFile true))! !!Preferences class methodsFor: 'themes' stamp: 'jmv 11/7/2008 14:17'!juans

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook true)
		(alternativeWindowLook true)		(alternativeWindowBoxesLook true)		(alwaysHideHScrollbar false)		(alwaysShowHScrollbar false)		(alwaysShowVScrollbar true)
		(annotationPanes true)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips true)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(menuColorFromWorld false)
		(menuKeyboardControl true)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow true)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating true)		(subPixelRenderFonts true)
		(thoroughSenders true)	)! !!Preferences class methodsFor: 'themes' stamp: 'jmv 11/7/2008 14:17'!paloAlto
	"Similar to the brightSqueak theme, but with a number of idiosyncratic personal settings.   Note that caseSensitiveFinds is true"


	self setPreferencesFrom:
	#(
		(abbreviatedBrowserButtons false)
		(accessOnlineModuleRepositories noOpinion)
		(alternativeBrowseIt noOpinion)
		(alternativeScrollbarLook false)
		(alternativeWindowLook false)
		(annotationPanes true)
		(automaticFlapLayout true)
		(automaticPlatformSettings noOpinion)
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(canRecordWhilePlaying noOpinion)
		(caseSensitiveFinds true)
		(changeSetVersionNumbers true)
		(checkForSlips true)
		(classicNewMorphMenu false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(confirmFirstUseOfStyle true)
		(conservativeModuleDeActivation noOpinion)
		(conversionMethodsAtFileOut true)
		(cpuWatcherEnabled noOpinion)
		(debugHaloHandle true)
		(debugPrintSpaceLog true)
		(debugShowDamage false)
		(decorateBrowserButtons true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(dismissAllOnOptionClose true)
		(duplicateControlAndAltKeys false)
		(extraDebuggerButtons true)
		(fastDragWindowForMorphic true)
		(fullScreenLeavesDeskMargins true)
		(hiddenScrollBars false)
		(higherPerformance noOpinion)
		(honorDesktopCmdKeys true)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars false)
		(lenientScopeForGlobals noOpinion)
		(logDebuggerStackToFile true)
		(menuButtonInToolPane false)
		(menuColorFromWorld false)
		(menuKeyboardControl true)  
		(modalColorPickers true)
		(modularClassDefinitions noOpinion)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu true)
		(projectsSentToDisk noOpinion)
		(propertySheetFromHalo false)
		(restartAlsoProceeds false)
		(reverseWindowStagger true)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(selectiveHalos false)
		(showBoundsInHalo false)
		(simpleMenus false)
		(smartUpdating true)
		(soundQuickStart noOpinion)
		(soundsEnabled true)
		(soundStopWhenDone noOpinion)
		(strongModules noOpinion)
		(swapControlAndAltKeys noOpinion)
		(swapMouseButtons  noOpinion)
		(systemWindowEmbedOK false)
		(thoroughSenders true)
		(twentyFourHourFileStamps false)
		(uniqueNamesInHalos false)
		(warnIfNoChangesFile true)
		(warnIfNoSourcesFile true))! !!Preferences class methodsFor: 'themes' stamp: 'jmv 11/7/2008 14:16'!slowMachine

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook true)
		(alternativeWindowLook false)		(alternativeWindowBoxesLook false)		(alwaysHideHScrollbar true)		(alwaysShowHScrollbar false)		(alwaysShowVScrollbar false)
		(annotationPanes false)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(menuColorFromWorld false)
		(menuKeyboardControl false)
		(noviceMode false)
		(optionalButtons false)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating false)		(subPixelRenderFonts false)
		(thoroughSenders false)	)! !!Preferences class methodsFor: 'themes' stamp: 'jmv 11/7/2008 14:16'!smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak in recent years. Caution: this theme removes the standard Squeak flaps, turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more."

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook false)
		(alternativeWindowLook false)
		(annotationPanes false)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars false)
		(menuColorFromWorld false)
		(menuKeyboardControl false)
		(noviceMode false)
		(optionalButtons false)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating false)
		(thoroughSenders false)	)! !Preferences class removeSelector: #mouseOverForKeyboardFocus!OldSystemWindow removeSelector: #activeOnFirstClick!OldSystemWindow removeSelector: #activeOnlyOnTop!OldSystemWindow removeSelector: #isActive!OldSystemWindow removeSelector: #lockInactivePortions!OldSystemWindow removeSelector: #mouseUp:!OldSimpleHierarchicalListMorph removeSelector: #mouseDown:onItem:!OldSimpleHierarchicalListMorph removeSelector: #mouseEnter:!OldSimpleHierarchicalListMorph removeSelector: #mouseLeaveDragging:onItem:!OldSimpleHierarchicalListMorph removeSelector: #startDrag:onItem:!OldPluggableListMorph removeSelector: #doubleClick:onItem:!OldPluggableListMorph removeSelector: #mouseDown:onItem:!OldPluggableListMorph removeSelector: #mouseEnter:!OldPluggableListMorph removeSelector: #mouseLeave:!OldPluggableListMorph removeSelector: #mouseLeaveDragging:onItem:!OldPluggableListMorph removeSelector: #startDrag:onItem:!!OldPluggableListMorph reorganize!('accessing' highlightSelector itemFromPoint: rowAtLocation:)('as yet unclassified' listMorph listMorphClass)('debug and other' installModelIn:)('drag and drop' acceptDroppingMorph:event: potentialDropItem potentialDropRow resetPotentialDropRow startDrag:)('drawing' highlightSelection unhighlightSelection)('dropping/grabbing' wantsDroppedMorph:event:)('event handling' handlesKeyboard: handlesMouseOverDragging: keyStroke: keyboardFocusChange:)('events' doubleClick: handlesBasicKeys mouseDown: mouseEnterDragging: mouseLeaveDragging: mouseMove: mouseUp:)('events-processing' handleMouseMove:)('geometry' extent: scrollDeltaHeight scrollDeltaWidth)('initialization' autoDeselect: doubleClickSelector: font font: getListElementSelector: initForKeystrokes keystrokeActionSelector: list: listItemHeight on:list:selected:changeSelected:menu:keystroke: textColor textColor: textHighlightColor textHighlightColor:)('menu' getMenu:)('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard setListFont)('model access' basicKeyPressed: changeModelSelection: commandKeyTypedIntoMenu: getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: modifierKeyPressed: specialKeyPressed:)('obsolete' mouseEnterDragging:onItem: mouseUp:onItem: removeObsoleteEventHandlers)('scrolling' hExtraScrollRange hUnadjustedScrollRange vUnadjustedScrollRange)('selection' getListSelector maximumSelection minimumSelection numSelectionsInView scrollSelectionIntoView selectedMorph: selection selection: selectionIndex selectionIndex: setGetListSelector: setSelectedMorph:)('updating' update: updateList verifyContents)!!OldLazyListMorph reorganize!('initialization' initialize listSource:)('list management' drawBoundsForRow: listChanged rowAtLocation: selectRow: selectedRow selectedRow:)('drawing' adjustHeight adjustWidth bottomVisibleRowForCanvas: colorForRow: display:atRow:on: drawBackgroundForMulti:on: drawOn: drawSelectionOn: font font: highlightPotentialDropRow:on: topVisibleRowForCanvas:)('list access' getListItem: getListSize item:)('scroll range' hUnadjustedScrollRange widthToDisplayItem:)('private' noSelection)!!OldMorph reorganize!('WiW support' addMorphInFrontOfLayer: addMorphInLayer: morphicLayerNumber morphicLayerNumberWithin: shouldGetStepsFrom:)('accessing' adoptPaneColor: balloonText beSticky borderColor borderColor: borderStyle borderStyle: borderStyleForSymbol: borderWidth borderWidth: color color: colorForInsets doesBevels eventHandler eventHandler: forwardDirection highlight highlightColor insetColor isLocked isSticky lock lock: raisedColor rememberedColor rememberedColor: resistsRemoval scaleFactor setBorderStyle: sticky: toggleResistsRemoval toggleStickiness unlock unlockContents userString)('accessing - extension' assureExtension extension hasExtension initializeExtension privateExtension: resetExtension)('accessing - properties' hasProperty: otherProperties removeProperty: setProperty:toValue: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)('as yet unclassified' rotationDegrees:)('button' doButtonAction)('caching' fullReleaseCachedState releaseCachedState)('change reporting' addedMorph: invalidRect: invalidRect:from: ownerChanged privateInvalidateMorph:)('classification' isAlignmentMorph isHandMorph isPlayfieldLike isTextMorph isWorldMorph isWorldOrHandMorph)('copying' copy deepCopy duplicate duplicateMorphCollection: veryDeepCopyWith: veryDeepFixupWith: veryDeepInner:)('creation' asMorph)('debug and other' addDebuggingItemsTo:hand: allStringsAfter: altSpecialCursor0 altSpecialCursor1 altSpecialCursor2 altSpecialCursor3 altSpecialCursor3: buildDebugMenu: inspectOwnerChain installModelIn: ownerChain resumeAfterDrawError resumeAfterStepError)('dispatching' disableSubmorphFocusForHand:)('drawing' areasRemainingToFill: changeClipSubmorphs clipLayoutCells clipLayoutCells: clipSubmorphs clipSubmorphs: clippingBounds drawDropHighlightOn: drawErrorOn: drawHighlightOn: drawMouseDownHighlightOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hasClipSubmorphsString hide highlightForMouseDown highlightForMouseDown: highlightedForMouseDown imageForm imageForm:forRectangle: imageFormForRectangle: isKnownFailing refreshWorld shadowForm show visible visible:)('drop shadows' shadowColor shadowColor:)('dropping/grabbing' aboutToBeGrabbedBy: dragEnabled dragEnabled: dragNDropEnabled dropEnabled dropEnabled: dropHighlightColor enableDrag: enableDragNDrop enableDragNDrop: enableDrop: formerOwner formerOwner: formerPosition formerPosition: highlightForDrop highlightForDrop: highlightedForDrop justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: repelsMorph:event: resetHighlightForDrop separateDragAndDrop slideBackToFormerSituation: startDrag:with: vanishAfterSlidingTo:event: wantsDroppedMorph:event: wantsToBeDroppedInto: wantsToBeOpenedInWorld)('e-toy support' allMorphsAndBookPagesInto: changeAllBorderColorsFrom:to: containingWindow cursor defaultValueOrNil embeddedInMorphicWindowLabeled: rotationStyle rotationStyle: unlockOneSubpart wantsRecolorHandle wrappedInWindow: wrappedInWindowWithTitle:)('event handling' click click: cursorPoint doubleClick: doubleClickTimeout: dropFiles: handleMouseDown: handlesKeyboard: handlesMouseDown: handlesMouseOver: handlesMouseOverDragging: handlesMouseStillDown: hasFocus keyDown: keyStroke: keyUp: mouseDown: mouseEnter: mouseEnterDragging: mouseLeave: mouseLeaveDragging: mouseMove: mouseStillDown: mouseStillDownStepRate mouseStillDownThreshold mouseUp: on:send:to: on:send:to:withValue: startDrag: suspendEventHandler transformFrom: transformFromOutermostWorld transformFromWorld wantsDropFiles: wantsKeyboardFocusFor: wouldAcceptKeyboardFocus wouldAcceptKeyboardFocusUponTab)('events-accessing' actionMap updateableActionMap)('events-alarms' addAlarm:after: addAlarm:at: addAlarm:with:after: addAlarm:with:at: addAlarm:with:with:after: addAlarm:with:with:at: addAlarm:withArguments:after: addAlarm:withArguments:at: alarmScheduler removeAlarm: removeAlarm:at:)('events-processing' containsPoint:event: defaultEventDispatcher handleDropFiles: handleDropMorph: handleEvent: handleFocusEvent: handleKeyDown: handleKeyUp: handleKeystroke: handleListenEvent: handleMouseEnter: handleMouseLeave: handleMouseMove: handleMouseOver: handleMouseStillDown: handleMouseUp: handleUnknownEvent: handlerForMouseDown: mouseDownPriority processEvent: processEvent:using: rejectDropEvent: rejectsEvent: transformedFrom:)('events-removing' releaseActionMap)('fileIn/out' prepareToBeSaved)('filter streaming' drawOnCanvas:)('focus handling' gotNavigationFocus gotNavigationFocus: keyboardFocusChange: lostNavigationFocus seizesNavigationFocus skipsNavigationFocus)('geometry' align:with: bottom bottom: bottomCenter bottomLeft bottomRight bounds bounds: bounds:in: boundsIn: boundsInWorld center center: extent extent: fullBoundsInWorld globalPointToLocal: height height: innerBounds left left: leftCenter localPointToGlobal: minimumExtent point:from: point:in: pointFromWorld: pointInWorld: position position: positionInWorld positionSubmorphs right right: rightCenter setConstrainedPosition:hangOut: top top: topCenter topLeft topRight width width: worldBounds worldBoundsForHalo)('geometry eToy' addTransparentSpacerOfSize: beTransparent forwardDirection: heading referencePosition referencePosition: rotationCenter rotationCenter: setDirectionFrom: transparentSpacerOfSize:)('geometry testing' containsPoint: fullContainsPoint:)('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: addSimpleHandlesTo:box: addWorldHandlesTo:box: balloonColor balloonColor: balloonFont balloonFont: balloonHelpAligner balloonHelpDelayTime balloonHelpTextForHandle: boundsForBalloon comeToFrontAndAddHalo defaultBalloonColor defaultBalloonFont deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloClass mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: setBalloonText:maxLineLength: setCenteredBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloFromClick wantsHaloHandleWithSelector:inHalo:)('initialization' defaultBounds defaultColor inATwoWayScrollPane initialize intoWorld: openCenteredInWorld openInHand openInWorld openInWorld:)('layout' acceptDroppingMorph:event: adjustLayoutBounds doLayoutIn: fullBounds layoutBounds layoutBounds: layoutChanged layoutInBounds: layoutProportionallyIn: minExtent minHeight minWidth privateFullBounds submorphBounds)('layout-menu' addCellLayoutMenuItems:hand: addLayoutMenuItems:hand: addTableLayoutMenuItems:hand: changeCellInset: changeClipLayoutCells changeDisableTableLayout changeLayoutInset: changeListDirection: changeMaxCellSize: changeMinCellSize: changeNoLayout changeProportionalLayout changeReverseCells changeRubberBandCells changeTableLayout hasClipLayoutCellsString hasDisableTableLayoutString hasNoLayoutString hasProportionalLayoutString hasReverseCellsString hasRubberBandCellsString hasTableLayoutString layoutMenuPropertyString:from:)('layout-properties' assureLayoutProperties assureTableProperties cellInset cellInset: cellPositioning cellPositioning: cellPositioningString: cellSpacing cellSpacing: cellSpacingString: disableTableLayout disableTableLayout: hResizing hResizing: hResizingString: layoutFrame layoutFrame: layoutInset layoutInset: layoutPolicy layoutPolicy: layoutProperties layoutProperties: listCentering listCentering: listCenteringString: listDirection listDirection: listDirectionString: listSpacing listSpacing: listSpacingString: maxCellSize maxCellSize: minCellSize minCellSize: reverseTableCells reverseTableCells: rubberBandCells rubberBandCells: spaceFillWeight vResizing vResizing: vResizingString: wrapCentering wrapCentering: wrapCenteringString: wrapDirection wrapDirection: wrapDirectionString:)('macpal' flash)('menu' addBorderStyleMenuItems:hand:)('menus' addAddHandMenuItemsForHalo:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addFillStyleMenuItems:hand: addHaloActionsTo: addPaintingItemsTo:hand: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: adhereToEdge: adjustedCenter adjustedCenter: changeColor changeDragAndDrop chooseNewGraphic chooseNewGraphicCoexisting: chooseNewGraphicFromHalo collapse defaultArrowheadSize exportAsBMP exportAsGIF exportAsJPEG exportAsPNG hasDragAndDropEnabledString inspectInMorphic: lockUnlockMorph lockedString maybeAddCollapseItemTo: resetForwardDirection resistsRemovalString setToAdhereToEdge: snapToEdgeIfAppropriate stickinessString uncollapseSketch)('meta-actions' addEmbeddingMenuItemsTo:hand: blueButtonDown: blueButtonUp: buildHandleMenu: buildMetaMenu: changeColorTarget:selector:originalColor:hand: copyToPasteBuffer: dismissMorph: duplicateMorph: grabMorph: handlerForBlueButtonDown: handlerForMetaMenu: inspectAt:event: invokeMetaMenu: invokeMetaMenuAt:event: maybeDuplicateMorph maybeDuplicateMorph: potentialEmbeddingTargets resizeFromMenu resizeMorph: showActions)('miscellaneous' setExtentFromHalo:)('naming' name: nameForFindWindowFeature setNamePropertyTo: setNameTo:)('objects from disk' objectForDataStream: storeDataOn:)('other events' menuButtonMouseEnter: menuButtonMouseLeave:)('player' assureExternalName okayToDuplicate)('player commands' playSoundNamed:)('printing' clipText colorString: fullPrintOn: initString printConstructorOn:indent:nodeDict: printOn:)('rotate scale and flex' rotationDegrees)('stepping and presenter' arrangeToStartStepping arrangeToStartSteppingIn: start startStepping startStepping:at:arguments:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector:)('structure' activeHand allOwners allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld nearestOwnerThat: outermostMorphThat: outermostWorldMorph owner ownerThatIsA: pasteUpMorph pasteUpMorphHandlingTabAmongFields primaryHand root withAllOwnersDo: world)('submorphs-accessing' allMorphs allMorphsDo: allNonSubmorphMorphs findA: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: morphsInFrontOf:overlapping:do: noteNewOwner: rootMorphsAt: submorphCount submorphNamed: submorphNamed:ifNone: submorphThat:ifNone: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)('submorphs-add/remove' abandon actWhen actWhen: addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:fullFrame: addMorph:inFrontOf: addMorphBack: addMorphCentered: addMorphFront: addMorphFront:fromWorldPosition: addMorphFrontFromWorldPosition: comeToFront copyWithoutSubmorph: delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)('testing' canDrawAtHigherResolution canDrawBorder: isMorph knownName shouldDropOnMouseUp stepTime wantsSteps)('text-anchor' addTextAnchorMenuItems:hand: changeDocumentAnchor changeInlineAnchor changeParagraphAnchor hasDocumentAnchorString hasInlineAnchorString hasParagraphAnchorString relativeTextAnchorPosition relativeTextAnchorPosition: textAnchorType textAnchorType:)('updating' changed)('user interface' activateWindow defaultLabelForInspector initialExtent)('visual properties' fillStyle fillStyle: useDefaultFill)('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateBounds: privateColor: privateFullMoveBy: privateMoveBy: privateOwner: privateRemove: privateSubmorphs:)!!LightWidget reorganize!('DNU' doesNotUnderstand:)('accessing' eventHandler isLocked isSticky morphsAt:behind:unlocked: owner privateOwner: world)('accessing-properties' hasProperty:)('add/remove' delete dismissViaHalo privateDelete)('change reporting' invalidRect: invalidRect:from: ownerChanged)('classification' isHandMorph isWorldMorph isWorldOrHandMorph)('compatibility' allMorphsDo: isPlayfieldLike morphicLayerNumber morphicLayerNumberWithin: noteNewOwner: submorphs submorphsDo:)('copying' duplicate)('debug and other' buildDebugMenu:)('defaults' borderStyleFor: borderStyleWith: defaultFont pressedBorderStyleWith:)('drawing' areasRemainingToFill: drawErrorOn: drawHighlightOn: drawOn: drawingFails drawingFailsNot fullDrawOn: hide isKnownFailing shadowForm show visible visible:)('dropping/grabbing' aboutToBeGrabbedBy: formerOwner: formerPosition: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsToBeDroppedInto:)('event handling' handlesMouseDown: handlesMouseOver: handlesMouseOverDragging: handlesMouseStillDown: mouseMove: mouseStillDown: mouseStillDownStepRate mouseStillDownThreshold mouseUp: transformFrom: transformedFrom:)('events-processing' handleEvent: handleFocusEvent: handleKeyDown: handleKeyUp: handleKeystroke: handleMouseDown: handleMouseEnter: handleMouseLeave: handleMouseMove: handleMouseOver: handleMouseStillDown: handleMouseUp: handlerForMouseDown: handlesKeyboard: keyDown: keyStroke: keyUp: mouseDownPriority processEvent:using: rejectDropEvent: rejectsEvent:)('focus handling' gotNavigationFocus keyboardFocusChange: lostNavigationFocus seizesNavigationFocus skipsNavigationFocus)('geometry' bounds bounds: extent: fullBounds fullBoundsInWorld height height: point:from: point:in: pointInWorld: position position: referencePosition referencePosition: width width: worldBoundsForHalo)('geometry testing' containsPoint: containsPoint:event: fullContainsPoint:)('halos and balloon help' addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpTextForHandle: balloonText halo haloClass removeHalo setExtentFromHalo: transferHalo:from: wantsBalloon wantsHaloFromClick)('initialization' initialize intoWorld: openInWorld openInWorld:)('layout' layoutChanged)('menus' inspectInMorphic:)('meta actions' blueButtonDown: blueButtonUp: dismissMorph: duplicateMorph: handlerForBlueButtonDown:)('rotation scale and flex' rotationDegrees rotationDegrees:)('stepping and presenter' shouldGetStepsFrom: startStepping:at:arguments:stepTime: stopSteppingSelector:)('structure' activeHand allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld outermostWorldMorph ownerThatIsA: withAllOwnersDo:)('submorphs-add/remove' comeToFront goBehind)('testing' hasNavigationFocus isTextMorph resistsRemoval shouldDropOnMouseUp wantsSteps)('updating' changed)('user interface' activateWindow)('view of a model or target' action action: actionAdaptor actionAdaptor: aspect aspect: aspect:adaptor:action:adaptor: aspectAdaptor aspectAdaptor: beMainViewOn: modelChanged performActionWith: safeModelChanged target target: target:action: target:aspect: target:aspect:action: target:aspect:aspectAdaptor: target:aspect:aspectAdaptor:modelChangeEvent: targetAspect updateView)('private' privateFullMoveBy: privateMoveBy:)!