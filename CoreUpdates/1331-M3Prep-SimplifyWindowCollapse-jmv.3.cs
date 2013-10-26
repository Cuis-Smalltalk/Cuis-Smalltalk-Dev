'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 8:28:36 am'!

!HaloMorph methodsFor: 'private' stamp: 'jmv 4/12/2012 08:02'!
addHandle: handleSpec on: eventName send: selector to: recipient 
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint iconName colorToUse icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle bounds: (Rectangle center: aPoint extent: HandleSize asPoint).
	(iconName _ handleSpec iconSymbol) ifNotNil: [
			| form |
			form _ Icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon.
				icon morphPositionInOwner: 0@0 ]].
	handle 
		on: #mouseUp
		send: #endInteraction
		to: self.
	handle 
		on: eventName
		send: selector
		to: recipient.
	handle 
		setBalloonText: (target balloonHelpTextForHandle: handle).
	^handle! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 4/12/2012 08:11'!
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem
	"Present this menu at the given point under control of the given hand."

	| delta tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition - self morphPosition.
	sourceItem owner owner addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self fullBounds amountToTranslateWithin: sourceItem world bounds.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPoint first value: false;
		value: rightOrLeftPoint last - (self morphWidth @ 0) value: false;
		value: rightOrLeftPoint first value: true! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 4/12/2012 08:24'!
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
		(fastDragWindowForMorphic false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(simpleMenus false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 4/12/2012 08:24'!
slowMachine

	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)
		(menuKeyboardControl false)
		(optionalButtons false)
		(simpleMenus false)
		(smartUpdating false)
		(subPixelRenderFonts false)
		(thoroughSenders false)
	).
	ClassicTheme beCurrent! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 4/12/2012 08:24'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak in recent years. Caution: this theme removes the standard Squeak flaps, turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more."

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
	)! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 4/12/2012 08:08'!
morphPosition: newPos

	super morphPosition: newPos.

	self flag: #jmvVer2. "Maybe this would be better done in methods storing in #bounds...
	Better yet would be to remove this crap"
	isCollapsed
		ifTrue: [ collapsedFrame _ bounds ]
		ifFalse: [ fullFrame _ bounds ]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 4/12/2012 08:25'!
collapse

	| cf |
		
	self isCollapsed ifFalse: [

		"Collapse -- remove panes from morphics structure"

		isCollapsed _ true.
		fullFrame _ bounds.

		"First save latest fullFrame"
		layoutMorph ifNotNil: [ layoutMorph hide ].
		cf := RealEstateAgent assignCollapseFrameFor: self.
		collapsedFrame ifNil: [
			collapsedFrame _ cf].
		self bounds: cf.
	
		Taskbar visible ifTrue: [ Taskbar minimize: self ]
	]! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'jmv 4/12/2012 08:24'!
expand
	self isCollapsed
		ifTrue: [
		
			"Expand -- restore panes to morphics structure"
	
			isCollapsed _ false.
			self activate.  "Bring to front first"

			collapsedFrame _ bounds.

			self bounds: fullFrame.
			layoutMorph ifNotNil: [ layoutMorph show ].
			
			Taskbar visible ifTrue: [ Taskbar restore: self. ]
		]! !

!methodRemoval: SystemWindow #getCollapsedFrame!
SystemWindow removeSelector: #getCollapsedFrame!
!methodRemoval: Preferences class #collapseWindowsInPlace!
Preferences class removeSelector: #collapseWindowsInPlace!
!methodRemoval: PasteUpMorph #morphPosition:!
PasteUpMorph removeSelector: #morphPosition:!
!methodRemoval: HaloMorph #morphPosition:!
HaloMorph removeSelector: #morphPosition:!
