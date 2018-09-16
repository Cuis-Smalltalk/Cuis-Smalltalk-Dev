'From Cuis 5.0 of 7 November 2016 [latest update: #3368] on 14 September 2018 at 9:47:11 pm'!
!classDefinition: #TileResizeMorph category: #'Morphic-Views'!
BorderedRectMorph subclass: #TileResizeMorph
	instanceVariableNames: 'selectedResize selectionColor action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!Preferences class methodsFor: 'get/set' stamp: 'MM 9/14/2018 10:03:28'!
disableTileResizerInWindowMenu

	Preferences disable: #tileResizerInWindowMenu.! !

!Preferences class methodsFor: 'get/set' stamp: 'MM 9/14/2018 10:03:16'!
enableTileResizerInWindowMenu

	Preferences enable: #tileResizerInWindowMenu.! !


!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 09:40:43'!
action: aBlock
	action _ aBlock! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:34:14'!
drawOn: aCanvas

	super drawOn: aCanvas.
	
	selectedResize ifNil: [^ self].
	
	aCanvas fillRectangle: (self selectionRectangle: selectedResize) 
			color: selectionColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple
			baseColorForBorder: borderColor.! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 09:41:56'!
handlesMouseDown: aMouseButtonEvent

	^ true! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 09:44:47'!
handlesMouseHover
	^ true! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/14/2018 18:08:50'!
initialize

	super initialize.
	extent _ 120@60.
	color _ Color white.
	selectionColor _ Color lightYellow .
	self borderColor: Color black.
	self borderWidth: 1.! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:20:20'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	self selectResize: localEventPosition! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:20:57'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	self selectResize: localEventPosition.
	action ifNotNil: [
		action value: selectedResize.
		self delete]! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:10:21'!
mouseHover: aMouseButtonEvent localPosition: localEventPosition
	self selectResize: localEventPosition! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:37:24'!
resizeAtPoint: aPoint
	
	|region|
		
	region _ (aPoint min: extent - 1) // (extent // 3).
	
	^ region caseOf: {
		[0@0] -> [#topLeft].
		[1@0] -> [#top].
		[2@0] -> [#topRight].
		[0@1] -> [#left].
		[1@1] -> [#full].
		[2@1] -> [#right].
		[0@2] -> [#bottomLeft].
		[1@2] -> [#bottom].
		[2@2] -> [#bottomRight].
	} otherwise: [nil]! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:03:57'!
selectResize: localEventPosition

	| newResize |
	
	newResize _ self resizeAtPoint: localEventPosition.
	newResize ~= selectedResize ifTrue: [
		selectedResize _ newResize.
		self redrawNeeded]! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/14/2018 18:10:14'!
selectionColor: aColor

	selectionColor _ aColor! !

!TileResizeMorph methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 11:33:29'!
selectionRectangle: region

	^ region caseOf: {
		[#topLeft] -> [0@0 corner: (extent // 2)].
		[#top] -> [0@0 corner: (extent x@(extent y // 2))].
		[#topRight] -> [(extent x // 2)@0 corner: (extent x@(extent y // 2))].
		[#left] -> [0@0 corner: (extent x // 2)@extent y].
		[#full] -> [0@0 corner: extent].
		[#right] -> [(extent x // 2)@0 corner: extent].
		[#bottomLeft] -> [0@(extent y // 2) corner: (extent x // 2)@extent y].
		[#bottomRight] -> [(extent x // 2)@(extent y // 2) corner: extent].
		[#bottom] -> [0@(extent y // 2) corner: extent].
	}! !


!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 16:11:37'!
calculateTileRegions: aNumber
	
	^ self calculateTileRegionsIn: Display boundingBox by: aNumber! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 18:25:19'!
calculateTileRegionsHorizontallyIn: aRectangle by: aNumber

	| rects rects1 rects2 |
	aNumber <= 2 ifTrue: [^ self divideRectHorizontally: aRectangle by: aNumber].
	
	rects _ self divideRectHorizontally: aRectangle by: 2.

	rects1 _ self calculateTileRegionsVerticallyIn: rects first by: aNumber // 2.
	rects2 _  self calculateTileRegionsVerticallyIn: rects second by: (aNumber - (aNumber // 2)). 
	
	^ rects1, rects2! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 16:13:08'!
calculateTileRegionsIn: aRectangle by: aNumber
	
	^ self calculateTileRegionsHorizontallyIn: aRectangle by: aNumber! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 16:20:53'!
calculateTileRegionsVerticallyIn: aRectangle by: aNumber

	| rects rects1 rects2 |
	aNumber <= 3 ifTrue: [^ self divideRectVertically: aRectangle by: aNumber].
	
	rects _ self divideRectVertically: aRectangle by: 2.

	rects1 _ self calculateTileRegionsHorizontallyIn: rects first by: aNumber // 2.
	rects2 _  self calculateTileRegionsHorizontallyIn: rects second by: (aNumber - (aNumber // 2)). 
	
	^ rects1, rects2! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 16:08:58'!
divideRectHorizontally: aRectangle by: aNumber

	| w x rects |
	
	x _ aRectangle origin x.
	w _ aRectangle width // aNumber.
	rects _ OrderedCollection new.
	
	aNumber timesRepeat: [ |rect|
		rect _ Rectangle origin: x@aRectangle origin y extent: w@aRectangle height.
		x _ x + w.
		rects add: rect].
	
	 ^ rects
	
	! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 16:10:57'!
divideRectVertically: aRectangle by: aNumber

	| h y rects |
	
	y _ aRectangle origin y.
	h _ aRectangle height // aNumber.
	rects _ OrderedCollection new.
	
	aNumber timesRepeat: [ |rect|
		rect _ Rectangle origin: aRectangle origin x@y extent: aRectangle width@h.
		y _ y + h.
		rects add: rect].
	
	 ^ rects
	
	! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/14/2018 10:01:07'!
initialize

	Preferences
		addPreference: #tileResizerInWindowMenu
		category: #gui 
		default: true
		balloonHelp: 'If enabled, a tile resizer morph is embedded in windows menus.' withNewLines! !

!TileResizeMorph class methodsFor: 'as yet unclassified' stamp: 'MM 9/13/2018 15:59:18'!
tileOpenWindows
	
	|windows regions i |
	
	windows _ (SystemWindow
		windowsIn: self runningWorld
		satisfying: [ :w | w visible]).
		
	regions _ self calculateTileRegions: windows size.
	
	i _ 1.
	windows do: [:w | |r|
		r _ regions at: i.
		w resize: r.
		i _ i + 1]! !


!SystemWindow methodsFor: 'menu' stamp: 'MM 9/14/2018 18:21:38'!
buildWindowMenu

	| aMenu |

	aMenu _ MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' 				action: #relabel 						icon: #saveAsIcon;
		add: 'window color...' 			action: #setWindowColor 			icon: #graphicsIcon;
		addLine;
		add: 'send to back' 				action: #sendToBack 				icon: #goBottomIcon;
		add: 'make next-to-topmost' 	action: #makeSecondTopmost 	icon: #goUpIcon;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ])
											action: #toggleStickiness 			icon: (self isSticky ifFalse: [#pushPinIcon]);
		addLine;
		add: 'close' 						action: #delete 						icon: #closeIcon;
		add: 'collapse' 					action: #collapse 						icon: #collapseIcon;
		add: 'expand / contract' 		action: #expandBoxHit 				icon: #expandIcon;
		addLine;
		add: 'resize...' 					action: #resize.
	"We can look at preferences here to decide what too do"
	(Preferences tileResizerInWindowMenu) ifFalse: [
		aMenu add: 'resize full' 					action: #resizeFull icon: #resizeFullIcon;
		add: 'resize top' 					action: #resizeTop icon: #resizeTopIcon;
		add: 'resize left' 				action: #resizeLeft icon: #resizeLeftIcon;
		add: 'resize bottom' 			action: #resizeBottom icon: #resizeBottomIcon;
		add: 'resize right' 				action: #resizeRight icon: #resizeRightIcon;
		add: 'resize top left' 			action: #resizeTopLeft icon: #resizeTopLeftIcon;
		add: 'resize top right' 			action: #resizeTopRight icon: #resizeTopRightIcon;
		add: 'resize bottom left' 		action: #resizeBottomLeft icon: #resizeBottomLeftIcon;
		add: 'resize bottom right' 		action: #resizeBottomRight icon: #resizeBottomRightIcon]
	ifTrue: [ |resizeMorph|
		"Use embedded resize morph"
		resizeMorph _ TileResizeMorph new
							selectionColor: (self widgetsColor adjustSaturation: -0.2 brightness: 0.25) ;
							action: [:resize | |resizeMsg|
								resizeMsg _ ('resize', resize asString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].

	^ aMenu! !


!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'MM 9/14/2018 21:42:58'!
windowsMenu
        "Build the windows menu for the world."

	^ (self menu: 'Windows')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Tile open windows'.
				#object            ->       TileResizeMorph.
				#selector 		-> 		#tileOpenWindows.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Tile open windows'.
			} asDictionary.
			{
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 		-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 		-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#label 			-> 		'Delete Both of the Above'.
				#selector 		-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`! !

TileResizeMorph initialize!