'From Cuis 5.0 of 7 November 2016 [latest update: #3610] on 17 March 2019 at 6:14:00 pm'!

!TextModel methodsFor: 'as yet unclassified' stamp: 'jpb 3/17/2019 18:09:39'!
saveOn: stream as: format
	"Saves the model to the given stream"
	stream binary.
	stream nextPutAll: self actualContents asString.! !

!SystemWindow methodsFor: 'user interface' stamp: 'jpb 3/17/2019 18:12:16'!
saveContents
	"Prompts the user for a file name and saves the contents to the file"
	| fileName |
	self hasSaveAs ifFalse: [^self].
	
	fileName _ FillInTheBlankMorph request: 'Filename'.
	
	fileName isEmptyOrNil
		ifTrue: [ self notifyUserWith: 'Contents not saved']
		ifFalse: [ self saveContentsTo: fileName ].! !

!SystemWindow methodsFor: 'user interface' stamp: 'jpb 3/17/2019 18:12:28'!
saveContentsTo: fileName
	"Saves the contents to the given filename"
	| stream |
	self hasSaveAs ifFalse: [^self].
	
	stream _ StandardFileStream new.
	stream open: fileName forWrite: true.
	
	model saveOn: stream as: 'text/plain'.
	
	stream closed ifFalse: [stream close].
	self notifyUserWith: 'Contents saved'.! !

!SystemWindow methodsFor: 'menu' stamp: 'jpb 3/16/2019 17:00:59'!
addTileResizerMenuTo: aMenu
	"We can look at preferences here to decide what too do"
	(Preferences tileResizerInWindowMenu) ifFalse: [
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
								resizeMsg _ ('resize', resize asString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].
	^aMenu.! !

!SystemWindow methodsFor: 'user interface' stamp: 'jpb 3/17/2019 17:49:20'!
hasSaveAs
	"Returns true if the window has a model which can be saved to a file"
	^model respondsTo: #saveOn:as:! !

!SystemWindow methodsFor: 'user interface' stamp: 'jpb 3/17/2019 17:30:24'!
notifyUserWith: message
	"Notifies the user with a message and an 'ok' button"
	| morph |
	morph _ MenuMorph new.
	morph addTitle: message.
	morph add: 'Ok' action: nil.
	morph openInWorld.! !

!SystemWindow methodsFor: 'menu' stamp: 'jpb 3/16/2019 17:13:12'!
addWindowControlTo: aMenu

	aMenu
		add: 'send to back' 				action: #sendToBack 				icon: #goBottomIcon;
		add: 'make next-to-topmost' 	action: #makeSecondTopmost 	icon: #goUpIcon;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ])
											action: #toggleStickiness 			icon: (self isSticky ifFalse: [#pushPinIcon]);
		addLine;
		add: 'close' 						action: #delete 						icon: #closeIcon;
		add: 'collapse' 					action: #collapse 					icon: #collapseIcon;
		add: 'expand / contract' 		action: #expandBoxHit 				icon: #expandIcon;
		addLine;
		add: 'resize...' 					action: #resize.
		
	^aMenu! !


!SystemWindow methodsFor: 'menu' stamp: 'jpb 3/17/2019 17:42:09'!
buildWindowMenu

	| aMenu |

	aMenu _ MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' 			action: #relabel 						icon: #saveAsIcon;
		add: 'window color...' 			action: #setWindowColor 			icon: #graphicsIcon.
	
	self hasSaveAs
		ifTrue: [ aMenu add: 'Save as ...' action: #saveContents icon: #saveAsIcon ].
		
	aMenu
		addLine.
		
	self addWindowControlTo: aMenu.
	self addTileResizerMenuTo: aMenu.

	^ aMenu! !
