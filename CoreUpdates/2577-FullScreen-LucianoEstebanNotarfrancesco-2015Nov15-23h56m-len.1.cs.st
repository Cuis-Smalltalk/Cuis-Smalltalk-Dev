'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 16 November 2015 at 1:40:45.243135 am'!

!Theme methodsFor: 'other options' stamp: 'len 11/16/2015 01:21'!
fullScreenDeskMargin
	^ Preferences fullScreenLeavesDeskMargins ifTrue: [22] ifFalse: [0]! !


!SystemWindow methodsFor: 'menu' stamp: 'len 11/16/2015 01:22'!
fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	"SystemWindow fullScreen"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self morphBoundsInWorld.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			possibleBounds := possibleBounds insetBy: Theme current fullScreenDeskMargin
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self morphPosition: possibleBounds topLeft extent: possibleBounds extent! !


!Theme methodsFor: 'keyboard shortcuts' stamp: 'len 11/16/2015 00:20'!
keyStroke: aKeyboardEvent morph: aMorph
	aKeyboardEvent controlKeyPressed ifTrue: [^false].
	aKeyboardEvent commandAltKeyPressed ifFalse: [^false].

"	aKeyboardEvent keyValue = 9
		ifTrue: [aKeyboardEvent inspect. ^true].
"
	aKeyboardEvent keyValue = $x asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w delete. ^true ]].
	aKeyboardEvent keyValue = 27
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w fullScreen. ^true ]].

	aKeyboardEvent keyValue = $[ asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeLeft. ^true ]].
	aKeyboardEvent keyValue = $] asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeRight. ^true ]].
	aKeyboardEvent keyValue = ${ asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeTop. ^true ]].
	aKeyboardEvent keyValue = $} asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeBottom. ^true ]].

	"open new windows"
	aKeyboardEvent keyValue = $B asciiValue
		ifTrue: [BrowserWindow openBrowser. ^true].
	aKeyboardEvent keyValue = $W asciiValue
		ifTrue: [Workspace new contents: ''; openLabel: 'Worksspace'. ^true].
"	aKeyboardEvent keyValue = $T asciiValue
		ifTrue: [TranscriptWindow openTranscript. ^true].
	aKeyboardEvent keyValue = $P asciiValue
		ifTrue: [CodePackageListWindow openPackageList. ^true].
	aKeyboardEvent keyValue = $F asciiValue
		ifTrue: [FileListWindow openFileList. ^true].
	aKeyboardEvent keyValue = $C asciiValue
		ifTrue: [ChangeSorterWindow open: ChangeSorter new label: nil. ^true].
"
	^ false! !


!Theme class methodsFor: 'class initialization' stamp: 'len 11/16/2015 00:19'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		w hideTaskbar.
		CurrentTheme useTaskbar ifTrue: [w showTaskbar].
		w restoreMorphicDisplay ].
	
	^ CurrentTheme! !

!methodRemoval: Theme #fullScreenDeskMargins!
Theme removeSelector: #fullScreenDeskMargins!
