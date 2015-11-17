'From Cuis 4.2 of 25 July 2013 [latest update: #2577] on 16 November 2015 at 11:37:40.777416 pm'!

!Theme methodsFor: 'as yet unclassified' stamp: 'len 11/16/2015 03:18'!
windowClosed: aSystemWindow
	^ self! !

!Theme methodsFor: 'as yet unclassified' stamp: 'len 11/16/2015 03:20'!
windowOpen: aSystemWindow
	^ self! !


!SystemWindow methodsFor: 'initialization' stamp: 'len 11/16/2015 03:20'!
openInWorld
	"Ensure all widgets have proper colors before opening"
	self widgetsColor: self windowColor.
	super openInWorld.
	Theme current windowOpen: self! !

!SystemWindow methodsFor: 'open/close' stamp: 'len 11/16/2015 03:22'!
delete
	| thisWorld |
	self okToChange ifFalse: [^self].
	thisWorld _ self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	super delete.
	model _ nil.
	Theme current windowClosed: self! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'len 11/16/2015 05:02'!
openBrowser
	| browser |
	browser _ Browser new.
	^ self open: browser label: browser defaultBrowserTitle! !


!FileListWindow class methodsFor: 'instance creation' stamp: 'len 11/16/2015 04:50'!
openFileList
	"
	FileListWindow openFileList
	"
	^ FileListWindow open: (FileList new directory: FmDirectoryEntry currentDirectory) label: nil! !

!methodRemoval: Theme #deletedWindow:!
Theme removeSelector: #deletedWindow:!
