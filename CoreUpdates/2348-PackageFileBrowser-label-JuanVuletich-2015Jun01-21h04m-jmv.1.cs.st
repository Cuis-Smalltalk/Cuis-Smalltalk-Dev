'From Cuis 4.2 of 25 July 2013 [latest update: #2346] on 1 June 2015 at 9:14:29.432084 pm'!

!CodeFileBrowser methodsFor: 'other' stamp: 'jmv 6/1/2015 21:12'!
labelString
	"Answer the string for the window title"

	^ selectedSystemCategory ifNil: ['']! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 6/1/2015 21:13'!
browsePackageStream: aStream named: aString

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	codeFile _ (CodePackageFile new fullName: aString; buildFrom: aStream).
	aStream close.
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	CodeFileBrowserWindow open: browser label: nil! !

!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 6/1/2015 21:13'!
browseStream: aStream named: aString

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	codeFile _ (CodeFile new fullName: aString; buildFrom: aStream).
	aStream close.
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	CodeFileBrowserWindow open: browser label: nil! !

!methodRemoval: CodeFileBrowser #defaultBrowserTitle!
CodeFileBrowser removeSelector: #defaultBrowserTitle!
