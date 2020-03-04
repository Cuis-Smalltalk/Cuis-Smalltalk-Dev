'From Cuis 5.0 [latest update: #4047] on 3 March 2020 at 7:46:29 pm'!

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 19:45:18'!
browseCodeFileEntry: aFileEntry
	^ self browseCodeSource: (CodeFile newFromFile: aFileEntry )! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 19:45:11'!
browseCodeSource: aCaseCodeSource
	^ self browseCodeSource: aCaseCodeSource base: nil! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 19:45:02'!
browseCodeSource: aCaseCodeSource base: aBaseCodeSource
	| useCaseCodeSource useCaseClasses browser useCaseOrganizer useHeading |
	browser _ self new.
	browser
		caseCodeSource: aCaseCodeSource;
		baseCodeSource: aBaseCodeSource.
	useCaseCodeSource _ browser caseCodeSource.
	useCaseClasses _ useCaseCodeSource classes collect: [ :ea |
		ea name ].
	useCaseOrganizer _ useCaseCodeSource organization.
	useHeading _ (useCaseCodeSource isLiveSmalltalkImage not and: [ browser baseCodeSource isLiveSmalltalkImage ])
		ifTrue: [ useCaseCodeSource name ]
		ifFalse: [ "This is a non-standard configuration... make the user aware"
			useCaseCodeSource name , '(' , useCaseCodeSource class name , '), target: ' , aBaseCodeSource name , '(' , aBaseCodeSource class name , ')' ].
	(useCaseCodeSource notNil and: [ useCaseCodeSource isLiveSmalltalkImage not ]) ifTrue: [
		useCaseOrganizer
			classifyAll: useCaseClasses
			under: useHeading ].
	browser
		systemOrganizer: useCaseOrganizer;
		caseCodeSource: useCaseCodeSource.
	aBaseCodeSource ifNotNil: [ browser baseCodeSource: aBaseCodeSource ].
	^ CodeFileBrowserWindow
		open: browser
		label: nil.! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'pb 3/3/2020 19:45:27'!
browsePackageFileEntry: aFileEntry
	^ self browseCodeSource: (CodePackageFile newFromFile: aFileEntry )! !

