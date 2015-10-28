'From Cuis 4.2 of 25 July 2013 [latest update: #2558] on 28 October 2015 at 9:54:24.93578 am'!

!StandardFileMenu commentStamp: '<historical>' prior: 0!
I represent a SelectionMenu which operates like a modal dialog for selecting files, somewhat similar to the StandardFile dialogs in MacOS and Java Swing.

Try for example, the following:

	(StandardFileMenu oldFileFrom: FmDirectoryEntry smalltalkImageDirectory) inspect
	((StandardFileMenu new newFileFrom: FmDirectoryEntry smalltalkImageDirectory withPattern: '*.st') startUpWithCaption: 'dale') inspect!


!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 10/28/2015 09:53'!
menuSelectionsArray: aDirectory
"Answer a menu selections object corresponding to aDirectory.  The object is an array corresponding to each item, each element itself constituting a two-element array, the first element of which contains a selector to operate on and the second element of which contains the parameters for that selector."

	|dirSize|
	dirSize _ aDirectory pathComponents size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue:
			[s nextPut: (StandardFileMenuResult
				directory: aDirectory
				name: nil)].
		s nextPut: (StandardFileMenuResult
			directory: (FmDirectoryEntry roots first)
			name: '').
		aDirectory pathComponents doWithIndex: 
			[:d :i | s nextPut: (StandardFileMenuResult
					directory: (self 
						advance: dirSize - i
						containingDirectoriesFrom: aDirectory)
					name: '')].
		aDirectory directoriesDo:
			[:dir |  s nextPut: (StandardFileMenuResult
						directory: dir
						name: '')].
		aDirectory fileNames do: 
			[:fn | (self patternMatches: fn) ifTrue: [
					s nextPut: (StandardFileMenuResult
						directory: aDirectory
						name: fn)]]]! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'KenD 10/27/2015 19:00'!
confirmExistingFiles: aResult

	|choice|
	(aResult directory includesKey: aResult name) ifFalse: [^aResult].
	
	choice _ (PopUpMenu
		labels:
'overwrite that file
choose another name
cancel')
		startUpWithCaption: aResult name, '
already exists.'.

	choice = 1 ifTrue: [
		aResult directory 
			removeKey: aResult name
			ifAbsent: 
				[^self startUpWithCaption: 
'Can''t delete ', aResult name, '
Select another file'].
		^aResult].
	choice = 2 ifTrue: [^self startUpWithCaption: 'Select Another File'].
	^nil
 ! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'KenD 10/27/2015 19:03'!
getTypedFileName: aResult

	| name |
	name _ FillInTheBlankMorph 
		request: 'Enter a new file name' 
		initialAnswer: ''.
	name = '' ifTrue: [^self startUpWithCaption: 'Select a File:' ].
	^ StandardFileMenuResult
			directory: (aResult directory)
			name: name
! !

!StandardFileMenu methodsFor: 'private' stamp: 'KenD 10/27/2015 19:13'!
advance: anInteger containingDirectoriesFrom: aDirectory

	| theDirectory |
	theDirectory _ aDirectory.
	1 to: anInteger do: [:i | theDirectory _ theDirectory parent].
	^theDirectory! !


!StandardFileMenu class methodsFor: 'instance creation' stamp: 'KenD 10/27/2015 18:52'!
oldFileMenu: aDirectory

	(aDirectory isKindOf: FmDirectoryEntry)
		ifFalse: [ self error: 'I require a FmDirectoryEntry'].

	^self new oldFileFrom: aDirectory! !

