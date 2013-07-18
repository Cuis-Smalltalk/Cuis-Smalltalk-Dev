'From Cuis 4.1 of 12 December 2012 [latest update: #1572] on 21 January 2013 at 11:00:44 pm'!
!classDefinition: #StandardFileMenu category: #'Tools-FileList'!
SelectionMenu subclass: #StandardFileMenu
	instanceVariableNames: 'canTypeFileName pattern'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!

!StandardFileMenu commentStamp: '<historical>' prior: 0!
I represent a SelectionMenu which operates like a modal dialog for selecting files, somewhat similar to the StandardFile dialogs in MacOS and Java Swing.

Try for example, the following:

	StandardFileMenu oldFile inspect

	StandardFileMenu oldFileStream inspect

	StandardFileMenu newFile inspect

	StandardFileMenu newFileStream inspect!

!classDefinition: #StandardFileMenuResult category: #'Tools-FileList'!
Object subclass: #StandardFileMenuResult
	instanceVariableNames: 'directory name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!

!StandardFileMenu methodsFor: 'menu building' stamp: 'di 5/12/2000 10:31'!
directoryNamesString: aDirectory
"Answer a string concatenating the directory name strings in aDirectory, each string followed by a '[...]' indicator, and followed by a newLine."

	^ String streamContents:
		[:s | aDirectory directoryNames do: 
				[:dn | s nextPutAll: dn withBlanksTrimmed , ' [...]'; newLine]]

! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 7/9/2009 08:43'!
fileNamesString: aDirectory
"Answer a string concatenating the file name strings in aDirectory, each string followed by a newLine."

	^String streamContents:
		[:s | 
			aDirectory fileNames do: 
				[:fn |
					(self patternMatches: fn) ifTrue: [
						s nextPutAll: fn withBlanksTrimmed; newLine]]]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 7/24/2009 20:49'!
makeFileMenuFor: aDirectory
"Initialize an instance of me to operate on aDirectory"

	| theMenu |
	pattern ifNil: [ self pattern: '*'].
	Cursor wait showWhile: 
		[self 
			labels: 	(self menuLabelsString: aDirectory)
			lines: 	(self menuLinesArray: aDirectory).
		theMenu _ self selections: (self menuSelectionsArray: aDirectory)].
	^theMenu! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'acg 4/15/1999 21:57'!
menuLabelsString: aDirectory
"Answer a menu labels object corresponding to aDirectory"

	^ String streamContents: 
		[:s | 
			canTypeFileName ifTrue: 
				[s nextPutAll: 'Enter File Name...'; newLine].
			s nextPutAll: (self pathPartsString: aDirectory).
			s nextPutAll: (self directoryNamesString: aDirectory).
			s nextPutAll: (self fileNamesString: aDirectory).
			s skip: -1]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'tpr 11/28/2003 15:12'!
menuLinesArray: aDirectory
"Answer a menu lines object corresponding to aDirectory"

	| typeCount nameCnt dirDepth|
	typeCount _ canTypeFileName 
		ifTrue: [1] 
		ifFalse: [0].
	nameCnt _ aDirectory directoryNames size.
	dirDepth _ aDirectory pathParts size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue: [s nextPut: 1].
		s nextPut: dirDepth + typeCount + 1.
		s nextPut: dirDepth + nameCnt + typeCount + 1]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'jmv 7/9/2009 08:42'!
menuSelectionsArray: aDirectory
"Answer a menu selections object corresponding to aDirectory.  The object is an array corresponding to each item, each element itself constituting a two-element array, the first element of which contains a selector to operate on and the second element of which contains the parameters for that selector."

	|dirSize|
	dirSize _ aDirectory pathParts size.
	^Array streamContents: [:s |
		canTypeFileName ifTrue:
			[s nextPut: (StandardFileMenuResult
				directory: aDirectory
				name: nil)].
		s nextPut: (StandardFileMenuResult
			directory: (FileDirectory root)
			name: '').
		aDirectory pathParts doWithIndex: 
			[:d :i | s nextPut: (StandardFileMenuResult
					directory: (self 
						advance: dirSize - i
						containingDirectoriesFrom: aDirectory)
					name: '')].
		aDirectory directoryNames do: 
			[:dn |  s nextPut: (StandardFileMenuResult
						directory: (FileDirectory on: (aDirectory fullNameFor: dn))
						name: '')].
		aDirectory fileNames do: 
			[:fn | (self patternMatches: fn) ifTrue: [
					s nextPut: (StandardFileMenuResult
						directory: aDirectory
						name: fn)]]]! !

!StandardFileMenu methodsFor: 'menu building' stamp: 'acg 4/15/1999 21:03'!
pathPartsString: aDirectory
"Answer a string concatenating the path parts strings in aDirectory, each string followed by a newLine."

	^String streamContents:
		[:s | 
			s nextPutAll: '[]'; newLine.
			aDirectory pathParts asArray doWithIndex: 
				[:part :i |
					s next: i put: $ .
					s nextPutAll: part withBlanksTrimmed; newLine]]! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'acg 4/15/1999 21:52'!
confirmExistingFiles: aResult

	|choice|
	(aResult directory fileExists: aResult name) ifFalse: [^aResult].
	
	choice _ (PopUpMenu
		labels:
'overwrite that file
choose another name
cancel')
		startUpWithCaption: aResult name, '
already exists.'.

	choice = 1 ifTrue: [
		aResult directory 
			deleteFileNamed: aResult name
			ifAbsent: 
				[^self startUpWithCaption: 
'Can''t delete ', aResult name, '
Select another file'].
		^aResult].
	choice = 2 ifTrue: [^self startUpWithCaption: 'Select Another File'].
	^nil
 ! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'jmv 1/21/2013 22:59'!
getTypedFileName: aResult

	| name |
	name _ FillInTheBlankMorph 
		request: 'Enter a new file name' 
		initialAnswer: ''.
	name = '' ifTrue: [^self startUpWithCaption: 'Select a File:' ].
	name _ aResult directory fullNameFor: name.
	^ StandardFileMenuResult
			directory: (FileDirectory forFileName: name)
			name: (FileDirectory localNameFor: name)
! !

!StandardFileMenu methodsFor: 'basic control sequences' stamp: 'jmv 7/24/2009 20:46'!
startUpWithCaption: aString at: location

	|result|
	result _ super startUpWithCaption: aString at: location.
	result ifNil: [^nil].
	result isDirectory ifTrue:
		[self makeFileMenuFor: result directory.
		 ^self startUpWithCaption: aString at: location].
	result isCommand ifTrue: 
		[result _ self getTypedFileName: result.
		result ifNil: [^nil]].
	canTypeFileName ifTrue: [^self confirmExistingFiles: result].
	^result
	! !

!StandardFileMenu methodsFor: 'private' stamp: 'acg 4/15/1999 00:32'!
advance: anInteger containingDirectoriesFrom: aDirectory

	| theDirectory |
	theDirectory _ aDirectory.
	1 to: anInteger do: [:i | theDirectory _ theDirectory containingDirectory].
	^theDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:44'!
newFileFrom: aDirectory withPattern: aPattern

	canTypeFileName _ true.
	self pattern: aPattern.
	^self makeFileMenuFor: aDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'acg 4/15/1999 22:03'!
oldFileFrom: aDirectory

	canTypeFileName _ false.
	^self makeFileMenuFor: aDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:44'!
oldFileFrom: aDirectory withPattern: aPattern

	canTypeFileName _ false.
	self pattern: aPattern.
	^self makeFileMenuFor: aDirectory! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:44'!
pattern: patString
	" * for all files, or '*.cs' for changeSets, etc.  Just like fileLists
	jmv- Added support for multiple patterns, separated by spaces.
	For example '*.cs *.st' will show both changesets and fileouts."

	pattern _ patString substrings! !

!StandardFileMenu methodsFor: 'private' stamp: 'jmv 7/9/2009 08:45'!
patternMatches: aString

	^pattern anySatisfy: [ :pat | pat match: aString ]! !


!StandardFileMenu class methodsFor: 'instance creation' stamp: 'jmv 7/9/2009 08:36'!
oldFileMenu: aDirectory

	^self new oldFileFrom: aDirectory! !

!StandardFileMenu class methodsFor: 'standard file operations' stamp: 'tk 2/14/2000 14:28'!
oldFile

	^self oldFileFrom: (FileDirectory default)! !

!StandardFileMenu class methodsFor: 'standard file operations' stamp: 'jmv 1/21/2013 22:59'!
oldFileFrom: aDirectory

	^(self oldFileMenu: aDirectory)
		startUpWithCaption: 'Select a File:'! !

!StandardFileMenu class methodsFor: 'standard file operations' stamp: 'tk 2/14/2000 14:27'!
oldFileStreamFrom: aDirectory

	| sfmResult fileStream |
	sfmResult _ self oldFileFrom: aDirectory.
	sfmResult ifNil: [^nil].
	fileStream _ sfmResult directory oldFileNamed: sfmResult name.
	[fileStream isNil] whileTrue:
		[sfmResult _ self oldFileFrom: aDirectory.
		sfmResult ifNil: [^nil].
		fileStream _ sfmResult directory oldFileNamed: sfmResult name].
	^fileStream
! !


!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
directory

	^directory! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
directory: aDirectory

	^directory _ aDirectory! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
name

	^name! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'acg 4/15/1999 08:43'!
name: aString

	^name _ aString! !

!StandardFileMenuResult methodsFor: 'accessing' stamp: 'sw 6/9/1999 11:50'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' with directory: '.
	directory printOn: aStream.
	aStream nextPutAll: ' name: '.
	name printOn: aStream

"StandardFileMenu oldFile"! !

!StandardFileMenuResult methodsFor: 'testing' stamp: 'acg 4/15/1999 09:05'!
isCommand

	^name isNil! !

!StandardFileMenuResult methodsFor: 'testing' stamp: 'acg 4/15/1999 20:57'!
isDirectory

	^name = ''! !

!StandardFileMenuResult methodsFor: 'private' stamp: 'acg 4/15/1999 08:42'!
directory: aDirectory name: aString

	directory _ aDirectory.
	name _ aString.
	^self! !


!StandardFileMenuResult class methodsFor: 'instance creation' stamp: 'acg 4/15/1999 08:42'!
directory: aDirectory name: aString

	^super new directory: aDirectory name: aString! !

