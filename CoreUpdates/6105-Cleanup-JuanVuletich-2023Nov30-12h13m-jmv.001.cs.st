'From Cuis6.0 [latest update: #6104] on 30 November 2023 at 12:17:01 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 11/30/2023 11:59:40'!
versionString
	"Answer the version of this release."

	^SystemVersion current versionString! !


!SystemVersion methodsFor: 'accessing' stamp: 'jmv 11/30/2023 12:10:06'!
versionString
	^String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: versionMajor;
			nextPut: $.;
			print: versionMinor ].! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 11/30/2023 12:00:14'!
errorReportOn: strm
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
 	strm print: Date today; space; print: Time now; newLine.
	strm newLine.
	strm nextPutAll: 'VM: ';
		nextPutAll: Smalltalk platformName;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vmVersion;
		newLine.
	strm nextPutAll: 'Image: ';
		nextPutAll: Smalltalk versionString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: ']';
		newLine.
	strm newLine.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt := 0.  startPos := strm position.
	aContext := self.
	[aContext notNil and: [(cnt := cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		aContext printDetails: strm.	"variable values"
		strm newLine.
		aContext := aContext sender].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	aContext := self.
	cnt := 0.
	[aContext == nil] whileFalse: [
		cnt := cnt + 1.
		cnt = Debugger defaultDebugStackSize ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine ].
		strm print: aContext; newLine.  "just class>>selector"	

		strm position > (startPos+40000) ifTrue: [strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 100 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].
		aContext := aContext sender].
! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 11/30/2023 12:14:17'!
systemInformationString
	"Identify software version"
	^ self versionString, String newLineString,
		self lastUpdateString, String newLineString,
		'Running at :', self imageName.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 11/30/2023 12:14:27'!
timeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime := DateAndTime now.
	aStream
		nextPutAll: 'From ';
		nextPutAll: self versionString;
		nextPutAll: ' [';
		nextPutAll: self lastUpdateString;
		nextPutAll: '] on '.
	dateTime date printOn: aStream.
	aStream
		nextPutAll: ' at '.
	dateTime time print24: false showSeconds: true on: aStream! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 11/30/2023 12:15:05'!
writeConversionMethodIn: newClass fromInstVars: oldList to: newList renamedFrom: oldName
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify.  If method exists, append new info into the end."

	| code newOthers oldOthers copied newCode |

	newOthers := newList asOrderedCollection "copy".
	oldOthers := oldList asOrderedCollection "copy".
	copied := OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code := WriteStream on: (String new: 500).
	code newLine; newLine; tab; nextPutAll: '"From ', Smalltalk versionString, ' [', Smalltalk lastUpdateString;
			nextPutAll: '] on ', Date today printString, '"'; newLine.
	code tab; nextPutAll: '"These variables are automatically stored into the new instance: '.
	code nextPutAll: copied asArray printString; nextPut: $.; newLine.
	code tab; nextPutAll: 'Test for this particular conversion.'; 
		nextPutAll: '  Get values using expressions like (varDict at: ''foo'')."'; newLine; newLine.
	newOthers isEmpty & oldOthers isEmpty & oldName isNil ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	newOthers notEmpty ifTrue: [
		code tab; nextPutAll: '"New variables: ', newOthers asArray printString, 
			'.  If a non-nil value is needed, please assign it."'; newLine].
	oldOthers notEmpty ifTrue: [
		code tab; nextPutAll: '"These are going away ', oldOthers asArray printString, 
			'.  Possibly store their info in some other variable?"'; newLine].
	oldName ifNotNil: [
		code tab; nextPutAll: '"Test for instances of class ', oldName, '.'; newLine.
		code tab; nextPutAll: 'Instance vars with the same name have been moved here."'; newLine.
		].
	code tab; nextPutAll: '"Move your code above the ^ super...  Delete extra comments."'; newLine. 

	(newClass includesSelector: #convertToCurrentVersion:refStream:) 
		ifTrue: ["append to old methods"
			newCode := (newClass sourceCodeAt: #convertToCurrentVersion:refStream:),
				code contents]
		ifFalse: ["new method"
			newCode := 'convertToCurrentVersion: varDict refStream: smartRefStrm',
				code contents, 
				'	^ super convertToCurrentVersion: varDict refStream: smartRefStrm'].
	newClass compile: newCode classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."! !


!SystemVersion methodsFor: 'printing' stamp: 'jmv 11/30/2023 12:02:38'!
printOn: stream
	stream
		nextPutAll: self versionString;
		nextPutAll: ' update ' , self highestUpdate printString! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 11/30/2023 12:15:41'!
installNewUpdates: updatesFileDirectory

	| warnUser |
	self newUpdates: updatesFileDirectory do: [ :each |
		self install: each ].	

	warnUser := false.
	CodePackage installedPackages keysAndValuesDo: [ :pckName :package |
		package fullFileName ifNotNil: [ :fullFileName | | codePackageFile |
			codePackageFile := CodePackageFile onFileEntry: fullFileName asFileEntry.
			(codePackageFile provides isLaterThan: package provides) ifTrue: [
				warnUser := true.
				package hasUnsavedChanges
					ifTrue: [ ('Package: ', pckName, '. Package has unsaved changes, but there is a newer version of the file.') print ]
					ifFalse: [ ('Package: ', pckName, '. There is a newer version than the currently loaded.') print ]]]].
	warnUser
		ifTrue: [
			self inform: Smalltalk versionString, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk systemInformationString print ]! !

!methodRemoval: SystemVersion #version stamp: 'jmv 11/30/2023 12:16:21'!
SystemVersion removeSelector: #version!
!methodRemoval: FileSystemEntry #version stamp: 'jmv 11/30/2023 12:16:23'!
FileSystemEntry removeSelector: #version!
!methodRemoval: SystemDictionary #version stamp: 'jmv 11/30/2023 12:15:58'!
SystemDictionary removeSelector: #version!
