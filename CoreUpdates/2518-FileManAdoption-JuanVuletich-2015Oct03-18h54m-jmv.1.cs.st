'From Cuis 4.2 of 25 July 2013 [latest update: #2517] on 3 October 2015 at 6:54:32.586694 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/3/2015 17:41'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount tempPathName oldChangesLocalName oldChangesPathName |
	tempPathName _ FmDirectoryEntry smalltalkImageDirectory pathName, '/ST80.temp'.
	tempPathName asFileEntry forceWriteStream: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class | 
					bar value: (classCount _ classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition _ f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	tempPathName asFileEntry rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: (FileStream oldFileNamed: oldChangesPathName).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 10/3/2015 17:09'!
logError: errMsg inContext: aContext to: localFileName
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	[
		(FmDirectoryEntry smalltalkImageDirectory pathName, '/', localFileName) asFileEntry forceWriteStream: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: Error do: [] "avoid recursive errors"! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/3/2015 16:37'!
compareTallyIn: beforeFileName to: afterFileName
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer beforeDict a afterDict allKeys before after diff |
	beforeDict _ Dictionary new.
	beforeFileName asFileEntry readStream: [ :s |
		[s atEnd ] whileFalse: [
			a _ Array readFrom: s nextLine.
			beforeDict at: a first put: a allButFirst ]].

	afterDict _ Dictionary new.
	afterFileName asFileEntry readStream: [ :s |
		[ s atEnd ] whileFalse: [
			a _ Array readFrom: s nextLine.
			afterDict at: a first put: a allButFirst ]].

	answer _ String streamContents: [ :stream |
		allKeys _ (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asArray sort.
		allKeys do: [ :each |
			before _ beforeDict at: each ifAbsent: [#(0 0 0)].
			after _ afterDict at: each ifAbsent: [#(0 0 0)].
			diff _ before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
			diff = #(0 0 0) ifFalse: [
				stream nextPutAll: each, '  ', diff printString; newLine.
			].
		]].

	TextModel new contents: answer; openLabel: 'space diffs'! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/3/2015 16:38'!
saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', aFileName) asFileEntry forceWriteStream: [ :stream |
		results do: [:each | 
				stream nextPutAll: each analyzedClassName asString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 10/3/2015 16:32'!
exampleWithPictures
	"
	DataStream exampleWithPictures
	"
	| f result |
	f _ Form fromUser.
	'Test-Picture' asFileEntry writeStream: [ :file |
		file binary.
		(DataStream on: file) nextPut: f ].

	'Test-Picture' asFileEntry readStream: [ :file |
		file binary.
		result _ (DataStream on: file) next ].

	result display! !

!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 10/3/2015 16:34'!
testWith: anObject
	"As a test of DataStream/ReferenceStream, write out anObject and read it back.
	11/19/92 jhm: Set the file type. More informative file name.
	DataStream testWith: 'hi'
	ReferenceStream testWith: 'hi'
	"
	| result |

	(self name, ' test') asFileEntry writeStream: [ :file |
		file binary.
		(self on: file) nextPut: anObject ].

	(self name, ' test') asFileEntry readStream: [ :file |
		file binary.
		result _ (self on: file) next ].

	^ result! !

!methodRemoval: FileStream class #fileNamed:!
FileStream class removeSelector: #fileNamed:!
!methodRemoval: DataStream class #fileNamed:!
DataStream class removeSelector: #fileNamed:!
!methodRemoval: FileDirectory #fileNamed:!
FileDirectory removeSelector: #fileNamed:!
