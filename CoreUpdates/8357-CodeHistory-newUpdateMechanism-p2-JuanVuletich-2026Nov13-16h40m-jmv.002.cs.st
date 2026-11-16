'From Cuis7.9 [latest update: #8356] on 13 November 2026 at 4:45:05 pm'!

!CodeHistory methodsFor: 'add / update' stamp: 'jmv 13/Nov/2026, 7:40:44 pm (UTC)'!
addMethodVersion: newMethod libraryIndex: libIndex positionIndex: posIndex oldVersionOrNil: oldMethod
	"newMethod may be
	- entirely new (oldMethod is nil)
	- replacement for an existing method: oldMethod
	- re-addition of a previously existing and removed method (oldMethod is nil)
	Note: newMethod is still not installed, the old method, if existing is still installed"

	| newItem prevItem |
	
	"If re-added, take the entry for the removal. We don't have the old method, but its entry
	in CodeHistory was stored in removals. Retrieve it and remove it from removals."
	prevItem := oldMethod
		ifNotNil: [ oldMethod historyPointer ]
		ifNil: [ removals removeKey: newMethod asMethodReference ifAbsent: [0] ].
	
	"Add new entry to CodeHistory"
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If method already in CodeHistory, update its previous entry to point to new entry"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Make method point to the entry just added"
	newMethod historyPointer: newItem.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 13/Nov/2026, 7:42:39 pm (UTC)'!
writeToChangesFile: sourceStr withPreamble: preambleBlock oldMethodOrNil: existingMethod
	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position libIndex |

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].

	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		file padSpacesTo: SourceFiles pointerScaleForWriting.
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		
		
		libIndex := 16rFFFFFFFE . "Changes"
		CodeHistory current addMethodVersion: self libraryIndex: libIndex positionIndex: position oldVersionOrNil: existingMethod.
		
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].! !


!CodeHistory class methodsFor: 'class initialization' stamp: 'jmv 13/Nov/2026, 7:44:18 pm (UTC)'!
initialize
	Current := self new.! !

!methodRemoval: CodeHistory #initializeNotifications stamp: 'jmv 13/Nov/2026, 7:44:54 pm (UTC)'!
CodeHistory removeSelector: #initializeNotifications!
!methodRemoval: CodeHistory #addMethodVersion:oldVersionOrNil: stamp: 'jmv 13/Nov/2026, 7:42:54 pm (UTC)'!
CodeHistory removeSelector: #addMethodVersion:oldVersionOrNil:!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

SystemChangeNotifier uniqueInstance removeActionsWithReceiver: CodeHistory current.!

