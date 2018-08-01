'From Cuis 5.0 of 7 November 2016 [latest update: #3381] on 27 July 2018 at 2:21:57 pm'!

!ChangeList class methodsFor: 'public access' stamp: 'jmv 7/27/2018 14:20:49'!
browseRecentLogOn: origChangesFileName startingFrom: initialPos 
	"Prompt with a menu of how far back to go when browsing a changes file."

	| end banners positions pos chunk i |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
		banners _ OrderedCollection new.
		positions _ OrderedCollection new.
		end _ changesFile size.
		pos _ initialPos.
		[pos = 0
			or: [banners size > 20]]
			whileFalse: [
				changesFile position: pos.
				chunk _ changesFile nextChunk.
				i _ chunk indexOfSubCollection: 'priorSource: ' startingAt: 1.
				i > 0
					ifTrue: [
						positions addLast: pos.
						banners addLast: (chunk copyFrom: 5 to: i - 2).
						pos _ Number readFrom: (chunk copyFrom: i + 13 to: chunk size)]
					ifFalse: [
						positions addLast: 0.
						banners addLast: 'Whole file'.
						pos _ 0]].
	].
	banners size = 0 ifTrue: [^ self inform: 
'this image has never been saved
since changes were compressed'].
	pos _ (SelectionMenu labelList: banners selections: positions)
				startUpWithCaption: 'Browse as far back as...'.
	pos
		ifNil: [^ self].
	self browseRecent: end - pos on: origChangesFileName! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 7/27/2018 14:21:47'!
putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	Smalltalk assureStartupStampLogged.
	(SourceFiles notNil and: [(file _ SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString _ RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			preambleBlock value: stream.  "Write the preamble"
			stream nextChunkPut: sourceStr.
			stream nextChunkPut: ' ' ]]! !

