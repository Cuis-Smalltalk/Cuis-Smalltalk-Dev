'From Cuis 5.0 [latest update: #4045] on 21 February 2020 at 12:41:15 am'!

!Integer methodsFor: 'printing' stamp: 'jmv 2/20/2020 16:52:31'!
printOn: aStream length: minimum zeroPadded: zeroFlag
	"
	7 printOn: Transcript length: 4 zeroPadded: true. Transcript newLine.
	"
	self printOn: aStream base: 10 length: minimum padded: zeroFlag! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/20/2020 16:51:14'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp asString; newLine.
			self forceChangesToDisk ]].
	oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
	oldUserChanges exists ifTrue: [
		directory _ oldUserChanges parent.
		oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
		oldUserChanges rename: oldUserChangesName ].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp asString; newLine ].
	StartupStamp _ nil! !


!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 2/20/2020 16:40:54'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in senders for more details"

	| baseName version i j |
	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		baseName _ b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) = $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]! !


!DirectoryEntry methodsFor: 'services' stamp: 'jmv 2/21/2020 00:38:45'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version candidate |
	files _ self fileNamesMatching: (baseFileName,'*.', extension).
	splits _ files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits _ splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version _ 1 ]
			ifFalse: [ version _ (splits last at: 2) + 1 ].
	candidate _ (baseFileName, fileNameCoda, '.', (String streamContents: [ :strm | version printOn: strm length: 3 zeroPadded: true ]), '.', extension) asFileName.
	^ candidate! !

