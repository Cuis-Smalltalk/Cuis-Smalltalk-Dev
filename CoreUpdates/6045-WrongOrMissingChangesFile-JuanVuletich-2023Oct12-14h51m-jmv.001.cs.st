'From Cuis 6.0 [latest update: #6044] on 12 October 2023 at 2:54:01 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/12/2023 14:53:35'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems.
	Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes entry msg snapshotChunk entryPathName |

	"Do not open source files if internalized (i.e. notNil)"
	sources := SourceFiles at: 1.
	sources ifNil: [
		entry := Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry := Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources := [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
				msg := 'Cuis cannot locate the sources file named ' , entry pathName, '.', String newLineString,
					'Please check that the file is properly named and is in the same directory as this image.'.
			self logStartupError: msg.
		].

	"Do not open source files if internalized (i.e. notNil)"
	changes := SourceFiles at: 2.
	changes ifNil: [
		entry := Smalltalk defaultChangesName asFileEntry.
		entryPathName := entry pathName.
		"Read the last snapshot chunk. Validate that the .changes file is appropriate for this image."
		entry exists
			ifTrue: [
				entry readStreamDo: [ :changesFile |
					changesFile position: Smalltalk lastQuitLogPosition.
					snapshotChunk := changesFile nextChunk ].
				((snapshotChunk beginsWith: Smalltalk tagHeader) and: [
					snapshotChunk includesSubString: 'priorSource: ']) ifFalse: [
						(Preferences at: #warnIfNoChangesFile) ifTrue: [
							self logStartupError: 'Incorrect changes file: ', entryPathName.
							self logStartupError: 'Missing code will be decompiled'.
							self logStartupError: 'New source code will not be saved' ].
						entry := nil ]]
			ifFalse: [
				(Preferences at: #warnIfNoChangesFile) ifTrue: [
					self logStartupError: 'Could not find changes file: ', entryPathName.
					self logStartupError: 'Missing code will be decompiled'.
					self logStartupError: 'New source code will not be saved' ].
				entry := nil ].
		entry ifNotNil: [
			changes := [ entry appendStream ]
				on: FileWriteError
				do: [
					self logStartupError: 'Could not write to changes file: ', entryPathName.
					self logStartupError: 'Changes file will not be used.'.
					self logStartupError: 'Missing code will be decompiled'.
					self logStartupError: 'New source code will not be saved'.
					nil ]]].
	ChangesInitialFileSize := changes ifNotNil: [ changes position ].

	SourceFiles := Array with: sources with: changes.! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 10/12/2023 14:35:55'!
logStartupError: stuff
	"
	Smalltalk logStartupError: 'stuff'
	"
	[
		Transcript
			logToFile: true;
			logToStdout: true.
		stuff print
	] forkAt: Processor activePriority - 1! !

