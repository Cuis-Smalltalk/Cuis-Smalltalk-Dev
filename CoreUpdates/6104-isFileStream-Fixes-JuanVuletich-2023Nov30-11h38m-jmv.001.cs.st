'From Cuis 6.0 [latest update: #6104] on 30 November 2023 at 11:40:25 am'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 11/28/2023 16:45:24'!
currentChangesName
	"Answer the full path to the version-stable source code currently in use.
	Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)"

	^SourceFiles second ifNotNil: [ :changes |
		changes isFileStream
			ifTrue: [ changes name ]]! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 11/28/2023 16:45:58'!
currentSourcesName
	"Answer the full path to the version-stable source code currently in use
	Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)"

	^SourceFiles first ifNotNil: [ :sources |
		sources isFileStream
			ifTrue: [ sources name ]]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 11/28/2023 16:47:04'!
forceChangesToDisk
	"Just flush the buffer and trust the OS to do its job."

	"Expensive and not worth doing, esp. in Windows with antivirus active, when installing large packages"
	ChangeSet notInstallOrTestRun ifTrue: [
		SourceFiles second ifNotNil: [ :changes |
			changes isFileStream ifTrue: [ changes flush ]]].! !

