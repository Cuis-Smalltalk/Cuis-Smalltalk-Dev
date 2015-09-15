'From Cuis 4.2 of 25 July 2013 [latest update: #2489] on 14 September 2015 at 12:20:22.087209 pm'!
!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'defaultDirectoryPath slash drives '
	classVariableNames: 'Default '
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 9/14/2015 12:19'!
entriesIn: directoryFullPath from: parentEntryOrNil
	"
	FmFileIOAccessor default entriesIn: '' from: nil
	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self lookupEntryIn: directoryFullPath index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory
					ifTrue: [FmDirectoryEntry new]
					ifFalse: [FmFileEntry new].
				entry isDirectory: isDirectory.
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				entry fileSize: (entryArray at: 5).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/14/2015 12:17'!
drives
	"
	FmFileIOAccessor default drives
	"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [#()]
			ifFalse: [(self entriesIn: '' from: nil) collect: [ :each | each name]]].
	^drives! !

!FmFileIOAccessor methodsFor: 'accessing' stamp: 'jmv 9/14/2015 12:04'!
slash
	slash ifNil: [ slash _ self pathNameDelimiter asString ].
	^slash! !

!classDefinition: #FmFileIOAccessor category: #'FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'defaultDirectoryPath slash drives'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'FileMan-Core'!
