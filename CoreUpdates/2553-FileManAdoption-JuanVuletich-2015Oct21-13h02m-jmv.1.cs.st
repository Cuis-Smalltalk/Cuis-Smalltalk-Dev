'From Cuis 4.2 of 25 July 2013 [latest update: #2547] on 21 October 2015 at 1:04:19.560976 pm'!
!classDefinition: #FmDirectoryEntry category: #'FileMan-Core'!
FmEntry subclass: #FmDirectoryEntry
	instanceVariableNames: 'children '
	classVariableNames: 'CurrentDirectory ImageDirectory VMDirectory '
	poolDictionaries: ''
	category: 'FileMan-Core'!

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 10/21/2015 13:04'!
entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
		
	YAW (YetAnotherWarning): On Unix, directoryFullPath = '' means 
		FmDirectoryEntry currentDirectory
		FmDirectoryEntry smalltalkImageDirectory
			(NOT SURE!!!!!!!!)
	In Windows, it means 'give me the disk drives'...
	On Mac?
	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self primLookupEntryIn: (parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName]) index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				isDirectory
					ifTrue: [entry _ FmDirectoryEntry new]
					ifFalse: [
						entry _ FmFileEntry new.
						entry fileSize: (entryArray at: 5) ].
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ]
				ifNil: [
					entry pathName: entry name ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!classDefinition: #FmDirectoryEntry category: #'FileMan-Core'!
FmEntry subclass: #FmDirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: 'CurrentDirectory ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'FileMan-Core'!
