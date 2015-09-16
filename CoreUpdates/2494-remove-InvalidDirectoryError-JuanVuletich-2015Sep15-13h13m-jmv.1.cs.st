'From Cuis 4.2 of 25 July 2013 [latest update: #2493] on 15 September 2015 at 1:15:04.722077 pm'!

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 9/15/2015 13:14'!
entries
	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries index done entryArray |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	[ done ] whileFalse: [
		entryArray _ self primLookupEntryIn: pathName index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ #() ].
		entryArray
			ifNil: [ done _ true ]
			ifNotNil: [ entries addLast: (DirectoryEntry fromArray: entryArray directory: self) ].
		index _ index + 1 ].

	^ entries asArray! !

!classRemoval: #InvalidDirectoryError!
Smalltalk removeClassNamed: #InvalidDirectoryError!
