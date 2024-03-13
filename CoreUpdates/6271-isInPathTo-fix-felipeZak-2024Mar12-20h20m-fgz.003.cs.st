'From Cuis6.3 [latest update: #6270] on 12 March 2024 at 8:24:30 pm'!

!DirectoryEntry methodsFor: 'accessing' stamp: 'fgz 3/12/2024 20:21:31'!
isInPathTo: aFileSystemEntry
	| theirPathComponents size |
	size := pathComponents size.
	theirPathComponents := aFileSystemEntry pathComponents.
	theirPathComponents size >= size ifFalse: [
		^false ].
	1 to: size do: [ :index |
		(pathComponents at: index) = (theirPathComponents at: index) ifFalse: [^ false]].
	^ true! !

