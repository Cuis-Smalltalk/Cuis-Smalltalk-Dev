'From Cuis6.3 [latest update: #6352] on 10 May 2024 at 4:36:05 pm'!

!FileIOAccessor methodsFor: 'utilities' stamp: 'bp 5/10/2024 11:25:01'!
extensionFor: pathName 
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.
	
	FileIOAccessor default extensionFor: 'writings.txt'
	FileIOAccessor default extensionFor: 'folder.ext/file'
	FileIOAccessor default extensionFor: 'optionalstuff.pck.st'
	FileIOAccessor default extensionFor: 'code.cs.st'
	FileIOAccessor default extensionFor: 'code.cs'
	"
	| index |
	{ '.cs.st' . '.pck.st'. '.ws.st' } do: [ :specialExtension |
		(pathName endsWith: specialExtension)
			ifTrue: [ ^specialExtension copyFrom: 2 to: specialExtension size ]].
	index := pathName
				findLast: [ :c | c = $.].
	^ (index = 0 or: [ pathName indexOfLastPathSeparator > index ])
		ifTrue: ['']
		ifFalse: [pathName copyFrom: index + 1 to: pathName size]! !

