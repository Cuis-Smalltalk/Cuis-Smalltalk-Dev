'From Cuis 4.2 of 25 July 2013 [latest update: #2855] on 1 August 2016 at 12:34:12 pm'!

!FileSystemEntry methodsFor: 'comparing' stamp: 'jmv 8/1/2016 12:33:19'!
= aFileEntry
	| isCaseSensitive myDrive otherDrive theirs mine |
	self class = aFileEntry class ifFalse: [ ^false ].

	isCaseSensitive _ self fileAccessor isCaseSensitive.

	"Check for drive nil or same."
	myDrive _ self drive.
	otherDrive _ aFileEntry drive.
	isCaseSensitive
		ifTrue: [ self drive = aFileEntry drive ifFalse: [ ^false ]]
		ifFalse: [
			myDrive isNil = otherDrive isNil ifFalse: [ ^false ].		"only one of them is nil"
			myDrive ifNotNil: [											"none is nil"
				(myDrive sameAs: otherDrive) ifFalse: [ ^false ]]].

	"Check for all path components same."
	mine _ self pathComponents.
	theirs _ aFileEntry pathComponents.

	isCaseSensitive ifTrue: [
		^mine = theirs ].

	mine size = theirs size ifFalse: [
		^false ].

	mine with: theirs do: [ :m :t |
		(m sameAs: t) ifFalse: [ ^false ]].

	^ true! !

