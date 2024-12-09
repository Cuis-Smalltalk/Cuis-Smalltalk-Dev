'From Cuis7.3 [latest update: #6899] on 9 December 2024 at 1:09:02 pm'!

!FileSystemEntry methodsFor: 'comparing' stamp: 'HAW 10/29/2024 16:40:50'!
= aFileEntry
	| isCaseSensitive myDrive otherDrive theirs mine |

	self == aFileEntry ifTrue: [ ^ true ].
	self class == aFileEntry class ifFalse: [ ^false ].

	isCaseSensitive := self fileAccessor isCaseSensitive.

	"Check for drive nil or same."
	myDrive := self drive.
	otherDrive := aFileEntry drive.
	isCaseSensitive
		ifTrue: [ self drive = aFileEntry drive ifFalse: [ ^false ]]
		ifFalse: [
			myDrive isNil = otherDrive isNil ifFalse: [ ^false ].		"only one of them is nil"
			myDrive ifNotNil: [											"none is nil"
				(myDrive sameAs: otherDrive) ifFalse: [ ^false ]]].

	"Check for all path components same."
	mine := self pathComponents.
	theirs := aFileEntry pathComponents.

	isCaseSensitive ifTrue: [
		^mine = theirs ].

	mine size = theirs size ifFalse: [
		^false ].

	^mine 
		with: theirs 
		allSatisfy: [ :m :t | m sameAs: t ]! !

