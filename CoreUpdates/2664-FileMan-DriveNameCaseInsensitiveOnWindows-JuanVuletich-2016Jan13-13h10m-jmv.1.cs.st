'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 13 January 2016 at 1:33:09.898947 pm'!

!FmEntry methodsFor: 'comparing' stamp: 'jmv 1/13/2016 13:32'!
= aFileEntry
	| isCaseSensitive myDrive otherDrive |
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
	self pathComponents with: aFileEntry pathComponents do: [ :mine :other |
		isCaseSensitive
			ifTrue: [ mine = other ifFalse: [ ^false ]]
			ifFalse: [ (mine sameAs: other) ifFalse: [ ^false ]]].

	^ true! !

!FmEntry methodsFor: 'private' stamp: 'jmv 1/13/2016 13:10'!
initValuesFromParent
	| targets target |
	self ensureParent.
	self parent ifNil: [ ^self ].

	targets := self isDirectory ifTrue: [ self parent directories ] ifFalse: [ self parent files ].
	target := targets detect: [ :each | each = self ] ifNone: [
		^ self ].

	self initValuesFrom: target! !

