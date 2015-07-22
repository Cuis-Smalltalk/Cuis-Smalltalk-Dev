'From Cuis 4.2 of 25 July 2013 [latest update: #2415] on 11 July 2015 at 3:44:21.032618 pm'!

!Morph methodsFor: 'accessing - properties' stamp: 'KenD 7/9/2015 19:21'!
nameForWorkspace
	"Answer a name suitable for a Workspace variable"
	| displayName |
	displayName := self name.
	^ displayName 
		ifNotNil: [ | name |
			name := displayName asIdentifier: false.
			(name size < 1)
			ifTrue: [ self class name asLowercase , self identityHash asString ]
			ifFalse: [ name at: 1 put:  (name at: 1) asLowercase. name ]
		] 
		ifNil: [ self class name asLowercase , self identityHash asString ]! !

