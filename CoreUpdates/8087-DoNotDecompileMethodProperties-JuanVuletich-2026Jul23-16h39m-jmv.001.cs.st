'From Cuis7.9 [latest update: #8086] on 23 July 2026 at 4:55:10 pm'!

!MethodNode methodsFor: 'printing' stamp: 'jmv 23/Jul/2026, 7:40:17 pm (UTC)'!
printPropertiesOn: aStream
	properties ifNil: [^self].
	"
	properties propertyKeysAndValuesDo:
		[:prop :val|
		aStream newLine; tab; nextPut: $<.
		prop = #on:in:
			ifTrue:
				[prop keywords with: val do:
					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]
			ifFalse:
				[prop = #on
					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 
					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 
		aStream nextPut: $>]
	"! !


!CompiledMethod methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 7:53:50 pm (UTC)'!
hasLiteralSuchThat: litBlock
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure.
	Note: This will find stuff in method properties that is not strictly method literals, as they are not stored
	in the method source code.
	For instance, see senders of #libraryName and #MinimalKernel01."
	
	| lit |
	
	self withPropertiesDo: [ :properties | (properties hasLiteralSuchThat: litBlock) ifTrue: [ ^true ]]. 

	2 to: self numLiterals + 1 do: [ :index |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true]].
	^false! !


!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 7:54:48 pm (UTC)'!
hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	Note: This will find stuff in method properties that is not strictly method literals, as they are not stored
	in the method source code.
	For instance, see senders of #libraryName and #MinimalKernel01."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		propertyOrPragma isPragma
			ifFalse: [
				((aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]])
								ifTrue: [^true]]
			ifTrue: [
				(propertyOrPragma hasLiteralSuchThat: aBlock)
					ifTrue: [^true]].
		].
	^false! !

