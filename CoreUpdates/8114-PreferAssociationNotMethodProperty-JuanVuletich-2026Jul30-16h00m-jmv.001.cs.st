'From Cuis7.9 [latest update: #8113] on 30 July 2026 at 4:08:05 pm'!

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 23/Jul/2026, 6:06:52 pm (UTC)'!
propertyValueAt: propName put: propValue
	"Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property."
	
	self 
		withPropertiesDo: [ :properties |
			(properties propertyOrPragmaAt: propName)
				ifNotNil: [ :propertyOrPragma |
					propertyOrPragma isPragma
						ifFalse: [ propertyOrPragma value: propValue ]
						ifTrue: [ self error: 'I already have a Pragma with requested name' ]]
				ifNil: [
					self penultimateLiteral: (properties
						copyWith: (Association
							key: propName asSymbol
							value: propValue))].
			^propValue ] 
		ifSelector: [ :selector |
			self penultimateLiteral: ((AdditionalMethodState
									selector: selector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
			^propValue].! !


!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 6:00:49 pm (UTC)'!
at: aKey ifAbsent: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue: [
			^propertyOrPragma isPragma
				ifFalse: [propertyOrPragma value]
				ifTrue: [propertyOrPragma]]].
	^aBlock value! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 5:59:24 pm (UTC)'!
keysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isPragma
			ifFalse: [aBlock value: propertyOrPragma key value: propertyOrPragma value]
			ifTrue: [aBlock value: propertyOrPragma keyword value: propertyOrPragma]]! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 6:00:26 pm (UTC)'!
removeKey: aKey ifAbsent: aBlock
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."
	
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isPragma
				ifFalse: [propertyOrPragma key]
				ifTrue: [propertyOrPragma keyword])
			== aKey ifTrue:
			[^method removeProperty: aKey]].
	^aBlock value! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 5:14:51 pm (UTC)'!
setMethod: aMethod
	| propertyOrPragma "<Association|Pragma>" |
	method := aMethod.
	1 to: self basicSize do:
		[ :i |
		(propertyOrPragma := self basicAt: i) isPragma ifTrue: [
			propertyOrPragma setMethod: aMethod]]! !

!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 23/Jul/2026, 6:04:10 pm (UTC)'!
propertyKeysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isPragma ifFalse: [
			aBlock value: propertyOrPragma key value: propertyOrPragma value]]! !

!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 23/Jul/2026, 6:04:33 pm (UTC)'!
propertyValueAt: aKey ifAbsent: aBlock
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isPragma not
				 and: [propertyOrPragma key == aKey]) ifTrue: [
			^propertyOrPragma value]].
	^aBlock value! !

!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 30/Jul/2026, 6:19:50 pm (UTC)'!
copyWith: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(#(Association Pragma)
			includes: aPropertyOrPragma class name) ifFalse: [
		self error: self class name, ' instances should hold only Method Properties (Association) and Pragmas.'].
	copy := self class new: (bs := self basicSize) + 1.
	1 to: bs do: [ :i |
		copy basicAt: i put: (self basicAt: i)].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	^copy
		selector: selector;
		setMethod: method;
		yourself
! !

!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 23/Jul/2026, 3:08:47 pm (UTC)'!
copyWithout: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which no longer includes aPropertyOrPragma
	if not found, answer self."
	| bs copy offset |
	copy := self class new: (bs := self basicSize) - 1.
	offset := 0.
	1 to: bs do:
		[ :i |
		(self basicAt: i) == aPropertyOrPragma
			ifTrue: [offset := 1]
			ifFalse: [copy basicAt: i - offset put: (self basicAt: i)]].
	offset = 1
		ifTrue: [ 
			^copy
				selector: selector;
				setMethod: method;
				yourself ]
		ifFalse: [ ^self ].! !

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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
CompiledMethod allInstancesDo: [ :cm |
	| p pp k |
	p := cm properties.
	1 to: p size do: [ :i |
		pp := p basicAt: i.
		(pp class == MethodProperty and: [ #(libraryName methodNode) includes: (k := pp key) ]) ifTrue: [
			p basicAt: i put: (Association key: k value: pp value) ]]].
Smalltalk garbageCollect. Smalltalk garbageCollect.
MethodProperty allInstances.!

