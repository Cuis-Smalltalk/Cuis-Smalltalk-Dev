'From Cuis7.9 [latest update: #8082] on 23 July 2026 at 3:14:46 pm'!

!Pragma methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 5:04:45 pm (UTC)'!
isPragma
	^true! !


!Association methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 5:04:59 pm (UTC)'!
isPragma
	^false! !

!Association methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 4:49:42 pm (UTC)'!
isVariableBinding
	^true! !


!Ephemeron methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 4:49:51 pm (UTC)'!
isVariableBinding
	^false! !


!ReadOnlyVariableBinding methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 5:04:52 pm (UTC)'!
isPragma
	^false! !

!ReadOnlyVariableBinding methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 4:50:02 pm (UTC)'!
isVariableBinding
	^true! !


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

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 5:37:07 pm (UTC)'!
at: aKey ifAbsentPut: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue: [
			^propertyOrPragma isPragma
				ifFalse: [propertyOrPragma value]
				ifTrue: [propertyOrPragma]]].
	^method propertyValueAt: aKey put: aBlock value! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 5:31:28 pm (UTC)'!
at: aKey put: aValue
	"Replace the property value or pragma associated with aKey."

	| keyAlreadyExists propertyOrPragma "<Association|Pragma>" |
	keyAlreadyExists := false.
	
	1 to: self basicSize do: [ :i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue: [
			keyAlreadyExists := true.
			propertyOrPragma isPragma
				ifFalse: [ propertyOrPragma value: aValue ]
				ifTrue: [ self basicAt: i put: aValue ]]].
	
	keyAlreadyExists ifFalse: [
		method propertyValueAt: aKey put: aValue ].
	
	^ aValue! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 5:59:24 pm (UTC)'!
keysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isPragma
			ifFalse: [aBlock value: propertyOrPragma key value: propertyOrPragma value]
			ifTrue: [aBlock value: propertyOrPragma keyword value: propertyOrPragma]]! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 5:10:28 pm (UTC)'!
pragmas
	"Answer the raw messages comprising my pragmas."
	| pragmaStream propertyOrPragma "<Association|Message>" |
	pragmaStream := WriteStream on: (Array new: self basicSize).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isPragma ifTrue: [
			pragmaStream nextPut: propertyOrPragma ]].
	^pragmaStream contents! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 23/Jul/2026, 6:02:49 pm (UTC)'!
properties

	| propertyStream propertyOrPragma "<Association|Pragma>" |
	propertyStream := WriteStream on: (Array new: self basicSize * 2).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isPragma ifFalse: [
			propertyStream nextPut: propertyOrPragma key; nextPut: propertyOrPragma value]].
	^IdentityDictionary newFromPairs: propertyStream contents! !

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

!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 23/Jul/2026, 6:05:04 pm (UTC)'!
copyWith: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(#(Association ReadOnlyVariableBinding Pragma)
			includes: aPropertyOrPragma class name) ifFalse: [
		self error: self class name, ' instances should hold only Associations or Pragmas.'].
	copy := self class new: (bs := self basicSize) + 1.
	1 to: bs do: [ :i |
		copy basicAt: i put: (self basicAt: i)].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	^copy
		selector: selector;
		setMethod: method;
		yourself
! !

!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 6:09:01 pm (UTC)'!
hasAtLeastTheSamePropertiesAs: aMethodProperties
	"Answer if the recever has at least the same properties as the argument.
	 N.B. The receiver may have additional properties and still answer true."
	aMethodProperties keysAndValuesDo: [ :k :v |
		v isPragma
			"ifTrue: [Pragmas have already been checked]"
			ifFalse: [
				(self includesKey: k) ifFalse: [^false]]].
	^true! !

!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 23/Jul/2026, 6:01:47 pm (UTC)'!
hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		propertyOrPragma isVariableBinding
			ifTrue: [
				((aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]])
								ifTrue: [^true]].
		propertyOrPragma isPragma
			ifTrue: [
				(propertyOrPragma hasLiteralSuchThat: aBlock)
					ifTrue: [^true]].
		].
	^false! !

!methodRemoval: LookupKey #isVariableBinding stamp: 'jmv 23/Jul/2026, 5:55:53 pm (UTC)'!
LookupKey removeSelector: #isVariableBinding!
!methodRemoval: Pragma #is: stamp: 'jmv 23/Jul/2026, 5:57:13 pm (UTC)'!
Pragma removeSelector: #is:!
!methodRemoval: AdditionalMethodState #includes: stamp: 'jmv 23/Jul/2026, 6:07:10 pm (UTC)'!
AdditionalMethodState removeSelector: #includes:!

!AdditionalMethodState reorganize!
('accessing' at: at:ifAbsent: at:ifAbsentPut: at:put: keysAndValuesDo: method: pragmas properties removeKey:ifAbsent: selector selector: setMethod:)
('properties' propertyKeysAndValuesDo: propertyOrPragmaAt: propertyValueAt: propertyValueAt:ifAbsent: removeKey:)
('copying' copyWith: copyWithout:)
('literals' allLiteralsDo:)
('testing' analogousCodeTo: hasAtLeastTheSamePropertiesAs: hasLiteralSuchThat: includesKey: isEmpty isMethodProperties notEmpty)
!

