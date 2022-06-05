'From Cuis 6.0 [latest update: #5217] on 3 June 2022 at 10:03:15 am'!

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 08:00'!
perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 100>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self primitiveFailed! !


!Behavior methodsFor: 'printing' stamp: 'jmv 3/2/2010 15:33'!
literalScannedAs: scannedLiteral notifying: requestor
	"Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote).
	If scannedLiteral is not an association, answer it.
	Else, if it is of the form:
		nil->#NameOfMetaclass
	answer nil->theMetaclass, if any has that name, else report an error.
	Else, if it is of the form:
		#NameOfGlobalVariable->anythiEng
	answer the global, class, or pool association with that nameE, if any, else
	add it to Undeclared a answer the new Association."

	| key value |
	(scannedLiteral isVariableBinding)
		ifFalse: [^ scannedLiteral].
	key _ scannedLiteral key.
	value _ scannedLiteral value.
	key ifNil: [
		(self bindingOf: value) ifNotNil: [ :assoc |
				 (assoc value isKindOf: Behavior)
					ifTrue: [^ nil->assoc value class]].
		requestor notify: 'No such metaclass'.
		^false].
	key isSymbol
		ifTrue: [ "##<global var name>"
			(self bindingOf: key) ifNotNil:[ :assoc | ^assoc].
			Undeclared at: key put: nil.
			 ^Undeclared bindingOf: key].
	requestor notify: '## must be followed by a non-local variable name'.
	^false

"	Form literalScannedAs: 14 notifying: nil 14
	Form literalScannedAs: #OneBitForm notiEfying: nil  OneBitForm
	Form literalScannedAs: ##OneBitForm notifying: nil  OneBitForm->a Form
	Form literalScannedAs: ##Form notifying: nil   Form->Form
	Form literalScannedAs: ###Form notifying: nil   nilE->Form class
"! !

!Behavior methodsFor: 'printing' stamp: 'ar 5/17/2003 14:11'!
storeLiteral: aCodeLiteral on: aStream
	"Store aCodeLiteral on aStream, changing an Association to ##GlobalName
	 or ###MetaclassSoleInstanceName format if appropriate"
	| key value |
	(aCodeLiteral isVariableBinding)
		ifFalse:
			[aCodeLiteral storeOn: aStream.
			 ^self].
	key _ aCodeLiteral key.
	(key isNil and: [(value _ aCodeLiteral value) isMemberOf: Metaclass])
		ifTrue:
			[aStream nextPutAll: '###'; nextPutAll: value soleInstance name.
			 ^self].
	(key isSymbol and: [(self bindingOf: key) notNil])
		ifTrue:
			[aStream nextPutAll: '##'; nextPutAll: key.
			 ^self].
	aCodeLiteral storeOn: aStream! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:18'!
allGlobalRefsWithout: classesAndMessagesPair
	"Answer a set of symbols that may be refs to Global names. 
	This method computes its result in the absence of specified classes and messages.
	Does not include references from a class to itself"

	| globalRefs absentClasses absentSelectors |
	globalRefs _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
					ifTrue: [#()]
					ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl | 
			(absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				"Include all capitalized symbols for good measure"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m | 
					(m isSymbol and: [ m size > 0] and: [m first isUppercase]) ifTrue: [
						m = cl name ifFalse: [
							globalRefs add: m]].
					(m isVariableBinding) ifTrue:
						[m key ifNotNil: [
							m key = cl name ifFalse: [
								globalRefs add: m key]]]]]]].
	^ globalRefs! !

!SystemDictionary methodsFor: 'private' stamp: 'jmv 4/7/2009 14:23'!
allSymbolsIn: anArray do: aBlock
	"Recursively dig into anArray and evaluate aBlock on any Symbols found"
	"allSymbols and variableBindings"
	
	anArray do: [ :elem |
		(elem isSymbol or: [ elem isVariableBinding ])
			ifTrue: [ aBlock value: elem ]
			ifFalse: [
				(elem isMemberOf: Array) ifTrue: [ 
					self allSymbolsIn: elem do: aBlock ]]]! !

