'From Cuis7.9 [latest update: #8225] on 24 September 2026 at 2:35:49 pm'!

!Behavior methodsFor: 'method dictionary' stamp: 'FJG 27/Oct/2025, 3:00:04 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel05ClassAndMethodCreation}!
lookupSelector: selector ifAbsent: alternativeBlock
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Answer the value of alternativeBlock if no method is found."
	
	| lookupClass |
	
	lookupClass := self.
	[lookupClass == nil]
		whileFalse: 
			[(lookupClass includesSelector: selector)
				ifTrue: [^ lookupClass compiledMethodAt: selector].
			lookupClass := lookupClass superclass].
	
	^ alternativeBlock value! !


!ProtoObject methodsFor: 'system primitives' stamp: 'FJG 21/Sep/2026, 3:24:57 pm (UTC)'!
cannotInterpret: aMessage 
	"Handle the fact that there was an attempt to send the given message to the receiver but a null methodDictionary was encountered while looking up the message selector.  Hopefully this is the result of encountering a stub for a swapped out class which induces this exception on purpose."

	"If this is the result of encountering a swap-out stub, then simulating the lookup in Smalltalk should suffice to install the class properly, and the message may be resent."

	self class lookupSelector: aMessage selector ifAbsent: [
		Error signal: 'MethodDictionary fault' ].
	
	^ aMessage sentTo: self! !


!Behavior methodsFor: 'method dictionary' stamp: 'FJG 27/Oct/2025, 3:00:08 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel05ClassAndMethodCreation}!
lookupSelector: selector
	"Same as #lookupSelector:ifAbsent:, answering nil if no method is found."
	
	^ self lookupSelector: selector ifAbsent: nil! !


!ContextPart methodsFor: 'controlling' stamp: 'FJG 21/Sep/2026, 3:19:33 pm (UTC)'!
send: selector to: receiver with: arguments lookupIn: lookupClass
	"Simulate the action of sending a message with selector and arguments to rcvr. The argument, lookupClass, is the class in which to lookup the message. This is the receiver's class for normal messages, but for super messages it will be some specific class related to the source method."

	| foundMethod |
	foundMethod := lookupClass lookupSelector: selector ifAbsent: [
		selector == #doesNotUnderstand: ifTrue:
			[self error: 'Recursive message not understood!!' translated].
		
		^ self 
			send: #doesNotUnderstand:
			to: receiver
			with: {(Message selector: selector arguments: arguments) lookupClass: lookupClass}
			lookupIn: lookupClass].
	
	^ self
		executeMethod: foundMethod
		forSelector: selector
		withArgs: arguments
		receiver: receiver! !


!ExtractMethodValidNameGenerator methodsFor: 'validation' stamp: 'FJG 21/Sep/2026, 3:26:34 pm (UTC)'!
isAvailableKeywordName: newKeyword

	^ (methodNode methodClass canUnderstand: newKeyword asSymbol) not! !

