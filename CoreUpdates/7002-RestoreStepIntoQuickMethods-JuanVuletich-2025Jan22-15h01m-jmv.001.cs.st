'From Cuis7.3 [latest update: #7000] on 22 January 2025 at 3:02:14 pm'!

!ContextPart methodsFor: 'controlling' stamp: 'jmv 1/22/2025 15:01:43'!
executeMethod: meth forSelector: selector withArgs: arguments receiver: rcvr

	| primIndex val ctxt isPrimitive |
	(self objectClass: meth) isCompiledMethodClass ifFalse:
		["Object as Methods (OaM) protocol: 'The contract is that, when the VM encounters an ordinary object (rather than a compiled method) in the method dictionary during lookup, it sends it the special selector #run:with:in: providing the original selector, arguments, and receiver.'. DOI: 10.1145/2991041.2991062."
		^self send: #run:with:in:
			to: meth
			with: {selector. arguments. rcvr}].
	
	meth numArgs = arguments size ifFalse:
		[^ self class primitiveFailTokenFor: #'bad number of arguments'].
	primIndex := meth primitive.
	isPrimitive := primIndex > 0 and: [ meth isQuick not ].
	isPrimitive ifTrue:
		[val := self doPrimitive: primIndex method: meth receiver: rcvr args: arguments.
		(self isPrimFailToken: val) ifFalse:
			[^val]].
	
	ctxt := self activateMethod: meth withArgs: arguments receiver: rcvr.
	isPrimitive ifTrue:
		[ctxt failPrimitiveWith: val].
	
	^ctxt! !

