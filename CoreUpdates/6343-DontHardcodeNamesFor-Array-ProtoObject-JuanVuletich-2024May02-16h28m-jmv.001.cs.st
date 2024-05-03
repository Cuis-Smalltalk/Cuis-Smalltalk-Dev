'From Cuis6.3 [latest update: #6343] on 2 May 2024 at 4:29:32 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/2/2024 16:28:04'!
definition
	"Answer a String that defines the receiver."

	^String streamContents: [ :strm |
		strm
			nextPutAll: (superclass
				ifNotNil: [ superclass name ]
				"Avoid hardcoding 'ProtoObject', for robusness against improbable rename."
				ifNil: [ ProtoObject name asString ]);
			nextPutAll: self kindOfSubclass;
			store: self name.
		strm
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString.
		strm
			newLine;
			tab;
			nextPutAll: 'category: ';
			store: self category asString.

		superclass ifNil: [ 
			strm nextPutAll: '.'; newLine.
			strm nextPutAll: self name.
			strm space; nextPutAll: 'superclass: nil' ]]! !


!BraceNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/2/2024 16:29:02'!
sizeCodeForValue: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	emitNode := elements size <= 4
		ifTrue: ["Short form: Array braceWith: a with: b ... "
				MessageNode new
					"Avoid hardcoding #Array, for robusness against improbable rename."
					receiver: (encoder encodeVariable: Array name)
					selector: (self selectorForShortForm: elements size)
					arguments: elements precedence: 3 from: encoder]
		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
				CascadeNode new
					receiver: (MessageNode new
								"Avoid hardcoding #Array, for robusness against improbable rename."
								receiver: (encoder encodeVariable: Array name)
								selector: #braceStream:
								arguments: (Array with: (encoder encodeLiteral: elements size))
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt | MessageNode new receiver: nil
														selector: #nextPut:
														arguments: (Array with: elt)
														precedence: 3 from: encoder])
								copyWith: (MessageNode new receiver: nil
														selector: #braceArray
														arguments: (Array new)
														precedence: 1 from: encoder))].
	^emitNode sizeCodeForValue: encoder! !

