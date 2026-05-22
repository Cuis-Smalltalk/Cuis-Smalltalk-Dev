Class {
	#name : 'LiteralRefLocatorTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Methods',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Methods'
}

{ #category : 'tests' }
LiteralRefLocatorTest >> testFindLiteralsInBytecode [
	"Create a method referencing integers, characters, special selectors and nil, true & false.
	 Compile it and check that the objects are found."
	| source binarySpecials integers characters |
	binarySpecials := Smalltalk specialSelectors select: [:s| s isSymbol and: [s isInfix]].
	"-65536 to 65535 in powers of two"
	integers := ((16 to: 1 by: -1) collect: [:power| (2 raisedTo: power) negated]),
				((0 to: 16) collect: [:power| (2 raisedTo: power) - 1]).
	"some printable characters; alas none have code > 255"
	characters := (0 to: 65535)
					select: [:n| (n between: 132 and: 160) not "these have no glyph in typical fonts"
								and: [(Character value: n) shouldBePrintedAsLiteral]]
					thenCollect: [:n| Character value: n].
	[characters size > 32] whileTrue:
		[characters := (1 to: characters size by: 2) collect: [:i| characters at: i]].
	
	#(('' '') ('^[' ']')) do: "And the locators should work whether in a block or not"
		[:pFixes|
		source := String streamContents:
					[:s| | binaries |
					binaries := binarySpecials readStream.
					s nextPutAll: 'exampleMethod'; cr; tab; nextPutAll: pFixes first.
					integers
						do: [:n| s print: n]
						separatedBy:
							[binaries atEnd ifTrue: [binaries reset].
							 s space; nextPutAll: binaries next; space].
					s nextPut: $.; cr; tab.
					s nextPut: ${; space.
					characters
						do: [:c| s print: c]
						separatedBy: [s nextPut: $.; space].
					s space; nextPut: $}; nextPut: $.; cr; tab.
					s nextPutAll: 'true ifTrue: [^nil] ifFalse: [^false]'; nextPutAll: pFixes last].
		BytecodeEncoder subclasses do:
			[:encoderClass| | method |
			method := (Parser new
								encoderClass: encoderClass;
								parse: source class: self class)
							generate.
			binarySpecials, integers, characters, #(nil false true) do: 
				[:literal | self assert: (method hasLiteral: literal)].

			"Now test for false positives..."
			integers, characters, #(nil false true) do:
				[:literal| | simpleSource simpleMethod |
				simpleSource := String streamContents:
									[:s| s nextPutAll: 'exampleMethod'; cr;tab; nextPutAll: pFixes first; print: literal; nextPutAll: ' class'; nextPutAll: pFixes last].
				simpleMethod := (Parser new
										encoderClass: encoderClass;
										parse: simpleSource class: self class)
									generate.
				binarySpecials, integers, characters, #(nil false true) do: [:anyLiteral |
					anyLiteral == literal
						ifTrue: [self assert: (simpleMethod hasLiteral: anyLiteral)]
						ifFalse: [self deny: (simpleMethod hasLiteral: anyLiteral)]]]]]
]

{ #category : 'tests' }
LiteralRefLocatorTest >> testThoroughFindLiteralsInBytecode [
	"Create a method referencing integers, characters, special selectors and nil, true & false.
	 Compile it and check that the objects are found."
	| literals problem |
	literals := #(-1 0 1 $0 $1 1.0 #[1 2 3 4] 'one' #one nil true false NaN).
	problem := Float bindingOf: #NaN.
	BytecodeEncoder subclasses do:
		[:encoderClass| | method |
		#(('' '') ('^[' ']')) do: "And the locators should work whether in a block or not"
			[:pFixes|
			"NaN's binding should still be found even though (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)"
			method := (Parser new
								encoderClass: encoderClass;
								parse: 'foo ', pFixes first, '^NaN', pFixes last class: Float)
							generate.

			"Check our problematic case first."
			 self assert: (method hasLiteral: problem).
			 "The selector of a method should never be found (unless it occurs as a literal or in a pragma)"
			 self deny: (method hasLiteral: method selector).

			"All the literals should be found in a thorough search, but not otherwise"
			method := (Parser new
								encoderClass: encoderClass;
								parse: 'foo ', pFixes first, '^', literals storeString, pFixes last class: Float)
							generate.
			literals, {problem key} do:
				[:literal | self assert: (method hasLiteral: literal)]].

		"Likewise if in a pragma"
		method := (Parser new
							encoderClass: encoderClass;
							parse: 'foo <pragma: ', literals storeString, ' with: ', problem key storeString, '>' class: Float)
						generate.
		literals, {problem key} do:
			[:literal | self assert: (method hasLiteral: literal)]]
]
