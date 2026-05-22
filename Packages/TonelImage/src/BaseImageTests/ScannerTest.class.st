Class {
	#name : 'ScannerTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'testing' }
ScannerTest >> testLiteralSymbols [

	self assert: ('*+-/\~=<>&@%,|' allSatisfy: [:char | Symbol isLiteralSymbol: (String with: char) asSymbol])
		description: 'single letter binary symbols can be printed without string quotes'.
		
	self assert: (#('x' 'x:' 'x:y:' 'from:to:by:' 'yourself') allSatisfy: [:str | Symbol isLiteralSymbol: str asSymbol])
		description: 'valid ascii selector symbols can be printed without string quotes'.
		
	((32 to: 94), (96 to: 126) collect: [:ascii | Character codePoint: ascii]) ,
	#(':x:yourself' '::' 'x:yourself' '123' 'x0:x1:x2:' 'x.y.z' '1abc' 'a1b0c2' ' x' 'x ' '+x-y' '||' '-' '++' '+' '+/-' '-/+' '<|>' '#x' '()' '[]' '{}' '')
		do: [:str |
			self assert: (Compiler evaluate: str asSymbol printString) = str asSymbol
				description: 'in all case, a Symbol must be printed in an interpretable fashion']
]
