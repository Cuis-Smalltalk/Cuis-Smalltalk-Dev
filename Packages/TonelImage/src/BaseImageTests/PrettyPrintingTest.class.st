Class {
	#name : 'PrettyPrintingTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'testing' }
PrettyPrintingTest >> test1 [
	"
	self new test1
	"
	| prettyPrinted source |

	source := 
'sample1: x
	x with: [
		self print.
		self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test2 [
	"
	self new test2
	"
	| prettyPrinted source |

	source := 
'sample2: x
	x
		print;
		with: [ :a :b :c | | d e f |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test3 [
	"
	self new test3
	"
	| prettyPrinted source |

	source := 
'sample3: x
	x
		print;
		with: [ :a :b :c |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test4 [
	"
	self new test4
	"
	| prettyPrinted source |

	source := 
'sample4: x
	x
		print;
		with: [ | d e f |
			self print.
			self print.
			self do: [ :each |
				self print.
				self print.
				x size + each size ]].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test5 [
	"
	self new test5
	"
	| prettyPrinted source |

	source := 
'sample5: x
	x
		print;
		print;
		print;
		with: [
			self print.
			self print ]
		do: [
			self print.
			self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test6 [
	"
	self new test6
	"
	| prettyPrinted source |

	source := 
'sample6: x
	x
		with: [
			self print.
			self print ]
		do: [
			self print.
			self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test7 [
	"
	self new test7
	"
	| prettyPrinted source |

	source := 
'sample7: x
	x with: [ self print ]  do: [ self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]

{ #category : 'testing' }
PrettyPrintingTest >> test8 [
	"
	self new test8
	"
	| prettyPrinted source |

	source := 
'sample8: x
	x
		print;
		with: [ self print ] do: [ self print ].'.

	prettyPrinted := Compiler format: source in: PrettyPrintingTest notifying: nil.

	self assert: prettyPrinted equals: source
]
