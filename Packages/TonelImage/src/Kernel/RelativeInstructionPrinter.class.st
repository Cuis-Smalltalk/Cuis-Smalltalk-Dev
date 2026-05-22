Class {
	#name : 'RelativeInstructionPrinter',
	#superclass : 'InstructionPrinter',
	#instVars : [
		'printCode',
		'labels',
		'labelling'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instruction decoding' }
RelativeInstructionPrinter >> jump: offset [
	"Print the Unconditional Jump bytecode."

	labelling
		ifTrue:
			[labels at: scanner pc + offset + 1 put: true.
			 self print: 'jumpBy: ', offset printString,
				' to: ', (scanner pc + offset - method initialPC) printString]
		ifFalse:
			[self print: 'jumpTo: ', (labels at: scanner pc + offset + 1)]
]

{ #category : 'instruction decoding' }
RelativeInstructionPrinter >> jump: offset if: condition  [
	"Print the Conditional Jump bytecode."

	labelling
		ifTrue:
			[labels at: scanner pc + offset + 1 put: true.
			 self print: 
				(condition ifTrue: ['jumpTrueBy: '] ifFalse: ['jumpFalseBy: ']), offset printString,
				' to: ', (labelling
							ifTrue: [(scanner pc + offset - method initialPC) printString]
							ifFalse: [labels at: scanner pc + offset])]
		ifFalse:
			[self print: 
				(condition ifTrue: ['jumpTrueTo: '] ifFalse: ['jumpFalseTo: ']), (labels at: scanner pc + offset + 1)]
]

{ #category : 'printing' }
RelativeInstructionPrinter >> print: instruction  [
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	labelling
		ifTrue: [stream print: oldPC - method initialPC; space]
		ifFalse: [stream tab].
	stream tab: (innerIndents at: oldPC).
	self printCode ifTrue: [
		stream nextPut: $<.
		 oldPC to: scanner pc - 1 do: [ :i |
			code := (method at: i) printStringBase: 16.
			stream
				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);
				nextPut: code last;
				space].
		 stream skipBack; nextPut: $>; space].
	stream nextPutAll: instruction.
	stream newLine.
	labelling ifFalse: [
		(labels at: scanner pc + 1) ~~ false ifTrue:
			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; newLine]].
	oldPC := scanner pc
]

{ #category : 'printing' }
RelativeInstructionPrinter >> printCode [
	^printCode ~~ false
]

{ #category : 'initialization' }
RelativeInstructionPrinter >> printCode: aBoolean [
	printCode := aBoolean
]

{ #category : 'printing' }
RelativeInstructionPrinter >> printInstructionsOn: aStream [
	"Append to the stream, aStream, a description of each bytecode in the instruction stream."
	
	| label |
	labelling := true.
	labels := Array new: method size + 1 withAll: false.
	super printInstructionsOn: String writeStream.
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream
]

{ #category : 'printing' }
RelativeInstructionPrinter >> printInstructionsOn: aStream do: aBlock [
	"Append to the stream, aStream, a description of each bytecode in the instruction stream.
	  Evaluate aBlock with the receiver, the scanner and the stream after each instruction."
	
	| label |
	labelling := true.
	labels := Array new: method size withAll: false.
	super printInstructionsOn: String writeStream do: [:ig :no :re|].
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream do: aBlock
]

{ #category : 'instruction decoding' }
RelativeInstructionPrinter >> send: selector super: supered numArgs: numArgs [
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered
				ifTrue: ['superSend: ']
				ifFalse: ['send: '])
			, selector storeString
			, (numArgs = 1
				ifTrue: [' (1 arg)']
				ifFalse: [' (', numArgs printString, ' args)'])
]
