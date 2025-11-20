'From Cuis7.5 [latest update: #7706] on 20 November 2025 at 1:20:50 pm'!

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 11/20/2025 13:00:52'!
printQuickOn: aStream indent: tabs

	self isQuick ifTrue:  [
		self isReturnSpecial
			ifTrue: [
				aStream tab: tabs; nextPutAll: 'Quick return ' , 
					(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255);
					newLine ]
			ifFalse: [
				aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'; newLine ]].! !


!CompiledMethod methodsFor: 'inspecting' stamp: 'jmv 11/20/2025 13:17:50'!
explorerContents
	"(CompiledMethod compiledMethodAt: #explorerContents) explore"
	
	^Array streamContents:
		[:s| | tokens |
		tokens := Scanner new scanTokens: (self headerDescription readStream skipTo: $"; upTo: $").
		s nextPut: (ObjectExplorerWrapper
						with: ((0 to: tokens size by: 2) collect:
								[:i| i = 0 ifTrue: [self header] ifFalse: [{tokens at: i - 1. tokens at: i}]])
						name: 'header'
						model: self).
		(1 to: self numLiterals) do:
			[:key|
			s nextPut: (ObjectExplorerWrapper
							with: (self literalAt: key)
							name: ('literal', key printString contractTo: 32)
							model: self)].
		self isQuick
			ifTrue: [s nextPut: (ObjectExplorerWrapper
									with: (String streamContents: [ :strm | self printQuickOn: strm indent: 0])
									name: #symbolic
									model: self)].
		self symbolicLinesDo:
				[:pc :line|
				pc <= 1
					ifTrue:
						[s nextPut: (ObjectExplorerWrapper
										with: line
										name: 'pragma'
										model: self)]
					ifFalse:
						[s nextPut: (ObjectExplorerWrapper
										with: line
										name: pc printString
										model: self)]].
				"should be self numLiterals + 1 * Smalltalk wordSize + 1"
		self endPC + 1
			to: self basicSize
			do: [:key|
				s nextPut: (ObjectExplorerWrapper
								with: (self basicAt: key)
								name: key printString
								model: self)]]! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 11/20/2025 13:10:31'!
longPrintOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue:
		[ self printQuickOn: aStream indent: tabs ].

	self primitive = 0 ifFalse:
		[aStream tab: tabs. self printPrimitiveOn: aStream].
	(InstructionPrinter on: self) indent: tabs; printInstructionsOn: aStream.
! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 11/20/2025 13:15:18'!
symbolicLinesDo: aBlock
	"Evaluate aBlock with each of the lines in the symbolic output."

	| aStream pc |
	aStream := ReadWriteStream on: (String new: 64).

	self primitive ~= 0 ifTrue:
		[self printPrimitiveOn: aStream.
		 aBlock value: 1 value: aStream contents.
		 aStream resetContents].

	pc := self initialPC.
	(InstructionPrinter on: self)
		indent: 0;
		printPC: false; "explorer provides pc anyway"
		printInstructionsOn: aStream
		do:	[:printer :scanner :stream| | line firstLine index |
			line := stream contents allButLast.
			firstLine := line lines first.
			firstLine size < line size ifTrue: [
				line := firstLine, '...'' (continues)'].
			(index := line indexOf: $>) > 0 ifTrue:
				[[(line at: index + 1) isSeparator] whileTrue: [index := index + 1].
				 line := ((line copyFrom: 1 to: index) copyReplaceAll: (String with: Character tab) with: (String new: 8 withAll: Character space)),
						(line copyFrom: index + 1 to: line size)].
			aBlock value: pc value: line.
			pc := scanner pc.
			stream resetContents]! !

