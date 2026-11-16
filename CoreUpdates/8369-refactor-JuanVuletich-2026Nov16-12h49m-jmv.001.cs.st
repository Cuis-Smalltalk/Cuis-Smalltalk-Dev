'From Cuis7.9 [latest update: #8366] on 16 November 2026 at 12:52:22 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 16/Nov/2026, 3:51:10 pm (UTC)'!
testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes := OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldMethod := cls compiledMethodAt: selector.
					oldCodeString := oldMethod decompileString.
					methodNode := cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod := methodNode generate: #(0 0 0 0 ).
					oldCodeString = newMethod decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	self
		browseMessageList: badOnes
		name: 'Decompiler Discrepancies'.! !

