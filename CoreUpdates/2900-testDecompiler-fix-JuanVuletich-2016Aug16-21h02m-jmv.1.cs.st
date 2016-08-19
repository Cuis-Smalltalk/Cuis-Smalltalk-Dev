'From Cuis 4.2 of 25 July 2013 [latest update: #2899] on 16 August 2016 at 9:03:06 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/16/2016 20:51:27'!
testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					oldMethod _ cls compiledMethodAt: selector.
"					oldCodeString _ (cls decompilerClass new"
	"See comment at #useOldCompiler"
					oldCodeString _ (Smalltalk clsDecompiler new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
"						(cls decompilerClass new"
	"See comment at #useOldCompiler"
						(Smalltalk clsDecompiler new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Decompiler Discrepancies'.! !

