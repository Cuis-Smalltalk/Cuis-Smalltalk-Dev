'From Cuis6.3 [latest update: #6326] on 23 April 2024 at 11:58:22 am'!

!Compiler methodsFor: 'public access' stamp: 'jmv 4/23/2024 11:57:49'!
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method sourceCodeWithDoIt |
	
	class := (aReceiver ifNotNil: [ aReceiver ] ifNil: [ aContext ifNotNil: [ :c | c receiver ]]) class.
	sourceCodeWithDoIt := Scanner 		doItSourceCodeFor: textOrStream readStream contents in: aContext.
	(sourceCodeWithDoIt beginsWith: Scanner doItSelector)
		ifTrue: [
			methodNode := self compileDoIt: sourceCodeWithDoIt in: class context: aContext notifying: aRequestor ifFail: [^failBlock value]]
		ifFalse: [
			methodNode := self compileNoPattern: sourceCodeWithDoIt in: class context: aContext notifying: aRequestor ifFail: [^failBlock value]].
	method := methodNode generate.
	method methodNode: methodNode.
	
	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile! !

