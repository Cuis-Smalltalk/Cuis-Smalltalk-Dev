'From Cuis 6.0 [latest update: #5759] on 11 April 2023 at 1:07:35 pm'!

!BreakingMethodWrapper methodsFor: 'system primitives' stamp: 'jmv 4/11/2023 12:56:32'!
run: aSelector with: arguments in: aReceiver
	"I am sent by the virtual machine, if an instance of a class other
	than CompiledMethod is bound in a method dictionary.
	
	See SystemDictionary>>#recreateSpecialObjectsArray at index 50."

	| process |
	Smalltalk isDevelopmentEnvironmentPresent ifFalse: [
		"Get rid of that nuisance."
		self uninstall.
		^ method valueWithReceiver: aReceiver arguments: arguments ].
	process := Process 		
		forContext:
			(MethodContext
				sender: thisContext sender
				receiver: aReceiver
				method: method
				arguments: arguments)
			priority: Processor activeProcess 
		priority.	
	process	 debugWithTitle:
		'Breakpoint in ' , method methodClass name , '>>#' , method selector.	
	thisContext swapSender: nil.
	Processor activeProcess terminate! !

