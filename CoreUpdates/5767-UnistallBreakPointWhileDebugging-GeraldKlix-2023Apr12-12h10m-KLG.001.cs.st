'From Haver 6.0 [latest update: #5760] on 12 April 2023 at 12:09:36 pm'!

!BreakingMethodWrapper methodsFor: 'class membership' stamp: 'KLG 4/12/2023 11:47:59'!
class
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	^ BreakingMethodWrapper! !


!BreakingMethodWrapper methodsFor: 'system primitives' stamp: 'KLG 4/12/2023 11:48:52'!
run: aSelector with: arguments in: aReceiver
	"I am sent by the virtual machine, if an instance of a class other
	than CompiledMethod is bound in a method dictionary.
	
	See SystemDictionary>>#recreateSpecialObjectsArray at index 50."

	| process |
	"Get rid of that break point, we don't want debuggers popping up while debugging."
	self uninstall.
	Smalltalk isDevelopmentEnvironmentPresent ifFalse: [
		^ method valueWithReceiver: aReceiver arguments: arguments ].
	process := Process 		
		forContext:
			(MethodContext
				sender: 
					(MethodContext
						sender: thisContext sender
						receiver: self
						method: self class>>#install
						arguments: #())
				receiver: aReceiver
				method: method
				arguments: arguments)
			priority: Processor activeProcess 
		priority.	
	process	 debugWithTitle:
		'Breakpoint in ' , method methodClass name , '>>#' , method selector.	
	thisContext swapSender: nil.
	Processor activeProcess terminate! !


!BreakingMethodWrapper reorganize!
('system primitives' doesNotUnderstand: run:with:in:)
('printing' printAs:limitedTo: printOn: printStringLimitedTo: printTextLimitedTo:)
('install/uninstall' install uninstall)
('breakpoints' hasBreakpoint)
('initialization' initializeOn:)
('class membership' class)
!

