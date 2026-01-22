'From Cuis7.7 [latest update: #7804] on 22 January 2026 at 12:21:44 pm'!

!Object methodsFor: 'system primitives' stamp: 'jmv 22/Jan/2026 12:21:19'!
instVarAt: index
	"Primitive. Answer a fixed variable in an object. The numbering of the variables
	 corresponds to the named instance variables, followed by the indexed instance
	 variables. Fail if the index is not an Integer or is not the index of a fixed variable.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 173 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim73: index ].
	self primitiveFailed.! !

!Object methodsFor: 'system primitives' library: #MinimalKernel01 stamp: 'jmv 22/Jan/2026 12:21:08'!
instVarAt: index put: anObject
	"Primitive. Store a value into a fixed variable in an object. The numbering of the
	 variables corresponds to the named instance variables, followed by the indexed
	 instance variables. Fail if the index is not an Integer or is not the index of a fixed
	 variable. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim74: index put: anObject ].
	self primitiveFailed.! !

!Object methodsFor: 'system primitives' stamp: 'jmv 22/Jan/2026 12:20:52'!
instVarAtPrim73: index
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	variables corresponds to the named instance variables. Fail if the index 
	is not an Integer or is not the index of a fixed variable. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize! !

!Object methodsFor: 'system primitives' stamp: 'jmv 22/Jan/2026 12:21:26'!
instVarAtPrim74: anInteger put: anObject
	"Primitive. Store a value into a fixed variable in the receiver. The 
	numbering of the variables corresponds to the named instance variables. 
	Fail if the index is not an Integer or is not the index of a fixed variable. 
	Answer the value stored as the result. Using this message violates the 
	principle that each object has sovereign control over the storing of 
	values into its instance variables. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 74>
	"Access beyond fixed fields"
	^self basicAt: anInteger - self class instSize put: anObject! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 22/Jan/2026 12:21:36'!
interpreterClass
	"Interpreter class (Cog VM only)
	nil for classic Interpreter VM
	"
	^self getSystemAttribute: 1007! !

