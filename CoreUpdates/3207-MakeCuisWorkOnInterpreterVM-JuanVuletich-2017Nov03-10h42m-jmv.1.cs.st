'From Cuis 5.0 of 7 November 2016 [latest update: #3206] on 3 November 2017 at 10:42:58 am'!

!Object methodsFor: 'system primitives' stamp: 'jmv 12/31/1969 21:14:14'!
instVarAtPrim73: index
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	variables corresponds to the named instance variables. Fail if the index 
	is not an Integer or is not the index of a fixed variable. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize! !

!Object methodsFor: 'system primitives' stamp: 'jmv 12/31/1969 21:15:46'!
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


!Object methodsFor: 'system primitives' stamp: 'jmv 12/31/1969 21:17:57'!
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
	self primitiveFailed! !

!Object methodsFor: 'system primitives' stamp: 'jmv 12/31/1969 21:17:51'!
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
	self primitiveFailed! !


!Character methodsFor: 'accessing' stamp: 'jmv 8/11/2016 09:46:09'!
nonImmediateNumericValue
	"Answer the numeric value of the receiver, if instances happen to be regular (i.e. not in Spur)"

	^self instVarAt: 1! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/31/1969 21:13:09'!
interpreterClass
	"Interpreter class (Cog VM only)
	nil for classic Interpreter VM
	"
	^self getSystemAttribute: 1007! !

