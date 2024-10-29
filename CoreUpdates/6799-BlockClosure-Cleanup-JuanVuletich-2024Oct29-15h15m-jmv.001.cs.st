'From Cuis7.1 [latest update: #6798] on 29 October 2024 at 3:20:31 pm'!

!BlockClosure methodsFor: 'evaluating' stamp: 'eem 10/29/2024 15:16:35'!
value
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 201>

	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' stamp: 'eem 10/29/2024 15:14:58'!
value: firstArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the argument and copied values to the activation
	 as its argument and copied temps. Primitive. Essential."
	<primitive: 202>
	
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' stamp: 'eem 10/29/2024 15:16:47'!
value: firstArg value: secondArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 203>

	numArgs ~= 2 ifTrue:
		[self numArgsError: 2].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' stamp: 'eem 10/29/2024 15:15:51'!
value: firstArg value: secondArg value: thirdArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 204>
	
	numArgs ~= 3 ifTrue:
		[self numArgsError: 3].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' stamp: 'eem 10/29/2024 15:17:05'!
value: firstArg value: secondArg value: thirdArg value: fourthArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 205>

	numArgs ~= 4 ifTrue:
		[self numArgsError: 4].
	self primitiveFailed.! !

!BlockClosure methodsFor: 'evaluating' stamp: 'eem 10/29/2024 15:18:25'!
valueWithArguments: anArray
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments in an anArray and copied values to
	 the activation as its arguments and copied temps. Primitive. Essential."
	<primitive: 206>

	numArgs ~= anArray size ifTrue:
		[self numArgsError: anArray size].
	self primitiveFailed! !

