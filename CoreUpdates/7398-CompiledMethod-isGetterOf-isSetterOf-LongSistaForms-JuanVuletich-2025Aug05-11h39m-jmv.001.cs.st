'From Cuis7.5 [latest update: #7397] on 5 August 2025 at 11:51:25 am'!

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 8/5/2025 11:50:33'!
isGetterOf: anInstVarName at: anInstVarIndex
	
	| scanner instruction |
	
	"Quick return. (Short form)."
	(self selector = anInstVarName 
		and: [ self isReturnField 
		and: [ self returnField + 1 = anInstVarIndex ]])
			ifTrue: [ ^true ].
	
	"Long Form"
	self isQuick ifTrue: [ ^false ].
	self selector = anInstVarName ifFalse: [ ^false ].
	scanner := InstructionStream on: self.
	instruction := scanner nextInstruction.
	(instruction selector == #pushReceiverVariable:
	 and: [instruction argument = (anInstVarIndex - 1)]) ifFalse:
		[^false].
	^scanner nextInstruction selector == #methodReturnTop! !

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 8/5/2025 11:41:41'!
isSetterOf: anInstVarName at: anInstVarIndex
	"Check if the receiver is a setter of the give inst var located at instVarIndex.
	 Only look at the bytecode to avoid soiurce code parsing issues."
	| scanner instruction |
	
	self isQuick ifTrue: [ ^false ].
	self selector = (anInstVarName, ':') ifFalse: [ ^false ].
	scanner := InstructionStream on: self.
	instruction := scanner nextInstruction.
	(instruction selector == #pushTemporaryVariable:
	 and: [instruction argument = 0]) ifFalse:
		[^false].
	instruction := scanner nextInstruction.
	(instruction selector == #popIntoReceiverVariable:
	 and: [instruction argument = (anInstVarIndex - 1)]) ifFalse:
		[^false].
	scanner peekInstruction selector == #doNop ifTrue: [
		scanner nextInstruction ].
	^scanner nextInstruction selector == #methodReturnReceiver! !

