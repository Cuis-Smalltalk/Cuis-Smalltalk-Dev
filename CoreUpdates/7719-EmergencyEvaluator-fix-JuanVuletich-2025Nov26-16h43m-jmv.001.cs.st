'From Cuis7.5 [latest update: #7718] on 26 November 2025 at 4:43:41 pm'!

!Object methodsFor: 'private' stamp: 'jmv 11/26/2025 16:37:31'!
primitiveError: aString signalContext: signalContext
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| emergencyEvaluator lines r context i |
	r := `10@10` extent: (Display extent -100 min: `1700@5000`).
	lines := r height // FontFamily defaultLineSpacing.
	emergencyEvaluator := Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	
	context := signalContext.
	i := 1.
	[context notNil and: [i < (30 min: lines - 15)]] whileTrue: [
		i := i + 1.
		emergencyEvaluator
			print: context;
			newLine.
		context := context sender ].
	
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint.! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 11/26/2025 15:07:18'!
openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil

	[
		bool
			ifTrue: [
				PreDebugWindow
					open: self
					label: label
					message: preDebugMessageOrNil ]
			ifFalse: [ self openFullMorphicLabel: label ]
	]
		on: UnhandledError
		do: [ :exOuter | | errorDescription |
			errorDescription := 'Error while trying to open Debugger' , String newLineString , 
				'Orginal error: ' , label asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
			errorDescription print.
			self primitiveError: errorDescription signalContext: exOuter signalerContext ]! !

!methodRemoval: Object #primitiveError: stamp: 'jmv 11/26/2025 16:43:06'!
Object removeSelector: #primitiveError:!
