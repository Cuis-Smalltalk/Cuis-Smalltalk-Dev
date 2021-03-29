'From Cuis 5.0 [latest update: #4550] on 29 March 2021 at 10:28:21 am'!

!Compiler class methodsFor: 'evaluating' stamp: 'HAW 3/29/2021 10:26:03'!
evaluate: textOrString 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	"See SHST80RangeType>>#receiverAsNumber - Hernan"
	^[ self evaluate: textOrString for: nil logged: false ]
		on: SyntaxErrorNotification 
		do: [ :anError | anError return: nil ]! !


!SHST80RangeType methodsFor: 'range type - private' stamp: 'HAW 3/29/2021 10:16:33'!
receiverAsNumber

	"if the user typed 1xe for example, asNumber will throw an exception because it is not a valid number
	even though the SHParser recognized it as such. I return nil because it is not really a number.
	Thank you Luciano for reporting the bug - Hernan"
	^[ (self sourceCodeIn: receiverRange) asNumber ]
		on: Error
		do: [ :anError | anError return: nil ]! !

