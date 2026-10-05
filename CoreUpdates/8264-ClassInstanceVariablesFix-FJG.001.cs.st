'From Cuis7.9 [latest update: #8239] on 4 October 2026 at 12:06:48 pm'!

!Metaclass methodsFor: 'instance variables' stamp: 'FJG 16/Sep/2026, 7:48:30 pm (UTC)'!
addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	self instanceVariableNames: self instanceVariablesString, ' ', aString! !

!Metaclass methodsFor: 'instance variables' stamp: 'FJG 16/Sep/2026, 8:25:57 pm (UTC)'!
removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."

	(self instVarNames includes: aString) ifFalse: [
		self error: aString , ' is not one of my instance variables'].
	
	self instanceVariableNames: (' ' join: (self instVarNames copyWithout: aString))! !

