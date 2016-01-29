'From Cuis 4.2 of 25 July 2013 [latest update: #2676] on 29 January 2016 at 3:11:46.848459 pm'!

!ChangeRecord methodsFor: 'access' stamp: 'jmv 1/29/2016 15:10'!
compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass! !

