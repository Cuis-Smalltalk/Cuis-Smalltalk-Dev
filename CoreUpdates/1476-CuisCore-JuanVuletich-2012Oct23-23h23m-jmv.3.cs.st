'From Cuis 4.0 of 21 April 2012 [latest update: #1475] on 23 October 2012 at 11:26:36 pm'!
!classDefinition: #TaskbarTest category: #'Taskbar-Tests'!
TestCase subclass: #TaskbarTest
	instanceVariableNames: 'taskbar needsDelete '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Tests'!

!ProjectX class methodsFor: 'nil' stamp: 'jmv 10/23/2012 23:18'!
ui
	^UIProcess animatedUI! !


!TaskbarTest methodsFor: 'Running' stamp: 'jmv 10/23/2012 23:12'!
tearDown

	needsDelete ifTrue: [
		taskbar delete ]! !


!ScannerTest methodsFor: 'testing' stamp: 'jmv 10/23/2012 23:20'!
testLiteralSymbols

	self assert: ('*+-/\~=<>&@%,|' allSatisfy: [:char | Scanner isLiteralSymbol: (String with: char) asSymbol])
		description: 'single letter binary symbols can be printed without string quotes'.
		
	self assert: (#('x' 'x:' 'x:y:' 'from:to:by:' 'yourself') allSatisfy: [:str | Scanner isLiteralSymbol: str asSymbol])
		description: 'valid ascii selector symbols can be printed without string quotes'.
		
	((32 to: 94), (96 to: 126) collect: [:ascii | Character value: ascii]) ,
	#(':x:yourself' '::' 'x:yourself' '123' 'x0:x1:x2:' 'x.y.z' '1abc' 'a1b0c2' ' x' 'x ' '+x-y' '||' '-' '++' '+' '+/-' '-/+' '<|>' '#x' '()' '[]' '{}' '')
		do: [:str |
			self assert: (Compiler evaluate: str asSymbol printString) = str asSymbol
				description: 'in all case, a Symbol must be printed in an interpretable fashion']! !


!SystemDictionaryTest methodsFor: 'testing' stamp: 'jmv 10/23/2012 23:25'!
testPointersToEachIn
	"
	SystemDictionaryTest new testPointersToEachIn
	"
	| p1 p2 |
	p1 _ (Smalltalk pointersTo: Smalltalk).
	p2 _ (Smalltalk pointersToEachIn: {Smalltalk}) first.
	self assert: p1 = p2! !


!TaskbarTest methodsFor: 'Running' stamp: 'jmv 10/23/2012 23:18'!
setUp

	Taskbar reset.	
	taskbar _ Taskbar singleton.
	needsDelete _ taskbar owner isNil.
	needsDelete ifTrue: [ taskbar openInWorld: ProjectX ui ].
	taskbar visible: false.
	taskbar step! !

!classDefinition: #TaskbarTest category: #'Taskbar-Tests'!
TestCase subclass: #TaskbarTest
	instanceVariableNames: 'taskbar needsDelete'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Tests'!
!methodRemoval: Taskbar #initialize!
Taskbar removeSelector: #initialize!
