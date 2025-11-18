'From Cuis7.5 [latest update: #7641] on 25 October 2025 at 8:15:24 pm'!

!Browser methodsFor: 'running tests' stamp: 'HAW 10/25/2025 20:13:20'!
debugMethodTest
	
	currentCompiledMethod ifNotNil: [ 
		currentCompiledMethod isTestMethod ifTrue: [
			currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ]]
! !

!Browser methodsFor: 'running tests' stamp: 'HAW 10/25/2025 20:14:49'!
runMethodTest
	
	currentCompiledMethod ifNotNil: [ | suite |
		suite := TestSuite forCompiledMethod: currentCompiledMethod.
		(ProgressiveTestRunner for: suite) value ]
		! !

