'From Cuis7.1 [latest update: #6427] on 8 August 2024 at 8:10:36 pm'!

!TestCase class methodsFor: 'instance creation' stamp: 'HAW 8/8/2024 20:04:14'!
debugAsFailure: aSymbol 

	^(self selector: aSymbol) debugAsFailure! !


!Browser methodsFor: 'tdd' stamp: 'HAW 8/8/2024 20:04:36'!
debugMethodTest
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ]
! !


!MethodSet methodsFor: 'run tests' stamp: 'HAW 8/8/2024 20:04:43'!
debugMethodTest
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ]
! !


!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 8/8/2024 20:04:51'!
acceptAndDebugTest
	
	^self acceptAndWithMethodDo: [ :aPotencialTestMethod | 
		aPotencialTestMethod isTestMethod ifTrue: [ 
			aPotencialTestMethod methodClass debugAsFailure: aPotencialTestMethod selector ]]! !


!TestCase methodsFor: 'debugging' stamp: 'HAW 8/8/2024 20:03:50'!
debugAsFailure
	
	^self debugInstance runCaseAsFailure! !

!methodRemoval: TestCase class #debugAsFailure:ifCanNot: stamp: 'HAW 8/8/2024 20:04:59'!
TestCase class removeSelector: #debugAsFailure:ifCanNot:!
!methodRemoval: TestCase class #canNotDebugMethodErrorDescription stamp: 'HAW 8/8/2024 20:05:54'!
TestCase class removeSelector: #canNotDebugMethodErrorDescription!
!methodRemoval: TestCase #debugAsFailureIfCanNot: stamp: 'HAW 8/8/2024 20:06:18'!
TestCase removeSelector: #debugAsFailureIfCanNot:!
!methodRemoval: TestCase #signalCanNotDebugMethod stamp: 'HAW 8/8/2024 20:05:10'!
TestCase removeSelector: #signalCanNotDebugMethod!
!methodRemoval: TestCase #canNotDebugMethodErrorDescription stamp: 'HAW 8/8/2024 20:05:57'!
TestCase removeSelector: #canNotDebugMethodErrorDescription!
!methodRemoval: TestCase #ifCanNotDebugDo: stamp: 'HAW 8/8/2024 20:06:24'!
TestCase removeSelector: #ifCanNotDebugDo:!
