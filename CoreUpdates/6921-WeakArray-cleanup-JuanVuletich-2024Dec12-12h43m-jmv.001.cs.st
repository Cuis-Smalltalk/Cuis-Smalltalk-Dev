'From Cuis7.3 [latest update: #6913] on 12 December 2024 at 12:57:17 pm'!
!classDefinition: #WeakArray category: #'Collections-Weak'!
Array weakSubclass: #WeakArray
	instanceVariableNames: ''
	classVariableNames: 'FinalizationDependents FinalizationLock FinalizationProcess FinalizationSemaphore '
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakArray class methodsFor: 'class initialization' stamp: 'jmv 12/12/2024 12:50:09'!
initialize
	"
	WeakArray initialize.
	"

	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.! !

!methodRemoval: WeakArray class #startUp stamp: 'jmv 12/12/2024 12:51:31'!
WeakArray class removeSelector: #startUp!
!methodRemoval: WeakArray class #restartFinalizationProcess stamp: 'jmv 12/12/2024 12:54:09'!
WeakArray class removeSelector: #restartFinalizationProcess!
!methodRemoval: WeakArray class #pvtCreateTemporaryObjectIn: stamp: 'jmv 12/12/2024 12:52:06'!
WeakArray class removeSelector: #pvtCreateTemporaryObjectIn:!
!methodRemoval: WeakArray class #isFinalizationSupported stamp: 'jmv 12/12/2024 12:45:27'!
WeakArray class removeSelector: #isFinalizationSupported!
!classDefinition: #WeakArray category: #'Collections-Weak'!
Array weakSubclass: #WeakArray
	instanceVariableNames: ''
	classVariableNames: 'FinalizationDependents FinalizationLock'
	poolDictionaries: ''
	category: 'Collections-Weak'!
WeakArray initialize!

!WeakArray class reorganize!
('accessing' addWeakDependent: removeWeakDependent:)
('class initialization' initialize)
('finalization' doWeakFinalizationWork ensureFinalizationComplete finalizationProcess runningFinalizationProcess stopFinalizationProcess)
!

