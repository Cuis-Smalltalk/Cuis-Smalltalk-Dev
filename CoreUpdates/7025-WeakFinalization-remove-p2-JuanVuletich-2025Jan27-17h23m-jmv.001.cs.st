'From Cuis7.3 [latest update: #7024] on 27 January 2025 at 5:25:38 pm'!
!classDefinition: #WeakArray category: #'Collections-Weak'!
Array weakSubclass: #WeakArray
	instanceVariableNames: ''
	classVariableNames: 'FinalizationDependents FinalizationLock '
	poolDictionaries: ''
	category: 'Collections-Weak'!
!methodRemoval: EphemeronFinalizationProcess class #doWeakFinalizationWork stamp: 'jmv 1/27/2025 17:23:36'!
EphemeronFinalizationProcess class removeSelector: #doWeakFinalizationWork!
!methodRemoval: WeakArray class #initialize stamp: 'jmv 1/27/2025 17:24:19'!
WeakArray class removeSelector: #initialize!
!methodRemoval: WeakArray class #removeWeakDependent: stamp: 'jmv 1/27/2025 17:24:17'!
WeakArray class removeSelector: #removeWeakDependent:!
!methodRemoval: WeakArray class #addWeakDependent: stamp: 'jmv 1/27/2025 17:24:15'!
WeakArray class removeSelector: #addWeakDependent:!
!methodRemoval: WeakArray class #doWeakFinalizationWork stamp: 'jmv 1/27/2025 17:23:41'!
WeakArray class removeSelector: #doWeakFinalizationWork!
!classDefinition: #WeakArray category: #'Collections-Weak'!
Array weakSubclass: #WeakArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!
WeakArray initialize!

!WeakArray class reorganize!
('finalization' ensureFinalizationComplete finalizationProcess runningFinalizationProcess stopFinalizationProcess)
!

