'From Cuis7.7 [latest update: #7899] on 3 May 2026 at 5:42:43 pm'!

!ChangeListElement methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:39:54'!
addToCodeFile: aCodeFile

	self subclassResponsibility! !


!ClassCommentChange methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:39:59'!
addToCodeFile: aCodeFile

	 ^aCodeFile classComment: self! !


!ClassDefinitionChange methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:02'!
addToCodeFile: aCodeFile

	 ^aCodeFile classDefinition: self! !


!ClassDeletionChange methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:05'!
addToCodeFile: aCodeFile

	 ^aCodeFile classRemoval: self! !


!ClassRenamedChange methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:09'!
addToCodeFile: aCodeFile

	 ^aCodeFile classRenamed: self! !


!FeatureProvidesRecord methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:11'!
addToCodeFile: aCodeFile

	 ^aCodeFile provides: self! !


!FeatureRequiresRecord methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:15'!
addToCodeFile: aCodeFile

	 ^aCodeFile requires: self! !


!MethodDefinitionChange methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:18'!
addToCodeFile: aCodeFile

	 ^aCodeFile method: self! !


!MethodDeletionChange methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:21'!
addToCodeFile: aCodeFile

	^aCodeFile removedMethod: self command with: self ! !


!PreambleRecord methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:24'!
addToCodeFile: aCodeFile

	 ^aCodeFile preamble: self! !


!UnspecificRecord methodsFor: 'perform' stamp: 'jmv 3/May/2026 17:40:29'!
addToCodeFile: aCodeFile

	 ^aCodeFile doIt: self! !


!CodeFile methodsFor: 'reading' stamp: 'jmv 3/May/2026 17:40:39'!
buildFrom: changes informingTo: barBlock 

	changes withIndexDo: [ :change :anIndex | 
		barBlock value: anIndex.
		change addToCodeFile: self ].! !

!methodRemoval: UnspecificRecord #performOn: stamp: 'jmv 3/May/2026 17:40:43'!
UnspecificRecord removeSelector: #performOn:!
!methodRemoval: PreambleRecord #performOn: stamp: 'jmv 3/May/2026 17:40:43'!
PreambleRecord removeSelector: #performOn:!
!methodRemoval: MethodDeletionChange #performOn: stamp: 'jmv 3/May/2026 17:40:43'!
MethodDeletionChange removeSelector: #performOn:!
!methodRemoval: MethodDefinitionChange #performOn: stamp: 'jmv 3/May/2026 17:40:42'!
MethodDefinitionChange removeSelector: #performOn:!
!methodRemoval: FeatureRequiresRecord #performOn: stamp: 'jmv 3/May/2026 17:40:42'!
FeatureRequiresRecord removeSelector: #performOn:!
!methodRemoval: FeatureProvidesRecord #performOn: stamp: 'jmv 3/May/2026 17:40:42'!
FeatureProvidesRecord removeSelector: #performOn:!
!methodRemoval: ClassRenamedChange #performOn: stamp: 'jmv 3/May/2026 17:40:42'!
ClassRenamedChange removeSelector: #performOn:!
!methodRemoval: ClassDeletionChange #performOn: stamp: 'jmv 3/May/2026 17:40:42'!
ClassDeletionChange removeSelector: #performOn:!
!methodRemoval: ClassDefinitionChange #performOn: stamp: 'jmv 3/May/2026 17:40:41'!
ClassDefinitionChange removeSelector: #performOn:!
!methodRemoval: ClassCommentChange #performOn: stamp: 'jmv 3/May/2026 17:40:41'!
ClassCommentChange removeSelector: #performOn:!
!methodRemoval: ChangeListElement #performOn: stamp: 'jmv 3/May/2026 17:40:41'!
ChangeListElement removeSelector: #performOn:!
