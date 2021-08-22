'From Cuis 5.0 [latest update: #4787] on 22 August 2021 at 1:51:43 pm'!

!LargePositiveInteger methodsFor: 'testing' stamp: 'jmv 8/22/2021 13:50:42'!
hasContentsInExplorer
	^true! !


!Collection methodsFor: 'testing' stamp: 'jmv 8/22/2021 13:42:46'!
hasContentsInExplorer

	^self class isPointers and: [ self size between: 1 and: 100 ]! !

!methodRemoval: Dictionary #hasContentsInExplorer stamp: 'jmv 8/22/2021 13:49:32'!
Dictionary removeSelector: #hasContentsInExplorer!
!methodRemoval: Set #hasContentsInExplorer stamp: 'jmv 8/22/2021 13:49:29'!
Set removeSelector: #hasContentsInExplorer!
!methodRemoval: OrderedCollection #hasContentsInExplorer stamp: 'jmv 8/22/2021 13:49:19'!
OrderedCollection removeSelector: #hasContentsInExplorer!
!methodRemoval: Integer #hasContentsInExplorer stamp: 'jmv 8/22/2021 13:50:47'!
Integer removeSelector: #hasContentsInExplorer!
