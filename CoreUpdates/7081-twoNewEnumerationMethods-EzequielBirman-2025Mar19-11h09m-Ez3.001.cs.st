'From Cuis7.3 [latest update: #7080] on 19 March 2025 at 11:13:48 am'!

!Collection methodsFor: 'enumerating' stamp: 'Ez3 3/7/2025 15:16:02'!
reject: rejectBlock thenCollect: collectBlock

	^self select: [ :each | (rejectBlock value: each) not ] thenCollect: collectBlock! !

!Collection methodsFor: 'enumerating' stamp: 'Ez3 3/7/2025 15:31:41'!
reject: rejectBlock thenDo: doBlock

	self do: [ :each | (rejectBlock value: each) ifFalse: [ doBlock value: each ]]! !

