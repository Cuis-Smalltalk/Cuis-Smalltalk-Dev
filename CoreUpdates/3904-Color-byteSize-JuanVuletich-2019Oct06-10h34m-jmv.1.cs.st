'From Cuis 5.0 [latest update: #3903] on 6 October 2019 at 10:34:39 am'!

!Float32SlotsObject methodsFor: 'accessing' stamp: 'jmv 10/6/2019 10:34:22'!
byteSize
	^self size * 4! !


!Float32SlotsObject reorganize!
('accessing' byteSize)
('private' slotAt: slotAt:put:)
!

