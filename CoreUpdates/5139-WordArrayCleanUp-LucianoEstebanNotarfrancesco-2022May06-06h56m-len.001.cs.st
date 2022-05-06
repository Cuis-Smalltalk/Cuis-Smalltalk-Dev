'From Cuis 6.0 [latest update: #5138] on 6 May 2022 at 6:57:22 am'!
!methodRemoval: WordArray #at:put: stamp: 'len 5/6/2022 06:56:55'!
WordArray removeSelector: #at:put:!
!methodRemoval: WordArray #basicAt:put: stamp: 'len 5/6/2022 06:56:59'!
WordArray removeSelector: #basicAt:put:!
!methodRemoval: WordArray #at: stamp: 'len 5/6/2022 06:56:52'!
WordArray removeSelector: #at:!
!methodRemoval: WordArray #basicAt: stamp: 'len 5/6/2022 06:56:57'!
WordArray removeSelector: #basicAt:!

!WordArray reorganize!
('accessing' atAllPut: bytesAt: bytesAt:put: bytesPerElement defaultElement)
('private' replaceFrom:to:with:startingAt:)
!

