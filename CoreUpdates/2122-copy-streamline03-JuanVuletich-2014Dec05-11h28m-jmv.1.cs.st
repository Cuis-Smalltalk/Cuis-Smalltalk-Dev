'From Cuis 4.2 of 25 July 2013 [latest update: #2120] on 5 December 2014 at 11:29:07.840368 am'!

!MethodDictionary methodsFor: 'private' stamp: 'jmv 12/5/2014 11:02'!
postCopy

	array _ array copy! !

!methodRemoval: MethodDictionary #copy!
MethodDictionary removeSelector: #copy!

!MethodDictionary reorganize!
('accessing' add: associationAt:ifAbsent: at:ifAbsent: at:put: keyAtIdentityValue:ifAbsent: keyAtValue:ifAbsent:)
('removing' removeAll removeKey:ifAbsent:)
('enumeration' associationsDo: keysAndValuesDo: keysDo: valuesDo:)
('private' fixCollisionsFrom: grow keyAt: methodArray postCopy rehash rehashWithoutBecome removeDangerouslyKey:ifAbsent: scanFor: swap:with:)
('copying')
!

