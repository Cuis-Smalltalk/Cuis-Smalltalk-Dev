'From Cuis 4.0 of 21 April 2012 [latest update: #1305] on 13 June 2012 at 8:40:40 am'!

!CompiledMethod methodsFor: 'organization' stamp: 'dkh 4/23/2012 21:01'!
category

	^self methodClass organization categoryOfElement: self selector! !


!SystemDictionary methodsFor: 'accessing' stamp: 'dkh 4/23/2012 21:06'!
classes

	^self classNames collect: [:each | self at: each ]! !

