'From Cuis 5.0 of 7 November 2016 [latest update: #3379] on 27 July 2018 at 9:49:22 am'!

!ClassDescription methodsFor: 'accessing' stamp: 'jmv 7/27/2018 09:42:53'!
comment: aStringOrText
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asString.! !

!ClassDescription methodsFor: 'accessing' stamp: 'jmv 7/27/2018 09:41:06'!
comment: aStringOrText stamp: aStamp
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asString stamp: aStamp.! !


!PseudoClass methodsFor: 'class' stamp: 'jmv 7/27/2018 09:43:05'!
comment: aString
	self classComment: aString asString! !

