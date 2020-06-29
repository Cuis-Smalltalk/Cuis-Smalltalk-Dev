'From Cuis 5.0 [latest update: #4242] on 27 June 2020 at 7:09:35 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 19:08:46'!
isLiteralExpression

	^ hereType == #string or: [hereType == #number or: [hereType == #literal]]! !

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 19:09:05'!
isLiteralNumberExpression

	^ here == #- and: [tokenType == #number]! !

