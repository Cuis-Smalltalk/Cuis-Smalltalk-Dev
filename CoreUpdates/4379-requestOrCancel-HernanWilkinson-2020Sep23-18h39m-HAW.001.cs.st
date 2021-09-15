'From Cuis 5.0 [latest update: #4365] on 23 September 2020 at 7:26:03 pm'!

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:49:48'!
defaultInitialAnswer

	^''! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:49:25'!
request: queryString orCancel: cancelBlock

	^self request: queryString initialAnswer: self defaultInitialAnswer orCancel: cancelBlock ! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:47:29'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock
	
	^ self 
		request: queryString 
		centeredAt: self runningWorld activeHand morphPosition 
		initialAnswer: defaultAnswer 
		validationBlock: [:aString| true] 
		acceptBlock: acceptBlock 
		cancelBlock: []! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:47:57'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock

	| morph world |

	morph _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer.
	(world _ self runningWorld) addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	
	^ morph getUserResponseOrCancel: cancelBlock! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'HAW 9/23/2020 18:48:44'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	
	^ self 
		request: queryString 
		centeredAt: self runningWorld activeHand morphPosition 
		initialAnswer: defaultAnswer 
		validationBlock: validationBlock 
		acceptBlock: acceptBlock
		cancelBlock: cancelBlock! !

