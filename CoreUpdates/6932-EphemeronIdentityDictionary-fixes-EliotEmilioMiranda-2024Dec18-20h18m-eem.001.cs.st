'From Cuis7.3 [latest update: #6922] on 18 December 2024 at 8:22:43 pm'!

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:47:15'!
associationAt: key ifAbsent: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super associationAt: key ifAbsent: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:26:40'!
at: key ifAbsent: absentBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super at: key ifAbsent: absentBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:29:09'!
at: key ifPresent: presentBlock ifAbsent: absentBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super at: key ifPresent: presentBlock ifAbsent: absentBlock ]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:29:26'!
at: key put: anObject
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super at: key put: anObject]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:29:41'!
collect: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super collect: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:29:53'!
do: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super do: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:30:09'!
keys
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keys]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:30:50'!
keysAndValuesDo: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keysAndValuesDo: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:31:05'!
keysAndValuesRemove: keyValueBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keysAndValuesRemove: keyValueBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:31:51'!
keysDo: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keysDo: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:32:07'!
rehash
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super rehash]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:32:49'!
removeAll
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super removeAll]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:39:36'!
removeKey: key ifAbsent: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super removeKey: key ifAbsent: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:44:56'!
select: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super select: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:46:26'!
valuesDo: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super valuesDo: aBlock]! !

!EphemeronIdentityDictionary methodsFor: 'copying' stamp: 'eem 12/18/2024 12:07:19'!
postCopy
	"Must copy the associations and tell them I (the copy) am their new container"

	mutex := Mutex new.
	mutex critical:
		[array := array collect: 
			[ :assoc |
			assoc ifNotNil:
				[assoc copy container: self ] ] ]! !

!EphemeronIdentityDictionary methodsFor: 'finalization' stamp: 'eem 12/18/2024 11:55:20'!
finalizeEphemeron: anEphemeron

	"Remove the ephemeron from myself.  Note that no finalization of the ephemeron's key is done.
	 Here we are using ephemerons to clean up property tables."

	mutex critical:
		[self removeKey: anEphemeron key ifAbsent: []]! !

!methodRemoval: EphemeronIdentityDictionary #size stamp: 'eem 12/18/2024 20:21:36'!
EphemeronIdentityDictionary removeSelector: #size!
