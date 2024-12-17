'From Cuis7.3 [latest update: #6918] on 13 December 2024 at 12:18:56 pm'!
!classDefinition: #EphemeronIdentityDictionary category: #'System-Finalization'!
IdentityDictionary subclass: #EphemeronIdentityDictionary
	instanceVariableNames: 'mutex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Finalization'!

!EphemeronIdentityDictionary commentStamp: 'eem 10/1/2020 16:31' prior: 0!
An EphemeronIdentityDictionary is an identity dictionary that uses ephemerons for its associations, and hence is able to arrange that its keys are finalizable.  See Ephemeron's class comment.

Instance Variables
	mutex:		<Mutex>

mutex
	- an access protect that prevents the receiver getting corrupted as finalization occurs while lower priority processes are adding and/or rremoving ephemerons.
!


!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:45:00'!
associationAt: key ifAbsent: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super associationAt: key ifAbsent: aBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:27:37'!
at: key ifAbsent: absentBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super at: key ifAbsent: absentBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:25:34'!
at: key ifPresent: presentBlock ifAbsent: absentBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super at: key ifPresent: presentBlock ifAbsent: absentBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:25:23'!
at: key put: anObject
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super at: key put: anObject ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:30:42'!
collect: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super collect: aBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:30:32'!
do: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super do: aBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:49:11'!
keys
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super keys ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:31:01'!
keysAndValuesDo: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super keysAndValuesDo: aBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:26:43'!
keysAndValuesRemove: keyValueBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super keysAndValuesRemove: keyValueBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:31:25'!
keysDo: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super keysDo: aBlock].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:45:40'!
rehash
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super rehash ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:51:39'!
removeAll
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super removeAll ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:26:30'!
removeKey: key ifAbsent: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super removeKey: key ifAbsent: aBlock ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:32:05'!
select: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super select: aBlock].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:46:02'!
size
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super size ].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'jmv 12/13/2024 11:32:16'!
valuesDo: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	| answer |
	mutex critical: [
		answer := super valuesDo: aBlock].
	^ answer! !

!EphemeronIdentityDictionary methodsFor: 'finalization' stamp: 'jmv 12/13/2024 11:52:37'!
finalizeValues
	"legacy systems might invoke this as it's an api on the old WeakRegistry"
	Smalltalk garbageCollect! !

!EphemeronIdentityDictionary methodsFor: 'initialize-release' stamp: 'eem 10/1/2020 10:50'!
init: n
	mutex := Mutex new.
	super init: n! !


!EphemeronIdentityDictionary methodsFor: 'copying' stamp: 'jmv 12/13/2024 11:15:20'!
postCopy
	"Must copy the associations and tell them I (the copy) am their new container"

	mutex critical:
	[
		array := array collect: 
		[ :assoc |
		         assoc ifNotNil:
				[
				        assoc copy container: self ] ] ]! !

!EphemeronIdentityDictionary methodsFor: 'finalization' stamp: 'jmv 12/13/2024 11:15:43'!
finalizeEphemeron: anEphemeron

	"Finalize the ephemeron finalizer and remove it from myself.
	Avoid errors during the finalization to let the finalization process proceed"

	mutex critical: [
		self removeKey: anEphemeron key ifAbsent: []].
! !

!classDefinition: #EphemeronIdentityDictionary category: #'System-Finalization'!
IdentityDictionary subclass: #EphemeronIdentityDictionary
	instanceVariableNames: 'mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Finalization'!

!EphemeronIdentityDictionary reorganize!
('accessing' associationClass)
('adding' add:)
('access protected protocol' associationAt:ifAbsent: at:ifAbsent: at:ifPresent:ifAbsent: at:put: collect: do: keys keysAndValuesDo: keysAndValuesRemove: keysDo: rehash removeAll removeKey:ifAbsent: select: size valuesDo:)
('private' atNewIndex:put:)
('copying' postCopy)
('finalization' finalizeEphemeron: finalizeValues)
('initialize-release' init:)
!

