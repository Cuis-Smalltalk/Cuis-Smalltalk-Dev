'From Cuis7.5 [latest update: #7336] on 8 July 2025 at 11:36:54 am'!

!Object methodsFor: 'events-removing' stamp: 'jmv 7/8/2025 11:32:33'!
removeAllActions
	"Empty our #actionMap.
	Use this method when done with some object holding dependents. Do not call #releaseActionMap.
	Actually, usually you don't need to call any of these. Our #actionMap is cleared autonatically when
	we are collected,	as ActionMaps is an EphemeronIdentityDictionary.
	"
	self removeActionsSatisfying: [ :any | true ]! !


!ActiveModel methodsFor: 'accessing' stamp: 'jmv 7/8/2025 11:31:12'!
removeAllActions

    actionMap := nil! !


!Object methodsFor: 'events-removing' stamp: 'jmv 7/8/2025 11:27:36'!
releaseActionMap
	"Modifying ActionMaps is kinda dangerous.
	It is an instance of EphemeronIdentityDictionary.
	Its integrity is protected by a Mutex, meaning that nested accesses from the same process could
	break it, leading to system crash if not done very carefully.
	As this is public protocol, and in any case EphemeronIdentityDictionary are automatically cleared
	when keys are collected, the risk is not worth it.
	"
	"ActiveModel releaseActionMapFor: self."
	self removeAllActions.! !

