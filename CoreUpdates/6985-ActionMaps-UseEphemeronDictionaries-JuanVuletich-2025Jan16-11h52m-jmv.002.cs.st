'From Cuis7.3 [latest update: #6982] on 16 January 2025 at 12:09:31 pm'!

!ActiveModel class methodsFor: 'initialization' stamp: 'jmv 1/16/2025 12:06:37'!
recreateActionMapsCollection
	"
	ActiveModel recreateActionMapsCollection
	"
	| newOne |
	newOne := EphemeronIdentityDictionary new.
	ActionMaps keysAndValuesDo: [ :k :v |
		newOne at: k put: v ].
	ActionMaps := newOne.! !


!ActiveModel class methodsFor: 'accessing' stamp: 'jmv 1/16/2025 12:06:43'!
actionMaps

	ActionMaps ifNil: [
		ActionMaps := EphemeronIdentityDictionary new ].
	^ActionMaps! !

!ActiveModel class methodsFor: 'initialization' stamp: 'jmv 1/16/2025 12:58:12'!
flushEventSystem
	"ActiveModel flushEventSystem"
	self actionMaps keysAndValuesDo: [ :rcvr :evtDict | 
		rcvr ifNotNil:[
			"make sure we don't modify evtDict while enumerating"
			evtDict keys do: [ :evtName | | msgSet |
				msgSet := evtDict at: evtName ifAbsent: nil.
				msgSet ifNil: [ rcvr removeActionsForEvent: evtName]]]].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ActiveModel recreateActionMapsCollection!

