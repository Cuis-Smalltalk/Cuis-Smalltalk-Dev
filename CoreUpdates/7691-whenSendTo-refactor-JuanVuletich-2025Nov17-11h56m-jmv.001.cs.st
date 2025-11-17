'From Cuis7.5 [latest update: #7690] on 17 November 2025 at 12:00:11 pm'!

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
	self
		when: anEventNameSymbol
		send: aMessageSelector
		to: anObserver
		withArguments: #()! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
with: messageArgument
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."

	self
		when: anEventNameSymbol
		send: aMessageSelector
		to: anObserver
		withArguments: (Array with: messageArgument)! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
withArguments: messageArgumentsArray
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
	self
		when: anEventNameSymbol
		evaluate: (WeakMessageSend
			receiver: anObserver
			selector: aMessageSelector
			arguments: messageArgumentsArray)! !

